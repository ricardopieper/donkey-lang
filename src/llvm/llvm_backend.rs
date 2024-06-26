use core::panic;
use std::collections::{HashMap, VecDeque};
use std::error::Error;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};

use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, FunctionValue, GlobalValue,
    PointerValue, StructValue,
};
use inkwell::{AddressSpace, OptimizationLevel};

use crate::ast::lexer::Operator;
use crate::compiler::layouts::FunctionLayout;
use crate::interner::InternedString;
use crate::llvm::linker::{link, LinkerError};

use crate::semantic::mir::{
    LiteralMIRExpr, MIRBlock, MIRBlockFinal, MIRBlockNode, MIRExpr, MIRExprLValue, MIRExprRValue,
    MIRScope, MIRTypedBoundName, ScopeId,
};

use crate::types::type_instance_db::{StructMember, TypeInstance, TypeInstanceId};
use crate::{semantic::mir::MIRTopLevelNode, types::type_instance_db::TypeInstanceManager};

#[allow(clippy::enum_variant_names)] //RValue and LValue are known terms, let me use it
pub enum LlvmExpression<'ctx> {
    RValue(BasicValueEnum<'ctx>),
    LValue(PointerValue<'ctx>),
    LoadedLValue(BasicValueEnum<'ctx>, PointerValue<'ctx>),
}

impl<'ctx> LlvmExpression<'ctx> {
    //If this is a variable or a deref expression result or a field access, we load it. Otherwise just return it.

    //@TODO maybe officialize these state transitions as types without possibility of panic.
    pub fn load_if_lvalue(self, builder: &Builder<'ctx>) -> LlvmExpression<'ctx> {
        match self {
            Self::LValue(ptr) => Self::LoadedLValue(builder.build_load(ptr, "loaded_ptr"), ptr),
            Self::RValue(val) => Self::RValue(val),
            Self::LoadedLValue(val, ptr) => Self::LoadedLValue(val, ptr),
        }
    }

    pub fn expect_rvalue(self) -> BasicValueEnum<'ctx> {
        match self {
            Self::LValue(ptr) => panic!("Expected an rvalue but got ${ptr:?}, need to deref first"),
            Self::RValue(val) => val,
            Self::LoadedLValue(val, _) => val,
        }
    }
    pub fn expect_lvalue(self) -> PointerValue<'ctx> {
        match self {
            Self::LValue(ptr) => ptr,
            Self::RValue(val) => panic!("Expected an lvalue but got ${val:?}, address is unknown"),
            Self::LoadedLValue(_, ptr) => ptr,
        }
    }
}

pub struct CodeGen<'codegen_scope, 'ctx> {
    context: &'ctx Context,
    builder: &'codegen_scope Builder<'ctx>,
    module: &'codegen_scope Module<'ctx>,
    type_db: &'codegen_scope TypeInstanceManager,
    type_cache: HashMap<TypeInstanceId, AnyTypeEnum<'ctx>>,
    functions: HashMap<InternedString, FunctionValue<'ctx>>,
    type_data: HashMap<TypeInstanceId, GlobalValue<'ctx>>,
    next_temporary: u32,
    //InternedString here is a hack to avoid cloning strings
    mangled_name_cache: HashMap<TypeInstanceId, InternedString>,
    mangled_method_cache: HashMap<(TypeInstanceId, InternedString), InternedString>,
    type_data_llvm_type: Option<AnyTypeEnum<'ctx>>,
    codegen_queue: VecDeque<&'codegen_scope MIRTopLevelNode<'codegen_scope>>,
    top_lvl_names: HashMap<InternedString, &'codegen_scope MIRTopLevelNode<'codegen_scope>>,
    //    fpm: PassManager<FunctionValue<'ctx>>
}

impl<'codegen_scope, 'ctx> CodeGen<'codegen_scope, 'ctx> {
    pub fn as_basic_type(&self, typ: AnyTypeEnum<'ctx>) -> BasicTypeEnum<'ctx> {
        match typ.try_into() {
            Ok(b) => b,
            Err(_) =>  panic!("Tried to convert AnyTypeEnum to BasicTypeEnum, but AnyTypeEnum {typ:?} is not supported")
        }
    }

    pub fn make_llvm_type(&mut self, instance: TypeInstanceId) -> AnyTypeEnum<'ctx> {
        if let Some(t) = self.type_cache.get(&instance) {
            return *t;
        }

        let this_type = self.type_db.get_instance(instance);
        let llvm_any_type: AnyTypeEnum = if instance == self.type_db.common_types.void {
            self.context.void_type().into()
        } else if instance == self.type_db.common_types.i32
            || instance == self.type_db.common_types.u32
        {
            self.context.i32_type().into()
        } else if instance == self.type_db.common_types.i64
            || instance == self.type_db.common_types.u64
        {
            self.context.i64_type().into()
        } else if instance == self.type_db.common_types.f32 {
            self.context.f32_type().into()
        } else if instance == self.type_db.common_types.f64 {
            self.context.f64_type().into()
        } else if instance == self.type_db.common_types.bool {
            self.context.bool_type().into()
        } else if instance == self.type_db.common_types.u8 {
            self.context.i8_type().into()
        } else if this_type.base == self.type_db.constructors.common_types.ptr {
            //@TODO LLVM is moving towards opaque ptrs, llvm 17 won't support this
            let pointee = self.make_llvm_type(this_type.type_args[0]);
            self.as_basic_type(pointee)
                .ptr_type(AddressSpace::default())
                .into()
        } else {
            //if it's not primitive then it's a struct or func
            if this_type.is_function {
                todo!("function types not implemented")
            } else {
                let field_types = this_type
                    .fields
                    .iter()
                    .map(|x| self.make_llvm_type(x.field_type))
                    //double collect as a quick fix for borrowing issues
                    .collect::<Vec<_>>()
                    .iter()
                    .map(|x| self.as_basic_type(*x))
                    .collect::<Vec<_>>();

                let struct_type = self
                    .context
                    .opaque_struct_type(&format!("struct.{}", this_type.name));
                struct_type.set_body(&field_types, false);
                struct_type.into()
            }
        };

        self.type_cache.insert(instance, llvm_any_type);

        llvm_any_type
    }

    pub fn create_function(
        &mut self,
        function_name: InternedString,
        parameters: &[MIRTypedBoundName],
        return_type: TypeInstanceId,
        is_external: bool,
        is_varargs: bool,
    ) -> FunctionValue<'ctx> {
        let params = parameters
            .iter()
            .map(|x| {
                let llvm_param_type = self.make_llvm_type(x.type_instance);
                let basic: BasicMetadataTypeEnum = self.as_basic_type(llvm_param_type).into();
                basic
            })
            .collect::<Vec<_>>();

        let function_type = if self.type_db.common_types.void == return_type {
            self.context.void_type().fn_type(&params, is_varargs)
        } else {
            let llvm_return_type = self.make_llvm_type(return_type);
            self.as_basic_type(llvm_return_type)
                .fn_type(&params, is_varargs)
        };

        let linkage = if function_name == InternedString::new("main") || is_external {
            None
        } else {
            Some(inkwell::module::Linkage::Private)
        };
        let function = self
            .module
            .add_function(&function_name.to_string(), function_type, linkage);

        for (i, param) in function.get_param_iter().enumerate() {
            param.set_name(&parameters[i].name.to_string());
        }
        self.functions.insert(function_name, function);

        function
    }

    fn create_variable_stack_alloc(
        &mut self,
        function: FunctionValue<'ctx>,
        var_name: &str,
        var_type: TypeInstanceId,
    ) -> PointerValue<'ctx> {
        //we create a new builder here because we want to ensure we create the variables in the very beginning
        let builder = self.context.create_builder();
        let first_block = function
            .get_first_basic_block()
            .expect("Function has no entry basic block");

        match first_block.get_first_instruction() {
            Some(instr) => builder.position_before(&instr),
            None => builder.position_at_end(first_block),
        };

        let llvm_type = self.make_llvm_type(var_type);
        let llvm_type = self.as_basic_type(llvm_type);

        let alloca = builder.build_alloca(llvm_type, var_name);
        alloca
    }

    /// Registers the top-level declaration in LLVM without generating its code.
    pub fn register_top_lvl<'source>(&mut self, node: &'source MIRTopLevelNode<'source>) {
        match node {
            MIRTopLevelNode::DeclareFunction {
                function_name,
                parameters,
                body: _,
                scopes: _,
                return_type,
            } => {
                self.create_function(*function_name, parameters, *return_type, false, false);
            }

            MIRTopLevelNode::IntrinsicOrExternalFunction {
                function_name,
                parameters,
                return_type,
                is_varargs,
                is_external,
            } => {
                self.create_function(
                    *function_name,
                    parameters,
                    *return_type,
                    *is_external,
                    *is_varargs,
                );
            }
        }
    }

    fn add_type_data(&mut self, type_instance: &TypeInstance) {
        //we need to build a str with the name, and a size
        let name = self.build_string_struct_value_for_globals(&type_instance.name);
        let size = self
            .context
            .i64_type()
            .const_int(type_instance.size.0 as u64, false);

        let llvm_struct_instance = self
            .type_data_llvm_type
            .unwrap()
            .into_struct_type()
            .const_named_struct(&[name.into(), size.into()]);

        let as_basic_type = self.type_data_llvm_type.unwrap().into_struct_type();

        let global = self.module.add_global(
            as_basic_type,
            None,
            &format!(".TypeData_{}", &type_instance.name),
        );
        global.set_initializer(&llvm_struct_instance);
        global.set_linkage(Linkage::Internal);
        global.set_constant(true);
        self.type_data.insert(type_instance.id, global);
    }

    pub fn generate_for_top_lvl<'source>(&mut self, node: &'source MIRTopLevelNode<'source>) {
        match node {
            MIRTopLevelNode::DeclareFunction {
                function_name,
                parameters,
                body,
                scopes,
                return_type: _,
            } => {
                let function_layout =
                    crate::compiler::layouts::generate_function_layout(scopes, self.type_db);
                let function_signature = *self
                    .functions
                    .get(function_name)
                    .expect("Expected function to be registered already");

                let cur_basic = self.context.append_basic_block(function_signature, "entry");
                self.builder.position_at_end(cur_basic);

                //@TODO cloneless: HashMap<&'str, ..
                let mut llvm_variables: HashMap<InternedString, PointerValue<'_>> = HashMap::new();

                for (i, param) in function_signature.get_param_iter().enumerate() {
                    let param_name = &parameters[i].name;
                    let alloca = self
                        .builder
                        .build_alloca(param.get_type(), param_name.to_string().as_str());
                    self.builder.build_store(alloca, param);
                    llvm_variables.insert(*param_name, alloca);
                }

                //register the variables first by navigating in the scopes,
                //but we will just create stack vars on the entry block
                //if a variable is declared twice with the same name on different scopes
                //i,e for loop variables, we attach the scope number in which they are.

                for block in body.iter() {
                    let block_scope = &scopes[block.scope.0];
                    for MIRTypedBoundName {
                        type_instance,
                        name,
                    } in block_scope.boundnames.iter()
                    {
                        //if this function has been already loaded by paarameters, then skip
                        if llvm_variables.contains_key(name) {
                            continue;
                        };
                        let ptr = self.create_variable_stack_alloc(
                            function_signature,
                            name.to_string().as_str(),
                            *type_instance,
                        );
                        llvm_variables.insert(*name, ptr);
                    }
                }

                /*
                however we still have another problem:

                if we are generating code in block 9, which uses scope 9, we might reference a variable that was defined
                on scope 4. If we naively try to find a variable named i_9 we won't find it.

                Therefore we need to create a map between:
                (name, scope of the block we are generating) => PointerValue<'ctx>

                luckily we have the function variable layout calculated already
                */

                let symbol_table =
                    build_function_symbol_table(body, scopes, &function_layout, llvm_variables);

                let mut llvm_basic_blocks = vec![];
                //foreach block, generate the blocks
                for (i, _) in body.iter().enumerate() {
                    let block = self
                        .context
                        .append_basic_block(function_signature, &format!("block_{i}"));
                    llvm_basic_blocks.push(block);
                }
                //make the entry point go towards the first block
                let function_entry = function_signature.get_first_basic_block().unwrap();
                self.builder.position_at_end(function_entry);
                self.builder
                    .build_unconditional_branch(llvm_basic_blocks[0]);

                for (i, block) in body.iter().enumerate() {
                    let basic_block = llvm_basic_blocks[i];
                    self.builder.position_at_end(basic_block);
                    for node in block.nodes.iter() {
                        match node {
                            MIRBlockNode::Assign {
                                path, expression, ..
                            } => {
                                let lvalue = self.compile_lvalue(path, &symbol_table, block.scope);
                                let ptr = lvalue.expect_lvalue();
                                let value_to_write = self
                                    .compile_expr(block.scope, &symbol_table, expression)
                                    .load_if_lvalue(self.builder)
                                    .expect_rvalue();
                                self.builder.build_store(ptr, value_to_write);
                            }

                            MIRBlockNode::FunctionCall { function, args, .. } => {
                                //TODO deduplicate this code
                                let llvm_args = args
                                    .iter()
                                    .map(|x| {
                                        self.compile_expr(block.scope, &symbol_table, x)
                                            .load_if_lvalue(self.builder)
                                            .expect_rvalue()
                                            .into()
                                    })
                                    .collect::<Vec<_>>();

                                self.build_function_call_and_enqueue_codegen(function, llvm_args);
                            }
                            MIRBlockNode::MethodCall {
                                object,
                                method_name,
                                args,
                                meta_ast: _,
                                meta_expr: _,
                                return_type: _,
                            } => {
                                let method_name =
                                    self.get_method_name_type_id(object.get_type(), *method_name);
                                let mut llvm_args: Vec<BasicMetadataValueEnum> = vec![];
                                let self_value = self
                                    .compile_expr(block.scope, &symbol_table, object)
                                    .load_if_lvalue(self.builder)
                                    .expect_rvalue()
                                    .into();
                                //TODO deduplicate this code
                                let call_args: Vec<BasicMetadataValueEnum> = args
                                    .iter()
                                    .map(|x| {
                                        self.compile_expr(block.scope, &symbol_table, x)
                                            .load_if_lvalue(self.builder)
                                            .expect_rvalue()
                                            .into()
                                    })
                                    .collect::<Vec<_>>();

                                llvm_args.push(self_value);
                                llvm_args.extend(call_args);

                                if let Some(f) = self.functions.get(&method_name) {
                                    self.builder.build_call(*f, &llvm_args, &self.make_temp(""));
                                } else {
                                    panic!("Method not yet added to llvm IR {method_name}");
                                }
                            }
                        }
                    }

                    match &block.finish {
                        MIRBlockFinal::If(expr, true_branch, false_branch, _) => {
                            let expr_compiled = self
                                .compile_expr(block.scope, &symbol_table, expr)
                                .load_if_lvalue(self.builder)
                                .expect_rvalue();
                            self.builder.build_conditional_branch(
                                expr_compiled.into_int_value(),
                                llvm_basic_blocks[true_branch.0],
                                llvm_basic_blocks[false_branch.0],
                            );
                        }
                        MIRBlockFinal::GotoBlock(block) => {
                            self.builder
                                .build_unconditional_branch(llvm_basic_blocks[block.0]);
                        }
                        MIRBlockFinal::Return(expr, _) => {
                            let expr_compiled = self
                                .compile_expr(block.scope, &symbol_table, expr)
                                .load_if_lvalue(self.builder)
                                .expect_rvalue();
                            self.builder.build_return(Some(&expr_compiled));
                        }
                        MIRBlockFinal::EmptyReturn(..) => {
                            self.builder.build_return(None);
                        }
                    }
                }

                function_signature.verify(true);
            }

            MIRTopLevelNode::IntrinsicOrExternalFunction {
                function_name,
                is_external,
                ..
            } => {
                if function_name.as_ref().starts_with("reinterpret_ptr") && !is_external {
                    let function_signature = *self
                        .functions
                        .get(function_name)
                        .expect("Expected function reinterpret_ptr to be registered already");
                    let cur_basic = self.context.append_basic_block(function_signature, "entry");
                    self.builder.position_at_end(cur_basic);

                    let origin = function_signature
                        .get_first_param()
                        .unwrap()
                        .into_pointer_value();
                    let target = function_signature
                        .get_type()
                        .get_return_type()
                        .unwrap()
                        .into_pointer_type();

                    let casted = self.builder.build_pointer_cast(origin, target, "casted");
                    self.builder.build_return(Some(&casted));
                }
            }
        }
    }

    fn build_function_call_and_enqueue_codegen(
        &mut self,
        function: &InternedString,
        llvm_args: Vec<BasicMetadataValueEnum<'ctx>>,
    ) -> CallSiteValue<'ctx> {
        if let Some(f) = self.functions.get(function) {
            self.builder
                .build_call(*f, &llvm_args, &self.make_temp("fcall_ret"))
        } else {
            //
            let top_lvl = self.top_lvl_names.get(function).expect(
                "Function not found in top lvl declarations, this is a bug in the compiler",
            );
            self.codegen_queue.push_back(top_lvl);
            self.register_top_lvl(top_lvl);
            if let Some(f) = self.functions.get(function) {
                self.builder
                    .build_call(*f, &llvm_args, &self.make_temp("fcall_ret"))
            } else {
                panic!("Function not found in top lvl declarations, this is a bug in the compiler");
            }
        }
    }

    pub fn compile_expr(
        &mut self,
        current_scope: ScopeId,
        symbol_table: &FunctionSymbolTable<'ctx>,
        expression: &MIRExpr<'_>,
    ) -> LlvmExpression<'ctx> {
        match expression {
            MIRExpr::RValue(rvalue) => self.compile_rvalue(rvalue, current_scope, symbol_table),
            MIRExpr::LValue(lvalue) => self.compile_lvalue(lvalue, symbol_table, current_scope),
        }
    }

    fn compile_rvalue(
        &mut self,
        rvalue: &MIRExprRValue<'_>,
        current_scope: ScopeId,
        symbol_table: &FunctionSymbolTable<'ctx>,
    ) -> LlvmExpression<'ctx> {
        let types = &self.type_db.common_types;

        match rvalue {
            MIRExprRValue::Literal(literal, ty, _) => self.build_literal(literal, types, ty),
            MIRExprRValue::BinaryOperation(lhs, op, rhs, _ty, _) => {
                self.build_binary_expr(lhs, rhs, current_scope, symbol_table, op, types)
            }
            MIRExprRValue::MethodCall(object, method_name, args, _ty, _) => {
                let method_name = self.get_method_name_type_id(object.get_type(), *method_name);
                let mut llvm_args: Vec<BasicMetadataValueEnum> = vec![];
                let self_value = self
                    .compile_expr(current_scope, &symbol_table, object)
                    .load_if_lvalue(self.builder)
                    .expect_rvalue()
                    .into();
                //TODO deduplicate this code
                let call_args: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|x| {
                        self.compile_expr(current_scope, &symbol_table, x)
                            .load_if_lvalue(self.builder)
                            .expect_rvalue()
                            .into()
                    })
                    .collect::<Vec<_>>();

                llvm_args.push(self_value);
                llvm_args.extend(call_args);

                if let Some(f) = self.functions.get(&method_name) {
                    let call_site_val =
                        self.builder.build_call(*f, &llvm_args, &self.make_temp(""));
                    LlvmExpression::RValue(
                    call_site_val.try_as_basic_value()
                        .expect_left("Expected method to return some value, but this function probably returns void and somehow passed the type checker")
                )
                } else {
                    panic!("Method not yet added to llvm IR {method_name}");
                }
            }
            MIRExprRValue::FunctionCall(function_expr, arg_exprs, _ty, _, _) => {
                //let expr = self.compile_expr(current_scope, symbol_table, &function_expr);
                let MIRExprLValue::Variable(var_name, ..) = function_expr.as_ref() else {
                    panic!(
                        "Not callable, this is a bug in the type checker: {:?}",
                        function_expr
                    )
                };

                let llvm_args = arg_exprs
                    .iter()
                    .map(|x| {
                        let expr_compiled = self
                            .compile_expr(current_scope, symbol_table, x)
                            .load_if_lvalue(self.builder)
                            //.load(self.builder);
                            ;
                        expr_compiled.expect_rvalue().into()
                    })
                    .collect::<Vec<_>>();

                let call_site_val =
                    self.build_function_call_and_enqueue_codegen(var_name, llvm_args);

                let expr = call_site_val
                    .try_as_basic_value()
                    .expect_left("Expected function to return some value, but this function probably returns void and somehow passed the type checker");

                LlvmExpression::RValue(expr)
            }
            MIRExprRValue::Ref(expr, ..) => {
                //you can only create a pointer to something that is an lvalue (i.e. has a memory location)
                //and the result is an rvalue, a pointer i.e. ptr<i8>. You can't create a reference to a reference immediately, like &&.

                //So the semantics of it isn't "create a pointer", but rather "get the pointer to the lvalue".

                //Therefore in practice this only extracts the pointer from the lvalue.
                //Suppose you have a variable x and we are doing &x.
                //The X lives somewhere in memory (there is an alloca related to it, a ptr),
                //then we do a compile_expr on that variable. The result is an lvalue which contains the pointer.

                let ptr = self.compile_lvalue(expr, symbol_table, current_scope);

                LlvmExpression::RValue(ptr.expect_lvalue().into())
            }
            MIRExprRValue::UnaryExpression(op, expr, _, _) => {
                let lhs_type = expr.get_type();
                let compiled = self
                    .compile_expr(current_scope, symbol_table, expr)
                    .load_if_lvalue(self.builder)
                    .expect_rvalue();
                let expr = match op.0 {
                    Operator::Plus => compiled,
                    Operator::Minus => {
                        if lhs_type.is_integer(self.type_db) || lhs_type == types.bool {
                            self.builder
                                .build_int_neg(compiled.into_int_value(), &self.make_temp("int"))
                                .into()
                        } else if lhs_type.is_float(self.type_db) {
                            self.builder
                                .build_float_neg(
                                    compiled.into_float_value(),
                                    &self.make_temp("float"),
                                )
                                .into()
                        } else {
                            todo!("Not implemented")
                        }
                    }
                    _ => {
                        todo!("Not implemented unary operator: {op:?}")
                    }
                };
                LlvmExpression::RValue(expr)
            }
            MIRExprRValue::Array(_, _, _) => todo!(),
            MIRExprRValue::StructInstantiate(ty, _) => {
                let llvm_str_type = self.make_llvm_type(*ty);
                let llvm_struct = self
                    .builder
                    .build_alloca(llvm_str_type.into_struct_type(), &self.make_temp("struct"));
                LlvmExpression::LValue(llvm_struct)
            }
            MIRExprRValue::Cast(expr, casted_type, _) => {
                let expr_compiled = self
                    .compile_expr(current_scope, symbol_table, expr)
                    .load_if_lvalue(self.builder)
                    .expect_rvalue();

                let llvm_casted_type = self.make_llvm_type(*casted_type);

                //generate a numeric cast, i.e. i32 -> i64, f32 -> f64, i32 -> f64, etc

                let casted: BasicValueEnum = if expr.get_type().is_integer(self.type_db) {
                    if (*casted_type).is_integer(self.type_db) {
                        self.builder
                            .build_int_cast(
                                expr_compiled.into_int_value(),
                                self.as_basic_type(llvm_casted_type).into_int_type(),
                                &self.make_temp("casted"),
                            )
                            .into()
                    } else if casted_type.is_float(self.type_db) {
                        self.builder
                            .build_signed_int_to_float(
                                expr_compiled.into_int_value(),
                                self.as_basic_type(llvm_casted_type).into_float_type(),
                                &self.make_temp("casted"),
                            )
                            .into()
                    } else {
                        todo!("Cast not implemented")
                    }
                } else if expr.get_type().is_float(self.type_db) {
                    if (*casted_type).is_integer(self.type_db) {
                        self.builder
                            .build_float_to_signed_int(
                                expr_compiled.into_float_value(),
                                self.as_basic_type(llvm_casted_type).into_int_type(),
                                &self.make_temp("casted"),
                            )
                            .into()
                    } else if casted_type.is_float(self.type_db) {
                        self.builder
                            .build_float_cast(
                                expr_compiled.into_float_value(),
                                self.as_basic_type(llvm_casted_type).into_float_type(),
                                &self.make_temp("casted"),
                            )
                            .into()
                    } else {
                        todo!("Cast not implemented")
                    }
                } else {
                    todo!("Cast not implemented")
                };

                LlvmExpression::RValue(casted)
            }
            MIRExprRValue::TypeVariable { type_variable, .. } => {
                let global = self.type_data.get(type_variable);

                match global {
                    Some(type_data) => {
                        let loaded_global_var = self
                            .builder
                            .build_load(type_data.as_pointer_value(), "loaded_type_data");
                        LlvmExpression::LoadedLValue(
                            loaded_global_var,
                            type_data.as_pointer_value(),
                        )
                    }
                    None => {
                        let type_instance = self.type_db.get_instance(*type_variable);
                        self.add_type_data(type_instance);
                        let global = self.type_data.get(type_variable).unwrap();
                        let loaded_global_var = self
                            .builder
                            .build_load(global.as_pointer_value(), "loaded_type_data");
                        LlvmExpression::LoadedLValue(loaded_global_var, global.as_pointer_value())
                    }
                }
            }
        }
    }

    fn compile_lvalue(
        &mut self,
        lvalue: &MIRExprLValue<'_>,
        symbol_table: &FunctionSymbolTable<'ctx>,
        current_scope: ScopeId,
    ) -> LlvmExpression<'ctx> {
        match lvalue {
            MIRExprLValue::Variable(name, _, _) => {
                log!(
                    "LValue compile, name = {} {:?}, {}",
                    name,
                    name,
                    symbol_table.len()
                );

                match symbol_table.get(&(*name, current_scope)) {
                    Some(a) => LlvmExpression::LValue(*a),
                    None => {
                        panic!(
                            "Variable not found in symbol table or type data globals: {}",
                            name
                        )
                    }
                }
            }
            MIRExprLValue::MemberAccess(obj, member, _, _) => {
                let llvm_expr = self
                    .compile_expr(current_scope, symbol_table, obj)
                    .expect_lvalue();

                let index = match self.type_db.find_struct_member(obj.get_type(), *member) {
                    StructMember::Field(_, idx) => idx,
                    StructMember::Method(_) => todo!("Unsupported: method as struct member, it should be a MIRExprRValue::MethodCall"),
                    StructMember::NotFound => {
                        panic!("Should not reach LLVM with not found struct member")
                    }
                };

                println!("index = {}", index);
                println!("obj = {obj:#?}");
                println!("member = {member:#?}");
                println!("member = {llvm_expr:#?}");

                let pointee = llvm_expr.get_type().get_element_type();
                println!("pointee = {pointee:#?}");

                let field_ptr = self
                    .builder
                    .build_struct_gep(llvm_expr, index as u32, &self.make_temp("var"))
                    .unwrap();

                LlvmExpression::LValue(field_ptr)
            }
            MIRExprLValue::Deref(expr, _, _) => {
                //you can only dereference an expression that is a pointer.
                //in this case we need to handle both lvalues and rvalues.

                //suppose we called a function that returns a ptr and we want to immediately deref it, like so:
                //x = *foo();

                //by virtue of make_llvm_type, the type in LLVM will also be a pointer.
                //suppose foo returns ptr<i8>. This is an RValue.

                //The result of a dereference is a loaded lvalue. If you can dereference it, then it has a location in memory,
                //which means it's an lvalue.

                //So yes, even if the value came from an RValue, through the deref it can become an lvalue, like so:
                // *get_string_buf("abc") = 1

                let expr = self.compile_expr(current_scope, symbol_table, expr);

                match expr {
                    LlvmExpression::RValue(basic) => {
                        let as_ptr = basic.into_pointer_value();
                        LlvmExpression::LValue(as_ptr)
                    }
                    LlvmExpression::LValue(ptr) | LlvmExpression::LoadedLValue(_, ptr) => {
                        let loaded_ptr = self
                            .builder
                            .build_load(ptr, &self.make_temp("deref_load_ptr"))
                            .into_pointer_value();
                        let derefed_ptr = self
                            .builder
                            .build_load(loaded_ptr, &self.make_temp("deref"));
                        LlvmExpression::LoadedLValue(derefed_ptr, loaded_ptr)
                    }
                }
            }
        }
    }

    fn build_binary_expr(
        &mut self,
        lhs: &MIRExpr<'_>,
        rhs: &MIRExpr<'_>,
        current_scope: ScopeId,
        symbol_table: &FunctionSymbolTable<'ctx>,
        op: &crate::ast::parser::SpannedOperator,
        types: &crate::types::type_instance_db::CommonTypeInstances,
    ) -> LlvmExpression<'ctx> {
        let lhs_type = lhs.get_type();
        let rhs_type = rhs.get_type();

        if lhs_type != rhs_type {
            panic!("both sides of expression must be the same type")
        }
        let lhs = self
            .compile_expr(current_scope, symbol_table, lhs)
            .load_if_lvalue(self.builder)
            .expect_rvalue();

        let rhs = self
            .compile_expr(current_scope, symbol_table, rhs)
            .load_if_lvalue(self.builder)
            .expect_rvalue();

        use paste::paste;

        macro_rules! make_op {
            ($method:tt) => {
                if lhs_type.is_integer(self.type_db) {
                    paste! {
                        self.builder.[<build_int_ $method >] (
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int")).into()
                    }
                } else if lhs_type == types.f32 || lhs_type == types.f64 {
                    paste! {
                        self.builder.[<build_float_ $method >] (
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        &self.make_temp("float")).into()
                    }
                } else {
                    todo!("Not implemented")
                }
            };
        }

        let val = match op.0 {
            Operator::Plus => make_op!(add),
            Operator::Minus => make_op!(sub),
            Operator::Multiply => make_op!(mul),
            Operator::Divide => {
                if lhs_type == types.i32 || lhs_type == types.i64 {
                    self.builder
                        .build_int_signed_div(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else if lhs_type == types.u32 || lhs_type == types.u64 {
                    self.builder
                        .build_int_unsigned_div(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else if lhs_type == types.f32 || lhs_type == types.f64 {
                    self.builder
                        .build_float_div(
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            &self.make_temp("float"),
                        )
                        .into()
                } else {
                    panic!("unsupported op")
                }
            }
            Operator::Mod => {
                if lhs_type == types.i32 || lhs_type == types.i64 {
                    self.builder
                        .build_int_signed_rem(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else if lhs_type == types.u32 || lhs_type == types.u64 {
                    self.builder
                        .build_int_unsigned_rem(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else {
                    panic!("unsupported op")
                }
            }
            Operator::BitShiftLeft => {
                if lhs_type.is_integer(self.type_db) {
                    self.builder
                        .build_left_shift(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else {
                    panic!("Cannot << in non-int type")
                }
            }
            Operator::BitShiftRight => {
                if lhs_type.is_integer(self.type_db) {
                    self.builder
                        .build_right_shift(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            false,
                            &self.make_temp("int"),
                        )
                        .into()
                } else {
                    panic!("Cannot >> in non-int type")
                }
            }

            Operator::Equals => {
                if lhs_type.is_integer(self.type_db) || lhs_type == types.bool {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::EQ,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else if lhs_type.is_float(self.type_db) {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OEQ,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            &self.make_temp("float"),
                        )
                        .into()
                } else {
                    panic!(
                        "Not implemented Equals for type {}",
                        lhs_type.to_string(self.type_db)
                    )
                }
            }
            Operator::NotEquals => {
                if lhs_type.is_integer(self.type_db) || lhs_type == types.bool {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::NE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else if lhs_type.is_float(self.type_db) {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::ONE,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            &self.make_temp("float"),
                        )
                        .into()
                } else {
                    panic!(
                        "Not implemented Not Equals for type {}",
                        lhs_type.to_string(self.type_db)
                    )
                }
            }
            Operator::Or => {
                if lhs_type.is_integer(self.type_db) || lhs_type == types.bool {
                    self.builder
                        .build_or(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else {
                    panic!(
                        "Not implemented OR for type {}",
                        lhs_type.to_string(self.type_db)
                    )
                }
            }
            Operator::And => {
                if lhs_type.is_integer(self.type_db) || lhs_type == types.bool {
                    self.builder
                        .build_and(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else {
                    panic!(
                        "Not implemented And for type {}",
                        lhs_type.to_string(self.type_db)
                    )
                }
            }
            Operator::Xor => {
                if lhs_type.is_integer(self.type_db) || lhs_type == types.bool {
                    self.builder
                        .build_xor(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else {
                    panic!(
                        "Not implemented Xor for type {}",
                        lhs_type.to_string(self.type_db)
                    )
                }
            }
            Operator::Greater => {
                if lhs_type == types.i32 || lhs_type == types.i64 {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::SGT,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else if lhs_type == types.u32 || lhs_type == types.u64 {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::UGT,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else if lhs_type == types.f32 || lhs_type == types.f64 {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OGT,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else {
                    panic!(
                        "Not implemented Greater Than comparison for type {}",
                        lhs_type.to_string(self.type_db)
                    )
                }
            }
            Operator::GreaterEquals => {
                if lhs_type == types.i32 || lhs_type == types.i64 {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::SGE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else if lhs_type == types.u32 || lhs_type == types.u64 {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::UGE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else if lhs_type == types.f32 || lhs_type == types.f64 {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OGE,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else {
                    panic!(
                        "Not implemented Greater Than Or Equals comparison for type {}",
                        lhs_type.to_string(self.type_db)
                    )
                }
            }
            Operator::Less => {
                if lhs_type == types.i32 || lhs_type == types.i64 {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::SLT,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else if lhs_type == types.u32 || lhs_type == types.u64 {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::ULT,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else if lhs_type == types.f32 || lhs_type == types.f64 {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OLT,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else {
                    panic!(
                        "Not implemented Less Than comparison for type {}",
                        lhs_type.to_string(self.type_db)
                    )
                }
            }
            Operator::LessEquals => {
                if lhs_type == types.i32 || lhs_type == types.i64 {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::SLE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else if lhs_type == types.u32 || lhs_type == types.u64 {
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::ULE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else if lhs_type == types.f32 || lhs_type == types.f64 {
                    self.builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OLE,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            &self.make_temp("int"),
                        )
                        .into()
                } else {
                    panic!(
                        "Not implemented Less Than Or Equals comparison for type {}",
                        lhs_type.to_string(self.type_db)
                    )
                }
            }
            _ => panic!("Operator not implemented: {op:?}"),
        };
        LlvmExpression::RValue(val)
    }

    fn build_literal(
        &mut self,
        literal: &LiteralMIRExpr,
        types: &crate::types::type_instance_db::CommonTypeInstances,
        literal_type: &TypeInstanceId,
    ) -> LlvmExpression<'ctx> {
        match literal {
            LiteralMIRExpr::Integer(i) => {
                //@TODO review the sign_extend, I think this is not correct
                let val = if types.i32 == *literal_type {
                    self.context.i32_type().const_int(*i as u64, false)
                } else if types.i64 == *literal_type {
                    self.context.i64_type().const_int(*i as u64, false)
                } else if types.u32 == *literal_type {
                    self.context.i32_type().const_int(*i as u64, false)
                } else if types.u64 == *literal_type {
                    self.context.i64_type().const_int(*i as u64, false)
                } else {
                    panic!("Unknown type")
                };
                LlvmExpression::RValue(val.into())
            }
            LiteralMIRExpr::Char(c) => {
                let val = self.context.i8_type().const_int(*c as u64, false);
                LlvmExpression::RValue(val.into())
            }
            LiteralMIRExpr::Float(f) => {
                let val = if types.f32 == *literal_type {
                    self.context.f32_type().const_float(f.0 as f32 as f64)
                } else if types.f64 == *literal_type {
                    self.context.f64_type().const_float(f.0)
                } else {
                    panic!("Unkown type")
                };
                LlvmExpression::RValue(val.into())
            }
            LiteralMIRExpr::Boolean(b) => LlvmExpression::RValue(
                self.context
                    .bool_type()
                    .const_int(if *b { 1 } else { 0 }, false)
                    .into(),
            ),
            LiteralMIRExpr::String(s) => {
                let str_alloc = self.build_string_literal(&s.to_string());
                LlvmExpression::LValue(str_alloc).load_if_lvalue(self.builder)
            } //LiteralMIRExpr::None => todo!("none not implemented in llvm"),
        }
    }

    fn build_string_literal(&mut self, s: &str) -> PointerValue<'ctx> {
        //build a buf, len struct

        let mut null_terminated = s.to_string();
        null_terminated.push('\0');

        //let buf_val = self.context.const_string(null_terminated.as_bytes(), true);
        let len_val = self
            .context
            .i64_type()
            .const_int(s.to_string().as_str().len() as u64, false);
        let str = self.type_db.find_by_name("str").unwrap();
        let llvm_str_type = self.make_llvm_type(str.id);

        let global_str = self
            .builder
            .build_global_string_ptr(&null_terminated, ".str");
        let buf_val = global_str.as_pointer_value();

        //allocate struct
        let str_alloc = self
            .builder
            .build_alloca(llvm_str_type.into_struct_type(), "_str");
        {
            let ptr_to_buf = self
                .builder
                .build_struct_gep(str_alloc, 0, "_str_buf")
                .unwrap();
            self.builder.build_store(ptr_to_buf, buf_val);
        }

        {
            let ptr_to_len = self
                .builder
                .build_struct_gep(str_alloc, 1, "_str_len")
                .unwrap();
            self.builder.build_store(ptr_to_len, len_val);
        }
        str_alloc
    }

    fn build_string_struct_value_for_globals(&mut self, s: &str) -> StructValue<'ctx> {
        //build a buf, len struct

        let mut null_terminated = s.to_string();
        null_terminated.push('\0');

        //let buf_val = self.context.const_string(null_terminated.as_bytes(), true);

        let str = self.type_db.find_by_name("str").unwrap();
        let llvm_str_type = self.make_llvm_type(str.id);

        let len_val = self
            .context
            .i64_type()
            .const_int(s.to_string().as_str().len() as u64, false);

        let global_str = self
            .builder
            .build_global_string_ptr(&null_terminated, ".str");

        let buf_val = global_str.as_pointer_value();

        let str_struct_instance = llvm_str_type
            .into_struct_type()
            .const_named_struct(&[buf_val.into(), len_val.into()]);

        /*let global = self.module.add_global(llvm_str_type.into_struct_type(), None, ".global_str");
        global.set_linkage(inkwell::module::Linkage::Internal);
        global.set_constant(true);
        global.set_initializer(&str_struct_instance);*/

        return str_struct_instance;
    }

    fn make_temp(&mut self, _ty: &str) -> String {
        let ret = format!("_{}", self.next_temporary);
        self.next_temporary += 1;
        ret
    }

    fn generate_intrinsics(&mut self) {
        //go over all type instances, check if their constructor is a intrinsic we support
        for type_data in self.type_db.types.iter() {
            if type_data.base == self.type_db.constructors.common_types.ptr {
                self.generate_ptr_write_intrinsic(type_data);
                self.generate_ptr_index_ptr_intrinsic(type_data);
            }
        }
    }

    //makes a type name including generics without special characters
    fn get_mangled_type_name<'a, 'b>(&'a mut self, type_data: &'b TypeInstance) -> InternedString {
        let existing_mangled_name = self.mangled_name_cache.get(&type_data.id);
        if let Some(existing_mangled_name) = existing_mangled_name {
            return *existing_mangled_name;
        }
        let base_constructor = self.type_db.constructors.find(type_data.base);
        let mut name = base_constructor.name.to_string();
        let args_len = type_data.type_args.len();
        if args_len > 0 {
            name.push('<');
            //  name.push('_');
            //  name.push('A');
            //  name.push_str(&args_len.to_string());

            for type_arg in type_data.type_args.iter() {
                let type_instance = self.type_db.get_instance(*type_arg);
                let type_arg_name = self.get_mangled_type_name(&type_instance);
                name.push_str(&format!("{},", type_arg_name));
                //name.push_str(&format!("_{}", type_arg_name));
            }
            name.push('>');
        }
        self.mangled_name_cache.insert(type_data.id, name.into());
        return *self.mangled_name_cache.get(&type_data.id).unwrap();
    }

    fn get_llvm_method_name(
        &mut self,
        type_data: &TypeInstance,
        method_name: InternedString,
    ) -> InternedString {
        //try to find in cache first
        let existing_mangled_name = self.mangled_method_cache.get(&(type_data.id, method_name));
        if let Some(existing_mangled_name) = existing_mangled_name {
            return *existing_mangled_name;
        }

        let type_name = self.get_mangled_type_name(type_data);
        let mangled_method_name = format!("{}::{}", type_name, method_name);
        //let mangled_method_name = format!("{}__{}", type_name, method_name);
        self.mangled_method_cache
            .insert((type_data.id, method_name), mangled_method_name.into());
        return *self
            .mangled_method_cache
            .get(&(type_data.id, method_name))
            .unwrap();
    }

    fn get_method_name_type_id(
        &mut self,
        type_id: TypeInstanceId,
        method_name: InternedString,
    ) -> InternedString {
        let type_data = self.type_db.get_instance(type_id);
        return self.get_llvm_method_name(&type_data, method_name);
    }

    fn generate_ptr_write_intrinsic(&mut self, type_data: &TypeInstance) {
        //generate intrinsic for ptr<T>.write
        let method_write = type_data
            .find_method_by_name("write".into())
            .expect("ptr type HAS to have write method!");
        let method_name = self.get_llvm_method_name(type_data, "write".into());

        let method_type_details = self.type_db.get_instance(method_write.function_type);
        assert!(method_type_details.is_function);

        //get the first argument which will be a u64 (trust me bro)
        let index_arg = method_type_details.function_args[0];
        let element_arg = method_type_details.function_args[1]; //this will be a ptr<T>, i.e. exact same type as type_data (we hope!)

        let parameters = vec![
            MIRTypedBoundName {
                name: "self".into(),
                type_instance: type_data.id,
            },
            MIRTypedBoundName {
                name: "index".into(),
                type_instance: index_arg,
            },
            MIRTypedBoundName {
                name: "element".into(),
                type_instance: element_arg,
            },
        ];

        let return_type = method_type_details.function_return_type.unwrap();

        let llvm_function =
            self.create_function(method_name.into(), &parameters, return_type, false, false);
        let cur_basic = self.context.append_basic_block(llvm_function, "entry");
        self.builder.position_at_end(cur_basic);

        let self_arg = llvm_function.get_params()[0].into_pointer_value();
        let index_arg = llvm_function.get_params()[1].into_int_value();
        let element_arg = llvm_function.get_params()[2];

        let store_address = unsafe {
            self.builder
                .build_in_bounds_gep(self_arg, &[index_arg], "self_at_index")
        };
        self.builder.build_store(store_address, element_arg);

        self.builder.build_return(None);
    }

    fn generate_ptr_index_ptr_intrinsic(&mut self, type_data: &TypeInstance) {
        //generate intrinsic for ptr<T>.write
        let method_index_ptr = type_data
            .find_method_by_name("__index_ptr__".into())
            .expect("ptr type HAS to have write method!");
        let method_name = self.get_llvm_method_name(type_data, "__index_ptr__".into());

        let method_type_details = self.type_db.get_instance(method_index_ptr.function_type);
        assert!(method_type_details.is_function);

        //get the first argument which will be a u64 (trust me bro)
        let index_arg = method_type_details.function_args[0];
        let parameters = vec![
            MIRTypedBoundName {
                name: "self".into(),
                type_instance: type_data.id,
            },
            MIRTypedBoundName {
                name: "index".into(),
                type_instance: index_arg,
            },
        ];

        let return_type = method_type_details.function_return_type.unwrap();

        let llvm_function =
            self.create_function(method_name.into(), &parameters, return_type, false, false);
        let cur_basic = self.context.append_basic_block(llvm_function, "entry");
        self.builder.position_at_end(cur_basic);

        let self_arg = llvm_function.get_params()[0].into_pointer_value();
        let index_arg = llvm_function.get_params()[1].into_int_value();

        let store_address = unsafe {
            self.builder
                .build_in_bounds_gep(self_arg, &[index_arg], "self_at_index")
        };

        self.builder.build_return(Some(&store_address));
    }
}

type FunctionSymbolTable<'llvm_ctx> = HashMap<(InternedString, ScopeId), PointerValue<'llvm_ctx>>;

fn build_function_symbol_table<'mir, 'function_layout, 'ctx>(
    body: &'mir [MIRBlock<'_>],
    scopes: &'mir [MIRScope],
    function_layout: &'function_layout FunctionLayout,
    llvm_variables: HashMap<InternedString, PointerValue<'ctx>>,
) -> FunctionSymbolTable<'ctx> {
    let mut symbol_table = HashMap::<(InternedString, ScopeId), PointerValue<'ctx>>::new();
    for block in body.iter() {
        let _block_scope = &scopes[block.scope.0];

        let variables_on_scope = &function_layout.variables_for_each_scope[block.scope.0];

        for (name, (_, _declaring_scope)) in variables_on_scope {
            let variable_llvm = llvm_variables
                .get(name)
                .expect("Should find the variable here");

            //log!("Symbol table insert: {:?} -> {:?}", name, variable_llvm);
            symbol_table.insert((*name, block.scope), *variable_llvm);
        }
    }
    symbol_table
}

fn optimize_module(target_machine: &TargetMachine, module: &Module) {
    let opts = PassBuilderOptions::create();
    opts.set_loop_interleaving(true);
    opts.set_loop_slp_vectorization(true);
    opts.set_loop_vectorization(true);
    opts.set_loop_unrolling(true);
    opts.set_merge_functions(true);
    opts.set_verify_each(true);

    module
        .run_passes("default<O0>", target_machine, opts)
        .unwrap();
}

pub fn generate_llvm(
    type_db: &TypeInstanceManager,
    mir_top_level_nodes: &[MIRTopLevelNode<'_>],
) -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("program");
    let builder = context.create_builder();

    let mut codegen = CodeGen {
        context: &context,
        module: &module,
        builder: &builder,
        type_db,
        type_cache: HashMap::new(),
        next_temporary: 0,
        type_data: HashMap::new(),
        functions: HashMap::new(),
        mangled_name_cache: HashMap::new(),
        mangled_method_cache: HashMap::new(),
        type_data_llvm_type: None,
        codegen_queue: VecDeque::new(),
        top_lvl_names: HashMap::new(),
    };

    for top_lvl in mir_top_level_nodes {
        match top_lvl {
            MIRTopLevelNode::IntrinsicOrExternalFunction { function_name, .. } => {
                codegen.top_lvl_names.insert(*function_name, top_lvl);
            }
            MIRTopLevelNode::DeclareFunction { function_name, .. } => {
                codegen.top_lvl_names.insert(*function_name, top_lvl);
            }
        }
    }

    codegen.generate_intrinsics();

    //codegen.generate_type_data_globals();

    let type_data_type = codegen.type_db.find_by_name("TypeData").unwrap();
    let llvm_type_data_type = codegen.make_llvm_type(type_data_type.id);
    codegen.type_data_llvm_type = Some(llvm_type_data_type);
    codegen
        .type_cache
        .insert(type_data_type.id, llvm_type_data_type);

    //for mir_node in mir_top_level_nodes {
    //    codegen.register_top_lvl(mir_node);
    //}

    //find main function
    let main_function = codegen.top_lvl_names.get(&InternedString::new("main"));

    codegen.codegen_queue.push_back(main_function.unwrap());
    codegen.register_top_lvl(main_function.unwrap());
    while !codegen.codegen_queue.is_empty() {
        let item = codegen.codegen_queue.pop_front().unwrap();
        codegen.generate_for_top_lvl(&item);
    }

    println!("Codegen done");

    module.print_to_stderr();
    match codegen.module.verify() {
        Ok(_) => {
            println!("Verified");
        }
        Err(e) => {
            //codegen.module.print_to_stderr();
            panic!(
                "Error compiling code to LLVM\n{llvm_err}",
                llvm_err = e.to_string()
            )
        }
    }

    let target_machine = get_native_target_machine();

    //optimize_module(&target_machine, &module);

    //module.print_to_stderr();

    //let elapsed = now.elapsed();
    //println!("Code took {}s to run, {out}", elapsed.as_secs_f64() / 1.0);

    let asm_buffer = target_machine
        .write_to_memory_buffer(codegen.module, FileType::Assembly)
        .unwrap();

    let obj_buffer = target_machine
        .write_to_memory_buffer(codegen.module, FileType::Object)
        .unwrap();
    // let object_file = obj_buffer.create_object_file().unwrap();

    let file_name = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis()
        .to_string();
    let output_obj_file = format!("./donkey_{file_name}.obj");

    std::fs::write("./last_compiled.asm", asm_buffer.as_slice()).unwrap();
    std::fs::write(&output_obj_file, obj_buffer.as_slice()).unwrap();

    match link(&output_obj_file, "./last_compiled") {
        Ok(_) => {}
        Err(LinkerError::GenericLinkerError(e)) => {
            eprintln!("Linker error!\n{e}")
        }
    }
    std::fs::remove_file(output_obj_file).unwrap();

    let ee = module
        .create_jit_execution_engine(OptimizationLevel::Aggressive)
        .unwrap();

    let maybe_fn = unsafe { ee.get_function::<unsafe extern "C" fn() -> i32>("main") };
    let compiled_fn = match maybe_fn {
        Ok(f) => f,
        Err(err) => {
            panic!("!> Error during ee: {:?}", err);
        }
    };
    println!("Start running code...");
    unsafe {
        compiled_fn.call();
    };

    Ok(())
}

fn get_host_cpu_name() -> String {
    TargetMachine::get_host_cpu_name().to_string()
}
fn get_host_cpu_features() -> String {
    TargetMachine::get_host_cpu_features().to_string()
}

fn get_native_target_machine() -> TargetMachine {
    Target::initialize_native(&InitializationConfig::default())
        .expect("Failed to initialize native target");
    let target_triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&target_triple).unwrap();
    target
        .create_target_machine(
            &target_triple,
            &get_host_cpu_name(),
            &get_host_cpu_features(),
            OptimizationLevel::Aggressive,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap()
}
