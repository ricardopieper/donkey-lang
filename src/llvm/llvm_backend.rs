use std::collections::HashMap;
use std::error::Error;
use std::ffi::CString;
use std::time::Instant;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;

use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{LLVMReference, OptimizationLevel};
use llvm_sys::prelude::LLVMModuleRef;
use llvm_sys::transforms::pass_builder::{
    LLVMCreatePassBuilderOptions, LLVMDisposePassBuilderOptions,
    LLVMPassBuilderOptionsSetLicmMssaOptCap, LLVMPassBuilderOptionsSetLoopInterleaving,
    LLVMPassBuilderOptionsSetLoopUnrolling, LLVMPassBuilderOptionsSetLoopVectorization,
    LLVMPassBuilderOptionsSetMergeFunctions, LLVMPassBuilderOptionsSetSLPVectorization,
    LLVMRunPasses,
};

use crate::ast::lexer::Operator;
use crate::compiler::layouts::FunctionLayout;
use crate::semantic::hir::HIRExpr;
use crate::semantic::mir::{
    MIRBlock, MIRBlockFinal, MIRBlockNode, MIRScope, MIRTypedBoundName, ScopeId,
};
use crate::types::type_instance_db::TypeInstanceId;
use crate::{
    semantic::{hir::Checked, mir::MIRTopLevelNode},
    types::type_instance_db::TypeInstanceManager,
};

pub struct CodeGen<'codegen_scope, 'ctx> {
    context: &'ctx Context,
    builder: &'codegen_scope Builder<'ctx>,
    module: &'codegen_scope Module<'ctx>,
    type_db: &'codegen_scope TypeInstanceManager,
    type_cache: HashMap<TypeInstanceId, AnyTypeEnum<'ctx>>,
    next_temporary: u32,
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
        } else {
            //if it's not primitive then it's a struct or func
            if this_type.is_function {
                todo!()
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
                self.context.struct_type(&field_types, false).into()
            }
        };

        self.type_cache.insert(instance, llvm_any_type.clone());

        return llvm_any_type;
    }

    pub fn compile_function_signature(
        &mut self,
        function_name: &str,
        parameters: &[MIRTypedBoundName],
        return_type: TypeInstanceId,
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
            self.context.void_type().fn_type(&params, false)
        } else {
            let llvm_return_type = self.make_llvm_type(return_type);
            self.as_basic_type(llvm_return_type).fn_type(&params, false)
        };

        let function = self.module.add_function(function_name, function_type, None);

        for (i, param) in function.get_param_iter().enumerate() {
            param.set_name(&parameters[i].name);
        }

        let cur_basic = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(cur_basic);
        return function;
    }

    fn create_variable_stack_alloc(
        &mut self,
        function: FunctionValue<'ctx>,
        var_name: &str,
        var_type: TypeInstanceId,
        scope: Option<ScopeId>,
    ) -> (String, PointerValue<'ctx>) {
        //we create a new builder here because we want to ensure we create the variables in the very beginning
        let builder = self.context.create_builder();

        let first_block = function
            .get_first_basic_block()
            .expect("Function has no entry basic block");

        match first_block.get_first_instruction() {
            Some(instr) => builder.position_before(&instr),
            None => builder.position_at_end(first_block),
        };

        let name = make_var_name(scope, var_name);

        let llvm_type = self.make_llvm_type(var_type);
        let llvm_type = self.as_basic_type(llvm_type);

        let alloca = builder.build_alloca(llvm_type, &name);
        (name.to_string(), alloca)
    }

    pub fn generate_for_top_lvl(&mut self, node: &MIRTopLevelNode<Checked>) {
        match node {
            MIRTopLevelNode::DeclareFunction {
                function_name,
                parameters,
                body,
                scopes,
                return_type,
            } => {
                let function_layout =
                    crate::compiler::layouts::generate_function_layout(&scopes, self.type_db);
                let function_signature =
                    self.compile_function_signature(&function_name, parameters, *return_type);

                for param in parameters.iter() {
                    self.create_variable_stack_alloc(
                        function_signature,
                        &param.name,
                        param.type_instance,
                        None,
                    );
                }

                //register the variables first by navigating in the scopes,
                //but we will just create stack vars on the entry block
                //if a variable is declared twice with the same name on different scopes
                //i,e for loop variables, we attach the scope number in which they are.

                let mut llvm_variables = HashMap::new();

                for block in body.iter() {
                    let block_scope = &scopes[block.scope.0];
                    for MIRTypedBoundName {
                        type_instance,
                        name,
                    } in block_scope.boundnames.iter()
                    {
                        let (llvm_name, ptr) = self.create_variable_stack_alloc(
                            function_signature,
                            &name,
                            *type_instance,
                            Some(block.scope),
                        );
                        llvm_variables.insert(llvm_name, ptr);
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

                for (i, block) in body.iter().enumerate() {
                    let basic_block = llvm_basic_blocks[i];
                    self.builder.position_at_end(basic_block);
                    for node in block.nodes.iter() {
                        //@TODO make basic block
                        //@TODO
                        match node {
                            MIRBlockNode::Assign {
                                path, expression, ..
                            } => {
                                if path.len() > 1 {
                                    panic!("Assignment to path with len > 1")
                                }
                                let expr_compiled =
                                    self.compile_expr(block.scope, &symbol_table, expression);
                                let ptr =
                                    *symbol_table.get(&(path[0].as_str(), block.scope)).unwrap();
                                self.builder.build_store(ptr, expr_compiled);
                            }
                            MIRBlockNode::FunctionCall { .. } => {}
                        }
                    }

                    match &block.finish {
                        MIRBlockFinal::If(expr, true_branch, false_branch, _) => {
                            let expr_compiled =
                                self.compile_expr(block.scope, &symbol_table, &expr);
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
                            let expr_compiled =
                                self.compile_expr(block.scope, &symbol_table, &expr);
                            self.builder.build_return(Some(&expr_compiled));
                        }
                        MIRBlockFinal::EmptyReturn => {
                            self.builder.build_return(None);
                        }
                    }
                }

                //make the entry point go towards the first block
                let function_entry = function_signature.get_first_basic_block().unwrap();
                self.builder.position_at_end(function_entry);
                self.builder
                    .build_unconditional_branch(llvm_basic_blocks[0]);

                function_signature.verify(true);
            }
            MIRTopLevelNode::StructDeclaration {
                struct_name: _,
                body: _,
            } => todo!(),
        }
    }

    pub fn compile_expr(
        &mut self,
        current_scope: ScopeId,
        symbol_table: &FunctionSymbolTable<'_, 'ctx>,
        expression: &HIRExpr<TypeInstanceId, Checked>,
    ) -> BasicValueEnum<'ctx> {
        let types = &self.type_db.common_types;
        match expression {
            HIRExpr::Literal(literal, literal_type, ..) => match literal {
                crate::semantic::hir::LiteralHIRExpr::Integer(i) => {
                    if types.i32 == *literal_type {
                        self.context.i32_type().const_int(*i as u64, false).into()
                    } else if types.i64 == *literal_type {
                        self.context.i64_type().const_int(*i as u64, false).into()
                    } else if types.u32 == *literal_type {
                        self.context.i32_type().const_int(*i as u64, false).into()
                    } else if types.u64 == *literal_type {
                        self.context.i64_type().const_int(*i as u64, false).into()
                    } else {
                        panic!("Unkown type")
                    }
                }
                crate::semantic::hir::LiteralHIRExpr::Float(f) => {
                    if types.f32 == *literal_type {
                        self.context
                            .f32_type()
                            .const_float(f.0 as f32 as f64)
                            .into()
                    } else if types.f64 == *literal_type {
                        self.context.f64_type().const_float(f.0).into()
                    } else {
                        panic!("Unkown type")
                    }
                }
                crate::semantic::hir::LiteralHIRExpr::Boolean(b) => self
                    .context
                    .bool_type()
                    .const_int(if *b { 1 } else { 0 }, false)
                    .into(),
                crate::semantic::hir::LiteralHIRExpr::String(_) => {
                    todo!("string literals not implemented in llvm")
                }
                crate::semantic::hir::LiteralHIRExpr::None => todo!("none not implemented in llvm"),
            },
            HIRExpr::Variable(name, ..) => {
                let ptr = *symbol_table.get(&(name.as_str(), current_scope)).unwrap();
                self.builder.build_load(ptr, &self.make_temp("var")).into()
            }
            HIRExpr::Cast(_, _, _) => todo!(),
            HIRExpr::BinaryOperation(lhs, op, rhs, _, ..) => {
                let lhs_type = lhs.get_type();
                let rhs_type = rhs.get_type();

                if lhs_type != rhs_type {
                    panic!("both sides of expression must be the same type")
                }

                let lhs = self.compile_expr(current_scope, symbol_table, lhs);
                let rhs = self.compile_expr(current_scope, symbol_table, rhs);
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
                            panic!("Not implemented")
                        }
                    };
                }

                match *op {
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
                                lhs_type.as_string(self.type_db)
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
                                lhs_type.as_string(self.type_db)
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
                                lhs_type.as_string(self.type_db)
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
                                lhs_type.as_string(self.type_db)
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
                                lhs_type.as_string(self.type_db)
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
                                lhs_type.as_string(self.type_db)
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
                                lhs_type.as_string(self.type_db)
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
                                lhs_type.as_string(self.type_db)
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
                                lhs_type.as_string(self.type_db)
                            )
                        }
                    }
                    _ => panic!("Operator not implemented: {op:?}"),
                }
            }
            HIRExpr::MethodCall(_, _, _, _, _) => todo!(),
            HIRExpr::FunctionCall(_, _, _, _) => todo!(),
            HIRExpr::UnaryExpression(op, expr, ..) => {
                let lhs_type = expr.get_type();
                let compiled = self.compile_expr(current_scope, symbol_table, expr);
                match op {
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
                            panic!("Not implemented")
                        }
                    }
                    _ => {
                        panic!("Not implemented unary operator: {op:?}")
                    }
                }
            }
            HIRExpr::MemberAccess(_, _, _, _) => todo!(),
            HIRExpr::Array(_, _, _) => todo!(),
            _ => {
                panic!("Unimplemented expr")
            }
        }
    }

    fn make_temp(&mut self, _ty: &str) -> String {
        let ret = format!("_{}", self.next_temporary);
        self.next_temporary += 1;
        return ret;
    }
}

type FunctionSymbolTable<'a, 'llvm_ctx> = HashMap<(&'a str, ScopeId), PointerValue<'llvm_ctx>>;

fn build_function_symbol_table<'mir, 'f, 'ctx>(
    body: &'mir [MIRBlock<Checked>],
    scopes: &'mir [MIRScope],
    function_layout: &'f FunctionLayout,
    llvm_variables: HashMap<String, PointerValue<'ctx>>,
) -> FunctionSymbolTable<'f, 'ctx> {
    let mut symbol_table = HashMap::<(&'f str, ScopeId), PointerValue<'ctx>>::new();
    for block in body.iter() {
        let _block_scope = &scopes[block.scope.0];

        let variables_on_scope = &function_layout.variables_for_each_scope[block.scope.0];

        for (name, (_, declaring_scope)) in variables_on_scope {
            let llvm_name = make_var_name(Some(*declaring_scope), &name);
            let variable_llvm = llvm_variables
                .get(&llvm_name)
                .expect("Should find the variable here");
            symbol_table.insert((name, block.scope), *variable_llvm);
        }
    }
    symbol_table
}

//@TODO maybe use Cow<&str>
fn make_var_name(scope: Option<ScopeId>, var_name: &str) -> String {
    match scope {
        None => var_name.to_string(),
        Some(ScopeId(0)) => var_name.to_string(),
        Some(ScopeId(scope_id)) => format!("{var_name}{scope_id}"),
    }
}

fn optimize_module(target_machine: &TargetMachine, llmod: LLVMModuleRef) {
    unsafe {
        let opts = LLVMCreatePassBuilderOptions();
        LLVMPassBuilderOptionsSetLoopUnrolling(opts, 1);
        LLVMPassBuilderOptionsSetLoopInterleaving(opts, 1);
        LLVMPassBuilderOptionsSetLoopVectorization(opts, 1);
        LLVMPassBuilderOptionsSetLicmMssaOptCap(opts, 1);
        LLVMPassBuilderOptionsSetSLPVectorization(opts, 1);
        LLVMPassBuilderOptionsSetMergeFunctions(opts, 1);

        let as_libc = CString::new("default<O3>").unwrap();
        LLVMRunPasses(llmod, as_libc.as_ptr(), target_machine.get_ref(), opts);

        LLVMDisposePassBuilderOptions(opts);
    }
}

pub fn generate_llvm(
    type_db: &TypeInstanceManager,
    mir_top_level_nodes: &[MIRTopLevelNode<Checked>],
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
    };

    for mir_node in mir_top_level_nodes {
        codegen.generate_for_top_lvl(mir_node);
    }

    let target_machine = get_native_target_machine();

    optimize_module(&target_machine, unsafe { module.get_ref() });

    module.print_to_stderr();

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

    let now = Instant::now();
    let mut out = 0;
    for _i in 0..1 {
        unsafe {
            out += compiled_fn.call();
            println!("{out}")
        }
    }

    let elapsed = now.elapsed();
    println!("Code took {}s to run, {out}", elapsed.as_secs_f64() / 1.0);

    let asm_buffer = target_machine
        .write_to_memory_buffer(&mut codegen.module, FileType::Assembly)
        .unwrap();

    let obj_buffer = target_machine
        .write_to_memory_buffer(&mut codegen.module, FileType::Object)
        .unwrap();
    // let object_file = obj_buffer.create_object_file().unwrap();

    std::fs::write("./compiled.asm", asm_buffer.as_slice()).unwrap();
    std::fs::write("./compiled.obj", obj_buffer.as_slice()).unwrap();

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
