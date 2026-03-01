use core::panic;
use std::collections::{BTreeMap, HashMap, VecDeque};
use std::error::Error;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};

use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{
  CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallSiteValue, FunctionValue, GlobalValue, PointerValue, StructValue};
use inkwell::{AddressSpace, OptimizationLevel};

use crate::ast::lexer::Operator;
use crate::llvm::linker::{LinkerError, link};

use crate::semantic::mir::{LiteralMIRExpr, MIRBlock, MIRBlockFinal, MIRBlockNode, MIRExpr, MIRExprLValue, MIRExprRValue, MIRScope, MIRTopLevelNode, MIRTypedBoundName, ScopeId};

use std::collections::hash_map::DefaultHasher;
use std::hash::BuildHasherDefault;
type DeterministicMap<K, V> = BTreeMap<K, V>;
use std::{
  fmt::Display,
  iter::Sum,
  ops::{Add, AddAssign, Shl, Sub, SubAssign},
};

use crate::interner::InternedString;
use crate::interner::StringInterner;
use crate::semantic::hir::{MonoType, PolyType, TypeTable};
use crate::types::type_constructor_db::{
  Bytes, TypeConstructor, TypeConstructorDatabase, TypeConstructorId, TypeKind,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ByteRange {
  pub begin: Bytes,
  pub end: Bytes,
}

impl ByteRange {
  pub fn size(&self) -> Bytes {
    self.end - self.begin
  }
}

pub type ScopeVariables = DeterministicMap<InternedString, (ByteRange, ScopeId)>;
pub type ScopesVariables = Vec<ScopeVariables>;

pub enum EmitMode {
  LValue,
  RValue,
}

fn build_write_scope_byte_layout(
  scope: &MIRScope,
  all_scopes: &[MIRScope],
  type_db: &TypeConstructorDatabase,
  type_table: &TypeTable,
) -> DeterministicMap<InternedString, (ByteRange, ScopeId)> {
  let mut current_index = scope.id;
  let mut found_var = vec![];
  loop {
    let scope = &all_scopes[current_index.0];

    for var in &scope.boundnames {
      let ty = &type_table[var.type_instance];
      let ty_data = type_db.find(ty.mono.get_ctor_id());
      let size = ty_data.compute_size(type_db);
      found_var.push((var.name, size, scope.id));
    }

    if current_index.0 == 0 {
      break;
    }
    current_index = scope.inherit;
  }

  let mut map: DeterministicMap<InternedString, (ByteRange, ScopeId)> = DeterministicMap::new();
  let mut used_bytes = Bytes(0);
  for (name, size, scope) in found_var.into_iter().rev() {
    map.insert(
      name,
      (
        ByteRange {
          begin: used_bytes,
          end: used_bytes + size,
        },
        scope,
      ),
    );
    used_bytes += size;
  }

  map
}

#[derive(Debug)]
pub struct FunctionLayout {
  //Tells all variables accessible on a given scope, and on which scope it was declared
  pub variables_for_each_scope: ScopesVariables,

  pub largest_scope_size: Bytes,
}

pub fn generate_function_layout(
  scopes: &[MIRScope],
  type_db: &TypeConstructorDatabase,
  type_table: &TypeTable,
) -> FunctionLayout {
  let scope_byte_layout = scopes
      .iter()
      .map(|scope| build_write_scope_byte_layout(scope, scopes, type_db, type_table))
      .collect::<Vec<_>>();

  //println!("Scope byte layout built: {scope_byte_layout:#?}");
  let mut largest_scope = Bytes(0);
  for sbl in &scope_byte_layout {
    let sum: Bytes = sbl.values().map(|x| x.0.size()).sum();
    if sum > largest_scope {
      largest_scope = sum;
    }
  }
  FunctionLayout {
    variables_for_each_scope: scope_byte_layout,
    largest_scope_size: largest_scope,
  }
}

pub struct LlvmRValue<'ctx>(BasicValueEnum<'ctx>);

//the pointer to the memory location of the value, and the pointee type.
//x: i32 = 0
//this is an lvalue, ptr is the memory location for x, such that *x = 0 would set 0 at the location of x,
//and pointee type is i32
pub struct LlvmLValue<'ctx>(PointerValue<'ctx>, AnyTypeEnum<'ctx>);

impl<'ctx> LlvmRValue<'ctx> {
  pub fn get_value(&self) -> BasicValueEnum<'ctx> {
    self.0
  }
}

pub struct CodeGen<'codegen_scope, 'ctx> {
  context: &'ctx Context,
  builder: &'codegen_scope Builder<'ctx>,
  module: &'codegen_scope Module<'ctx>,
  type_db: &'codegen_scope TypeConstructorDatabase,
  type_cache: DeterministicMap<TypeConstructorId, AnyTypeEnum<'ctx>>,
  functions: DeterministicMap<InternedString, FunctionValue<'ctx>>,
  type_data: DeterministicMap<TypeConstructorId, GlobalValue<'ctx>>,
  next_temporary: u32,
  //InternedString here is a hack to avoid cloning strings
  mangled_name_cache: DeterministicMap<TypeConstructorId, InternedString>,
  mangled_method_cache: DeterministicMap<(TypeConstructorId, InternedString), InternedString>,
  type_data_llvm_type: Option<AnyTypeEnum<'ctx>>,
  codegen_queue: VecDeque<&'codegen_scope MIRTopLevelNode>,
  top_lvl_names: DeterministicMap<InternedString, &'codegen_scope MIRTopLevelNode>,
  //    fpm: PassManager<FunctionValue<'ctx>>
}

impl<'codegen_scope, 'ctx> CodeGen<'codegen_scope, 'ctx> {
  pub fn as_basic_type(&self, typ: AnyTypeEnum<'ctx>) -> BasicTypeEnum<'ctx> {
    match typ.try_into() {
      Ok(b) => b,
      Err(_) => panic!(
        "Tried to convert AnyTypeEnum to BasicTypeEnum, but AnyTypeEnum {typ:?} is not supported"
      ),
    }
  }

  pub fn as_any_type(&self, typ: BasicTypeEnum<'ctx>) -> AnyTypeEnum<'ctx> {
    match typ {
      BasicTypeEnum::ArrayType(t) => AnyTypeEnum::ArrayType(t),
      BasicTypeEnum::FloatType(t) => AnyTypeEnum::FloatType(t),
      BasicTypeEnum::IntType(t) => AnyTypeEnum::IntType(t),
      BasicTypeEnum::PointerType(t) => AnyTypeEnum::PointerType(t),
      BasicTypeEnum::StructType(t) => AnyTypeEnum::StructType(t),
      BasicTypeEnum::VectorType(t) => AnyTypeEnum::VectorType(t),
      BasicTypeEnum::ScalableVectorType(t) => AnyTypeEnum::ScalableVectorType(t),
    }
  }

  pub fn load_any(
    &self,
    ptr: PointerValue<'ctx>,
    ty: AnyTypeEnum<'ctx>,
    name: &str,
  ) -> BasicValueEnum<'ctx> {
    match ty {
      AnyTypeEnum::ArrayType(t) => self.builder.build_load(t, ptr, name),
      AnyTypeEnum::FloatType(t) => self.builder.build_load(t, ptr, name),
      AnyTypeEnum::FunctionType(t) => panic!("load lvalue of type function"),
      AnyTypeEnum::IntType(t) => self.builder.build_load(t, ptr, name),
      AnyTypeEnum::PointerType(t) => self.builder.build_load(t, ptr, name),
      AnyTypeEnum::StructType(t) => self.builder.build_load(t, ptr, name),
      AnyTypeEnum::VectorType(t) => self.builder.build_load(t, ptr, name),
      AnyTypeEnum::ScalableVectorType(t) => self.builder.build_load(t, ptr, name),
      AnyTypeEnum::VoidType(t) => panic!("load lvalue of type void"),
    }
        .unwrap()
  }
  pub fn make_llvm_type(&mut self, instance: TypeConstructorId) -> AnyTypeEnum<'ctx> {
    if let Some(t) = self.type_cache.get(&instance) {
      return *t;
    }

    let this_type = self.type_db.find(instance);
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
    } else if this_type.id == self.type_db.common_types.ptr {
      self.context.ptr_type(AddressSpace::default()).into()
    } else {
      //if it's not primitive then it's a struct or func
      if this_type.kind == TypeKind::Function {
        todo!("function types not implemented")
      } else {
        let field_types = this_type
            .fields
            .iter()
            .map(|x| self.make_llvm_type(x.field_type.get_ctor_id()))
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
    return_type: TypeConstructorId,
    is_external: bool,
    is_varargs: bool,
    type_table: &TypeTable,
    struct_name: Option<&InternedString>,
  ) -> FunctionValue<'ctx> {
    let params = parameters
        .iter()
        .map(|x| {
          let ty = &type_table[x.type_instance];
          let llvm_param_type = self.make_llvm_type(ty.mono.get_ctor_id());
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

    match struct_name {
      Some(struct_name) => {
        let mangled = self
            .get_mangled_type_name_from_name(StringInterner::get().borrow(*struct_name));

        let method_name = format!("{mangled}::{function_name}");

        let function =
            self.module
                .add_function(&method_name.to_string(), function_type, linkage);

        for (i, param) in function.get_param_iter().enumerate() {
          param.set_name(&parameters[i].name.to_string());
        }
        self.functions.insert(method_name.into(), function);

        function
      }
      None => {
        let function =
            self.module
                .add_function(&function_name.to_string(), function_type, linkage);

        for (i, param) in function.get_param_iter().enumerate() {
          param.set_name(&parameters[i].name.to_string());
        }
        self.functions.insert(function_name, function);

        function
      }
    }
  }

  fn create_variable_stack_alloc(
    &mut self,
    function: FunctionValue<'ctx>,
    var_name: &str,
    var_type: MonoType,
  ) -> (PointerValue<'ctx>, BasicTypeEnum<'ctx>) {
    //we create a new builder here because we want to ensure we create the variables in the very beginning
    let builder = self.context.create_builder();
    let first_block = function
        .get_first_basic_block()
        .expect("Function has no entry basic block");

    match first_block.get_first_instruction() {
      Some(instr) => builder.position_before(&instr),
      None => builder.position_at_end(first_block),
    };

    let llvm_type = self.make_llvm_type(var_type.get_ctor_id());
    let llvm_type = self.as_basic_type(llvm_type);

    let result_ptr = builder.build_alloca(llvm_type, var_name).unwrap();
    (result_ptr, llvm_type)
  }

  /// Registers the top-level declaration in LLVM without generating its code.
  pub fn register_top_lvl(&mut self, node: &MIRTopLevelNode) {
    match node {
      MIRTopLevelNode::DeclareFunction {
        function_name,
        parameters,
        body: _,
        scopes: _,
        return_type,
        type_table,
        struct_name,
      } => {
        self.create_function(
          *function_name,
          parameters,
          type_table[return_type].mono.get_ctor_id(),
          false,
          false,
          type_table,
          struct_name.as_ref(),
        );
      }

      MIRTopLevelNode::IntrinsicOrExternalFunction {
        function_name,
        parameters,
        return_type,
        is_varargs,
        is_external,
        type_table,
      } => {
        self.create_function(
          *function_name,
          parameters,
          type_table[return_type].mono.get_ctor_id(),
          *is_external,
          *is_varargs,
          type_table,
          None,
        );
      }
    }
  }

  fn add_type_data(&mut self, type_instance: TypeConstructorId) {
    //we need to build a str with the name, and a size
    let instance = self.type_db.find(type_instance);
    let name =
        self.build_string_struct_value_for_globals(StringInterner::get().borrow(instance.name));
    let size = self
        .context
        .i64_type()
        .const_int(instance.compute_size(self.type_db).0 as u64, false);

    let llvm_struct_instance = self
        .type_data_llvm_type
        .unwrap()
        .into_struct_type()
        .const_named_struct(&[name.into(), size.into()]);

    let as_basic_type = self.type_data_llvm_type.unwrap().into_struct_type();

    let global = self.module.add_global(
      as_basic_type,
      None,
      &format!(".TypeData_{}", &instance.name.to_string()),
    );
    global.set_initializer(&llvm_struct_instance);
    global.set_linkage(Linkage::Internal);
    global.set_constant(true);
    self.type_data.insert(type_instance, global);
  }

  pub fn generate_for_top_lvl<'source>(&mut self, node: &'source MIRTopLevelNode) {
    match node {
      MIRTopLevelNode::DeclareFunction {
        function_name,
        parameters,
        body,
        scopes,
        return_type: _,
        type_table,
        struct_name,
      } => {
        let function_layout = generate_function_layout(scopes, self.type_db, type_table);
        let function_name = match struct_name {
          Some(struct_name) => format!("{struct_name}::{function_name}"),
          None => format!("{function_name}"),
        };
        let function_signature = *self
            .functions
            .get(&function_name.into())
            .expect("Expected function to be registered already");

        let cur_basic = self.context.append_basic_block(function_signature, "entry");
        self.builder.position_at_end(cur_basic);

        //@TODO cloneless: HashMap<&'str, ..
        let mut llvm_variables: DeterministicMap<
          InternedString,
          (PointerValue<'_>, BasicTypeEnum<'_>),
        > = DeterministicMap::new();

        for (i, param) in function_signature.get_param_iter().enumerate() {
          let param_name = &parameters[i].name;
          let alloca = self
              .builder
              .build_alloca(param.get_type(), param_name.to_string().as_str())
              .unwrap();
          self.builder.build_store(alloca, param).unwrap();
          llvm_variables.insert(*param_name, (alloca, param.get_type()));
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
            //if this function has been already loaded by parameters, then skip
            if llvm_variables.contains_key(name) {
              continue;
            };
            let mono_type = &type_table[type_instance];
            let ptr = self.create_variable_stack_alloc(
              function_signature,
              name.to_string().as_str(),
              mono_type.mono.clone(),
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
            .build_unconditional_branch(llvm_basic_blocks[0])
            .unwrap();

        for (i, block) in body.iter().enumerate() {
          let basic_block = llvm_basic_blocks[i];
          self.builder.position_at_end(basic_block);
          for node in block.nodes.iter() {
            match node {
              MIRBlockNode::Assign {
                path, expression, ..
              } => {
                let LlvmLValue(ptr, _) = self.compile_expr_lvalue(
                  block.scope,
                  &symbol_table,
                  &MIRExpr::LValue(path.clone()),
                  type_table,
                );
                let value_to_write = self
                    .compile_expr_rvalue(
                      block.scope,
                      &symbol_table,
                      expression,
                      type_table,
                    )
                    .get_value();
                self.builder.build_store(ptr, value_to_write).unwrap();
              }

              MIRBlockNode::FunctionCall { function, args, .. } => {
                //TODO deduplicate this code
                let llvm_args = args
                    .iter()
                    .map(|x| {
                      self.compile_expr_rvalue(
                        block.scope,
                        &symbol_table,
                        x,
                        type_table,
                      )
                          .get_value().into()
                    })
                    .collect::<Vec<_>>();

                self.build_function_call_and_enqueue_codegen(function, llvm_args);
              }
              MIRBlockNode::MethodCall {
                object,
                method_name,
                args,
                return_type: _,
                ..
              } => {
                let object_type = &type_table[object.get_type()];
                let method_name = self.get_method_name_type_id(
                  object_type.mono.get_ctor_id(),
                  *method_name,
                );
                let mut llvm_args: Vec<BasicMetadataValueEnum> = vec![];
                let LlvmLValue(ptr, _) = self
                    .compile_expr_lvalue(
                      block.scope,
                      &symbol_table,
                      object,
                      type_table,
                    );
                //TODO deduplicate this code
                let call_args: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|x| {
                      self.compile_expr_rvalue(
                        block.scope,
                        &symbol_table,
                        x,
                        type_table,
                      )
                          .get_value()
                          .into()
                    })
                    .collect::<Vec<_>>();

                llvm_args.push(ptr.into());
                llvm_args.extend(call_args);

                if let Some(f) = self.functions.get(&method_name) {
                  //@TODO this is weird
                  self.builder
                      .build_call(*f, &llvm_args, &self.make_temp(""))
                      .unwrap();
                } else {
                  self.build_function_call_and_enqueue_codegen(
                    &method_name,
                    llvm_args,
                  );
                }
              }
            }
          }

          match &block.finish {
            MIRBlockFinal::If(expr, true_branch, false_branch, _) => {
              let expr_compiled = self
                  .compile_expr_rvalue(block.scope, &symbol_table, expr, type_table)
                  .get_value();
              self.builder
                  .build_conditional_branch(
                    expr_compiled.into_int_value(),
                    llvm_basic_blocks[true_branch.0],
                    llvm_basic_blocks[false_branch.0],
                  )
                  .unwrap();
            }
            MIRBlockFinal::GotoBlock(block) => {
              self.builder
                  .build_unconditional_branch(llvm_basic_blocks[block.0])
                  .unwrap();
            }
            MIRBlockFinal::Return(expr, _) => {
              let expr_compiled = self
                  .compile_expr_rvalue(block.scope, &symbol_table, expr, type_table)
                  .get_value();
              self.builder.build_return(Some(&expr_compiled)).unwrap();
            }
            MIRBlockFinal::EmptyReturn(..) => {
              self.builder.build_return(None).unwrap();
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
        if function_name.as_ref().starts_with("ptr_cast") && !is_external {
          let function_signature = *self
              .functions
              .get(function_name)
              .expect("Expected function ptr_cast to be registered already");
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

          let casted = self
              .builder
              .build_pointer_cast(origin, target, "casted")
              .unwrap();
          self.builder.build_return(Some(&casted)).unwrap();
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
          .unwrap()
    } else {
      log!("build_function_call_and_enqueue_codegen {function}");
      let top_lvl = self.top_lvl_names.get(function).expect(
        "Function not found in top lvl declarations, this is a bug in the compiler",
      );
      self.codegen_queue.push_back(top_lvl);
      self.register_top_lvl(top_lvl);
      if let Some(f) = self.functions.get(function) {
        self.builder
            .build_call(*f, &llvm_args, &self.make_temp("fcall_ret"))
            .unwrap()
      } else {
        panic!("Function not found in top lvl declarations, this is a bug in the compiler");
      }
    }
  }

  pub fn compile_expr_rvalue(
    &mut self,
    current_scope: ScopeId,
    symbol_table: &FunctionSymbolTable<'ctx>,
    expression: &MIRExpr,
    type_table: &TypeTable,
  ) -> LlvmRValue<'ctx> {
    match expression {
      MIRExpr::RValue(rvalue) => {
        let types = &self.type_db.common_types;

        match rvalue {
          MIRExprRValue::Literal(literal, ty, _) => {
            self.build_literal(literal, types, &type_table[*ty].mono.get_ctor_id())
          }
          MIRExprRValue::BinaryOperation(lhs, op, rhs, _ty, _) => self.build_binary_expr(
            lhs,
            rhs,
            current_scope,
            symbol_table,
            op,
            types,
            type_table,
          ),
          MIRExprRValue::MethodCall(object, method_name, args, _ty, _) => {
            let mono = &type_table[object.get_type()].mono;
            let MonoType::Application(ctor, param) = mono else {
              panic!("Unsupported type on method call: {mono:?}")
            };

            if *ctor == self.type_db.common_types.ptr {
              //generate intrinsic

              if method_name == "__index_ptr__" {
                let self_value = self
                    .compile_expr_rvalue(
                      current_scope,
                      symbol_table,
                      object,
                      type_table,
                    )
                    .get_value()
                    .into_pointer_value();

                let pointee =
                    self.make_llvm_type(param.first().unwrap().get_ctor_id());
                let index_arg = self
                    .compile_expr_rvalue(
                      current_scope,
                      symbol_table,
                      args.first().unwrap(),
                      type_table,
                    ).get_value();

                let index_arg = index_arg.into_int_value();

                let gep = unsafe {
                  match pointee {
                    AnyTypeEnum::ArrayType(ty) => {
                      self.builder.build_in_bounds_gep(
                        ty,
                        self_value,
                        &[index_arg],
                        "self_at_index",
                      )
                    }
                    AnyTypeEnum::FloatType(ty) => {
                      self.builder.build_in_bounds_gep(
                        ty,
                        self_value,
                        &[index_arg],
                        "self_at_index",
                      )
                    }
                    AnyTypeEnum::FunctionType(ty) => {
                      panic!("Function type not supported")
                    }
                    AnyTypeEnum::IntType(ty) => {
                      self.builder.build_in_bounds_gep(
                        ty,
                        self_value,
                        &[index_arg],
                        "self_at_index",
                      )
                    }
                    AnyTypeEnum::PointerType(ty) => {
                      self.builder.build_in_bounds_gep(
                        ty,
                        self_value,
                        &[index_arg],
                        "self_at_index",
                      )
                    }
                    AnyTypeEnum::StructType(ty) => {
                      self.builder.build_in_bounds_gep(
                        ty,
                        self_value,
                        &[index_arg],
                        "self_at_index",
                      )
                    }
                    AnyTypeEnum::VectorType(ty) => {
                      self.builder.build_in_bounds_gep(
                        ty,
                        self_value,
                        &[index_arg],
                        "self_at_index",
                      )
                    }
                    AnyTypeEnum::ScalableVectorType(ty) => {
                      self.builder.build_in_bounds_gep(
                        ty,
                        self_value,
                        &[index_arg],
                        "self_at_index",
                      )
                    }
                    AnyTypeEnum::VoidType(ty) => {
                      panic!("Void type not supported")
                    }
                  }
                };
                let gep = gep.unwrap();
                LlvmRValue(gep.as_basic_value_enum())
              } else {
                panic!("ptr intrinsic not found: {method_name}")
              }
            } else {
              let method_name = self.get_method_name_type_id(*ctor, *method_name);
              let mut llvm_args: Vec<BasicMetadataValueEnum> = vec![];
              let self_value = self
                  .compile_expr_lvalue(
                    current_scope,
                    &symbol_table,
                    object,
                    type_table,
                  );
              //TODO deduplicate this code
              let call_args: Vec<BasicMetadataValueEnum> = args
                  .iter()
                  .map(|x| {
                    self.compile_expr_rvalue(
                      current_scope,
                      symbol_table,
                      x,
                      type_table,
                    )
                        .get_value()
                        .into()
                  })
                  .collect::<Vec<_>>();

              llvm_args.push(self_value.0.into());
              llvm_args.extend(call_args);

              if let Some(f) = self.functions.get(&method_name) {
                let call_site_val =
                    self.builder.build_call(*f, &llvm_args, &self.make_temp(""));
                LlvmRValue(
                  call_site_val.unwrap().try_as_basic_value()
                      .expect_basic("Expected method to return some value, but this function probably returns void and somehow passed the type checker")
                )
              } else {
                let call_site_val =self.build_function_call_and_enqueue_codegen(
                  &method_name,
                  llvm_args,
                );
                LlvmRValue(
                  call_site_val.try_as_basic_value()
                      .expect_basic("Expected method to return some value, but this function probably returns void and somehow passed the type checker")
                )
                //panic!("Method not yet added to llvm IR {method_name}");
              }
            }
          }
          MIRExprRValue::FunctionCall(function_expr, arg_exprs, _ty, _, _) => {
            //let expr = self.compile_expr_rvalue(current_scope, symbol_table, &function_expr);
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
                      .compile_expr_rvalue(current_scope, symbol_table, x, type_table)
                      .get_value()
                      //.load(self.builder);
                      ;
                  expr_compiled.into()
                })
                .collect::<Vec<_>>();

            let call_site_val =
                self.build_function_call_and_enqueue_codegen(var_name, llvm_args);

            let expr = call_site_val
                .try_as_basic_value()
                .expect_basic("Expected function to return some value, but this function probably returns void and somehow passed the type checker");

            LlvmRValue(expr)
          }
          MIRExprRValue::Ref(expr, ..) => {
            //you can only create a pointer to something that is an lvalue (i.e. has a memory location)
            //and the result is an rvalue, a pointer i.e. ptr<i8>. You can't create a reference to a reference immediately, like &&.

            //So the semantics of it isn't "create a pointer", but rather "get the pointer to the lvalue".

            //Therefore in practice this only extracts the pointer from the lvalue.
            //Suppose you have a variable x and we are doing &x.
            //The X lives somewhere in memory (there is an alloca related to it, a ptr),
            //then we do a compile_expr on that variable. The result is an lvalue which contains the pointer.

            let LlvmLValue(ptr, _) = self.compile_expr_lvalue(
              current_scope,
              symbol_table,
              &MIRExpr::LValue(*expr.clone()),
              type_table,
            );
            LlvmRValue(ptr.as_basic_value_enum())
          }
          MIRExprRValue::UnaryExpression(op, expr, _, _) => {
            let lhs_type = expr.get_type();
            let LlvmRValue(compiled) = self
                .compile_expr_rvalue(current_scope, symbol_table, expr, type_table);

            let lhs_type = type_table[lhs_type].mono.get_ctor_id();

            let expr = match op.0 {
              Operator::Plus => compiled,
              Operator::Minus => {
                if self.type_db.is_integer(lhs_type) || lhs_type == types.bool {
                  self.builder
                      .build_int_neg(
                        compiled.into_int_value(),
                        &self.make_temp("int"),
                      )
                      .unwrap()
                      .into()
                } else if self.type_db.is_float(lhs_type) {
                  self.builder
                      .build_float_neg(
                        compiled.into_float_value(),
                        &self.make_temp("float"),
                      )
                      .unwrap()
                      .into()
                } else {
                  todo!("Not implemented")
                }
              }
              _ => {
                todo!("Not implemented unary operator: {op:?}")
              }
            };
            LlvmRValue(expr)
          }
          MIRExprRValue::Array(_, _, _) => todo!(),
          MIRExprRValue::StructInstantiate(ty, _) => {
            let ctor = type_table[*ty].mono.get_ctor_id();
            let ctor = self.type_db.find(ctor);
            let llvm_str_type = self.make_llvm_type(type_table[*ty].mono.get_ctor_id());
            let llvm_struct = self
                .builder
                .build_alloca(
                  llvm_str_type.into_struct_type(),
                  &self.make_temp("struct"),
                )
                .unwrap();
            let ctor_name = ctor.name.to_string();
            LlvmRValue(
              self.load_any(llvm_struct, llvm_str_type, &ctor_name)
            )
          }
          MIRExprRValue::Cast(expr, casted_type, _) => {
            let LlvmRValue(expr_compiled) = self
                .compile_expr_rvalue(current_scope, symbol_table, expr, type_table);

            let llvm_casted_type =
                self.make_llvm_type(type_table[casted_type].mono.get_ctor_id());

            //generate a numeric cast, i.e. i32 -> i64, f32 -> f64, i32 -> f64, etc

            let casted: BasicValueEnum = if self
                .type_db
                .is_integer(type_table[expr.get_type()].mono.get_ctor_id())
            {
              if self
                  .type_db
                  .is_integer(type_table[casted_type].mono.get_ctor_id())
              {
                self.builder
                    .build_int_cast(
                      expr_compiled.into_int_value(),
                      self.as_basic_type(llvm_casted_type).into_int_type(),
                      &self.make_temp("casted"),
                    )
                    .unwrap()
                    .into()
              } else if self
                  .type_db
                  .is_float(type_table[casted_type].mono.get_ctor_id())
              {
                self.builder
                    .build_signed_int_to_float(
                      expr_compiled.into_int_value(),
                      self.as_basic_type(llvm_casted_type).into_float_type(),
                      &self.make_temp("casted"),
                    )
                    .unwrap()
                    .into()
              } else {
                todo!("Cast not implemented")
              }
            } else if self
                .type_db
                .is_float(type_table[expr.get_type()].mono.get_ctor_id())
            {
              if self
                  .type_db
                  .is_integer(type_table[casted_type].mono.get_ctor_id())
              {
                self.builder
                    .build_float_to_signed_int(
                      expr_compiled.into_float_value(),
                      self.as_basic_type(llvm_casted_type).into_int_type(),
                      &self.make_temp("casted"),
                    )
                    .unwrap()
                    .into()
              } else if self
                  .type_db
                  .is_float(type_table[casted_type].mono.get_ctor_id())
              {
                self.builder
                    .build_float_cast(
                      expr_compiled.into_float_value(),
                      self.as_basic_type(llvm_casted_type).into_float_type(),
                      &self.make_temp("casted"),
                    )
                    .unwrap()
                    .into()
              } else {
                todo!("Cast not implemented")
              }
            } else {
              todo!("Cast not implemented")
            };

            LlvmRValue(casted)
          }
          MIRExprRValue::TypeVariable { type_variable, .. } => {
            let type_data_type = self.type_db.find_by_name("TypeData".into()).unwrap();
            let llvm_type_data_type = self.make_llvm_type(type_data_type.id);
            let ctor_id = type_table[*type_variable].mono.get_ctor_id();
            let global = self.type_data.get(&ctor_id);
            match global {
              Some(type_data) => {
                let loaded_global_var = self
                    .builder
                    .build_load(
                      llvm_type_data_type.into_struct_type(),
                      type_data.as_pointer_value(),
                      "loaded_type_data",
                    )
                    .unwrap();
                LlvmRValue(loaded_global_var)
              }
              None => {
                self.add_type_data(ctor_id);
                let global = self.type_data.get(&ctor_id).unwrap();
                let loaded_global_var = self
                    .builder
                    .build_load(
                      llvm_type_data_type.into_struct_type(),
                      global.as_pointer_value(),
                      "loaded_type_data",
                    )
                    .unwrap();
                LlvmRValue(loaded_global_var)
              }
            }
          }
        }
      }
      MIRExpr::LValue(lvalue) => {
        match lvalue {
          MIRExprLValue::Variable(name, _, _) => {
            log!(
                            "LValue compile, name = {} {:?}, {}",
                            name,
                            name,
                            symbol_table.len()
                        );

            match symbol_table.get(&(*name, current_scope)) {
              Some((ptr, ty)) => {
                let as_any = self.as_any_type(*ty);
                let loaded = self.load_any(*ptr, as_any, &name.to_string());
                LlvmRValue(loaded)
              }
              None => {
                panic!(
                  "Variable not found in symbol table or type data globals: {}",
                  name
                )
              }
            }
          }
          MIRExprLValue::MemberAccess(obj, member, _, _) => {
            let LlvmLValue(llvm_expr, llvm_expr_ty) = self
                .compile_expr_lvalue(current_scope, symbol_table, obj, type_table);

            let type_ctor = self
                .type_db
                .find(type_table[obj.get_type()].mono.get_ctor_id());
            let (index, field) = type_ctor
                .fields
                .iter()
                .enumerate()
                .find(|(index, field)| field.name == *member)
                .unwrap();

            let field_ty = self.make_llvm_type(field.field_type.get_ctor_id());

            println!("index = {}", index);
            println!("obj = {obj:#?}");
            println!("member = {member:#?}");
            println!("member = {llvm_expr:#?}");

            let field_ptr = self
                .builder
                .build_struct_gep(
                  llvm_expr_ty.into_struct_type(),
                  llvm_expr,
                  index as u32,
                  &self.make_temp("var"),
                )
                .unwrap();
            let loaded = self.load_any(field_ptr, field_ty, &field.name.to_string());
            LlvmRValue(loaded)
          }
          MIRExprLValue::Deref(expr, _, _) => {
            log!("DEREF: {expr:?}");
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
            let mono = &type_table[expr.get_type()].mono;

            let MonoType::Application(ctor_id, param) = mono else {
              panic!("deref on non-application type")
            };

            if *ctor_id != self.type_db.common_types.ptr {
              panic!("deref on non-ptr")
            }

            let pointee = param
                .first()
                .expect("must have at least 1 type arg for pointer");

            let type_ctor = self.make_llvm_type(pointee.get_ctor_id());


            let LlvmRValue(basic) =
                self.compile_expr_rvalue(current_scope, symbol_table, expr, type_table);

            //basic needs to be a pointer. If it was a variable, compile_expr_rvalue would have loaded it already.
            //if it was a field, then the field type itself if a ptr, like buf in str
            //if it was a method,
            let as_ptr = basic.into_pointer_value();

            //now we load because we are in rvalue ctx

            let loaded = self.load_any(as_ptr, type_ctor, "deref_rvalue_ptr");

            return LlvmRValue(loaded);
          }
        }
      }
    }
  }

  pub fn compile_expr_lvalue(
    &mut self,
    current_scope: ScopeId,
    symbol_table: &FunctionSymbolTable<'ctx>,
    expression: &MIRExpr,
    type_table: &TypeTable,
  ) -> LlvmLValue<'ctx> {
    match expression {
      MIRExpr::RValue(rvalue) => {
        match rvalue {
          MIRExprRValue::Literal(literal, ty, _) => {
            if let LiteralMIRExpr::String(s) = literal {
              let str = self.type_db.find_by_name("str".into()).unwrap();
              let llvm_str_type = self.make_llvm_type(str.id);
              let (str_alloc, _) = self.build_string_literal(&s.to_string());
              LlvmLValue(str_alloc, llvm_str_type)
            } else {
              panic!("invalid lvalue {expression:?}")
            }
          }
          MIRExprRValue::BinaryOperation(lhs, op, rhs, _ty, _) => {
            panic!("invalid lvalue {expression:?}")
          }
          MIRExprRValue::MethodCall(.., ty, _) => {
            panic!("invalid lvalue {expression:?}")
          }
          MIRExprRValue::FunctionCall(.., ty, _, _) => {
            panic!("invalid lvalue {expression:?}")
          }
          MIRExprRValue::Ref(expr, ..) => {
            //you can only create a pointer to something that is an lvalue (i.e. has a memory location)
            //and the result is an rvalue, a pointer i.e. ptr<i8>. You can't create a reference to a reference immediately, like &&.

            //So the semantics of it isn't "create a pointer", but rather "get the pointer to the lvalue".

            //Therefore in practice this only extracts the pointer from the lvalue.
            //Suppose you have a variable x and we are doing &x.
            //The X lives somewhere in memory (there is an alloca related to it, a ptr),
            //then we do a compile_expr on that variable. The result is an lvalue which contains the pointer.

            self.compile_expr_lvalue(
              current_scope,
              symbol_table,
              &MIRExpr::LValue(*expr.clone()),
              type_table,
            )
          }
          MIRExprRValue::UnaryExpression(op, expr, _, _) => {
            panic!("invalid lvalue {expression:?}")
          }
          MIRExprRValue::Array(_, _, _) => panic!("invalid lvalue {expression:?}"),
          MIRExprRValue::StructInstantiate(ty, _) => {
            panic!("invalid lvalue {expression:?}")
          }
          MIRExprRValue::Cast(expr, casted_type, _) => {
            panic!("invalid lvalue {expression:?}")
          }
          MIRExprRValue::TypeVariable { type_variable, .. } => {
            let type_data_type = self.type_db.find_by_name("TypeData".into()).unwrap();
            let llvm_type_data_type = self.make_llvm_type(type_data_type.id);
            let ctor_id = type_table[*type_variable].mono.get_ctor_id();
            let global = self.type_data.get(&ctor_id);

            match global {
              Some(type_data) => {
                LlvmLValue(type_data.as_pointer_value(), llvm_type_data_type)
              }
              None => {
                self.add_type_data(ctor_id);
                let global = self.type_data.get(&ctor_id).unwrap();
                LlvmLValue(global.as_pointer_value(), llvm_type_data_type)
              }
            }
          }
        }
      }
      MIRExpr::LValue(lvalue) => {
        match lvalue {
          MIRExprLValue::Variable(name, _, _) => {
            log!(
                            "LValue compile, name = {} {:?}, {}",
                            name,
                            name,
                            symbol_table.len()
                        );

            match symbol_table.get(&(*name, current_scope)) {
              Some((ptr, ty)) => {
                let as_any = self.as_any_type(*ty);
                LlvmLValue(*ptr, as_any)
              }
              None => {
                panic!(
                  "Variable not found in symbol table or type data globals: {}",
                  name
                )
              }
            }
          }

          MIRExprLValue::MemberAccess(obj, member, _, _) => {
            //need to compile as lvalue because we need the place of obj
            let LlvmLValue(llvm_expr, llvm_expr_ty) = self
                .compile_expr_lvalue(current_scope, symbol_table, obj, type_table);

            let type_ctor = self
                .type_db
                .find(type_table[obj.get_type()].mono.get_ctor_id());
            let (index, field) = type_ctor
                .fields
                .iter()
                .enumerate()
                .find(|(index, field)| field.name == *member)
                .unwrap();

            let field_ty = self.make_llvm_type(field.field_type.get_ctor_id());

            println!("index = {}", index);
            println!("obj = {obj:#?}");
            println!("member = {member:#?}");
            println!("member = {llvm_expr:#?}");

            let field_ptr = self
                .builder
                .build_struct_gep(
                  llvm_expr_ty.into_struct_type(),
                  llvm_expr,
                  index as u32,
                  &self.make_temp("var"),
                )
                .unwrap();

            LlvmLValue(field_ptr, field_ty)
          }
          MIRExprLValue::Deref(expr, _, _) => {
            log!("DEREF: {expr:?}");
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
            let mono = &type_table[expr.get_type()].mono;

            let MonoType::Application(ctor_id, param) = mono else {
              panic!("deref on non-application type")
            };

            if *ctor_id != self.type_db.common_types.ptr {
              panic!("deref on non-ptr")
            }

            let pointee = param
                .first()
                .expect("must have at least 1 type arg for pointer");

            let type_ctor = self.make_llvm_type(pointee.get_ctor_id());


            let LlvmRValue(basic) =
                self.compile_expr_rvalue(current_scope, symbol_table, expr, type_table);

            //basic needs to be a pointer. If it was a variable, compile_expr_rvalue would have loaded it already.
            //if it was a field, then the field type itself if a ptr, like buf in str
            //if it was a method,
            let as_ptr = basic.into_pointer_value();

            return LlvmLValue(as_ptr, type_ctor);
          }
        }
      }
    }
  }

  fn build_binary_expr(
    &mut self,
    lhs: &MIRExpr,
    rhs: &MIRExpr,
    current_scope: ScopeId,
    symbol_table: &FunctionSymbolTable<'ctx>,
    op: &crate::ast::parser::SpannedOperator,
    types: &crate::types::type_constructor_db::CommonTypeConstructors,
    type_table: &TypeTable,
  ) -> LlvmRValue<'ctx> {
    let lhs_type = type_table[lhs.get_type()].mono.get_ctor_id();
    let rhs_type = type_table[rhs.get_type()].mono.get_ctor_id();

    if lhs_type != rhs_type {
      panic!(
        "both sides of expression must be the same type: {lhs_type:?} {rhs_type:?}  {lhs:?} {rhs:?}"
      )
    }
    let lhs = self
        .compile_expr_rvalue(current_scope, symbol_table, lhs, type_table)
        .get_value();

    let rhs = self
        .compile_expr_rvalue(current_scope, symbol_table, rhs, type_table)
        .get_value();

    use paste::paste;

    macro_rules! make_op {
            ($method:tt) => {
                if self.type_db.is_integer(lhs_type) {
                    paste! {
                        self.builder.[<build_int_ $method >] (
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &self.make_temp("int")).unwrap().into()
                    }
                } else if lhs_type == types.f32 || lhs_type == types.f64 {
                    paste! {
                        self.builder.[<build_float_ $method >] (
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        &self.make_temp("float")).unwrap().into()
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
              .unwrap()
              .into()
        } else if lhs_type == types.u32 || lhs_type == types.u64 {
          self.builder
              .build_int_unsigned_div(
                lhs.into_int_value(),
                rhs.into_int_value(),
                &self.make_temp("int"),
              )
              .unwrap()
              .into()
        } else if lhs_type == types.f32 || lhs_type == types.f64 {
          self.builder
              .build_float_div(
                lhs.into_float_value(),
                rhs.into_float_value(),
                &self.make_temp("float"),
              )
              .unwrap()
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
              .unwrap()
              .into()
        } else if lhs_type == types.u32 || lhs_type == types.u64 {
          self.builder
              .build_int_unsigned_rem(
                lhs.into_int_value(),
                rhs.into_int_value(),
                &self.make_temp("int"),
              )
              .unwrap()
              .into()
        } else {
          panic!("unsupported op")
        }
      }
      Operator::BitShiftLeft => {
        if self.type_db.is_integer(lhs_type) {
          self.builder
              .build_left_shift(
                lhs.into_int_value(),
                rhs.into_int_value(),
                &self.make_temp("int"),
              )
              .unwrap()
              .into()
        } else {
          panic!("Cannot << in non-int type")
        }
      }
      Operator::BitShiftRight => {
        if self.type_db.is_integer(lhs_type) {
          self.builder
              .build_right_shift(
                lhs.into_int_value(),
                rhs.into_int_value(),
                false,
                &self.make_temp("int"),
              )
              .unwrap()
              .into()
        } else {
          panic!("Cannot >> in non-int type")
        }
      }

      Operator::Equals => {
        if self.type_db.is_integer(lhs_type) || lhs_type == types.bool {
          self.builder
              .build_int_compare(
                inkwell::IntPredicate::EQ,
                lhs.into_int_value(),
                rhs.into_int_value(),
                &self.make_temp("int"),
              )
              .unwrap()
              .into()
        } else if self.type_db.is_float(lhs_type) {
          self.builder
              .build_float_compare(
                inkwell::FloatPredicate::OEQ,
                lhs.into_float_value(),
                rhs.into_float_value(),
                &self.make_temp("float"),
              )
              .unwrap()
              .into()
        } else {
          panic!(
            "Not implemented Equals for type {}",
            lhs_type.to_string(self.type_db)
          )
        }
      }
      Operator::NotEquals => {
        if self.type_db.is_integer(lhs_type) || lhs_type == types.bool {
          self.builder
              .build_int_compare(
                inkwell::IntPredicate::NE,
                lhs.into_int_value(),
                rhs.into_int_value(),
                &self.make_temp("int"),
              )
              .unwrap()
              .into()
        } else if self.type_db.is_float(lhs_type) {
          self.builder
              .build_float_compare(
                inkwell::FloatPredicate::ONE,
                lhs.into_float_value(),
                rhs.into_float_value(),
                &self.make_temp("float"),
              )
              .unwrap()
              .into()
        } else {
          panic!(
            "Not implemented Not Equals for type {}",
            lhs_type.to_string(self.type_db)
          )
        }
      }
      Operator::Or => {
        if self.type_db.is_integer(lhs_type) || lhs_type == types.bool {
          self.builder
              .build_or(
                lhs.into_int_value(),
                rhs.into_int_value(),
                &self.make_temp("int"),
              )
              .unwrap()
              .into()
        } else {
          panic!(
            "Not implemented OR for type {}",
            lhs_type.to_string(self.type_db)
          )
        }
      }
      Operator::And => {
        if self.type_db.is_integer(lhs_type) || lhs_type == types.bool {
          self.builder
              .build_and(
                lhs.into_int_value(),
                rhs.into_int_value(),
                &self.make_temp("int"),
              )
              .unwrap()
              .into()
        } else {
          panic!(
            "Not implemented And for type {}",
            lhs_type.to_string(self.type_db)
          )
        }
      }
      Operator::Xor => {
        if self.type_db.is_integer(lhs_type) || lhs_type == types.bool {
          self.builder
              .build_xor(
                lhs.into_int_value(),
                rhs.into_int_value(),
                &self.make_temp("int"),
              )
              .unwrap()
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
              .unwrap()
              .into()
        } else if lhs_type == types.u32 || lhs_type == types.u64 {
          self.builder
              .build_int_compare(
                inkwell::IntPredicate::UGT,
                lhs.into_int_value(),
                rhs.into_int_value(),
                &self.make_temp("int"),
              )
              .unwrap()
              .into()
        } else if lhs_type == types.f32 || lhs_type == types.f64 {
          self.builder
              .build_float_compare(
                inkwell::FloatPredicate::OGT,
                lhs.into_float_value(),
                rhs.into_float_value(),
                &self.make_temp("int"),
              )
              .unwrap()
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
              .unwrap()
              .into()
        } else if lhs_type == types.u32 || lhs_type == types.u64 {
          self.builder
              .build_int_compare(
                inkwell::IntPredicate::UGE,
                lhs.into_int_value(),
                rhs.into_int_value(),
                &self.make_temp("int"),
              )
              .unwrap()
              .into()
        } else if lhs_type == types.f32 || lhs_type == types.f64 {
          self.builder
              .build_float_compare(
                inkwell::FloatPredicate::OGE,
                lhs.into_float_value(),
                rhs.into_float_value(),
                &self.make_temp("int"),
              )
              .unwrap()
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
              .unwrap()
              .into()
        } else if lhs_type == types.u32 || lhs_type == types.u64 {
          self.builder
              .build_int_compare(
                inkwell::IntPredicate::ULT,
                lhs.into_int_value(),
                rhs.into_int_value(),
                &self.make_temp("int"),
              )
              .unwrap()
              .into()
        } else if lhs_type == types.f32 || lhs_type == types.f64 {
          self.builder
              .build_float_compare(
                inkwell::FloatPredicate::OLT,
                lhs.into_float_value(),
                rhs.into_float_value(),
                &self.make_temp("int"),
              )
              .unwrap()
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
              .unwrap()
              .into()
        } else if lhs_type == types.u32 || lhs_type == types.u64 {
          self.builder
              .build_int_compare(
                inkwell::IntPredicate::ULE,
                lhs.into_int_value(),
                rhs.into_int_value(),
                &self.make_temp("int"),
              )
              .unwrap()
              .into()
        } else if lhs_type == types.f32 || lhs_type == types.f64 {
          self.builder
              .build_float_compare(
                inkwell::FloatPredicate::OLE,
                lhs.into_float_value(),
                rhs.into_float_value(),
                &self.make_temp("int"),
              )
              .unwrap()
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
    LlvmRValue(val)
  }

  fn build_literal(
    &mut self,
    literal: &LiteralMIRExpr,
    types: &crate::types::type_constructor_db::CommonTypeConstructors,
    literal_type: &TypeConstructorId,
  ) -> LlvmRValue<'ctx> {
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
        LlvmRValue(val.into())
      }
      LiteralMIRExpr::Char(c) => {
        let val = self.context.i8_type().const_int(*c as u64, false);
        LlvmRValue(val.into())
      }
      LiteralMIRExpr::Float(f) => {
        let val = if types.f32 == *literal_type {
          self.context.f32_type().const_float(f.0 as f32 as f64)
        } else if types.f64 == *literal_type {
          self.context.f64_type().const_float(f.0)
        } else {
          panic!("Unkown type")
        };
        LlvmRValue(val.into())
      }
      LiteralMIRExpr::Boolean(b) => LlvmRValue(
        self.context
            .bool_type()
            .const_int(if *b { 1 } else { 0 }, false)
            .into(),
      ),
      LiteralMIRExpr::String(s) => {
        let str = self.type_db.find_by_name("str".into()).unwrap();
        let llvm_str_type = self.make_llvm_type(str.id);
        let (str_alloc, _) = self.build_string_literal(&s.to_string());
        LlvmRValue(self.load_any(str_alloc, llvm_str_type, "str"))
      } //LiteralMIRExpr::None => todo!("none not implemented in llvm"),
    }
  }

  fn build_string_literal(&mut self, s: &str) -> (PointerValue<'ctx>, AnyTypeEnum) {
    //build a buf, len struct

    let mut null_terminated = s.to_string();
    null_terminated.push('\0');

    //let buf_val = self.context.const_string(null_terminated.as_bytes(), true);
    let len_val = self
        .context
        .i64_type()
        .const_int(s.to_string().as_str().len() as u64, false);
    let str = self.type_db.find_by_name("str".into()).unwrap();
    let llvm_str_type = self.make_llvm_type(str.id);

    let global_str = self
        .builder
        .build_global_string_ptr(&null_terminated, ".str")
        .unwrap();
    let buf_val = global_str.as_pointer_value();

    //allocate struct
    let str_alloc = self
        .builder
        .build_alloca(llvm_str_type.into_struct_type(), "_str")
        .unwrap();
    {
      let ptr_to_buf = self
          .builder
          .build_struct_gep(llvm_str_type.into_struct_type(), str_alloc, 0, "_str_buf")
          .unwrap();
      self.builder.build_store(ptr_to_buf, buf_val).unwrap();
    }

    {
      let ptr_to_len = self
          .builder
          .build_struct_gep(llvm_str_type.into_struct_type(), str_alloc, 1, "_str_len")
          .unwrap();
      self.builder.build_store(ptr_to_len, len_val).unwrap();
    }
    (str_alloc, llvm_str_type)
  }

  fn build_string_struct_value_for_globals(&mut self, s: &str) -> StructValue<'ctx> {
    //build a buf, len struct

    let mut null_terminated = s.to_string();
    null_terminated.push('\0');

    //let buf_val = self.context.const_string(null_terminated.as_bytes(), true);

    let str = self.type_db.find_by_name("str".into()).unwrap();
    let llvm_str_type = self.make_llvm_type(str.id);

    let len_val = self
        .context
        .i64_type()
        .const_int(s.to_string().as_str().len() as u64, false);

    let global_str = self
        .builder
        .build_global_string_ptr(&null_terminated, ".str")
        .unwrap();

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
      if type_data.id == self.type_db.common_types.ptr {
        //self.generate_ptr_write_intrinsic(type_data);
        //self.generate_ptr_index_ptr_intrinsic(type_data);
      }
    }
  }

  //makes a type name including generics without special characters
  fn get_mangled_type_name(&mut self, ty: &TypeConstructorId) -> InternedString {
    let existing_mangled_name = self.mangled_name_cache.get(ty);
    if let Some(existing_mangled_name) = existing_mangled_name {
      return *existing_mangled_name;
    }
    let base_constructor = self.type_db.find(*ty);
    let name = base_constructor.name.to_string();
    //let name = name.replace("[", "__");
    //let name = mangled.replace("]", "__");
    self.mangled_name_cache.insert(*ty, name.into());
    return *self.mangled_name_cache.get(&ty).unwrap();
  }

  fn get_mangled_type_name_from_name(&mut self, name: &str) -> InternedString {
    let name = name.to_string();
    //let name = name.replace("[", "__");
    //let name = name.replace("]", "__");
    name.into()
  }

  fn get_llvm_method_name(
    &mut self,
    type_data: &TypeConstructorId,
    method_name: InternedString,
  ) -> InternedString {
    //try to find in cache first
    let existing_mangled_name = self.mangled_method_cache.get(&(*type_data, method_name));
    if let Some(existing_mangled_name) = existing_mangled_name {
      return *existing_mangled_name;
    }

    let type_name = self.get_mangled_type_name(type_data);
    let mangled_method_name = format!("{}::{}", type_name, method_name);
    //let mangled_method_name = format!("{}__{}", type_name, method_name);
    self.mangled_method_cache
        .insert((*type_data, method_name), mangled_method_name.into());
    return *self
        .mangled_method_cache
        .get(&(*type_data, method_name))
        .unwrap();
  }

  fn get_method_name_type_id(
    &mut self,
    type_id: TypeConstructorId,
    method_name: InternedString,
  ) -> InternedString {
    return self.get_llvm_method_name(&type_id, method_name);
  }

  fn generate_ptr_write_intrinsic(&mut self, type_data: &TypeConstructor) {
    //generate intrinsic for ptr<T>.write
    let method_write = type_data
        .find_method("write".into())
        .expect("ptr type HAS to have write method!");
    let method_name = self.get_llvm_method_name(&type_data.id, "write".into());

    let method_type_details = self.type_db.find(method_write.signature);
    assert_eq!(method_type_details.kind, TypeKind::Function);

    //get the first argument which will be a u64 (trust me bro)
    let index_arg = &method_type_details.function_params[0];
    let element_arg = &method_type_details.function_params[1]; //this will be a ptr<T>, i.e. exact same type as type_data (we hope!)

    let mut type_table = TypeTable::new();

    let parameters = vec![
      MIRTypedBoundName {
        name: "self".into(),
        type_instance: type_table.next_with(PolyType::mono(MonoType::simple(type_data.id))),
      },
      MIRTypedBoundName {
        name: "index".into(),
        type_instance: type_table.next_with(PolyType::mono(index_arg.clone())),
      },
      MIRTypedBoundName {
        name: "element".into(),
        type_instance: type_table.next_with(PolyType::mono(element_arg.clone())),
      },
    ];

    let return_type = method_type_details.function_return_type.as_ref().unwrap();

    //@TODO refaator create_function call, pass the struct type
    let llvm_function = self.create_function(
      method_name.into(),
      &parameters,
      return_type.get_ctor_id(),
      false,
      false,
      &type_table,
      None,
    );
    let cur_basic = self.context.append_basic_block(llvm_function, "entry");
    self.builder.position_at_end(cur_basic);

    let self_arg = llvm_function.get_params()[0].into_pointer_value();
    let index_arg = llvm_function.get_params()[1].into_int_value();
    let element_arg = llvm_function.get_params()[2];

    let self_llvm_type = self.make_llvm_type(type_data.id);
    let store_address = unsafe {
      match self_llvm_type {
        AnyTypeEnum::ArrayType(t) => {
          self.builder
              .build_in_bounds_gep(t, self_arg, &[index_arg], "self_at_index")
        }
        AnyTypeEnum::FloatType(t) => {
          self.builder
              .build_in_bounds_gep(t, self_arg, &[index_arg], "self_at_index")
        }
        AnyTypeEnum::FunctionType(t) => panic!("Cannot store in function ptr"),
        AnyTypeEnum::IntType(t) => {
          self.builder
              .build_in_bounds_gep(t, self_arg, &[index_arg], "self_at_index")
        }
        AnyTypeEnum::PointerType(t) => {
          self.builder
              .build_in_bounds_gep(t, self_arg, &[index_arg], "self_at_index")
        }
        AnyTypeEnum::StructType(t) => {
          self.builder
              .build_in_bounds_gep(t, self_arg, &[index_arg], "self_at_index")
        }
        AnyTypeEnum::VectorType(t) => {
          self.builder
              .build_in_bounds_gep(t, self_arg, &[index_arg], "self_at_index")
        }
        AnyTypeEnum::ScalableVectorType(t) => {
          self.builder
              .build_in_bounds_gep(t, self_arg, &[index_arg], "self_at_index")
        }
        AnyTypeEnum::VoidType(t) => panic!("Cannot store in void ptr"),
      }
    }
        .unwrap();
    self.builder
        .build_store(store_address, element_arg)
        .unwrap();

    self.builder.build_return(None).unwrap();
  }

  fn generate_ptr_index_ptr_intrinsic(&mut self, type_data: &TypeConstructor) {
    //generate intrinsic for ptr<T>.__index_ptr__

    let method_index_ptr = type_data
        .find_method("__index_ptr__".into())
        .expect("ptr type HAS to have write method!");
    let method_name = self.get_llvm_method_name(&type_data.id, "__index_ptr__".into());

    let method_type_details = self.type_db.find(method_index_ptr.signature);
    assert_eq!(method_type_details.kind, TypeKind::Function);

    let mut type_table = TypeTable::new();

    //get the first argument which will be a u64 (trust me bro)
    let index_arg = &method_type_details.function_params[0];
    let parameters = vec![
      MIRTypedBoundName {
        name: "self".into(),
        type_instance: type_table.next_with(PolyType::mono(MonoType::simple(type_data.id))),
      },
      MIRTypedBoundName {
        name: "index".into(),
        type_instance: type_table.next_with(PolyType::mono(index_arg.clone())),
      },
    ];

    let return_type = method_type_details.function_return_type.as_ref().unwrap();

    let llvm_function = self.create_function(
      method_name,
      &parameters,
      return_type.get_ctor_id(),
      false,
      false,
      &type_table,
      None,
    );
    let cur_basic = self.context.append_basic_block(llvm_function, "entry");
    self.builder.position_at_end(cur_basic);

    let self_arg = llvm_function.get_params()[0].into_pointer_value();
    let index_arg = llvm_function.get_params()[1].into_int_value();
    let self_llvm_type = self.make_llvm_type(type_data.id);
    let store_address = unsafe {
      match self_llvm_type {
        AnyTypeEnum::ArrayType(t) => {
          self.builder
              .build_in_bounds_gep(t, self_arg, &[index_arg], "self_at_index")
        }
        AnyTypeEnum::FloatType(t) => {
          self.builder
              .build_in_bounds_gep(t, self_arg, &[index_arg], "self_at_index")
        }
        AnyTypeEnum::FunctionType(t) => panic!("Cannot store in function ptr"),
        AnyTypeEnum::IntType(t) => {
          self.builder
              .build_in_bounds_gep(t, self_arg, &[index_arg], "self_at_index")
        }
        AnyTypeEnum::PointerType(t) => {
          self.builder
              .build_in_bounds_gep(t, self_arg, &[index_arg], "self_at_index")
        }
        AnyTypeEnum::StructType(t) => {
          self.builder
              .build_in_bounds_gep(t, self_arg, &[index_arg], "self_at_index")
        }
        AnyTypeEnum::VectorType(t) => {
          self.builder
              .build_in_bounds_gep(t, self_arg, &[index_arg], "self_at_index")
        }
        AnyTypeEnum::ScalableVectorType(t) => {
          self.builder
              .build_in_bounds_gep(t, self_arg, &[index_arg], "self_at_index")
        }
        AnyTypeEnum::VoidType(t) => panic!("Cannot store in void ptr"),
      }
    }
        .unwrap();

    self.builder.build_return(Some(&store_address)).unwrap();
  }
}

type FunctionSymbolTable<'llvm_ctx> =
DeterministicMap<(InternedString, ScopeId), (PointerValue<'llvm_ctx>, BasicTypeEnum<'llvm_ctx>)>;

fn build_function_symbol_table<'mir, 'function_layout, 'ctx>(
  body: &'mir [MIRBlock],
  scopes: &'mir [MIRScope],
  function_layout: &'function_layout FunctionLayout,
  llvm_variables: DeterministicMap<InternedString, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>,
) -> FunctionSymbolTable<'ctx> {
  let mut symbol_table =
      DeterministicMap::<(InternedString, ScopeId), (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>::new();
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
  type_db: &TypeConstructorDatabase,
  mir_top_level_nodes: &[MIRTopLevelNode],
) -> Result<(), Box<dyn Error>> {
  let context = Context::create();
  let module = context.create_module("program");
  let builder = context.create_builder();

  let mut codegen = CodeGen {
    context: &context,
    module: &module,
    builder: &builder,
    type_db,
    type_cache: DeterministicMap::new(),
    next_temporary: 0,
    type_data: DeterministicMap::new(),
    functions: DeterministicMap::new(),
    mangled_name_cache: DeterministicMap::new(),
    mangled_method_cache: DeterministicMap::new(),
    type_data_llvm_type: None,
    codegen_queue: VecDeque::new(),
    top_lvl_names: DeterministicMap::new(),
  };

  for top_lvl in mir_top_level_nodes {
    match top_lvl {
      MIRTopLevelNode::IntrinsicOrExternalFunction { function_name, .. } => {
        codegen.top_lvl_names.insert(*function_name, top_lvl);
      }
      MIRTopLevelNode::DeclareFunction {
        function_name,
        struct_name,
        ..
      } => {
        let mut name = function_name.to_string();
        if let Some(struct_name) = struct_name {
          name = format!("{}::{}", struct_name, name);
        };
        log!("Registered {name}");

        codegen.top_lvl_names.insert(name.into(), top_lvl);
      }
    }
  }

  codegen.generate_intrinsics();

  //codegen.generate_type_data_globals();

  let type_data_type = codegen.type_db.find_by_name("TypeData".into()).unwrap();
  let llvm_type_data_type = codegen.make_llvm_type(type_data_type.id);
  codegen.type_data_llvm_type = Some(llvm_type_data_type);
  codegen
      .type_cache
      .insert(type_data_type.id, llvm_type_data_type);

  for mir_node in mir_top_level_nodes {
    //    codegen.register_top_lvl(mir_node);
  }

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

  std::fs::write("./last_compiled.asm", asm_buffer.as_slice())?;
  std::fs::write(&output_obj_file, obj_buffer.as_slice())?;

  match link(&output_obj_file, "./last_compiled") {
    Ok(_) => {}
    Err(LinkerError::GenericLinkerError(e)) => {
      eprintln!("Linker error!\n{e}")
    }
  }
  std::fs::remove_file(output_obj_file)?;

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
