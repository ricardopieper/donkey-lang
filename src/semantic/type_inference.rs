use std::collections::HashMap;

use crate::ast::parser::{Spanned, SpannedOperator};
use crate::interner::InternedString;
use crate::report;
use crate::semantic::hir::{HIRExpr, HIRType, HIRTypedBoundName, LiteralHIRExpr, HIR};

use crate::types::type_constructor_db::{FunctionSignature, TypeConstructParams, TypeParameter, TypeConstructorId};
use crate::types::diagnostics::{
    BinaryOperatorNotFound, CallToNonCallableType, CompilerErrorData, ContextualizedCompilerError,
    DerefOnNonPointerError, ErrorReporter,
    InsufficientTypeInformationForArray, InternalError, OutOfTypeBounds, RefOnNonLValueError,
    TypeConstructionFailure, TypeErrors, TypePromotionFailure, UnaryOperatorNotFound,
    VariableNotFound, DerefOnNonPointerErrorUnconstructed, FieldNotFound, MethodNotFound,
};
use crate::types::type_instance_db::{TypeConstructionError, TypeInstanceId, TypeInstanceManager};

use super::compiler_errors::CompilerError;
use super::context::FileTableIndex;
use super::hir::{
    FirstAssignmentsDeclaredHIR, FirstAssignmentsDeclaredHIRRoot, FunctionCall, HIRAstMetadata,
    HIRExprMetadata, HIRRoot, HIRTypeDef, HIRUserTypeInfo, InferredTypeHIR, InferredTypeHIRRoot,
    MethodCall, TypeInferenceCertainty,
};
use super::hir_printer::HIRExprPrinter;
use super::hir_type_resolution::{hir_type_to_usage, RootElementType};
use super::top_level_decls::NameRegistry;
use super::type_name_printer::TypeNamePrinter;

pub type TypeInferenceInputHIRRoot<'source> = FirstAssignmentsDeclaredHIRRoot<'source>;
pub type TypeInferenceInputHIR<'source> = FirstAssignmentsDeclaredHIR<'source>;

pub struct TypeInferenceContext<'compiler_state, 'source> {
    pub on_function: RootElementType,
    //pub on_file: FileTableIndex,
    //If the function is def foo<T>(), this will contain ["T"]
    pub type_parameters: Vec<TypeParameter>,
    //@TODO move function parameters into here
    pub type_db: &'compiler_state mut TypeInstanceManager,
    pub errors: &'compiler_state mut TypeErrors<'source>,
    pub decls_in_scope: &'compiler_state mut NameRegistry,
}

impl ErrorReporter for TypeInferenceContext<'_, '_> {
    fn report<T: CompilerErrorData>(
        &self,
        error: T,
        span: &impl Spanned,
        compiler_code_location: &'static str,
    ) -> crate::types::diagnostics::CompilerErrorContext<T> {
        error.at_spanned(self.on_function, span, compiler_code_location)
    }
}

impl<'source> TypeInferenceContext<'_, 'source> {
    pub fn make_usage(
        &mut self,
        typedef: &HIRType,
        location: &impl Spanned,
    ) -> Result<TypeConstructParams, CompilerError> {
        hir_type_to_usage(
            self.on_function,
            typedef,
            self.type_db,
            &self.type_parameters,
            self.errors,
            location
        )
    }

    pub fn instantiate_type(
        &mut self,
        typedef: &HIRType,
        location: &impl Spanned,
    ) -> Result<TypeInferenceResult, CompilerError> {
        let usage = self.make_usage(typedef, location)?;
        //Now that we have a mf type usage, we may be able to construct the type.
        //If we have a type usage *at all*, it means that the type isn't completely wrong, just might be
        //missing some information. So we can try to construct it. If the typedb returns insufficient information
        //then we just delay the construction.
        self.try_construct(usage, location)
    }

    pub fn try_construct(
        &mut self,
        usage: TypeConstructParams,
        location: &impl Spanned,
    ) -> Result<TypeInferenceResult, CompilerError> {
        match self.type_db.construct_usage(&usage) {
            Ok(instance_id) => Ok(TypeInferenceResult::Monomorphic(instance_id)),
            Err(TypeConstructionError::InsufficientInformation) => {
                Ok(TypeInferenceResult::Polymorphic(usage))
            }
            Err(e) => {
                log!("Error constructing type: {:?}", usage);
                self.errors
                    .type_construction_failure
                    .push_inference_error(report!(
                        self,
                        location,
                        TypeConstructionFailure { error: e }
                    ))?
            }
        }
    }
}

pub struct FunctionTypeInferenceContext<'compiler_state, 'source> {
    pub ctx: TypeInferenceContext<'compiler_state, 'source>,
}

impl ErrorReporter for FunctionTypeInferenceContext<'_, '_> {
    fn report<T: CompilerErrorData>(
        &self,
        error: T,
        span: &impl Spanned,
        compiler_code_location: &'static str,
    ) -> crate::types::diagnostics::CompilerErrorContext<T> {
        error.at_spanned(
            self.ctx.on_function,
            span,
            compiler_code_location,
        )
    }
}

#[derive(Clone, Debug)]
pub enum TypeInferenceResult {
    //Fully resolved
    Monomorphic(TypeInstanceId),
    //Partially resolved, i.e. a function with some arguments
    Polymorphic(TypeConstructParams),
}

impl TypeInferenceResult {
    fn try_construct_argless(&self, type_db: &mut TypeInstanceManager) -> Option<TypeInstanceId> {
        match self {
            TypeInferenceResult::Monomorphic(id) => Some(*id),
            TypeInferenceResult::Polymorphic(usage) => usage
                .try_get_root_type_constructor_id()
                .and_then(|x| type_db.construct_type(x, &[]).ok()),
        }
    }

    pub fn expect_monomorphic(&self) -> TypeInstanceId {
        match self {
            TypeInferenceResult::Monomorphic(id) => *id,
            TypeInferenceResult::Polymorphic(_) => panic!("Expected monomorphic type"),
        }
    }
}

impl TypeNamePrinter for TypeInferenceResult {
    fn print_name(&self, type_db: &TypeInstanceManager) -> String {
        match self {
            TypeInferenceResult::Monomorphic(id) => id.print_name(type_db),
            TypeInferenceResult::Polymorphic(usage) => {
                format!("Polymorphic({})", usage.print_name(type_db))
            }
        }
    }
}

impl<'source> FunctionTypeInferenceContext<'_, 'source> {
    pub fn instantiate_type(
        &mut self,
        typedef: &HIRType,
        location: &impl Spanned,
    ) -> Result<TypeInferenceResult, CompilerError> {
        self.ctx.instantiate_type(typedef, location)
    }

    fn try_literal_promotion(
        &mut self,
        literal: &LiteralHIRExpr,
        type_hint: TypeInstanceId,
        meta: &'source crate::ast::parser::Expr,
    ) -> Result<TypeInstanceId, CompilerError> {
        match literal {
            LiteralHIRExpr::Integer(i) => {
                //we need to check for promotions and the attempted promoted type
                //and see if it's in range, i.e. we can't promote 23752432 to an u8

                macro_rules! check_promotion {
                    ($type:ty, $type_id:expr) => {
                        if type_hint == $type_id {
                            if *i >= <$type>::MIN as i128 && *i <= <$type>::MAX as i128 {
                                return Ok(type_hint);
                            }
                            else {
                                return self.ctx.errors.out_of_bounds.push_inference_error(report!(self, meta, OutOfTypeBounds {
                                    expr: meta, //@TODO unecessary, at_spanned already contains the metadata
                                    typ: type_hint
                                }))?;
                            }
                        }
                    };
                }
                macro_rules! check_promotions {
                    ($($type:ty: $type_id:expr),*) => {
                        $(
                            check_promotion!($type, $type_id);
                        )*
                    };
                }
                let type_db = &self.ctx.type_db;
                check_promotions!(
                    u8: type_db.common_types.u8,
                    u32: type_db.common_types.u32,
                    u64: type_db.common_types.u64,
                    i32: type_db.common_types.i32,
                    i64: type_db.common_types.i64
                );

                return self
                    .ctx
                    .errors
                    .type_promotion_failure
                    .push_inference_error(report!(
                        self,
                        meta,
                        TypePromotionFailure {
                            target_type: type_hint,
                        }
                    ))?;
            }
            _ => panic!("Cannot promote value: {:?}", literal),
        }
    }

    pub fn infer_expr(
        &mut self,
        expression: HIRExpr<'source, ()>,
        type_hint: Option<TypeInferenceResult>,
    ) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        match expression {
            HIRExpr::Variable(var, _, meta) => {
                log!("Infering variable: {}", var);
                let decl_type = self.ctx.decls_in_scope.get(&var);

                match decl_type {
                    Some(decl_type) => Ok(HIRExpr::Variable(var, decl_type.clone(), meta)),
                    None => {
                        let type_data_type_id = self.load_stdlib_builtin("TypeData");
                        
                        //try to find a type with this name
                        let type_id = self.ctx.type_db.find_by_name(&var.to_string());

                        //if we have it, then we're done
          
                        if let Some(type_id) = type_id {
                            return Ok(HIRExpr::TypeName { 
                                type_variable: TypeInferenceResult::Monomorphic(type_id.id), 
                                type_data: TypeInferenceResult::Monomorphic(type_data_type_id),
                                meta
                            });
                        }

                        //if we don't have it, maybe it's a type variable coming from a generic
                        let is_type_param = self
                            .ctx
                            .type_parameters
                            .iter()
                            .find(|x| x.0 == var);

                        match is_type_param {
                            Some(type_param) => {
                                Ok(
                                    HIRExpr::TypeName { 
                                        type_variable: TypeInferenceResult::Polymorphic(TypeConstructParams::Generic(type_param.clone())), 
                                        type_data: TypeInferenceResult::Monomorphic(type_data_type_id),
                                        meta 
                                    }
                                )
                            }
                            None => {
                                self
                                .ctx
                                .errors
                                .variable_not_found
                                .push_inference_error(report!(
                                    self,
                                    meta,
                                    VariableNotFound { variable_name: var }
                                ))
                            }
                        }
                    }
                }
            }
            HIRExpr::Literal(literal_expr, _, meta) => {
                self.infer_literal(literal_expr, &type_hint, meta)
            }
            HIRExpr::BinaryOperation(lhs, op, rhs, _, _, meta) => {
                self.infer_binop(*lhs, meta, *rhs, op)
            }
            HIRExpr::FunctionCall(fcall) => {
                let FunctionCall {
                    function,
                    args,
                    type_args,
                    return_type: _,
                    meta_expr,
                    ..
                } = *fcall;
                let fcall_inferred =
                    self.infer_function_call(function, type_args, args, meta_expr)?;
                Ok(HIRExpr::FunctionCall(fcall_inferred.into()))
            }
            HIRExpr::UnaryExpression(op, rhs, _, _, meta) => self.infer_unary_expr(*rhs, meta, op),
            HIRExpr::Deref(rhs, _, meta) => self.infer_deref_expr(*rhs, meta),
            HIRExpr::Ref(rhs, _, meta) => self.infer_ref_expr(*rhs, meta),
            HIRExpr::MemberAccess(obj, name, _, meta) => self.infer_member_access(*obj, meta, name),
            //we will get the type of the first item, and use it as a type and instantiate an Array generic type.
            //a later step will do the type checking.
            HIRExpr::Array(array_items, _, meta) => self.infer_array(array_items, type_hint, meta),
            HIRExpr::Cast(expr, user_type, _, meta) => {
                let expr = self.infer_expr(*expr, None)?;
                let ty = self.instantiate_type(&user_type, meta)?;
                Ok(HIRExpr::Cast(expr.into(), user_type, ty, meta))
            }
            HIRExpr::MethodCall(mcall) => {
                let MethodCall {
                    object,
                    method_name,
                    args,
                    return_type: _,
                    meta_expr,
                } = mcall;
                Ok(HIRExpr::MethodCall(self.infer_method_call(
                    *object,
                    method_name,
                    args,
                    meta_expr,
                )?))
            }
            HIRExpr::StructInstantiate(struct_name, type_args, _, meta) => {
                self.infer_struct_instantiate(struct_name, type_args, meta)
            }
            HIRExpr::TypeName { .. } => {
                panic!("Generic and given type names should not be created before type inference. They are created *during* type inference!")
            }
        }
    }

    fn load_stdlib_builtin(&mut self, name: &str) -> TypeInstanceId {

        //check if instance already exists
        let type_data_type = self.ctx.type_db.find_by_name(name);

        match type_data_type {
            Some(d) => return d.id,
            None => {
                let type_data_type_constructor = self
                    .ctx
                    .type_db
                    .constructors
                    .find_by_name(InternedString::new(name));

                match type_data_type_constructor {
                    Some(type_data_type_constructor) => {
                        let type_data_type_instance = self.ctx.type_db.construct_type(type_data_type_constructor.id, &[]).unwrap();
                        return type_data_type_instance;
                    }
                    None => {
                        //TODO: Fix: navigate through all types and roots before running inference,
                        //otherwise we can't use types in stdlib
                        panic!("Type {} not found in stdlib", name);
                    }
                }
            }
        }

        
    }

    fn infer_literal(
        &mut self,
        literal_expr: LiteralHIRExpr,
        type_hint: &Option<TypeInferenceResult>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        let literal_type = match literal_expr {
            LiteralHIRExpr::Integer(_) => {
                if let Some(hint) = type_hint {
                    match hint {
                        TypeInferenceResult::Monomorphic(mono_type_hint) => {
                            self.try_literal_promotion(&literal_expr, *mono_type_hint, meta)?
                        }
                        TypeInferenceResult::Polymorphic(_) => {
                            let constructed = hint.try_construct_argless(self.ctx.type_db);
                            match constructed {
                                Some(type_hint) => {
                                    self.try_literal_promotion(&literal_expr, type_hint, meta)?
                                }
                                None => self.ctx.type_db.common_types.i32,
                            }
                        }
                    }
                } else {
                    self.ctx.type_db.common_types.i32
                }
            }
            LiteralHIRExpr::Char(_) => {
                //@TODO promotable?
                self.ctx.type_db.common_types.char
            }
            LiteralHIRExpr::Float(_) => self.ctx.type_db.common_types.f32,
            LiteralHIRExpr::String(_) => {
                self.load_stdlib_builtin("str")
            }
            LiteralHIRExpr::Boolean(_) => self.ctx.type_db.common_types.bool,
            LiteralHIRExpr::None => todo!("Must implement None"),
        };
        Ok(HIRExpr::Literal(
            literal_expr,
            TypeInferenceResult::Monomorphic(literal_type),
            meta,
        ))
    }

    fn infer_struct_instantiate(
        &mut self,
        struct_name: InternedString,
        struct_type_args: Vec<HIRType>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        //since we already have a """good""" way to transform types into TypeUsage and potentially infer them,
        //we can transform the data we got into a HIRType, run InstantiateType in the whole thing
        //and badabing badaboom
        let as_hir_type = HIRType::Generic(struct_name, struct_type_args.clone());
        let printed_hir = as_hir_type.print_name(&self.ctx.type_db);
        log!("Struct type being constructed: {}", printed_hir);
        let ty = self.instantiate_type(&as_hir_type, meta)?;
        Ok(HIRExpr::StructInstantiate(
            struct_name,
            struct_type_args,
            ty,
            meta,
        ))
    }


    fn infer_function_call(
        &mut self,
        //this is the expression which the function was called, normally this is a variable (TODO: right now it's only variables/identifiers)
        fun_expr: HIRExpr<'source, ()>,
        //these are the type args at the function call site, i.e. if the function is `def foo<T(a: T) -> T` and we call it like `foo::<i32>(1)`, then this will be `i32`
        fun_type_args: Vec<HIRUserTypeInfo<()>>,
        fun_params: Vec<HIRExpr<'source, ()>>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<FunctionCall<'source, TypeInferenceResult>, CompilerError> {
        let HIRExpr::Variable(var, .., fcall_meta) = fun_expr else {
            todo!("Currently only function calls on names are supported");
        };

        log!("Infering function call: {}", var);

        let function_type = {
            let Some(function_type) = self.ctx.decls_in_scope.get(&var) else {
                return self
                    .ctx
                    .errors
                    .variable_not_found
                    .push_inference_error(report!(
                        self,
                        meta,
                        VariableNotFound { variable_name: var }
                    ));
            };
            function_type.clone()
        };

        match function_type {
            TypeInferenceResult::Monomorphic(function_type) => {
                let type_data = self.ctx.type_db.get_instance(function_type);

                if !type_data.is_function {
                    return self
                        .ctx
                        .errors
                        .call_non_callable
                        .push_inference_error(report!(
                            self,
                            meta,
                            CallToNonCallableType {
                                actual_type: Some(function_type)
                            }
                        ));
                }

                let function_args = type_data
                    .function_args
                    .iter()
                    .map(|x| TypeInferenceResult::Monomorphic(*x))
                    .collect::<Vec<_>>();

                let fun_params = self.infer_expr_array(fun_params, Some(&function_args))?;

                //end the previous borrow and start a new one
                let type_data = self.ctx.type_db.get_instance(function_type);

                let result = FunctionCall {
                    function: HIRExpr::Variable(
                        var,
                        TypeInferenceResult::Monomorphic(function_type),
                        fcall_meta,
                    )
                    .into(),
                    args: fun_params,
                    type_args: vec![],
                    return_type: TypeInferenceResult::Monomorphic(
                        type_data.function_return_type.unwrap(),
                    ),
                    meta_expr: meta,
                    meta_ast: None,
                };

                let strr = HIRExprPrinter::new(&self.ctx.type_db, false).print(&result.function);

                log!("Function call variable (monomorphic): {}", strr);

                Ok(result)
            }
            
            //In this case, the signature of the function will propagate unchanged,
            //but the types **on the arguments** and **return type** will be substituted.
            /*
            def read<T>(p: ptr<T>) -> T:
                intrinsic
            
            def main():
                let x = read<i32>(...)

            In this case, read will always be a function with signature <T>(ptr<T>) -> T,
            but the type of x will be substituted to i32.

            def read<T>(p: ptr<T>) -> T:
                intrinsic
            
            def get_item<X>(x: ptr<X>) -> X:
                item = read<X>(x)
                return item

            def main():
                let x = get_item<i32>(...)

            In this case, again, read will always be a function with signature <T>(ptr<T>) -> T,
            but x will be inferred as ptr<X>, and item will also be inferred as X.

            */
            TypeInferenceResult::Polymorphic(sig @ TypeConstructParams::FunctionSignature(_)) => {
                
         
                let positional_type_args = self.to_type_constructor_params(fun_type_args, meta)?;

                let substituted = self.generic_substitute(&positional_type_args, &sig, &[]);
                
                //the return of substituted has to be a FunctionSignature...
                let TypeConstructParams::FunctionSignature(substituted) = substituted else {
                    return self.ctx.errors.internal_error.push_inference_error(report!(
                        self,
                        meta,
                        InternalError {
                            error: "Expected substituted type to be a function signature"
                                .to_string()
                        }
                    ));
                };
                let cloned = substituted.clone();
                //try to run inference on the function parameters
                let args_inferred = substituted
                    .params
                    .into_iter()
                    .map(|param| self.ctx.try_construct(param, meta))
                    .collect::<Result<Vec<_>, _>>()?;

                let fun_params = self.infer_expr_array(fun_params, Some(&args_inferred))?;

                let type_args_inferred = self.try_construct_many(positional_type_args, meta)?;
                //println!("Type args inferred: {:?}", type_args_inferred);
                let result = FunctionCall {
                    function: HIRExpr::Variable(
                        var,
                        TypeInferenceResult::Polymorphic(sig), //this keeps the signature the same
                        fcall_meta,
                    )
                    .into(),
                    args: fun_params,
                    type_args: type_args_inferred,
                    return_type: TypeInferenceResult::Polymorphic(*substituted.return_type),
                    meta_expr: meta,
                    meta_ast: None,
                };

                let strr = HIRExprPrinter::new(&self.ctx.type_db, false).print(&result.function);

                log!("Function call variable (polymorphic): {}", strr);
                let type_printed = TypeInferenceResult::Polymorphic(
                    TypeConstructParams::FunctionSignature(cloned),
                )
                .print_name(&self.ctx.type_db);
                log!(
                    "Function call variable type (polymorphic): {}",
                    type_printed
                );

                Ok(result)
            }
            TypeInferenceResult::Polymorphic(_) => {
                return self
                    .ctx
                    .errors
                    .call_non_callable
                    .push_inference_error(report!(
                        self,
                        meta,
                        CallToNonCallableType { actual_type: None }
                    ))
            }
        }
    }

    fn infer_method_call(
        &mut self,
        obj: HIRExpr<'source, ()>,
        method_name: InternedString,
        params: Vec<HIRExpr<'source, ()>>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<MethodCall<'source, TypeInferenceResult>, CompilerError> {
        //compute type of obj
        //find method_name in type of obj
        let objexpr = self.infer_expr(obj, None)?;
        let typeof_obj = objexpr.get_type();

        match typeof_obj {
            TypeInferenceResult::Monomorphic(typeof_obj) => {
                let type_data = self.ctx.type_db.get_instance(typeof_obj);
                //we'll find the method call here by name
                let method = type_data.find_method_by_name(method_name);
                if let Some(method) = method {
                    let method_type = self.ctx.type_db.get_instance(method.function_type);
                    let return_type = method_type.function_return_type.unwrap();
                    let type_hints = method_type
                        .function_args
                        .clone()
                        .into_iter()
                        .map(|x| TypeInferenceResult::Monomorphic(x))
                        .collect::<Vec<_>>();
                    let args = self.infer_expr_array(params, Some(&type_hints))?;

                    let mcall = MethodCall {
                        object: objexpr.into(),
                        method_name,
                        args,
                        return_type: TypeInferenceResult::Monomorphic(return_type),
                        meta_expr: meta,
                    };

                    Ok(mcall)
                } else {
                    self.ctx
                        .errors
                        .method_not_found
                        .push_inference_error(report!(
                            self,
                            meta,
                            MethodNotFound {
                                object_type: typeof_obj,
                                method: method_name,
                            }
                        ))?
                }
            }
            
            TypeInferenceResult::Polymorphic(poly_ty) => {
                //we are going to see in the type constructors if the method exists and try to infer something about it
                let constructor = poly_ty.try_get_root_type_constructor_id();
                //println!("method call: {:?}", poly_ty.print_name(self.ctx.type_db));
                if let Some(constructor) = constructor {
                    let constructor = self.ctx.type_db.constructors.find(constructor);

                    let method = constructor.find_method(method_name);

                    if let Some(method) = method {
                        
                        let method_call = self.infer_polymorphic_method_call(poly_ty.clone(), &method.clone(), meta, params.clone(), &objexpr, method_name)?;
                        return Ok(method_call);
                    }   
                }

                //There wasn't a root constructor ID, the object is generic, this will have to be solved in monomorph,
                //and we have no type hints
                let args = self.infer_expr_array(params, None)?;

                let mcall = MethodCall {
                    object: objexpr.into(),
                    method_name,
                    args,
                    return_type: TypeInferenceResult::Polymorphic(poly_ty),
                    meta_expr: meta,
                };

                Ok(mcall)
            }
        }
    }

    fn infer_polymorphic_method_call(&mut self, 
        poly_ty: TypeConstructParams, 
        method: &crate::types::type_constructor_db::TypeConstructorFunctionDeclaration, 
        meta: HIRExprMetadata<'source>, 
        params: Vec<HIRExpr<'source, ()>>, 
        objexpr: &HIRExpr<'source, TypeInferenceResult>, 
        method_name: InternedString) -> Result<MethodCall<'source, TypeInferenceResult>, CompilerError> {
        let root_constructor = poly_ty.try_get_root_type_constructor_id().unwrap();
        match poly_ty {
            TypeConstructParams::Given(_) => unreachable!("Should not happen: Inference of such a simple type should have resulted in a monomorphic type"),
            TypeConstructParams::Generic(_) => unreachable!("Should not happen: The fact we have a root constructor means this is not just a type parameter"),
            TypeConstructParams::FunctionSignature(_) => todo!("Don't know what to do here yet but could be useful for metaprogramming?"),
            TypeConstructParams::Parameterized(root, args) => {
       
                //now we have a map of type parameters and their types substituted, 
                //let's pretend we actually have a function call whose first argument is the object
                
                let method_as_fn = method.into_function_signature(root_constructor, &self.ctx.type_db.constructors);
        
                let positional_type_args = args.clone().into_iter()
                    .map(|x| HIRUserTypeInfo { user_given_type: None, resolved_type: x } ).collect::<Vec<_>>();

                let substituted = self.generic_substitute(
                    &positional_type_args, 
                    &TypeConstructParams::FunctionSignature(method_as_fn.clone()), &[]);
        
                       
                //the return of substituted has to be a FunctionSignature...
                let TypeConstructParams::FunctionSignature(substituted) = substituted else {
                    return self.ctx.errors.internal_error.push_inference_error(report!(
                        self,
                        meta,
                        InternalError {
                            error: "Method call generic substitution: Expected substituted type to be a function signature"
                                .to_string()
                        }
                    ));
                };

                //since the method_as_fn has a faked-in object parameter (representing this/self), 
                //we need to skip it when inferring the arguments

                let skipped_first = substituted.params.iter().skip(1).cloned().collect::<Vec<_>>();

                let args_inferred = skipped_first
                    .into_iter()
                    .map(|param| self.ctx.try_construct(param, meta))
                    .collect::<Result<Vec<_>, _>>()?;
            
                let fun_params = self.infer_expr_array(params, Some(&args_inferred))?;

                let type_args_inferred = self.try_construct_many(positional_type_args, meta)?;
                //println!("Type args inferred: {:?}", type_args_inferred);
    
                let mcall = MethodCall {
                    object: objexpr.clone().into(),
                    method_name,
                    args: fun_params,
                    return_type: TypeInferenceResult::Polymorphic(*substituted.return_type),
                    meta_expr: meta,
                };

                return Ok(mcall);
            },
        }
    }



    fn infer_array(
        &mut self,
        array_items: Vec<HIRExpr<'source, ()>>,
        type_hint: Option<TypeInferenceResult>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        if array_items.is_empty() {
            if let Some(hint) = type_hint {
                Ok(HIRExpr::Array(vec![], hint, meta))
            } else {
                self.ctx
                    .errors
                    .insufficient_array_type_info
                    .push_inference_error(report!(
                        self,
                        meta,
                        InsufficientTypeInformationForArray {}
                    ))?
            }
        } else {
            let all_exprs = self.infer_expr_array(array_items, None)?;

            let first_typed_item = all_exprs.first();

            let array_type = self.ctx.type_db.constructors.common_types.array;

            if let Some(expr) = first_typed_item {
                if let TypeInferenceResult::Monomorphic(expr_type) = expr.get_type() {
                    let array_type_generic_replaced =
                        self.ctx.type_db.construct_type(array_type, &[expr_type]);

                    match array_type_generic_replaced {
                        Ok(array_type_generic_replaced) => Ok(HIRExpr::Array(
                            all_exprs,
                            TypeInferenceResult::Monomorphic(array_type_generic_replaced),
                            meta,
                        )),
                        Err(e) => self
                            .ctx
                            .errors
                            .type_construction_failure
                            .push_inference_error(report!(
                                self,
                                meta,
                                TypeConstructionFailure { error: e }
                            )),
                    }
                } else {
                    Ok(HIRExpr::Array(all_exprs.clone(), expr.get_type(), meta))
                }
            } else {
                //array has items but all of them failed type inference lmao
                //no choice but to give up and return a fully unresolved array
                //hint does not matter much
                self.ctx
                    .errors
                    .insufficient_array_type_info
                    .push_inference_error(report!(
                        self,
                        meta,
                        InsufficientTypeInformationForArray {}
                    ))?
            }
        }
    }

    fn infer_member_access(
        &mut self,
        obj: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
        name: InternedString,
    ) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        log!("Infering member access: {}", name);
        let obj_expr = self.infer_expr(obj, None)?;

        let typeof_obj = obj_expr.get_type();

        match typeof_obj {
            TypeInferenceResult::Monomorphic(typeof_obj) => {
                let type_data = self.ctx.type_db.get_instance(typeof_obj);

                let field = type_data.fields.iter().find(|field| field.name == name);
                if let Some(field) = field {
                    let resolved_type = field.field_type;
                    Ok(HIRExpr::MemberAccess(
                        obj_expr.into(),
                        name,
                        TypeInferenceResult::Monomorphic(resolved_type),
                        meta,
                    ))
                } else {
                    self.ctx
                        .errors
                        .field_not_found
                        .push_inference_error(report!(
                            self,
                            meta,
                            FieldNotFound {
                                object_type: typeof_obj,
                                field: name,
                            }
                        ))?
                }
            }
            TypeInferenceResult::Polymorphic(poly_ty) => {
                //we are going to see in the type constructors if the field exists and try to infer something about it
                let constructor = poly_ty.try_get_root_type_constructor_id();

                if let Some(constructor) = constructor {
                    let constructor = self.ctx.type_db.constructors.find(constructor);

                    let field = constructor.find_field(name);

                    if let Some(field) = field {
                        let return_type =
                            TypeInferenceResult::Polymorphic(field.field_type.clone());

                        return Ok(HIRExpr::MemberAccess(
                            obj_expr.into(),
                            name,
                            return_type,
                            meta,
                        ));
                    }
                }
                Ok(HIRExpr::MemberAccess(
                    obj_expr.into(),
                    name,
                    TypeInferenceResult::Polymorphic(poly_ty),
                    meta,
                ))
            }
        }
    }

    fn infer_deref_expr(
        &mut self,
        rhs: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        let rhs_expr = self.infer_expr(rhs, None)?;
        let typeof_obj = rhs_expr.get_type();

        match typeof_obj {
            TypeInferenceResult::Monomorphic(typeof_obj) => {
                let type_data = self.ctx.type_db.get_instance(typeof_obj);
                if type_data.base == self.ctx.type_db.constructors.common_types.ptr {
                    let derefed_type = type_data.type_args[0];
                    Ok(HIRExpr::Deref(
                        rhs_expr.into(),
                        TypeInferenceResult::Monomorphic(derefed_type),
                        meta,
                    ))
                } else {
                    self.ctx
                        .errors
                        .invalid_derefed_type
                        .push_inference_error(report!(
                            self,
                            meta,
                            DerefOnNonPointerError {
                                attempted_type: typeof_obj,
                            }
                        ))?
                }
            }
            TypeInferenceResult::Polymorphic(
                TypeConstructParams::Parameterized(maybe_ptr, pointee_type)) => {
                
                if let TypeConstructParams::Given(maybe_ptr_id) = *maybe_ptr && maybe_ptr_id == self.ctx.type_db.constructors.common_types.ptr {
                    Ok(HIRExpr::Deref(
                        rhs_expr.into(),
                        TypeInferenceResult::Polymorphic(pointee_type[0].clone()),
                        meta,
                    ))
                } else {
                    self.ctx
                        .errors
                        .invalid_derefed_type_unconstructed
                        .push_inference_error(report!(
                            self,
                            meta,
                            DerefOnNonPointerErrorUnconstructed {
                                attempted_type: *maybe_ptr.clone(),
                            }
                        ))?
                }

            },
            TypeInferenceResult::Polymorphic(params) => {
                self.ctx
                .errors
                .invalid_derefed_type_unconstructed
                .push_inference_error(report!(
                    self,
                    meta,
                    DerefOnNonPointerErrorUnconstructed {
                        attempted_type: params.clone(),
                    }
                ))?
            }
        }
    }

    fn infer_ref_expr(
        &mut self,
        rhs: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        let rhs_expr = self.infer_expr(rhs, None)?;
        let is_lvalue = rhs_expr.is_lvalue(self.ctx.type_db);
        if !is_lvalue {
            return self
                .ctx
                .errors
                .invalid_refed_type
                .push_inference_error(report!(self, meta, RefOnNonLValueError {}))?;
        }
        let typeof_obj = rhs_expr.get_type();
        let ptr_type = self.ctx.type_db.constructors.common_types.ptr;

        match typeof_obj.clone() {
            TypeInferenceResult::Monomorphic(typeof_obj) => {
                let ptr_type_generic_replaced =
                    self.ctx.type_db.construct_type(ptr_type, &[typeof_obj]);
                match ptr_type_generic_replaced {
                    Ok(ptr_type_generic_replaced) => Ok(HIRExpr::Ref(
                        rhs_expr.into(),
                        TypeInferenceResult::Monomorphic(ptr_type_generic_replaced),
                        meta,
                    )),
                    Err(e) => self
                        .ctx
                        .errors
                        .type_construction_failure
                        .push_inference_error(report!(
                            self,
                            meta,
                            TypeConstructionFailure { error: e }
                        ))?,
                }
            }
            TypeInferenceResult::Polymorphic(construct_params) => {
                let ref_expr = TypeInferenceResult::Polymorphic(
                    TypeConstructParams::Parameterized(
                        TypeConstructParams::Given(ptr_type).into(), vec![construct_params])
                );

                Ok(HIRExpr::Ref(rhs_expr.into(), ref_expr, meta))
            }

        }
    }

    fn infer_unary_expr(
        &mut self,
        rhs: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
        op: SpannedOperator,
    ) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        let rhs_expr = self.infer_expr(rhs, None)?;
        let rhs_type = rhs_expr.get_type();

        if let TypeInferenceResult::Monomorphic(rhs_type) = rhs_type {
            for (operator, result_type) in &self.ctx.type_db.get_instance(rhs_type).unary_ops {
                if *operator == op.0 {
                    return Ok(HIRExpr::UnaryExpression(
                        op,
                        rhs_expr.into(),
                        TypeInferenceResult::Monomorphic(*result_type),
                        TypeInferenceCertainty::Certain,
                        meta,
                    ));
                }
            }

            self.ctx
                .errors
                .unary_op_not_found
                .push(report!(
                    self,
                    &op.1,
                    UnaryOperatorNotFound {
                        rhs: rhs_type,
                        operator: op.0,
                    }
                ))
                .as_type_check_error()
        } else {
            return Ok(HIRExpr::UnaryExpression(
                op,
                rhs_expr.into(),
                rhs_type,
                TypeInferenceCertainty::Uncertain,
                meta,
            ));
        }
    }

    fn infer_expr_array(
        &mut self,
        exprs: Vec<HIRExpr<'source, ()>>,
        positional_type_hints: Option<&[TypeInferenceResult]>,
    ) -> Result<Vec<HIRExpr<'source, TypeInferenceResult>>, CompilerError> {
        let mut result = vec![];
        for (idx, expr) in exprs.into_iter().enumerate() {
            let type_hint = positional_type_hints.and_then(|hints| hints.get(idx));
            let res = self.infer_expr(expr, type_hint.map(|x| x.clone()))?;
            result.push(res);
        }
        Ok(result)
    }

    fn try_construct_many(
        &mut self,
        args: Vec<HIRUserTypeInfo<TypeConstructParams>>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<Vec<HIRUserTypeInfo<TypeInferenceResult>>, CompilerError> {
        let mut result = vec![];
        for arg in args {
            let res = self.ctx.try_construct(arg.resolved_type, meta)?;
            result.push(HIRUserTypeInfo {
                resolved_type: res,
                user_given_type: arg.user_given_type,
            });
        }
        Ok(result)
    }

    fn to_type_constructor_params(
        &mut self,
        args: Vec<HIRUserTypeInfo<()>>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<Vec<HIRUserTypeInfo<TypeConstructParams>>, CompilerError> {
        let mut result = vec![];
        for arg in args {
            let hir_type = arg
                .user_given_type
                .expect("Unexpected given type None during inference");
            let resolved_type = self.ctx.make_usage(&hir_type, meta)?;
            result.push(HIRUserTypeInfo {
                resolved_type,
                user_given_type: Some(hir_type),
            });
        }
        Ok(result)
    }

    fn generic_substitute_many(
        &mut self,
        params: &[TypeConstructParams],
        positional_type_args: &[HIRUserTypeInfo<TypeConstructParams>],
        generics: &[TypeParameter],
    ) -> Vec<TypeConstructParams> {
        params
            .iter()
            .map(|param| self.generic_substitute(positional_type_args, param, generics))
            .collect::<Vec<_>>()
    }

    //Generate a function signature for a type usage, given the type arguments.
    fn generic_substitute(
        &mut self,
        //these are the arguments passed, for instance, call<i32, T>()
        positional_type_args: &[HIRUserTypeInfo<TypeConstructParams>],
        //this is the type of the function being called. The first call to generic_substitute will be a FunctionSignature
        type_of_value: &TypeConstructParams,
        //The order of the type parameters of the function definition, def foo<T, U>(..) would be <T, U>,
        //but in the first call this will be empty.
        positional_type_parameters: &[TypeParameter],
    ) -> TypeConstructParams {
        log!("Generic substitute: \nType of value: {:?}\nPositional Type Args{:?}\nPositional type params:{:?}", type_of_value.to_string(&self.ctx.type_db.constructors), positional_type_args, positional_type_parameters);
        match type_of_value {
            //If the type is given then just use it.
            TypeConstructParams::Given(type_constructor) => {
                TypeConstructParams::Given(*type_constructor)
            }
            TypeConstructParams::Generic(type_param) => {
                //get the index of the parameter in type_parameters
                let index = positional_type_parameters
                    .iter()
                    .position(|x| x == type_param)
                    .expect("Type parameter not found");
                //println!("Index of type parameter: {}", index);
                //get the type argument at the same index
                let type_arg = positional_type_args
                    .get(index)
                    .expect("Type argument not found"); //TODO nicer error
                //return the type argument
                type_arg.resolved_type.clone()
            }
            //@TODO this is a bit weird.... the caller can pass positional_type_args because it knows it's a function....
            TypeConstructParams::FunctionSignature(FunctionSignature {
                generics,
                params,
                return_type,
                variadic,
            }) => {
                //we need to return a new function signature with the type parameters substituted.
                let substituted_params =
                    self.generic_substitute_many(params, positional_type_args, generics);
                let substituted_return_type =
                    self.generic_substitute(positional_type_args, return_type, generics);

                TypeConstructParams::FunctionSignature(FunctionSignature {
                    generics: generics.clone(),
                    params: substituted_params,
                    return_type: substituted_return_type.into(),
                    variadic: *variadic,
                })
            }
            TypeConstructParams::Parameterized(base, params) => {
             
                let base =
                    self.generic_substitute(positional_type_args, base, positional_type_parameters);
                let substituted_params = self.generic_substitute_many(
                    params,
                    positional_type_args,
                    positional_type_parameters,
                );
                TypeConstructParams::Parameterized(base.into(), substituted_params)
            }
        }
    }

    fn infer_binop(
        &mut self,
        lhs: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
        rhs: HIRExpr<'source, ()>,
        op: SpannedOperator,
    ) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        let lhs_expr = self.infer_expr(lhs, None)?;
        let lhs_type = lhs_expr.get_type();

        let s = lhs_type.print_name(&self.ctx.type_db);
        log!("RHS type hint for binop: {} {:?}", s, op.0);
        let rhs_expr = self.infer_expr(rhs, Some(lhs_type.clone()))?;

        let rhs_type = rhs_expr.get_type();

        let s = rhs_type.print_name(&self.ctx.type_db);
        log!("RHS type inferred: {}", s);

        match (lhs_type, rhs_type) {
            (
                TypeInferenceResult::Monomorphic(lhs_type),
                TypeInferenceResult::Monomorphic(rhs_type),
            ) => {
                let lhs_instance = self.ctx.type_db.get_instance(lhs_type);

                for (operator, rhs_supported, result_type) in &lhs_instance.rhs_binary_ops {
                    if *operator == op.0 && *rhs_supported == rhs_type {
                        return Ok(HIRExpr::BinaryOperation(
                            Box::new(lhs_expr),
                            op,
                            Box::new(rhs_expr),
                            TypeInferenceResult::Monomorphic(*result_type),
                            TypeInferenceCertainty::Certain,
                            meta,
                        ));
                    }
                }

                //operator not found, add binary op not found error
                self.ctx
                    .errors
                    .binary_op_not_found
                    .push_inference_error(report!(
                        self,
                        &op.1,
                        BinaryOperatorNotFound {
                            lhs: lhs_type,
                            rhs: rhs_type,
                            operator: op.0,
                        }
                    ))
            }
            (ty, _) => {
                //if one of the types is not inferred, we have to wait for monomorphization and rerun inference later
                Ok(HIRExpr::BinaryOperation(
                    Box::new(lhs_expr),
                    op,
                    Box::new(rhs_expr),
                    ty, //just pass anything here, not important
                    TypeInferenceCertainty::Uncertain,
                    meta,
                ))
            }
        }
    }

    fn infer_types_in_body(
        &mut self,
        body: Vec<TypeInferenceInputHIR<'source>>,
    ) -> Result<Vec<InferredTypeHIR<'source>>, CompilerError> {
        let mut new_mir = vec![];
        for node in body {
            let hir_node: InferredTypeHIR = match node {
                HIR::Declare {
                    var,
                    expression,
                    typedef: type_hint,
                    meta_ast,
                    meta_expr,
                    synthetic,
                } => self.infer_types_in_variable_declaration(
                    type_hint, expression, var, meta_ast, meta_expr, synthetic,
                )?,
                HIR::Assign {
                    path,
                    expression,
                    meta_ast,
                    meta_expr,
                } => self.infer_types_in_assignment(expression, path, meta_ast, meta_expr)?,
                HIR::FunctionCall(FunctionCall {
                    function,
                    type_args,
                    args,
                    return_type: (), //useless here
                    meta_ast: _,     //?
                    meta_expr,
                }) => {
                    let fcall_inferred =
                        self.infer_function_call(function, type_args, args, meta_expr)?;
                    HIR::FunctionCall(fcall_inferred)
                }
                HIR::If(condition, true_branch, false_branch, meta) => self
                    .infer_types_in_if_statement_and_blocks(
                        true_branch,
                        false_branch,
                        condition,
                        meta,
                    )?,
                HIR::Return(expr, meta) => self.infer_types_in_return(expr, meta)?,
                HIR::EmptyReturn(meta) => HIR::EmptyReturn(meta),
                HIR::While(condition, body, meta) => {
                    self.infer_types_in_while_statement_and_blocks(condition, body, meta)?
                }
                HIR::MethodCall(MethodCall {
                    object,
                    method_name,
                    args,
                    return_type: _,
                    meta_expr,
                }) => HIR::MethodCall(self.infer_method_call(
                    *object,
                    method_name,
                    args,
                    meta_expr,
                )?),
            };
            new_mir.push(hir_node);
        }

        Ok(new_mir)
    }

    fn infer_types_in_return(
        &mut self,
        expr: HIRExpr<'source, ()>,
        meta_ast: HIRAstMetadata<'source>,
    ) -> Result<InferredTypeHIR<'source>, CompilerError> {
        let typed_expr = self.infer_expr(expr, None)?;
        Ok(HIR::Return(typed_expr, meta_ast))
    }

    fn infer_types_in_while_statement_and_blocks(
        &mut self,
        condition: HIRExpr<'source, ()>,
        body: Vec<TypeInferenceInputHIR<'source>>,
        meta_ast: HIRAstMetadata<'source>,
    ) -> Result<InferredTypeHIR<'source>, CompilerError> {
        let body_inferred = self.infer_types_in_body(body)?;
        let condition_expr = self.infer_expr(condition, None)?;
        Ok(HIR::While(condition_expr, body_inferred, meta_ast))
    }

    fn infer_types_in_if_statement_and_blocks(
        &mut self,
        true_branch: Vec<TypeInferenceInputHIR<'source>>,
        false_branch: Vec<TypeInferenceInputHIR<'source>>,
        condition: HIRExpr<'source, ()>,
        meta_ast: HIRAstMetadata<'source>,
    ) -> Result<InferredTypeHIR<'source>, CompilerError> {
        let true_branch_inferred = self.infer_types_in_body(true_branch)?;
        let false_branch_inferred = self.infer_types_in_body(false_branch)?;
        let condition_expr = self.infer_expr(condition, None)?;
        Ok(HIR::If(
            condition_expr,
            true_branch_inferred,
            false_branch_inferred,
            meta_ast,
        ))
    }

    fn infer_types_in_assignment(
        &mut self,
        expression: HIRExpr<'source, ()>,
        path: HIRExpr<'source, ()>,
        meta_ast: HIRAstMetadata<'source>,
        meta_expr: HIRExprMetadata<'source>,
    ) -> Result<InferredTypeHIR<'source>, CompilerError> {
        let typed_lhs_expr = self.infer_expr(path, None)?;
        let lhs_type = typed_lhs_expr.get_type();
        let typed_expr = self.infer_expr(expression, Some(lhs_type))?;
        Ok(HIR::Assign {
            path: typed_lhs_expr,
            expression: typed_expr,
            meta_ast,
            meta_expr,
        })
    }

    fn infer_types_in_variable_declaration(
        &mut self,
        variable_typedecl: HIRTypeDef,
        assigned_value: HIRExpr<'source, ()>,
        variable_name: InternedString,
        meta_ast: HIRAstMetadata<'source>,
        meta_expr: HIRExprMetadata<'source>,
        synthetic: bool,
    ) -> Result<InferredTypeHIR<'source>, CompilerError> {
        let expr_str = HIRExprPrinter::new(&&self.ctx.type_db, false).print(&assigned_value);
        log!("Assigned value {expr_str}");
        //Type hint takes precedence over expr type
        let variable_chosen = match variable_typedecl {
            HIRTypeDef::PendingInference => {
                log!("Variable {} has no type hint", variable_name.to_string());
                None
            }
            HIRTypeDef::Provided(typedecl) => match self.instantiate_type(&typedecl, meta_expr) {
                Ok(id) => Some(id),
                Err(e) => return Err(e),
            },
        };

        let typed_expr = self.infer_expr(assigned_value, variable_chosen.clone())?;
        let actual_type = match variable_chosen {
            Some(decl) => decl,
            None => typed_expr.get_type(),
        };

        log!(
            "New variable {} with type {}",
            variable_name.to_string(),
            actual_type.print_name(&self.ctx.type_db)
        );

        self.ctx
            .decls_in_scope
            .insert(variable_name, actual_type.clone());

        Ok(HIR::Declare {
            var: variable_name,
            typedef: actual_type,
            expression: typed_expr,
            meta_ast,
            meta_expr,
            synthetic,
        })
    }

    fn infer_function(
        &mut self,
        parameters: &[HIRTypedBoundName<TypeInferenceResult>],
        body: Vec<TypeInferenceInputHIR<'source>>,
        _return_type: TypeInferenceResult,
        _meta: HIRAstMetadata<'source>,
    ) -> Result<Vec<InferredTypeHIR<'source>>, CompilerError> {
        let mut decls_in_scope = NameRegistry::new();
        for p in parameters {
            decls_in_scope.insert(p.name, p.type_data.clone());
        }
        //We should add the function itself in the scope, to allow recursion!
        //Luckily the function itself is already on the globals! (It should be...)
        decls_in_scope.include(self.ctx.decls_in_scope);

        let new_ctx_with_modified_scope = TypeInferenceContext {
            type_db: self.ctx.type_db,
            errors: self.ctx.errors,
            decls_in_scope: &mut decls_in_scope,
            on_function: self.ctx.on_function,
            type_parameters: self.ctx.type_parameters.clone(),
        };

        let mut new_type_inference = FunctionTypeInferenceContext {
            ctx: new_ctx_with_modified_scope,
        };

        Ok(new_type_inference.infer_types_in_body(body)?)
    }
}

pub fn infer_types<'source>(
    globals: &mut NameRegistry,
    type_db: &mut TypeInstanceManager,
    mir: Vec<TypeInferenceInputHIRRoot<'source>>,
    errors: &mut TypeErrors<'source>,
) -> Result<Vec<InferredTypeHIRRoot<'source>>, CompilerError> {
    //log!("globals:");
    //globals.print( type_db);

    let mut new_mir = vec![];

    for node in mir {
        let result: InferredTypeHIRRoot = match node {
            HIRRoot::DeclareFunction {
                function_name,
                type_parameters,
                parameters,
                body,
                return_type,
                meta,
                is_intrinsic,
                is_varargs,
                is_external
            } => {
                let inference_ctx = TypeInferenceContext {
                    on_function: RootElementType::Function(function_name),
                    type_db,
                    errors,
                    decls_in_scope: globals,
                    type_parameters: type_parameters.clone(),
                };

                let mut function_inference = FunctionTypeInferenceContext { ctx: inference_ctx };

                let body = function_inference.infer_function(
                    &parameters,
                    body,
                    return_type.clone(),
                    meta,
                )?;
                HIRRoot::DeclareFunction {
                    function_name,
                    type_parameters,
                    parameters,
                    body,
                    return_type,
                    meta,
                    is_intrinsic,
                    is_varargs,
                    is_external
                }
            }
            HIRRoot::StructDeclaration {
                struct_name,
                fields,
                type_parameters,
                meta,
            } => {
                let mut inference_ctx = TypeInferenceContext {
                    on_function: RootElementType::Struct(struct_name),
                    type_db,
                    errors,
                    decls_in_scope: globals,
                    type_parameters: type_parameters.clone(),
                };

                let resolved_fields = fields
                    .into_iter()
                    .map(|f| {
                        let resolved_type = inference_ctx.instantiate_type(&f.type_data, meta)?;

                        Ok(HIRTypedBoundName {
                            name: f.name,
                            type_data: resolved_type,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                HIRRoot::StructDeclaration {
                    struct_name,
                    fields: resolved_fields,
                    type_parameters,
                    meta,
                }
            }
        };
        new_mir.push(result);
    }

    Ok(new_mir)
}

//Why no tests?
//This is actually tested in the file analysis.rs!
