use std::collections::HashMap;
use std::hash::Hash;

use crate::ast::lexer::TokenSpanIndex;
use crate::ast::parser::{Spanned, SpannedOperator, StringSpan};
use crate::interner::{InternedString, StringInterner};
use crate::report;
use crate::semantic::hir::{HIRExpr, HIRType, HIRTypedBoundName, LiteralHIRExpr, HIR};


use crate::types::type_constructor_db::{TypeConstructParams, TypeParameter, FunctionSignature};
use crate::types::type_errors::{
    BinaryOperatorNotFound, CallToNonCallableType, DerefOnNonPointerError, FieldOrMethodNotFound,
    InsufficientTypeInformationForArray, OutOfTypeBounds, RefOnNonLValueError,
    TypeConstructionFailure, ContextualizedCompilerError, TypeErrors, TypePromotionFailure,
    UnaryOperatorNotFound, VariableNotFound, TypeNotFound, InternalError, ErrorReporter, CompilerErrorData,
};
use crate::types::type_instance_db::{TypeInstanceId, TypeInstanceManager, TypeConstructionError};

use super::compiler_errors::CompilerError;
use super::context::FileTableIndex;
use super::hir::{
    FirstAssignmentsDeclaredHIR, FirstAssignmentsDeclaredHIRRoot, HIRAstMetadata, HIRExprMetadata,
    HIRRoot, HIRTypeDef, InferredTypeHIR, InferredTypeHIRRoot, HIRUserTypeInfo, FunctionCall,
};
use super::hir_type_resolution::{hir_type_to_usage, RootElementType};
use super::top_level_decls::NameRegistry;
use super::type_name_printer::TypeNamePrinter;

pub type TypeInferenceInputHIRRoot<'source> = FirstAssignmentsDeclaredHIRRoot<'source>;
pub type TypeInferenceInputHIR<'source> = FirstAssignmentsDeclaredHIR<'source>;

pub struct TypeInferenceContext<'compiler_state, 'source, 'interner> {
    pub on_function: RootElementType,
    pub on_file: FileTableIndex,
    //If the function is def foo<T>(), this will contain ["T"]
    pub type_parameters: Vec<TypeParameter>,
    //@TODO move function parameters into here
    pub type_db: &'compiler_state mut TypeInstanceManager<'interner>,
    pub errors: &'compiler_state mut TypeErrors<'source>,
    pub decls_in_scope: &'compiler_state mut NameRegistry,
}

impl ErrorReporter for TypeInferenceContext<'_, '_, '_> {
    fn report<T: CompilerErrorData>(&self, error: T, span: &impl Spanned, compiler_code_location: &'static str) -> crate::types::type_errors::CompilerErrorContext<T> {
        error.at_spanned(self.on_function, self.on_file, span, compiler_code_location)
    }
}

impl<'source> TypeInferenceContext<'_, 'source, '_> {

    pub fn make_usage(&mut self,
        typedef: &HIRType,
        location: &impl Spanned) -> Result<TypeConstructParams, CompilerError> {
        hir_type_to_usage(
            self.on_function,
            typedef,
            self.type_db,
            &self.type_parameters,
            self.errors,
            location,
            self.on_file,
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

    pub fn try_construct(&mut self, usage: TypeConstructParams, location: &impl Spanned) -> Result<TypeInferenceResult, CompilerError> {
        match self.type_db.construct_usage(&usage) {
            Ok(instance_id) => Ok(TypeInferenceResult::Monomorphic(instance_id)),
            Err(TypeConstructionError::InsufficientInformation) => {
                Ok(TypeInferenceResult::Polymorphic(usage))
            }
            Err(e) => {
                self.errors.type_construction_failure.push_inference_error(
                    report!(self, location, TypeConstructionFailure { error: e })
                )?
            }
        }
    }
}



pub struct FunctionTypeInferenceContext<'compiler_state, 'source, 'interner> {
    pub ctx: TypeInferenceContext<'compiler_state, 'source, 'interner>,
}

impl ErrorReporter for FunctionTypeInferenceContext<'_, '_, '_> {
    fn report<T: CompilerErrorData>(&self, error: T, span: &impl Spanned, compiler_code_location: &'static str) -> crate::types::type_errors::CompilerErrorContext<T> {
        error.at_spanned(self.ctx.on_function, self.ctx.on_file, span, compiler_code_location)
    }
}


#[derive(Clone, Debug)]
pub enum TypeInferenceResult {
    //Fully resolved
    Monomorphic(TypeInstanceId),
    //Partially resolved, i.e. a function with some arguments
    Polymorphic(TypeConstructParams),
}

impl TypeNamePrinter for TypeInferenceResult {
    fn print_name(&self, type_db: &TypeInstanceManager, interner: &StringInterner) -> String {
       
        match self {
            TypeInferenceResult::Monomorphic(id) => id.print_name(type_db, interner),
            TypeInferenceResult::Polymorphic(usage) => {
                format!("Polymorphic({})", usage.print_name(type_db, interner))
            }
        }
    }
    
}

impl<'source> FunctionTypeInferenceContext<'_, 'source, '_> {
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

                return self.ctx.errors.type_promotion_failure.push_inference_error(
                    report!(self, meta, TypePromotionFailure {
                        target_type: type_hint,
                    }
                    ),
                )?;
            }
            _ => panic!("Cannot promote value: {:?}", literal),
        }
    }

    pub fn compute_and_infer_expr_type(
        &mut self,
        expression: HIRExpr<'source, ()>,
        type_hint: Option<TypeInferenceResult>,
    ) -> Result<HIRExpr<'source,TypeInferenceResult>, CompilerError> {
       
        match expression {
            HIRExpr::Variable(var, _, meta) => {
                let decl_type = self.ctx.decls_in_scope.get(&var);

                match decl_type {
                    Some(decl_type) => {
                        Ok(HIRExpr::Variable(var, decl_type.clone(), meta))
                    }
                    None => {
                        self.ctx.errors.variable_not_found.push_inference_error(
                            report!(self, meta, VariableNotFound { variable_name: var })
                        )
                    }
                }
            }
            HIRExpr::Literal(literal_expr, _, meta) => {
                self.infer_literal(literal_expr, &type_hint, meta)
            }
            HIRExpr::BinaryOperation(lhs, op, rhs, _, meta) => {
                self.infer_binop(*lhs, meta, *rhs, op)
            }
            HIRExpr::FunctionCall(fcall) => {
                let FunctionCall { function, args, type_args, return_type, meta_expr, .. } = *fcall;
                let fcall_inferred = self.infer_function_call(function, type_args, args, meta_expr)?;
                Ok(HIRExpr::FunctionCall(fcall_inferred.into()))
            }
            HIRExpr::UnaryExpression(op, rhs, _, meta) => {
                self.infer_unary_expr(*rhs, meta, op)
            }
            HIRExpr::Deref(rhs, _, meta) => self.infer_deref_expr(*rhs, meta),
            HIRExpr::Ref(rhs, _, meta) => self.infer_ref_expr(*rhs, meta),
            HIRExpr::MemberAccess(obj, name, _, meta) => {
                self.infer_member_access(*obj, meta, name)
            }
            //we will get the type of the first item, and use it as a type and instantiate an Array generic type.
            //a later step will do the type checking.
            HIRExpr::Array(array_items, _, meta) => {
                self.infer_array(array_items, type_hint, meta)
            }
            HIRExpr::Cast(..) => todo!("Casts haven't been figured out yet"),
            HIRExpr::MethodCall(obj, method_name, params, _, meta) => {
                self.infer_method_call(*obj, method_name, params, meta)
            }
            HIRExpr::StructInstantiate(struct_name, type_args, _, meta) => self.struct_instantiate_method_call(struct_name, type_args, meta),
        }
    }

    fn infer_literal(&mut self, literal_expr: LiteralHIRExpr, type_hint: &Option<TypeInferenceResult>, meta: HIRExprMetadata<'source>) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        let literal_type = match literal_expr {
            LiteralHIRExpr::Integer(_) => {
                if let Some(TypeInferenceResult::Monomorphic(type_hint)) = *type_hint {
                    self.try_literal_promotion(&literal_expr, type_hint, meta)?
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
                let str_type = self.ctx.type_db
                    .constructors
                    .find_by_name(self.ctx.type_db.constructors.interner.get("str"))
                    .expect("str intrinsic not loaded");
                self.ctx.type_db.construct_type(str_type.id, &[]).unwrap()
            }
            LiteralHIRExpr::Boolean(_) => self.ctx.type_db.common_types.bool,
            LiteralHIRExpr::None => todo!("Must implement None"),
        };
        Ok(HIRExpr::Literal(literal_expr, TypeInferenceResult::Monomorphic(literal_type), meta))
    }

    fn struct_instantiate_method_call(
        &mut self,
        struct_name: InternedString,
        struct_type_args: Vec<HIRType>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        //since we already have a """good""" way to transform types into TypeUsage and potentially infer them,
        //we can transform the data we got into a HIRType, run InstantiateType in the whole thing 
        //and badabing badaboom
        let as_hir_type = HIRType::Generic(struct_name, struct_type_args.clone());
        let ty = self.instantiate_type(&as_hir_type, meta)?;
        Ok(HIRExpr::StructInstantiate(struct_name, struct_type_args, ty, meta))
    }    


    fn infer_method_call(
        &mut self,
        obj: HIRExpr<'source, ()>,
        method_name: InternedString,
        params: Vec<HIRExpr<'source, ()>>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        //compute type of obj
        //find method_name in type of obj
        let objexpr = self.compute_and_infer_expr_type(obj, None)?;
        let typeof_obj = objexpr.get_type();
        if let TypeInferenceResult::Monomorphic(typeof_obj) = typeof_obj {
            let type_data = self.ctx.type_db.get_instance(typeof_obj);
            //we'll find the method call here by name
            let method = type_data
                .methods
                .iter()
                .find(|method| method.name == method_name);
            if let Some(method) = method {
                let method_type = self.ctx.type_db.get_instance(method.function_type);
                let return_type = method_type.function_return_type.unwrap();
                let type_hints = method_type
                    .function_args
                    .clone()
                    .into_iter()
                    .map(|x| TypeInferenceResult::Monomorphic(x))
                    .collect::<Vec<_>>();
                let args = self.infer_expr_array(params, 
                    Some(&type_hints))?;
    
                Ok(HIRExpr::MethodCall(
                    objexpr.into(),
                    method_name,
                    args,
                    TypeInferenceResult::Monomorphic(return_type),
                    meta,
                ))
            } else {
                self.ctx.errors.field_or_method_not_found.push_inference_error(
                    report!(self, meta, FieldOrMethodNotFound {
                        object_type: typeof_obj,
                        field_or_method: method_name,
                    }
                ))?
            }
        } else {

            let args = self.infer_expr_array(params, None)?;

            Ok(HIRExpr::MethodCall(
                objexpr.into(),
                method_name,
                args,
                todo!("Construct the type of the method call when it's polymorphic"),
                meta,
            ))
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
                self.ctx.errors.insufficient_array_type_info.push_inference_error(
                    report!(self, meta, InsufficientTypeInformationForArray {}))
                    ?
            }
        } else {
            let all_exprs = self.infer_expr_array(array_items, None)?;

            let first_typed_item = all_exprs.first();

            let array_type = self.ctx.type_db.constructors.common_types.array;
            
            if let Some(expr) = first_typed_item {
                if  let TypeInferenceResult::Monomorphic(expr_type) = expr.get_type() {
                    let array_type_generic_replaced =
                    self.ctx.type_db.construct_type(array_type, &[expr_type]);

                    match array_type_generic_replaced {
                        Ok(array_type_generic_replaced) => {
                            Ok(HIRExpr::Array(all_exprs, TypeInferenceResult::Monomorphic(array_type_generic_replaced), meta))
                        }
                        Err(e) => {
                            self.ctx.errors.type_construction_failure.push_inference_error(
                                report!(self, meta, TypeConstructionFailure { error: e })
                            )
                        }
                    }
                } else {
                    Ok(HIRExpr::Array(all_exprs.clone(), expr.get_type(), meta))
                }
            } else {
                //array has items but all of them failed type inference lmao
                //no choice but to give up and return a fully unresolved array
                //hint does not matter much
                self.ctx.errors.insufficient_array_type_info.push_inference_error(report!(self, meta,
                    InsufficientTypeInformationForArray {})
                )?
            }
        }
    }

    fn infer_member_access(
        &mut self,
        obj: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
        name: InternedString,
    ) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        let obj_expr = self.compute_and_infer_expr_type(obj, None)?;

        let typeof_obj = obj_expr.get_type();

        if let TypeInferenceResult::Monomorphic(typeof_obj) = typeof_obj {
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
                self.ctx.errors.field_or_method_not_found.push_inference_error(
                    report!(self, meta, FieldOrMethodNotFound {
                        object_type: typeof_obj,
                        field_or_method: name,
                    }
                    ),
                )?
            }
        } else {
            Ok(HIRExpr::MemberAccess(
                obj_expr.into(),
                name,
                typeof_obj,
                meta,
            ))
        }

       
    }

    fn infer_deref_expr(
        &mut self,
        rhs: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        let rhs_expr = self.compute_and_infer_expr_type(rhs, None)?;
        let typeof_obj = rhs_expr.get_type();

        if let TypeInferenceResult::Monomorphic(typeof_obj) = typeof_obj {
            let type_data = self.ctx.type_db.get_instance(typeof_obj);
            if type_data.base == self.ctx.type_db.constructors.common_types.ptr {
                let derefed_type = type_data.type_args[0];
                Ok(HIRExpr::Deref(rhs_expr.into(), TypeInferenceResult::Monomorphic(derefed_type), meta))
            } else {
                self.ctx.errors.invalid_derefed_type.push_inference_error(
                    report!(self, meta, DerefOnNonPointerError {
                        attempted_type: typeof_obj,
                    }  
                ))?
            }
        } else {
            Ok(HIRExpr::Deref(rhs_expr.into(), typeof_obj, meta))
        }

        
    }

    fn infer_ref_expr(
        &mut self,
        rhs: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        let rhs_expr = self.compute_and_infer_expr_type(rhs, None)?;
        let is_lvalue = rhs_expr.is_lvalue(self.ctx.type_db);
        if !is_lvalue {
            return self.ctx.errors
                .invalid_refed_type
                .push_inference_error(report!(self, meta, RefOnNonLValueError {}))
                ?;
        }
        let typeof_obj = rhs_expr.get_type();

        if let TypeInferenceResult::Monomorphic(typeof_obj) = typeof_obj.clone() {
            let ptr_type = self.ctx.type_db.constructors.common_types.ptr;
            let ptr_type_generic_replaced = self.ctx.type_db.construct_type(ptr_type, &[typeof_obj]);
            match ptr_type_generic_replaced {
                Ok(ptr_type_generic_replaced) => Ok(HIRExpr::Ref(
                    rhs_expr.into(),
                    TypeInferenceResult::Monomorphic(ptr_type_generic_replaced),
                    meta,
                )),
                Err(e) => {
                    self.ctx.errors.type_construction_failure.push_inference_error(
                        report!(self, meta, TypeConstructionFailure { error: e })
                    )?
                }
            }
        } else {
            Ok(HIRExpr::Ref(rhs_expr.into(), typeof_obj, meta))
        }

        
    }

    fn infer_unary_expr(
        &mut self,
        rhs: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
        op: SpannedOperator,
    ) -> Result<HIRExpr<'source, TypeInferenceResult>, CompilerError> {
        let rhs_expr = self.compute_and_infer_expr_type(rhs, None)?;
        let rhs_type = rhs_expr.get_type();

        if let TypeInferenceResult::Monomorphic(rhs_type) = rhs_type {
            for (operator, result_type) in &self.ctx.type_db.get_instance(rhs_type).unary_ops {
                if *operator == op.0 {
                    return Ok(HIRExpr::UnaryExpression(
                        op,
                        rhs_expr.into(),
                        TypeInferenceResult::Monomorphic(*result_type),
                        meta,
                    ));
                }
            }

            self.ctx.errors.unary_op_not_found.push(
                report!(self, &op.1, UnaryOperatorNotFound {
                    rhs: rhs_type,
                    operator: op.0,
                }),
            ).as_type_check_error()

        } else {
            return Ok(HIRExpr::UnaryExpression(
                op,
                rhs_expr.into(),
                rhs_type,
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
            let res = self.compute_and_infer_expr_type(expr, type_hint.map(|x| x.clone()))?;
            result.push(res);
        }
        Ok(result)
    }

    fn try_construct_many(&mut self, args: Vec<HIRUserTypeInfo<TypeConstructParams>>, meta: HIRExprMetadata<'source>) -> Result<Vec<HIRUserTypeInfo<TypeInferenceResult>>, CompilerError> {
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


    fn to_type_constructor_params(&mut self, args: Vec<HIRUserTypeInfo<()>>, meta: HIRExprMetadata<'source>) -> Result<Vec<HIRUserTypeInfo<TypeConstructParams>>, CompilerError> {
        let mut result = vec![];
        for arg in args {
            let hir_type = arg.user_given_type.expect("Unexpected given type None during inference");
            let resolved_type = self.ctx.make_usage(&hir_type, meta)?;
            result.push(HIRUserTypeInfo {
                resolved_type,
                user_given_type: Some(hir_type),
            });
        }
        Ok(result)
    }


    fn generic_substitute_many(&mut self, params: &[TypeConstructParams], positional_type_args: &[HIRUserTypeInfo<TypeConstructParams>], generics: &[TypeParameter]) -> Vec<TypeConstructParams> {
        params.iter().map(|param| 
            self.generic_substitute(positional_type_args, param, generics)
        ).collect::<Vec<_>>()
    }

    //Generate a function signature for a type usage, given the type arguments.
    fn generic_substitute(&mut self,    
        //these are the arguments passed, for instance, call<i32, T>()
        positional_type_args: &[HIRUserTypeInfo<TypeConstructParams>],
        //this is the type of the function being called. The first call will be a FunctionSignature
        type_of_value: &TypeConstructParams,
        //The order of the type parameters of the function definition, def foo<T, U>(..) would be <T, U>,
        //but in the first call this will be empty.
        positional_type_parameters: &[TypeParameter],
    ) -> TypeConstructParams {
        
        match type_of_value {
            //If the type is given then just use it.
            TypeConstructParams::Given(type_constructor) => TypeConstructParams::Given(*type_constructor),
            TypeConstructParams::Generic(type_param) => {
                //get the index of the parameter in type_parameters
                let index = positional_type_parameters.iter().position(|x| x == type_param).expect("Type parameter not found");
                //get the type argument at the same index
                let type_arg = positional_type_args.get(index).expect("Type argument not found");
                //return the type argument
                type_arg.resolved_type.clone()
            },
            //@TODO this is a bit weird.... the caller can pass positional_type_args because it knows it's a function....
            TypeConstructParams::FunctionSignature(FunctionSignature { generics, params, return_type, variadic }) => {
                //we need to return a new function signature with the type parameters substituted.
                let substituted_params = self.generic_substitute_many(params, positional_type_args, generics);
                let substituted_return_type = self.generic_substitute(positional_type_args, return_type, generics);

                TypeConstructParams::FunctionSignature(FunctionSignature {
                    generics: generics.clone(),
                    params: substituted_params,
                    return_type: substituted_return_type.into(),
                    variadic: *variadic,
                })
            }
            TypeConstructParams::Parameterized(base, params) => {
                let base = self.generic_substitute(positional_type_args, base, positional_type_parameters);
                let substituted_params = self.generic_substitute_many(params, positional_type_args, positional_type_parameters);
                TypeConstructParams::Parameterized(base.into(), substituted_params)
            }
        }
    }


    fn infer_function_call(
        &mut self,
        fun_expr: HIRExpr<'source, ()>,
        fun_type_args: Vec<HIRUserTypeInfo<()>>,
        fun_params: Vec<HIRExpr<'source, ()>>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<FunctionCall<'source, TypeInferenceResult>, CompilerError> {
        let HIRExpr::Variable(var, .., fcall_meta) = fun_expr else {
            todo!("Currently only function calls on names are supported");
        };


        let function_type = {
            let Some(function_type) = self.ctx.decls_in_scope.get(&var) else {
                return self.ctx.errors.variable_not_found.push_inference_error(report!(self, meta, VariableNotFound {
                    variable_name: var
                }))
            };
            function_type.clone()
        };

       
        
        match function_type {
            
            TypeInferenceResult::Monomorphic(function_type) => {
                let type_data = self.ctx.type_db.get_instance(function_type);
                let function_args = type_data.function_args.iter().map(|x| TypeInferenceResult::Monomorphic(*x)).collect::<Vec<_>>();
               
                let fun_params = self.infer_expr_array(fun_params, Some(&function_args))?;


                //end the previous borrow and start a new one
                let type_data = self.ctx.type_db.get_instance(function_type);

                let result = 
                    FunctionCall { 
                        function: HIRExpr::Variable(var, TypeInferenceResult::Monomorphic(function_type), fcall_meta).into(),
                        args: fun_params, 
                        type_args: vec![], 
                        return_type: TypeInferenceResult::Monomorphic(type_data.function_return_type.unwrap()), 
                        meta_expr: meta, 
                        meta_ast: None
                };

                Ok(result)
            }
            TypeInferenceResult::Polymorphic(sig @ TypeConstructParams::FunctionSignature(_)) => {
                //@TODO add checks for amount of parameters
                let positional_type_args = self.to_type_constructor_params(fun_type_args, meta)?;
                
                let substituted = self.generic_substitute(
                    &positional_type_args,
                    &sig,
                    &[],
                );

            

                //the return of substituted has to be a FunctionSignature...
                let TypeConstructParams::FunctionSignature(substituted) = substituted else {
                    return self.ctx.errors.internal_error.push_inference_error(report!(self, meta, InternalError {
                        error: "Expected substituted type to be a function signature".to_string()
                    }))
                };
                let cloned = substituted.clone();
                //try to run inference on the function parameters
                let args_inferred = substituted.params.into_iter().map(|param| {
                    self.ctx.try_construct(param, meta)
                }).collect::<Result<Vec<_>, _>>()?;

                let fun_params = self.infer_expr_array(fun_params, Some(&args_inferred))?;

                let type_args_inferred = self.try_construct_many(positional_type_args, meta)?;

                let result = 
                    FunctionCall { 
                        function: HIRExpr::Variable(var, TypeInferenceResult::Polymorphic(TypeConstructParams::FunctionSignature(cloned)), fcall_meta).into(),
                        args: fun_params,
                        type_args: type_args_inferred,
                        return_type: TypeInferenceResult::Polymorphic(*substituted.return_type),
                        meta_expr: meta,
                        meta_ast: None
                    }
                ;

                Ok(result)
            }
            TypeInferenceResult::Polymorphic(_) => {
                return self.ctx.errors.call_non_callable.push_inference_error(report!(self, meta, CallToNonCallableType { actual_type: None }))
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
        let lhs_expr = self.compute_and_infer_expr_type(lhs, None)?;
        let rhs_expr = self.compute_and_infer_expr_type(rhs, None)?;

        let lhs_type = lhs_expr.get_type();
        let rhs_type = rhs_expr.get_type();

        match (lhs_type, rhs_type) {
            (TypeInferenceResult::Monomorphic(lhs_type), TypeInferenceResult::Monomorphic(rhs_type)) => {
                let lhs_instance = self.ctx.type_db.get_instance(lhs_type);

                for (operator, rhs_supported, result_type) in &lhs_instance.rhs_binary_ops {
                    if *operator == op.0 && *rhs_supported == rhs_type {
                        return Ok(HIRExpr::BinaryOperation(
                            Box::new(lhs_expr),
                            op,
                            Box::new(rhs_expr),
                            TypeInferenceResult::Monomorphic(*result_type),
                            meta,
                        ));
                    }
                }
        
                //operator not found, add binary op not found error
                self.ctx.errors.binary_op_not_found.push_inference_error(
                    report!(self, meta,
                    BinaryOperatorNotFound {
                        lhs: lhs_type,
                        rhs: rhs_type,
                        operator: op.0,
                    }),
                )
            },
            (ty, _) => {
                //if one of the types is not inferred, we have to wait for monomorphization and rerun inference later
                Ok(HIRExpr::BinaryOperation(
                    Box::new(lhs_expr),
                    op,
                    Box::new(rhs_expr),
                    ty, //just pass anything here, not important
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
                } => self.infer_types_in_variable_declaration(
                    type_hint, expression, var, meta_ast, meta_expr,
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
                    meta_ast, //?
                    meta_expr,
                }) =>{
                    let fcall_inferred = self.infer_function_call(function, type_args, args, meta_expr)?;
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
        let typed_expr = self.compute_and_infer_expr_type(expr, None)?;
        Ok(HIR::Return(typed_expr, meta_ast))
    }

    fn infer_types_in_while_statement_and_blocks(
        &mut self,
        condition: HIRExpr<'source, ()>,
        body: Vec<TypeInferenceInputHIR<'source>>,
        meta_ast: HIRAstMetadata<'source>,
    ) -> Result<InferredTypeHIR<'source>, CompilerError> {
        let body_inferred = self.infer_types_in_body(body)?;
        let condition_expr = self.compute_and_infer_expr_type(condition, None)?;
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
        let condition_expr = self.compute_and_infer_expr_type(condition, None)?;
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
        let typed_lhs_expr = self.compute_and_infer_expr_type(path, None)?;
        let typed_expr = self.compute_and_infer_expr_type(expression, None)?;
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
    ) -> Result<InferredTypeHIR<'source>, CompilerError> {
        //Type hint takes precedence over expr type
        let variable_chosen = match variable_typedecl {
            HIRTypeDef::PendingInference => None,
            HIRTypeDef::Provided(typedecl) => {
                match self.instantiate_type(&typedecl, meta_expr) {
                    Ok(id) => Some(id),
                    Err(e) => return Err(e),
                }
            }
        };

        let typed_expr = self.compute_and_infer_expr_type(assigned_value, variable_chosen.clone())?;
        let actual_type = match variable_chosen {
            Some(decl) => decl,
            None => typed_expr.get_type(),
        };

        self.ctx.decls_in_scope.insert(variable_name, actual_type.clone());

        Ok(HIR::Declare {
            var: variable_name,
            typedef: actual_type,
            expression: typed_expr,
            meta_ast,
            meta_expr,
        })
    }

    fn infer_function(
        &mut self,
        parameters: &[HIRTypedBoundName<TypeInferenceResult>],
        body: Vec<TypeInferenceInputHIR<'source>>,
        return_type: TypeInferenceResult,
        meta: HIRAstMetadata<'source>
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
            on_file: self.ctx.on_file,
            type_parameters: self.ctx.type_parameters.clone()
        };

        let mut new_type_inference = FunctionTypeInferenceContext { ctx: new_ctx_with_modified_scope };

        Ok(new_type_inference.infer_types_in_body(body)?)
    }
}

pub fn infer_types<'source, 'interner>(
    globals: &mut NameRegistry,
    type_db: &mut TypeInstanceManager<'interner>,
    mir: Vec<TypeInferenceInputHIRRoot<'source>>,
    errors: &mut TypeErrors<'source>,
    interner: &'interner StringInterner,
    file: FileTableIndex,
) -> Result<Vec<InferredTypeHIRRoot<'source>>, CompilerError> {
    //println!("globals:");
    //globals.print(interner, type_db);

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
            } => {
                let inference_ctx = TypeInferenceContext {
                    on_function: RootElementType::Function(function_name),
                    on_file: file,
                    type_db,
                    errors,
                    decls_in_scope: globals,
                    type_parameters: type_parameters.clone(),
                };

                let mut function_inference = FunctionTypeInferenceContext {
                    ctx: inference_ctx,
                };

                let body =
                    function_inference.infer_function(&parameters, body, return_type.clone(), meta)?;
                HIRRoot::DeclareFunction {
                    function_name,
                    type_parameters,
                    parameters,
                    body,
                    return_type,
                    meta,
                    is_intrinsic,
                    is_varargs,
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
                    on_file: file,
                    type_db,
                    errors,
                    decls_in_scope: globals,
                    type_parameters: type_parameters.clone(),
                };

                let resolved_fields = fields
                    .into_iter()
                    .map(|f| {
                        let resolved_type = inference_ctx
                            .instantiate_type(&f.type_data, meta)?;

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
            },
        };
        new_mir.push(result);
    }

    Ok(new_mir)
}

//Why no tests?
//This is actually tested in the file analysis.rs!
