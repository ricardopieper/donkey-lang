use core::panic;

use crate::ast::parser::{Spanned, SpannedOperator};
use crate::interner::InternedString;
use crate::report;
use crate::semantic::hir::{HIRExpr, HIRType, HIRTypedBoundName, LiteralHIRExpr, HIR};

use crate::types::diagnostics::{
    BinaryOperatorNotFoundForTypeConstructor, CallToNonCallableType, CompilerErrorData,
    ContextualizedCompilerError, DerefOnNonPointerErrorUnconstructed, ErrorReporter, FieldNotFound,
    InsufficientTypeInformationForArray, InternalError, OutOfTypeBoundsTypeConstructor,
    RefOnNonLValueError, TypeErrors, TypePromotionFailure, VariableNotFound,
};
use crate::types::type_constructor_db::{
    FunctionSignature, TypeConstructParams, TypeConstructor, TypeConstructorId, TypeKind,
    TypeParameter,
};
use crate::types::type_instance_db::TypeInstanceManager;

use super::compiler_errors::CompilerError;
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
    //If the function is def foo<T>(), this will contain ["T"], like a Vec<InternedString>
    pub type_parameters: Vec<TypeParameter>,
    //@TODO move function parameters into here
    pub type_db: &'compiler_state mut TypeInstanceManager,
    pub errors: &'compiler_state mut TypeErrors<'source>,
    pub decls_in_scope: &'compiler_state mut NameRegistry<TypeConstructParams>,
    pub impl_of: Option<TypeConstructParams>,
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
            location,
        )
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
        error.at_spanned(self.ctx.on_function, span, compiler_code_location)
    }
}

impl<'source> FunctionTypeInferenceContext<'_, 'source> {
    pub fn instantiate_type(
        &mut self,
        typedef: &HIRType,
        location: &impl Spanned,
    ) -> Result<TypeConstructParams, CompilerError> {
        self.ctx.make_usage(typedef, location)
    }

    fn try_literal_promotion(
        &mut self,
        literal: &LiteralHIRExpr,
        type_hint: &TypeConstructorId,
        meta: &'source crate::ast::parser::Expr,
    ) -> Result<(), CompilerError> {
        let type_constructor_id = *type_hint;
        match literal {
            LiteralHIRExpr::Integer(i) => {
                //we need to check for promotions and the attempted promoted type
                //and see if it's in range, i.e. we can't promote 23752432 to an u8

                macro_rules! check_promotion {
                    ($type:ty, $type_id:expr) => {
                        if type_constructor_id == $type_id {
                            if *i >= <$type>::MIN as i128 && *i <= <$type>::MAX as i128 {
                                return Ok(());
                            }
                            else {
                                return self.ctx.errors.out_of_bounds_constructor.push_inference_error(report!(self, meta, OutOfTypeBoundsTypeConstructor {
                                    expr: meta, //@TODO unecessary, at_spanned already contains the metadata
                                    typ: TypeConstructParams::simple($type_id),
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
                    u8: type_db.constructors.common_types.u8,
                    u32: type_db.constructors.common_types.u32,
                    u64: type_db.constructors.common_types.u64,
                    i32: type_db.constructors.common_types.i32,
                    i64: type_db.constructors.common_types.i64
                );

                return self
                    .ctx
                    .errors
                    .type_promotion_failure
                    .push_inference_error(report!(
                        self,
                        meta,
                        TypePromotionFailure {
                            target_type: TypeConstructParams::simple(type_constructor_id)
                        }
                    ))?;
            }
            _ => panic!("Cannot promote value: {:#?}", literal),
        }
    }

    pub fn infer_expr(
        &mut self,
        expression: HIRExpr<'source, ()>,
        type_hint: Option<TypeConstructParams>,
    ) -> Result<HIRExpr<'source, TypeConstructParams>, CompilerError> {
        match expression {
            HIRExpr::Variable(var, _, meta) => {
                log!("Infering variable: {}", var);
                let decl_type = self.ctx.decls_in_scope.get(&var);

                match decl_type {
                    Some(decl_type) => Ok(HIRExpr::Variable(var, decl_type.clone(), meta)),
                    None => {
                        let type_data_type_id = self.load_stdlib_builtin("TypeData");
                        match type_data_type_id {
                            Some(type_data_type_id) => {
                                //try to find a type with this name
                                let type_data = self
                                    .ctx
                                    .type_db
                                    .constructors
                                    .find_by_name(InternedString::new(&var.to_string()));

                                //if we have it, then we're done

                                if let Some(td) = type_data {
                                    return Ok(HIRExpr::TypeName {
                                        type_variable: TypeConstructParams::simple(td.id),
                                        type_data: type_data_type_id,
                                        meta,
                                    });
                                }

                                //if we don't have it, maybe it's a type variable coming from a generic
                                let is_type_param =
                                    self.ctx.type_parameters.iter().find(|x| x.0 == var);

                                match is_type_param {
                                    Some(type_param) => Ok(HIRExpr::TypeName {
                                        type_variable: TypeConstructParams::Generic(
                                            type_param.clone(),
                                        ),

                                        type_data: type_data_type_id,
                                        meta,
                                    }),
                                    None => {
                                        self.ctx.errors.variable_not_found.push_inference_error(
                                            report!(
                                                self,
                                                meta,
                                                VariableNotFound { variable_name: var }
                                            ),
                                        )
                                    }
                                }
                            }
                            None => {
                                self.ctx
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
            //the self keyword, refers to the type of the impl block
            HIRExpr::SelfValue((), meta) => match self.ctx.impl_of.clone() {
                Some(impl_of) => Ok(HIRExpr::SelfValue(impl_of, meta)),
                None => self.ctx.errors.internal_error.push_inference_error(report!(
                    self,
                    meta,
                    InternalError {
                        error: "Self keyword used outside of an impl block".to_string()
                    }
                )),
            },
        }
    }

    fn load_stdlib_builtin(&mut self, name: &str) -> Option<TypeConstructParams> {
        //check if instance already exists
        let type_data_type = self
            .ctx
            .type_db
            .constructors
            .find_by_name(InternedString::new(name));

        match type_data_type {
            Some(d) => return Some(TypeConstructParams::simple(d.id)),
            None => {
                //TODO: Fix: navigate through all types and roots before running inference,
                //otherwise we can't use types in stdlib
                log!("Type {} not found in stdlib", name);
                None
            }
        }
    }

    fn infer_literal(
        &mut self,
        literal_expr: LiteralHIRExpr,
        type_hint: &Option<TypeConstructParams>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeConstructParams>, CompilerError> {
        let literal_type: TypeConstructParams = match literal_expr {
            LiteralHIRExpr::Integer(_) => {
                if let Some(TypeConstructParams::Parameterized(hint, _)) = type_hint {
                    self.try_literal_promotion(&literal_expr, hint, meta)?;
                    TypeConstructParams::simple(*hint)
                } else {
                    TypeConstructParams::simple(self.ctx.type_db.constructors.common_types.i32)
                }
            }
            LiteralHIRExpr::Char(_) => {
                //@TODO promotable?
                TypeConstructParams::simple(self.ctx.type_db.constructors.common_types.char)
            }
            LiteralHIRExpr::Float(_) => {
                TypeConstructParams::simple(self.ctx.type_db.constructors.common_types.f32)
            }
            LiteralHIRExpr::String(_) => {
                self.load_stdlib_builtin("str").expect("str type not found")
            }
            LiteralHIRExpr::Boolean(_) => {
                TypeConstructParams::simple(self.ctx.type_db.constructors.common_types.bool)
            }
            LiteralHIRExpr::None => todo!("Must implement None"),
        };
        Ok(HIRExpr::Literal(literal_expr, literal_type, meta))
    }

    fn infer_struct_instantiate(
        &mut self,
        struct_name: InternedString,
        struct_type_args: Vec<HIRType>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeConstructParams>, CompilerError> {
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

    fn replace_type_params(
        &self,
        map: &std::collections::HashMap<TypeParameter, TypeConstructParams>,
        type_construct_params: &TypeConstructParams,
    ) -> TypeConstructParams {
        match type_construct_params {
            TypeConstructParams::Generic(param) => {
                if let Some(replacement) = map.get(param) {
                    return replacement.clone();
                }
                log!(
                    "Type param not found in map: {:?}, continuing with generic param as is",
                    param
                );
                TypeConstructParams::Generic(param.clone())
            }
            TypeConstructParams::Parameterized(root, args) => {
                let mut new_args = vec![];
                for arg in args {
                    new_args.push(self.replace_type_params(map, arg));
                }
                TypeConstructParams::Parameterized(root.clone(), new_args)
            }
        }
    }

    fn function_type_constructor_substitute_generics(
        &self,
        type_args: &[TypeConstructParams],
        function_type: &TypeConstructor,
        additional_type_args: &std::collections::HashMap<TypeParameter, TypeConstructParams>,
    ) -> FunctionSignature {
        let map = {
            if function_type.type_params.len() != type_args.len() {
                todo!("Type args and function type params length mismatch");
            }

            let mut map = std::collections::HashMap::new();
            for (idx, arg) in type_args.iter().enumerate() {
                map.insert(function_type.type_params[idx].clone(), arg.clone());
            }
            if !additional_type_args.is_empty() {
                map.extend(additional_type_args.clone());
            }
            map
        };

        let new_params = function_type
            .function_params
            .iter()
            .map(|x| self.replace_type_params(&map, x))
            .collect::<Vec<_>>();

        let new_return_type = self.replace_type_params(
            &map,
            &function_type
                .function_return_type
                .clone()
                .expect("Expected return type"),
        );

        FunctionSignature {
            type_parameters: function_type.type_params.clone(),
            params: new_params,
            return_type: new_return_type,
            variadic: function_type.function_variadic,
        }
    }

    fn infer_function_call(
        &mut self,
        //this is the expression which the function was called, normally this is a variable (TODO: right now it's only variables/identifiers)
        fun_expr: HIRExpr<'source, ()>,
        //these are the type args at the function call site, i.e. if the function is `def foo<T(a: T) -> T` and we call it like `foo::<i32>(1)`, then this will be `i32`
        fun_type_args: Vec<HIRUserTypeInfo<()>>,
        fun_params: Vec<HIRExpr<'source, ()>>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<FunctionCall<'source, TypeConstructParams>, CompilerError> {
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

        let function_type_data = self.ctx.type_db.constructors.find(
            function_type
                .try_get_base()
                .expect("Type constructor not found"),
        );

        if function_type_data.kind != TypeKind::Function {
            return self
                .ctx
                .errors
                .call_non_callable_tc
                .push_inference_error(report!(
                    self,
                    meta,
                    CallToNonCallableType {
                        actual_type: Some(function_type_data.id)
                    }
                ));
        }

        let positional_type_args = self.to_type_constructor_params(fun_type_args, meta)?;

        let positional_type_args_resolved = positional_type_args
            .iter()
            .map(|x| x.resolved_type.clone())
            .collect::<Vec<_>>();

        let function_type_data = self.ctx.type_db.constructors.find(
            function_type
                .try_get_base()
                .expect("Type constructor not found"),
        );

        let ftype_substituted = self.function_type_constructor_substitute_generics(
            &positional_type_args_resolved,
            function_type_data,
            &std::collections::HashMap::new(),
        );

        let args = ftype_substituted.params.clone();
        let return_type = ftype_substituted.return_type.clone();

        let fun_params = self.infer_expr_array(fun_params, Some(&args))?;

        let type_args_inferred = self.try_construct_many(positional_type_args)?;

        //println!("Type args inferred: {:?}", type_args_inferred);
        let result = FunctionCall {
            function: HIRExpr::Variable(
                var,
                function_type, //this keeps the signature the same
                fcall_meta,
            )
            .into(),
            args: fun_params,
            type_args: type_args_inferred,
            return_type,
            meta_expr: meta,
            meta_ast: None,
        };

        Ok(result)
    }

    fn infer_method_call(
        &mut self,
        obj: HIRExpr<'source, ()>,
        method_name: InternedString,
        params: Vec<HIRExpr<'source, ()>>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<MethodCall<'source, TypeConstructParams>, CompilerError> {
        //compute type of obj
        //find method_name in type of obj
        let objexpr = self.infer_expr(obj, None)?;
        let typeof_obj = objexpr.get_type();

        let poly_ty = typeof_obj;
        //we are going to see in the type constructors if the method exists and try to infer something about it
        let constructor = poly_ty.try_get_base();
        //println!("method call: {:?}", poly_ty.print_name(self.ctx.type_db));
        if let Some(constructor) = constructor {
            let constructor = self.ctx.type_db.constructors.find(constructor);

            let method = constructor.find_method(method_name);

            if let Some(method) = method {
                let method_call = self.infer_polymorphic_method_call(
                    poly_ty.clone(),
                    &method.clone(),
                    meta,
                    params.clone(),
                    &objexpr,
                    method_name,
                )?;
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
            return_type: poly_ty,
            meta_expr: meta,
        };

        Ok(mcall)
    }

    fn infer_polymorphic_method_call(
        &mut self,
        object_type: TypeConstructParams,
        method: &crate::types::type_constructor_db::TypeConstructorFunctionDeclaration,
        meta: HIRExprMetadata<'source>,
        params: Vec<HIRExpr<'source, ()>>,
        objexpr: &HIRExpr<'source, TypeConstructParams>,
        method_name: InternedString,
    ) -> Result<MethodCall<'source, TypeConstructParams>, CompilerError> {
        match object_type {
            TypeConstructParams::Generic(_) => {
                unreachable!("Should not happen: We know it should not be Generic here")
            }
            TypeConstructParams::Parameterized(root, args) => {
                log!("Infering polymorphic method call with root: {:?} and method {method:?} and args {:?}", root, args);
                //now we have a map of type parameters and their types substituted,
                //let's pretend we actually have a function call whose first argument is the object

                // let positional_type_args = args.clone().into_iter()
                //    .map(|x| HIRUserTypeInfo { user_given_type: None, resolved_type: x } ).collect::<Vec<_>>();

                let constructor = self.ctx.type_db.constructors.find(root);
                let constructor_type_args = constructor.type_params.clone();

                let mut additional_type_args = std::collections::HashMap::new();
                for (idx, arg) in args.iter().enumerate() {
                    additional_type_args.insert(constructor_type_args[idx].clone(), arg.clone());
                }

                let method_type = self.ctx.type_db.constructors.find(method.signature);

                let Some(first_arg) = method_type.function_params.first() else {
                    panic!(
                        "Function {} does not seem to be a method of an object",
                        method_name
                    );
                };

                let function_type_sig = self.function_type_constructor_substitute_generics(
                    &vec![],
                    &method_type,
                    &additional_type_args,
                );

                let args = function_type_sig.params.clone();
                let return_type = function_type_sig.return_type.clone();

                let skip_self = args
                    .iter()
                    .skip(if first_arg.try_get_base().unwrap() == root {
                        1
                    } else {
                        0
                    })
                    .cloned()
                    .collect::<Vec<_>>();

                let fun_params = self.infer_expr_array(params, Some(&skip_self))?;

                //let type_args_inferred = self.try_construct_many(positional_type_args, meta)?;
                //println!("Type args inferred: {:?}", type_args_inferred);

                let mcall = MethodCall {
                    object: objexpr.clone().into(),
                    method_name,
                    args: fun_params,
                    return_type: return_type,
                    meta_expr: meta,
                };

                return Ok(mcall);
            }
        }
    }

    fn infer_array(
        &mut self,
        array_items: Vec<HIRExpr<'source, ()>>,
        type_hint: Option<TypeConstructParams>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeConstructParams>, CompilerError> {
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
                Ok(HIRExpr::Array(
                    all_exprs.clone(),
                    TypeConstructParams::Parameterized(array_type, vec![expr.get_type()]),
                    meta,
                ))
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

    //@TODO this is basically useless in the end, but it does provide information for debugging purposes
    //when we test the type inference without monomorphization (i.e. what would the IDE see)
    fn infer_member_access(
        &mut self,
        obj: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
        name: InternedString,
    ) -> Result<HIRExpr<'source, TypeConstructParams>, CompilerError> {
        log!("Infering member access: {}", name);
        let obj_expr = self.infer_expr(obj, None)?;

        let typeof_obj = obj_expr.get_type();

        match typeof_obj {
            TypeConstructParams::Generic(_) => {
                //If we got here, maybe we don't have the type yet because it's an object with a generic type,
                //so we can't infer the field type yet
                //typeof_obj
                return Ok(HIRExpr::MemberAccess(
                    obj_expr.into(),
                    name,
                    typeof_obj, //doesn't make much sense to return the same type of the field... but we have to return something
                    meta,
                ));
            }
            TypeConstructParams::Parameterized(constructor, args) => {
                let constructor = self.ctx.type_db.constructors.find(constructor);

                let mut type_map = std::collections::HashMap::new();
                for (idx, arg) in args.iter().enumerate() {
                    type_map.insert(constructor.type_params[idx].clone(), arg.clone());
                }

                let field = constructor.find_field(name);

                if let Some(field) = field {
                    let return_type = field.field_type.clone();

                    let replaced_return_type = self.replace_type_params(&type_map, &return_type);

                    return Ok(HIRExpr::MemberAccess(
                        obj_expr.into(),
                        name,
                        replaced_return_type,
                        meta,
                    ));
                } else {
                    self.ctx
                        .errors
                        .field_not_found_tc
                        .push_inference_error(report!(
                            self,
                            meta,
                            FieldNotFound {
                                field: name,
                                object_type: constructor.id
                            }
                        ))
                }
            }
        }
    }

    fn infer_deref_expr(
        &mut self,
        rhs: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeConstructParams>, CompilerError> {
        let rhs_expr = self.infer_expr(rhs, None)?;
        let typeof_obj = rhs_expr.get_type();

        if let TypeConstructParams::Parameterized(maybe_ptr, pointee_type) = typeof_obj {
            if maybe_ptr == self.ctx.type_db.constructors.common_types.ptr {
                return Ok(HIRExpr::Deref(
                    rhs_expr.into(),
                    pointee_type[0].clone(),
                    meta,
                ));
            } else {
                return self
                    .ctx
                    .errors
                    .invalid_derefed_type_unconstructed
                    .push_inference_error(report!(
                        self,
                        meta,
                        DerefOnNonPointerErrorUnconstructed {
                            attempted_type: TypeConstructParams::Parameterized(
                                maybe_ptr,
                                pointee_type
                            ),
                        }
                    ))?;
            }
        }
        self.ctx
            .errors
            .invalid_derefed_type_unconstructed
            .push_inference_error(report!(
                self,
                meta,
                DerefOnNonPointerErrorUnconstructed {
                    attempted_type: typeof_obj.clone(),
                }
            ))
    }

    fn infer_ref_expr(
        &mut self,
        rhs: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeConstructParams>, CompilerError> {
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

        let ref_expr = TypeConstructParams::Parameterized(ptr_type, vec![typeof_obj]);

        Ok(HIRExpr::Ref(rhs_expr.into(), ref_expr, meta))
    }

    fn infer_unary_expr(
        &mut self,
        rhs: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
        op: SpannedOperator,
    ) -> Result<HIRExpr<'source, TypeConstructParams>, CompilerError> {
        let rhs_expr = self.infer_expr(rhs, None)?;
        let rhs_type = rhs_expr.get_type();
        return Ok(HIRExpr::UnaryExpression(
            op,
            rhs_expr.into(),
            rhs_type,
            TypeInferenceCertainty::Uncertain,
            meta,
        ));
    }

    fn infer_expr_array(
        &mut self,
        exprs: Vec<HIRExpr<'source, ()>>,
        positional_type_hints: Option<&[TypeConstructParams]>,
    ) -> Result<Vec<HIRExpr<'source, TypeConstructParams>>, CompilerError> {
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
    ) -> Result<Vec<HIRUserTypeInfo<TypeConstructParams>>, CompilerError> {
        let mut result = vec![];
        for arg in args {
            let res = arg.resolved_type;
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

    fn infer_binop(
        &mut self,
        lhs: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
        rhs: HIRExpr<'source, ()>,
        op: SpannedOperator,
    ) -> Result<HIRExpr<'source, TypeConstructParams>, CompilerError> {
        let lhs_expr = self.infer_expr(lhs, None)?;
        let lhs_type = lhs_expr.get_type();

        let s = lhs_type.print_name(&self.ctx.type_db);
        log!("RHS type hint for binop: {} {:?}", s, op.0);
        let rhs_expr = self.infer_expr(rhs, Some(lhs_type.clone()))?;

        let rhs_type = rhs_expr.get_type();

        let s = rhs_type.print_name(&self.ctx.type_db);
        log!("RHS type inferred: {}", s);

        let lhs_type_id = lhs_type.try_get_base();
        let rhs_type_id = rhs_type.try_get_base();
        match (lhs_type_id, rhs_type_id) {
            (Some(lhs_type_id), Some(rhs_type_id)) => {
                let lhs_instance = self.ctx.type_db.constructors.find(lhs_type_id);

                for (operator, rhs_supported, result_type) in &lhs_instance.rhs_binary_ops {
                    if *operator == op.0 && rhs_supported.try_get_base() == Some(rhs_type_id) {
                        return Ok(HIRExpr::BinaryOperation(
                            Box::new(lhs_expr),
                            op,
                            Box::new(rhs_expr),
                            result_type.clone(),
                            TypeInferenceCertainty::Uncertain,
                            meta,
                        ));
                    }
                }

                //operator not found, add binary op not found error
                self.ctx
                    .errors
                    .binary_op_not_found_tc
                    .push_inference_error(report!(
                        self,
                        &op.1,
                        BinaryOperatorNotFoundForTypeConstructor {
                            lhs: lhs_type,
                            rhs: rhs_type,
                            operator: op.0,
                        }
                    ))
            }
            _ => {
                //if one of the types is not inferred, we have to wait for monomorphization and rerun inference later
                Ok(HIRExpr::BinaryOperation(
                    Box::new(lhs_expr),
                    op,
                    Box::new(rhs_expr),
                    lhs_type, //just pass anything here, not important
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
        let expr_str = HIRExprPrinter::new(&self.ctx.type_db, false).print(&assigned_value);
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
        parameters: &[HIRTypedBoundName<TypeConstructParams>],
        body: Vec<TypeInferenceInputHIR<'source>>,
        _return_type: TypeConstructParams,
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
            impl_of: self.ctx.impl_of.clone(),
        };

        let mut new_type_inference = FunctionTypeInferenceContext {
            ctx: new_ctx_with_modified_scope,
        };

        Ok(new_type_inference.infer_types_in_body(body)?)
    }
}

pub fn infer_types<'source>(
    globals: &mut NameRegistry<TypeConstructParams>,
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
                is_external,
                method_of,
            } => {
                let inference_ctx = TypeInferenceContext {
                    on_function: RootElementType::Function(function_name),
                    type_db,
                    errors,
                    decls_in_scope: globals,
                    type_parameters: type_parameters.clone(),
                    impl_of: method_of.clone(),
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
                    is_external,
                    method_of: method_of.clone(),
                }
            }
            HIRRoot::StructDeclaration {
                struct_name,
                fields,
                type_parameters,
                meta,
            } => {
                let resolved_fields = fields
                    .into_iter()
                    .map(|f| {
                        let type_data = hir_type_to_usage(
                            RootElementType::Struct(struct_name),
                            &f.type_data,
                            type_db,
                            &type_parameters,
                            errors,
                            meta,
                        )?;

                        Ok(HIRTypedBoundName {
                            name: f.name,
                            type_data,
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
            HIRRoot::ImplDeclaration {
                struct_name,
                type_parameters,
                methods,
                meta,
            } => {
                let mut methods_inferred = vec![];

                for method in methods {
                    if let HIRRoot::DeclareFunction {
                        function_name,
                        type_parameters,
                        parameters,
                        body,
                        return_type,
                        meta,
                        is_intrinsic,
                        is_varargs,
                        is_external,
                        method_of,
                    } = method
                    {
                        let inference_ctx = TypeInferenceContext {
                            on_function: RootElementType::Function(function_name),
                            type_db,
                            errors,
                            decls_in_scope: globals,
                            type_parameters: type_parameters.clone(),
                            impl_of: method_of.clone(),
                        };

                        let mut function_inference =
                            FunctionTypeInferenceContext { ctx: inference_ctx };

                        let body = function_inference.infer_function(
                            &parameters,
                            body,
                            return_type.clone(),
                            meta,
                        )?;
                        methods_inferred.push(HIRRoot::DeclareFunction {
                            function_name,
                            type_parameters,
                            parameters,
                            body,
                            return_type,
                            meta,
                            is_intrinsic,
                            is_varargs,
                            is_external,
                            method_of: method_of.clone(),
                        });
                    } else {
                        //TODO do not panic here
                        panic!("Method in impl declaration is not a function declaration");
                    }
                }

                HIRRoot::ImplDeclaration {
                    struct_name,
                    type_parameters,
                    methods: methods_inferred,
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
