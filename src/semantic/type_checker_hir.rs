
use super::hir::{HIRType, MonoType, NodeIndex, PolyType, TypeIndex, TypeTable};
use super::typer::Unifier;
use crate::interner::InternedString;
use crate::semantic::hir::{HIR, HIRExpr, HIRRoot, LiteralHIRExpr};
use crate::types::diagnostics::{
    ArrayExpressionsNotAllTheSameType, AssignContext, CompilerErrorContext, DerefOnNonPointerError, InternalError, OutOfTypeBounds,
    ReturnTypeContext, RootElementType, TypeErrors, TypeMismatch,
    UnificationError,
};
use crate::types::diagnostics::{
    IfStatementNotBoolean, UnexpectedTypeFound,
};
use crate::types::type_constructor_db::{
    CommonTypeConstructors, TypeConstructorDatabase,
};


pub struct HIRTypeCheckContext<'compiler_context> {
    //top_level_decls: &'compiler_context <TypeInstanceId>,
    type_db: &'compiler_context TypeConstructorDatabase,
    errors: &'compiler_context mut TypeErrors,
    on_element: RootElementType,
}

type CompilerError = ();

impl<'compiler_context> HIRTypeCheckContext<'compiler_context> {
    //We will check that function arguments and index operators are receiving the correct types.
    //We also implement sanity checks here, to prevent bugs in the type inference propagating to the backend code.
    //We also assert that literal types are within bounds.
    pub fn typecheck(
        &mut self,
        expr: &HIRExpr,
        type_table: &TypeTable,
    ) -> Result<(), CompilerError> {
        match expr {
            HIRExpr::Literal(literal, location, type_id) => {
                let mono = &type_table[*type_id].mono;
                self.check_literal_expr_value_bounds(
                    literal,
                    mono,
                    &self.type_db.common_types,
                    type_table,
                    *location,
                )
            }
            HIRExpr::Cast(cast_expr, cast_to, location, type_id) => {
                self.check_expr_cast(cast_expr.as_ref(), cast_to, location, type_id)
            }
            HIRExpr::BinaryOperation(lhs, op, rhs, location, expr_type) => {
                //self.check_fun_expr_bin_op(*lhs, *rhs, op, expr_type, location)
                Ok(())
            }
            HIRExpr::MethodCall(call, location) => {
                Ok(())
                //self.check_expr_method_call(obj, *name, args, *return_type, type_table, *location)
            }
            HIRExpr::FunctionCall(call, location) => Ok(()),
            HIRExpr::StructInstantiate(name, type_args, location, type_id) => Ok(()),
            HIRExpr::UnaryExpression(op, rhs, inferred_type, location) => {
                //Type inference would not work
                //if the unary operator is not found, so we can assume it is correct.
                Ok(())
            }
            HIRExpr::Array(expr_array, location, inferred_type) => {
                self.check_expr_array(expr_array, *inferred_type, type_table, *location)
            }
            HIRExpr::MemberAccess(obj, member, location, inferred_type) => {
                //elf.check_expr_member_access(*obj, member, inferred_type, location)
                Ok(())
            }
            HIRExpr::Variable(var, location, inferred_type) => Ok(()),
            HIRExpr::Deref(obj, location, expr_type) => {
                self.check_deref_expr(obj, *expr_type, type_table, *location)
            } //
            HIRExpr::Ref(obj, location, expr_type) => {
                self.check_ref_expr(obj, *expr_type, type_table, *location)
            }
            /*HIRExpr::TypeName {
                type_variable,
                type_data,
                location,
            } => {
                let type_data_type = &type_table[type_data];
                let type_data_type_data = self.type_db.find_by_name("TypeData".into()).unwrap();

                let mut unifier = Unifier::new(self.errors, self.type_db);
                let unify_result = unifier.unify(
                    &type_data_type.mono,
                    &MonoType::simple(type_data_type_data.id),
                    *location,
                    &self.on_element,
                    false,
                );

                match unify_result {
                    Ok(_) => Ok(()),
                    Err(e) => {
                        self.errors.internal_error.push(CompilerErrorContext {
                            on_element: self.on_element,
                            location: *location,
                            compiler_code_location: loc!(),
                            error: InternalError {
                                error: "Type variable type data is not TypeData, this is a type inference error.".to_string()
                            }
                        });
                        Err(())
                    }
                }
            },*/
            HIRExpr::MemberAccess(obj, member, inferred_type, location) => {
                //self.check_expr_member_access(*obj, member, inferred_type, location)
                Ok(())
            }
            HIRExpr::Variable(var, inferred_type, location) => Ok(()),
            HIRExpr::Deref(obj, location, expr_type) => {
                self.check_deref_expr(obj, *expr_type, type_table, *location)
            }
            _ => Ok(()), //should be OK...
        }
    }

    //*a where a: ptr<T>, result type = T
    fn check_deref_expr(
        &mut self,
        obj: &HIRExpr,
        expr_type: super::hir::TypeIndex,
        type_table: &TypeTable,
        location: NodeIndex,
    ) -> Result<(), CompilerError> {
        self.typecheck(obj, type_table)?;
        let mut unifier = Unifier::new(self.errors, self.type_db);

        let derefed_type = &type_table[expr_type].mono; //self.type_db. //get_instance(expr_type);
        let ptr_type = &type_table[obj.get_type()].mono;

        let ptr_generic = self.type_db.find_by_name("ptr".into()).unwrap();
        let mono_ptr = MonoType::Application(
            ptr_generic.id,
            vec![MonoType::variable(InternedString::new("T"))],
        );
        //typechecked should be ptr<T>
        let unify_result = unifier.unify(ptr_type, &mono_ptr, location, &self.on_element, false);

        match unify_result {
            Err(stack) => {
                self.errors.unify_error.push(CompilerErrorContext {
                    error: UnificationError {
                        stack,
                        context: "".into(),
                    },
                    on_element: self.on_element,
                    location,
                    compiler_code_location: loc!(),
                });
                self.errors.invalid_derefed_type.push(CompilerErrorContext {
                    error: DerefOnNonPointerError {
                        attempted_type: type_table[obj.get_type()].clone(),
                    },
                    on_element: self.on_element,
                    location,
                    compiler_code_location: loc!(),
                });
                return Err(());
            }
            Ok(subs) => {
                let type_t = subs.get(&super::hir::TypeVariable("T".into()));
                if let Some(t) = type_t {
                    let unify_result =
                        unifier.unify(derefed_type, t, location, &self.on_element, false);

                    match unify_result {
                        Ok(_) => {}
                        Err(e) => {
                            self.errors.internal_error.push(CompilerErrorContext {
                                on_element: self.on_element,
                                location,
                                compiler_code_location: loc!(),
                                error: InternalError {
                                    error: "Could not check the deref type".to_string(),
                                },
                            });
                            return Err(());
                        }
                    }
                } else {
                    self.errors.internal_error.push(CompilerErrorContext {
                        on_element: self.on_element,
                        location,
                        compiler_code_location: loc!(),
                        error: InternalError {
                            error: "Could not check the deref type".to_string(),
                        },
                    });
                    return Err(());
                }
            }
        }

        Ok(())
    }

    fn check_ref_expr(
        &mut self,
        obj: &HIRExpr, //value to be referenced, &obj, this will be only obj and wont be ptr<?>
        expr_type: super::hir::TypeIndex, //produced type, ptr<?>
        type_table: &TypeTable,
        location: NodeIndex,
    ) -> Result<(), CompilerError> {
        self.typecheck(obj, type_table)?;
        let ptr_type = &type_table[expr_type].mono;

        //and expr_type has to be ptr<T> where T is the type of the object

        let expected_type = MonoType::Application(
            self.type_db.find_by_name("ptr".into()).unwrap().id,
            vec![MonoType::variable(InternedString::new("T"))],
        );

        let mut unifier = Unifier::new(self.errors, self.type_db);
        let unify_result =
            unifier.unify(ptr_type, &expected_type, location, &self.on_element, false);

        match unify_result {
            Ok(subs) => {
                let type_t = subs.get(&super::hir::TypeVariable("T".into()));
                if let Some(t) = type_t {
                    let obj_type = &type_table[obj.get_type()].mono;
                    let unify_result =
                        unifier.unify(obj_type, t, location, &self.on_element, false);

                    match unify_result {
                        Ok(_) => {}
                        Err(stack) => {
                            self.errors.unify_error.push(CompilerErrorContext {
                                error: UnificationError {
                                    stack,
                                    context: "Type checking of reference & expression".into(),
                                },
                                on_element: self.on_element,
                                location,
                                compiler_code_location: loc!(),
                            });
                            return Err(());
                        }
                    }
                } else {
                    self.errors.internal_error.push(CompilerErrorContext {
                        on_element: self.on_element,
                        location,
                        compiler_code_location: loc!(),
                        error: InternalError {
                            error: "Could not find the type of the object in substitution during ref check".to_string(),
                        },
                    });
                    return Err(());
                }
            }
            Err(stack) => {
                self.errors.unify_error.push(CompilerErrorContext {
                    error: UnificationError {
                        stack,
                        context: "Type checking of reference & expression".into(),
                    },
                    on_element: self.on_element,
                    location,
                    compiler_code_location: loc!(),
                });
                self.errors.internal_error.push(CompilerErrorContext {
                    on_element: self.on_element,
                    location,
                    compiler_code_location: loc!(),
                    error: InternalError {
                        error: "Ref expression produced non-ptr<T> type".to_string(),
                    },
                });
                return Err(());
            }
        }

        Ok(())
    }

    fn make_unifier(&mut self) -> Unifier<'_> {
        Unifier::new(self.errors, self.type_db)
    }

    fn check_expr_array(
        &mut self,
        expr_array: &[HIRExpr],
        inferred_type: TypeIndex,
        type_table: &TypeTable,
        location: NodeIndex,
    ) -> Result<(), CompilerError> {
        let array_type = self.type_db.common_types.array;

        let _ = expr_array
            .iter()
            .map(|expr| self.typecheck(expr, type_table))
            .collect::<Result<Vec<_>, _>>()?;

        let all_types = expr_array.iter().map(|x| x.get_type()).collect::<Vec<_>>();
        let first_type = &type_table[all_types[0]];
        let cloned_self_element = self.on_element;

        let mut unifier = self.make_unifier();

        for t in all_types.iter() {
            let target_type = &type_table[t];

            let unification_result = unifier.unify_strict(
                &first_type.mono,
                &target_type.mono,
                location,
                &cloned_self_element,
            );

            match unification_result {
                Ok(_) => {}
                Err(e) => {
                    self.errors.array_expressions_not_all_the_same_type.push(
                        CompilerErrorContext {
                            error: ArrayExpressionsNotAllTheSameType {
                                expected_type: first_type.mono.clone(),
                                found_unexpected_type: target_type.mono.clone(),
                                unification_error_stack: e,
                            },
                            on_element: self.on_element,
                            location,
                            compiler_code_location: loc!(),
                        },
                    );

                    return Err(());
                }
            }
        }
        let unification_result = unifier.unify_strict(
            &MonoType::Application(array_type, vec![first_type.mono.clone()]),
            &type_table[inferred_type].mono,
            location,
            &cloned_self_element,
        );

        match unification_result {
            Ok(_) => Ok(()),
            Err(e) => {
                self.errors
                    .array_expressions_not_all_the_same_type
                    .push(CompilerErrorContext {
                        error: ArrayExpressionsNotAllTheSameType {
                            expected_type: first_type.mono.clone(),
                            found_unexpected_type: type_table[inferred_type].mono.clone(),
                            unification_error_stack: e,
                        },
                        on_element: self.on_element,
                        location,
                        compiler_code_location: loc!(),
                    });

                Err(())
            }
        }
    }

    /*
       fn check_expr_member_access(
           &mut self,
           obj: &MIRExpr,
           member: InternedString,
           type_table: &TypeTable,
           inferred_type: TypeIndex,
           location: NodeIndex,
       ) -> Result<MIRExprLValue, CompilerError> {
           let obj_type = obj.get_type();
           let _ = self.typecheck(obj, type_table)?;

           let inferred_mono = &type_table[inferred_type].mono;
           let obj_mono = &type_table[obj_type].mono;

           let mut unifier = Unifier::new(self.errors, self.type_db);

           let unification_result =
               unifier.unify(obj_mono, inferred_mono, location, &self.on_element, false);

           match unification_result {
               Ok(_) => {}
               Err(e) => {
                   self.errors.field_not_found.push_typecheck_error(
                       FieldNotFound {
                           object_type: obj_type,
                           field_name: member,
                       }
                       .at_spanned(self.on_element, location, loc!()),
                   );
                   return Err(());
               }
           }

           Ok(MIRExprLValue::MemberAccess(
               checked_obj.into(),
               member,
               inferred_type,
               location,
           ))
       }
    */

    /*
    fn check_fun_expr_bin_op(
        &mut self,
        lhs: TypecheckPendingExpression,
        rhs: TypecheckPendingExpression,
        op: SpannedOperator,
        expr_type: TypeInstanceId,
        location: NodeIndex,
    ) -> Result<(), CompilerError> {
        let checked_lhs = self.typecheck(lhs)?;
        let checked_rhs = self.typecheck(rhs)?;
        let lhs_type = self.type_db.get_instance(checked_lhs.get_type());
        if let Some(_t) = lhs_type
            .rhs_binary_ops
            .iter()
            .find(|(rhs_op, rhs_type, result_type)| {
                *rhs_op == op.0 && *rhs_type == checked_rhs.get_type() && *result_type == expr_type
            })
        {
            Ok(MIRExpr::RValue(MIRExprRValue::BinaryOperation(
                checked_lhs.into(),
                op,
                checked_rhs.into(),
                expr_type,
                location,
            )))
        } else {
            return self.errors.binary_op_not_found.push_typecheck_error(
                BinaryOperatorNotFound {
                    lhs: checked_lhs.get_type(),
                    rhs: checked_rhs.get_type(),
                    operator: op.0,
                }
                .at_spanned(self.on_element, &op.1, loc!()),
            );
        }
    }
    */
    fn check_literal_expr_value_bounds(
        &mut self,
        literal: &LiteralHIRExpr,
        type_mono: &MonoType,
        common_types: &CommonTypeConstructors,
        type_table: &TypeTable,
        location: NodeIndex,
    ) -> Result<(), CompilerError> {
        let MonoType::Application(type_id, _) = type_mono else {
            return Ok(());
        };

        //this clone is entirely unecessary, I was just too lazy to fix everything else here.
        let literal = literal.clone();
        let type_id = *type_id;

        match literal {
            LiteralHIRExpr::Integer(i) => {
                let is_within_bounds = if type_id == common_types.i32 {
                    i >= i128::from(i32::MIN) && i <= i128::from(i32::MAX)
                } else if type_id == common_types.i64 {
                    i >= i128::from(i64::MIN) && i <= i128::from(i64::MAX)
                } else if type_id == common_types.u32 {
                    i >= i128::from(u32::MIN) && i <= i128::from(u32::MAX)
                } else if type_id == common_types.u64 {
                    i >= i128::from(u64::MIN) && i <= i128::from(u64::MAX)
                } else {
                    self.errors.unexpected_types.push(CompilerErrorContext {
                        error: UnexpectedTypeFound {
                            type_def: type_mono.clone(),
                        },
                        on_element: self.on_element,
                        location,
                        compiler_code_location: loc!(),
                    });
                    return Err(());
                };
                if is_within_bounds {
                    true
                } else {
                    self.errors.out_of_bounds.push(CompilerErrorContext {
                        error: OutOfTypeBounds {
                            typ: type_mono.clone(),
                            expr: i.to_string(),
                        },
                        on_element: self.on_element,
                        location,
                        compiler_code_location: loc!(),
                    });
                    return Err(());
                }
            }
            LiteralHIRExpr::Float(f) => {
                let f = f.0;
                let is_within_bounds = if type_id == common_types.f32 {
                    f >= f64::from(f32::MIN) && f <= f64::from(f32::MAX)
                } else if type_id == common_types.f64 {
                    true
                } else {
                    self.errors.unexpected_types.push(CompilerErrorContext {
                        error: UnexpectedTypeFound {
                            type_def: type_mono.clone(),
                        },
                        on_element: self.on_element,
                        location,
                        compiler_code_location: loc!(),
                    });
                    return Err(());
                };
                if is_within_bounds {
                    true
                } else {
                    self.errors.out_of_bounds.push(CompilerErrorContext {
                        error: OutOfTypeBounds {
                            typ: type_mono.clone(),
                            expr: f.to_string(),
                        },
                        on_element: self.on_element,
                        location,
                        compiler_code_location: loc!(),
                    });
                    return Err(());
                }
            }
            LiteralHIRExpr::String(_) | LiteralHIRExpr::Boolean(_) | LiteralHIRExpr::Char(_) => {
                true
            }
            LiteralHIRExpr::None => unimplemented!(),
        };
        Ok(())
    }
    /*
       fn method_call_check(
           &mut self,
           obj: TypecheckPendingExpression,
           name: InternedString,
           args: Vec<TypecheckPendingExpression>,
           location: NodeIndex,
       ) -> Result<
           (
               TypecheckedExpression,
               Vec<TypecheckedExpression>,
           ),
           CompilerError,
       > {
           let checked_obj = self.typecheck(obj)?;
           log!(
               "method call check, checked_obj = {}",
               checked_obj.get_type().to_string(&self.type_db)
           );
           let obj_type = self.type_db.get_instance(checked_obj.get_type());

           let function_type = if let Some(ftype) = obj_type.methods.iter().find(|x| x.name == name) {
               self.type_db.get_instance(ftype.function_type)
           } else {
               log!("Error accessing method {} {}", name, obj_type.name);
               return self.errors.method_not_found.push_typecheck_error(
                   MethodNotFound {
                       object_type: checked_obj.get_type(),
                       method: name,
                   }
                   .at_spanned(self.on_element, location, loc!()),
               );
           };

           let checked_args = args
               .into_iter()
               .map(|arg| self.typecheck(arg))
               .collect::<Result<Vec<_>, _>>()?;

           let skip_self = function_type
               .function_args
               .iter()
               .skip(1)
               .map(|x| *x)
               .collect::<Vec<TypeInstanceId>>();

           //-1 por que é metodo e o primeiro é self...
           if skip_self.len() != checked_args.len() {
               return self
                   .errors
                   .function_call_argument_count
                   .push_typecheck_error(
                       FunctionCallArgumentCountMismatch {
                           called_function_name: make_method_name_or_index(
                               name,
                               &obj_type.name,
                               location,
                           ),
                           expected_count: skip_self.len(),
                           passed_count: checked_args.len(),
                       }
                       .at_spanned(self.on_element, location, loc!()),
                   );
           };

           let checked_arg_types = checked_args.iter().map(|x| x.get_type());
           let has_invalid_arg = skip_self
               .into_iter()
               .zip(checked_arg_types)
               .enumerate()
               .find(|(_index, (expected, actual))| *expected != *actual);

           if let Some((arg_pos, types)) = has_invalid_arg {
               return self.errors.function_call_mismatches.push_typecheck_error(
                   TypeMismatch {
                       context: FunctionCallContext {
                           called_function_name: make_method_name_or_index(
                               name,
                               &obj_type.name,
                               location,
                           ),
                           argument_position: arg_pos,
                       },
                       expected: types.0,
                       actual: types.1,
                   }
                   .at_spanned(self.on_element, location, loc!()),
               );
           }
           Ok((checked_obj, checked_args))
       }

       fn function_call_check<FName>(
           &mut self,
           function_type: TypeIndex,
           function_name: FName,
           args: &[TypecheckPendingExpression],
           type_table: &TypeTable,
           location: NodeIndex,
       ) -> Result<(), CompilerError>
       where
           FName: Fn() -> FunctionName,
       {
           //functions are already type checked during inference.
           args.iter()
               .map(|arg| self.typecheck(arg, type_table))
               .collect::<Result<Vec<_>, _>>()?;
           return Ok(());
       }
    */
    fn type_check_roots<'scopes>(
        &mut self,
        body: &[HIR],
        return_type: &MonoType,
        type_table: &TypeTable,
    ) -> Result<(), CompilerError> {
        let fname = self.on_element.get_name();

        let cloned_element = self.on_element;
        for block in body {

            match block {
                crate::semantic::hir::HIR::If(
                    bool_expr,
                    true_branch,
                    false_branch,
                    node_index,
                ) => {
                    self.check_if_statement_expression(
                        bool_expr,
                        *node_index,
                        type_table,
                    )?;
                }
                crate::semantic::hir::HIR::Assign {
                    path,
                    expression,
                    location,
                } => {
                    self.check_assignment(&fname, type_table, path, expression, location)?;
                },
                crate::semantic::hir::HIR::SyntheticDeclare {
                    location,
                    var,
                    typedef,
                    expression
                } => {
                   self.check_declare(&fname, type_table, location, var, typedef, expression)?;
                },
                crate::semantic::hir::HIR::Declare {
                    location,
                    var,
                    typedef,
                    expression } => {
                    (*self).check_declare(&fname, type_table, location, var, &typedef.type_variable, expression)?;
                },
                crate::semantic::hir::HIR::While(hirexpr, hirs, node_index) => {
                    //@TODO parameterize if/while
                    self.check_if_statement_expression(
                        hirexpr,
                        *node_index,
                        type_table,
                    )?;
                },
                crate::semantic::hir::HIR::Return(return_expr, location) => {
                    self.typecheck(return_expr, type_table)?;
                    let expr_type = return_expr.get_type();
                    let expr_mono = &type_table[expr_type].mono;
                    let mut unifier = self.make_unifier();

                    let unification_result =
                        unifier.unify_strict(
                            return_type,
                            expr_mono,
                            *location,
                            &cloned_element
                        );

                    match unification_result {
                        Ok(_) => {}
                        Err(e) => {
                            self.errors
                                .return_type_mismatches
                                .push(CompilerErrorContext {
                                    error: TypeMismatch {
                                        context: ReturnTypeContext(),
                                        expected: PolyType::mono(return_type.clone()),
                                        actual: PolyType::mono(expr_mono.clone()),
                                    },
                                    on_element: self.on_element,
                                    location: *location,
                                    compiler_code_location: loc!(),
                                });
                            return Err(());
                        }
                    }
                }
                crate::semantic::hir::HIR::EmptyReturn(location) => {
                    let MonoType::Application(ctor, _) = return_type else {
                        //Just in case a type variable reaches here... we throw a generic type mismatch
                        self.errors
                            .return_type_mismatches
                            .push(CompilerErrorContext {
                                error: TypeMismatch {
                                    context: ReturnTypeContext(),
                                    expected: PolyType::mono(return_type.clone()),
                                    actual: PolyType::mono(MonoType::simple(
                                        self.type_db.common_types.void,
                                    )),
                                },
                                on_element: self.on_element,
                                location: *location,
                                compiler_code_location: loc!(),
                            });
                        return Err(());
                    };

                    if ctor != &self.type_db.common_types.void {
                        self.errors
                            .return_type_mismatches
                            .push(CompilerErrorContext {
                                error: TypeMismatch {
                                    context: ReturnTypeContext(),
                                    expected: PolyType::mono(return_type.clone()),
                                    actual: PolyType::mono(MonoType::simple(
                                        self.type_db.common_types.void,
                                    )),
                                },
                                on_element: self.on_element,
                                location: *location,
                                compiler_code_location: loc!(),
                            });
                        return Err(());
                    }
                },
                _ => {}
            }

        }
        Ok(())
    }

    fn check_declare(
        &mut self,
        fname: &String,
        type_table: &TypeTable,
        location: &NodeIndex,
        var: &InternedString,
        typedef: &TypeIndex,
        expression: &HIRExpr) -> Result<(), CompilerError>
    {
        let expr_type = expression.get_type();

        let mono_expr_type = &type_table[expr_type].mono;
        let mono_ty = &type_table[typedef].mono;

        let mut unifier = Unifier::new(self.errors, self.type_db);
        let unify_result = unifier.unify(
            mono_expr_type,
            mono_ty,
            *location,
            &self.on_element,
            false,
        );

        match unify_result {
            Ok(_) => {}
            Err(e) => {
                self.errors.assign_mismatches.push(CompilerErrorContext {
                    error: TypeMismatch {
                        context: AssignContext {
                            assign_lvalue_expr: var.into(),
                        },
                        expected: PolyType::mono(mono_ty.clone()),
                        actual: PolyType::mono(mono_expr_type.clone()),
                    },
                    on_element: self.on_element,
                    location: *location,
                    compiler_code_location: loc!(),
                });
                return Err(());
            }
        }
        Ok(())
    }

    fn check_assignment(
        &mut self,
        fname: &String,
        type_table: &TypeTable,
        path: &HIRExpr,
        expression: &HIRExpr,
        location: &NodeIndex) -> Result<(), CompilerError> {
        //let path_lvalue = MIRExpr::LValue(path.clone());
        self.typecheck(path, type_table)?;
        self.typecheck(expression, type_table)?;

        let expr_type = expression.get_type();

        match path {
            HIRExpr::Variable(var_name, _node, ty) => {
                let mono_expr_type = &type_table[expr_type].mono;
                let mono_ty = &type_table[ty].mono;

                let mut unifier = Unifier::new(self.errors, self.type_db);
                let unify_result = unifier.unify(
                    mono_expr_type,
                    mono_ty,
                    *location,
                    &self.on_element,
                    false,
                );

                match unify_result {
                    Ok(_) => {}
                    Err(e) => {
                        self.errors.assign_mismatches.push(CompilerErrorContext {
                            error: TypeMismatch {
                                context: AssignContext {
                                    assign_lvalue_expr: var_name.into(),
                                },
                                expected: PolyType::mono(mono_ty.clone()),
                                actual: PolyType::mono(mono_expr_type.clone()),
                            },
                            on_element: self.on_element,
                            location: *location,
                            compiler_code_location: loc!(),
                        });
                        return Err(());
                    }
                }
            }
            HIRExpr::MemberAccess(_, member, location, ty) => {
                //in this case, we check that the member exists on the object,
                //and that the type of the member is the same as the type of the expression
                //this is redundant with the check in the type inference, but it's better to be safe

                let assign_type = ty;

                let assign_type_mono = &type_table[assign_type].mono;
                let rhs_type_mono = &type_table[expr_type].mono;

                let mut unifier = Unifier::new(self.errors, self.type_db);

                let unify_result = unifier.unify(
                    assign_type_mono,
                    rhs_type_mono,
                    *location,
                    &self.on_element,
                    false,
                );

                match unify_result {
                    Ok(_) => {}
                    Err(e) => {
                        self.errors.assign_mismatches.push(CompilerErrorContext {
                            error: TypeMismatch {
                                context: AssignContext {
                                    assign_lvalue_expr: member.into(),
                                },
                                expected: PolyType::mono(assign_type_mono.clone()),
                                actual: PolyType::mono(rhs_type_mono.clone()),
                            },
                            on_element: self.on_element,
                            location: *location,
                            compiler_code_location: loc!(),
                        });
                        return Err(());
                    }
                };
            }
            HIRExpr::Deref(ptr, location, ty) => {
                let lhs_type = ty;

                let mono_expr_type = &type_table[expr_type].mono;
                let mono_ty = &type_table[ty].mono;

                let mut unifier = Unifier::new(self.errors, self.type_db);

                let unify_result = unifier.unify(
                    mono_expr_type,
                    mono_ty,
                    *location,
                    &self.on_element,
                    false,
                );

                match unify_result {
                    Ok(_) => {}
                    Err(e) => {
                        self.errors.assign_mismatches.push(CompilerErrorContext {
                            error: TypeMismatch {
                                context: AssignContext {
                                    assign_lvalue_expr: "ptr deref".into(),
                                },
                                expected: PolyType::mono(mono_ty.clone()),
                                actual: PolyType::mono(mono_expr_type.clone()),
                            },
                            on_element: self.on_element,
                            location: *location,
                            compiler_code_location: loc!(),
                        });
                        return Err(());
                    }
                };
            },
            _ => {
                panic!("Unexpected assign: lvalue {path:?} rvalue {expression:?}")
            }
        }
        Ok(())
    }

    fn check_if_statement_expression(
        &mut self,
        expr: &HIRExpr,
        location: NodeIndex,
        type_table: &TypeTable,
    ) -> Result<(), CompilerError> {
        self.typecheck(expr, type_table)?;
        let expr_type = expr.get_type();

        let expr_mono = &type_table[expr_type].mono;

        let MonoType::Application(ctor, _) = expr_mono else {
            self.errors
                .if_statement_unexpected_type
                .push(CompilerErrorContext {
                    error: IfStatementNotBoolean {
                        actual_type: PolyType::mono(expr_mono.clone()),
                    },
                    on_element: self.on_element,
                    location,
                    compiler_code_location: loc!(),
                });
            return Err(());
        };

        if *ctor == self.type_db.common_types.bool {
            Ok(())
        } else {
            self.errors
                .if_statement_unexpected_type
                .push(CompilerErrorContext {
                    error: IfStatementNotBoolean {
                        actual_type: PolyType::mono(expr_mono.clone()),
                    },
                    on_element: self.on_element,
                    location,
                    compiler_code_location: loc!(),
                });
            Err(())
        }
    }

    fn check_expr_cast(&self, expr: &HIRExpr, cast_to: &HIRType, location: &NodeIndex, type_id: &TypeIndex) -> Result<(), CompilerError> {
        unimplemented!()
    }
}

pub fn typecheck(
    top_nodes: &[HIRRoot],
    type_db: &TypeConstructorDatabase,
    errors: &mut TypeErrors,
) -> Result<(), CompilerError> {
    for node in top_nodes {
        if let HIRRoot::DeclareFunction {
                function_name,
                type_parameters,
                parameters,
                body,
                return_type,
                method_of,
                is_intrinsic,
                is_external,
                is_varargs,
                type_table,
                has_been_monomorphized,
            } = node {
            let mut context = HIRTypeCheckContext {
                type_db,
                errors,
                on_element: RootElementType::Function(*function_name),
            };
            let mono_return_type = &type_table[return_type.type_variable].mono;
            context.type_check_roots(body, mono_return_type, type_table)?;
        }
    }
    Ok(())
}
