use std::collections::HashSet;

use super::hir::{MonoType, NodeIndex, PolyType, TypeIndex, TypeTable};
use super::mir_printer::{self, MIRPrinter};
use super::typer::{UnificationErrorStack, Unifier};
use crate::ast::parser::{Expr, SpannedOperator};
use crate::interner::InternedString;
use crate::types::diagnostics::{
    ArrayExpressionsNotAllTheSameType, AssignContext, CompilerErrorContext,
    ContextualizedCompilerError, DerefOnNonPointerError, FunctionCallArgumentCountMismatch,
    FunctionCallContext, InternalError, MethodNotFound, OutOfTypeBounds, RefOnNonLValueError,
    ReturnTypeContext, RootElementType, TypeErrors, TypeMismatch, UnaryOperatorNotFound,
    UnificationError,
};
use crate::types::diagnostics::{
    BinaryOperatorNotFound, FieldNotFound, IfStatementNotBoolean, UnexpectedTypeFound,
};
use crate::types::type_constructor_db::{
    CommonTypeConstructors, TypeConstructorDatabase, TypeConstructorId,
};

use super::mir::{
    BlockId, LiteralMIRExpr, MIRBlock, MIRBlockFinal, MIRBlockNode, MIRExpr, MIRExprLValue,
    MIRExprRValue, MIRScope, MIRTopLevelNode, ScopeId, TypecheckPendingExpression,
    TypecheckedExpression, TypecheckedMIRBlock,
};

pub struct TypeCheckContext<'compiler_context> {
    //top_level_decls: &'compiler_context <TypeInstanceId>,
    type_db: &'compiler_context TypeConstructorDatabase,
    errors: &'compiler_context mut TypeErrors,
    on_element: RootElementType,
}

type CompilerError = ();

impl<'compiler_context> TypeCheckContext<'compiler_context> {
    //We will check that function arguments and index operators are receiving the correct types.
    //Binary operators are actually certainly correct, since type inference will report errors if it can't find an appropriate operator.
    //We might implement sanity checks here, to prevent bugs in the type inference propagating to the backend code.
    //We also assert that literal types are within bounds.
    pub fn typecheck(
        &mut self,
        expr: &MIRExpr,
        type_table: &TypeTable,
    ) -> Result<(), CompilerError> {
        match expr {
            MIRExpr::RValue(MIRExprRValue::Literal(literal, type_id, location)) => {
                let mono = &type_table[*type_id].mono;
                self.check_literal_expr_value_bounds(
                    &literal,
                    mono,
                    &self.type_db.common_types,
                    type_table,
                    *location,
                )
            }
            MIRExpr::RValue(MIRExprRValue::Cast(cast_expr, cast_to, location)) => {
                //self.check_expr_cast(*cast_expr, cast_to, location)
                Ok(())
            }
            MIRExpr::RValue(MIRExprRValue::BinaryOperation(lhs, op, rhs, expr_type, location)) => {
                //self.check_fun_expr_bin_op(*lhs, *rhs, op, expr_type, location)
                Ok(())
            }
            MIRExpr::RValue(MIRExprRValue::MethodCall(obj, name, args, return_type, location)) => {
                Ok(())
                //self.check_expr_method_call(obj, *name, args, *return_type, type_table, *location)
            }
            MIRExpr::RValue(MIRExprRValue::FunctionCall(
                function_expr,
                args,
                return_type,
                location,
                _,
            )) => Ok(()),
            MIRExpr::RValue(MIRExprRValue::StructInstantiate(ty, location)) => Ok(()),

            MIRExpr::RValue(MIRExprRValue::UnaryExpression(op, rhs, inferred_type, location)) => {
                //Type inference would not work
                //if the unary operator is not found, so we can assume it is correct.
                Ok(())
            }

            MIRExpr::RValue(MIRExprRValue::Array(expr_array, inferred_type, location)) => {
                self.check_expr_array(expr_array, *inferred_type, type_table, *location)
            }
            MIRExpr::LValue(lvalue) => self.typecheck_lvalue(lvalue, type_table),

            MIRExpr::RValue(MIRExprRValue::Ref(obj, expr_type, location)) => {
                self.check_ref_expr(&*obj, *expr_type, type_table, *location)
            }
            MIRExpr::RValue(MIRExprRValue::TypeVariable {
                type_variable,
                type_data,
                location,
            }) => {
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
                            on_element: self.on_element.clone(),
                            location: *location,
                            compiler_code_location: loc!(),
                            error: InternalError {
                                error: "Type variable type data is not TypeData, this is a type inference error.".to_string()
                            }
                        });
                        return Err(());
                    }
                }
            }
        }
    }

    fn typecheck_lvalue(
        &mut self,
        lvalue: &MIRExprLValue,
        type_table: &TypeTable,
    ) -> Result<(), CompilerError> {
        match lvalue {
            MIRExprLValue::MemberAccess(obj, member, inferred_type, location) => {
                //elf.check_expr_member_access(*obj, member, inferred_type, location)
                Ok(())
            }
            MIRExprLValue::Variable(var, inferred_type, location) => Ok(()),
            MIRExprLValue::Deref(obj, expr_type, location) => {
                self.check_deref_expr(&obj, *expr_type, type_table, *location)
            } //should be OK...
        }
    }

    //*a where a: ptr<T>, result type = T
    fn check_deref_expr(
        &mut self,
        obj: &TypecheckPendingExpression,
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
        let unify_result = unifier.unify(&ptr_type, &mono_ptr, location, &self.on_element, false);

        match unify_result {
            Err(stack) => {
                self.errors.unify_error.push(CompilerErrorContext {
                    error: UnificationError {
                        stack,
                        context: "".into(),
                    },
                    on_element: self.on_element.clone(),
                    location,
                    compiler_code_location: loc!(),
                });
                self.errors.invalid_derefed_type.push(CompilerErrorContext {
                    error: DerefOnNonPointerError {
                        attempted_type: type_table[obj.get_type()].clone(),
                    },
                    on_element: self.on_element.clone(),
                    location,
                    compiler_code_location: loc!(),
                });
                return Err(());
            }
            Ok(subs) => {
                let type_t = subs.get(&super::hir::TypeVariable("T".into()));
                if let Some(t) = type_t {
                    let unify_result =
                        unifier.unify(&derefed_type, t, location, &self.on_element, false);

                    match unify_result {
                        Ok(_) => {}
                        Err(e) => {
                            self.errors.internal_error.push(CompilerErrorContext {
                                on_element: self.on_element.clone(),
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
                        on_element: self.on_element.clone(),
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

        return Ok(());
    }

    fn check_ref_expr(
        &mut self,
        obj: &MIRExprLValue, //value to be referenced, &obj, this will be only obj and wont be ptr<?>
        expr_type: super::hir::TypeIndex, //produced type, ptr<?>
        type_table: &TypeTable,
        location: NodeIndex,
    ) -> Result<(), CompilerError> {
        let _ = self.typecheck(&MIRExpr::LValue(obj.clone()), type_table)?;
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
                                on_element: self.on_element.clone(),
                                location,
                                compiler_code_location: loc!(),
                            });
                            return Err(());
                        }
                    }
                } else {
                    self.errors.internal_error.push(CompilerErrorContext {
                        on_element: self.on_element.clone(),
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
                    on_element: self.on_element.clone(),
                    location,
                    compiler_code_location: loc!(),
                });
                self.errors.internal_error.push(CompilerErrorContext {
                    on_element: self.on_element.clone(),
                    location,
                    compiler_code_location: loc!(),
                    error: InternalError {
                        error: "Ref expression produced non-ptr<T> type".to_string(),
                    },
                });
                return Err(());
            }
        }

        return Ok(());
    }

    fn make_unifier(&mut self) -> Unifier {
        Unifier::new(self.errors, &self.type_db)
    }

    fn check_expr_array(
        &mut self,
        expr_array: &[TypecheckPendingExpression],
        inferred_type: TypeIndex,
        type_table: &TypeTable,
        location: NodeIndex,
    ) -> Result<(), CompilerError> {
        let array_type = self.type_db.common_types.array;

        let _ = expr_array
            .into_iter()
            .map(|expr| self.typecheck(expr, type_table))
            .collect::<Result<Vec<_>, _>>()?;

        let all_types = expr_array.iter().map(|x| x.get_type()).collect::<Vec<_>>();
        log!(
            "Array expression types: {:?}",
            all_types
                .iter()
                .map(|x| type_table[*x].mono.print_name(&self.type_db))
                .collect::<Vec<_>>()
        );
        let first_type = &type_table[all_types[0]];
        let cloned_self_element = self.on_element.clone();

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
                            on_element: self.on_element.clone(),
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
                        on_element: self.on_element.clone(),
                        location,
                        compiler_code_location: loc!(),
                    });

                return Err(());
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
        literal: &LiteralMIRExpr,
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
            LiteralMIRExpr::Integer(i) => {
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
                        on_element: self.on_element.clone(),
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
                        on_element: self.on_element.clone(),
                        location,
                        compiler_code_location: loc!(),
                    });
                    return Err(());
                }
            }
            LiteralMIRExpr::Float(f) => {
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
                        on_element: self.on_element.clone(),
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
                        on_element: self.on_element.clone(),
                        location,
                        compiler_code_location: loc!(),
                    });
                    return Err(());
                }
            }
            LiteralMIRExpr::String(_) | LiteralMIRExpr::Boolean(_) | LiteralMIRExpr::Char(_) => {
                true
            }
        };
        return Ok(());
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
    fn type_check_function<'scopes>(
        &mut self,
        body: &[MIRBlock],
        scopes: &'scopes [MIRScope],
        return_type: &MonoType,
        type_table: &TypeTable,
    ) -> Result<(), CompilerError> {
        let fname = self.on_element.get_name();
        log!("Type checking {fname}");
        let cloned_element = self.on_element.clone();
        for block in body {
            //let scope = block.scope;
            //let index = block.index;
            match &block.finish {
                MIRBlockFinal::If(expr, goto_true, goto_false, location) => {
                    self.check_if_statement_expression(&expr, *location, type_table)?;
                }
                MIRBlockFinal::GotoBlock(id) => {}
                MIRBlockFinal::Return(return_expr, location) => {
                    let _ = self.typecheck(&return_expr, type_table)?;
                    let expr_type = return_expr.get_type();
                    let expr_mono = &type_table[expr_type].mono;
                    let original = return_type;
                    let mut unifier = self.make_unifier();

                    let unification_result =
                        unifier.unify_strict(original, expr_mono, *location, &cloned_element);

                    match unification_result {
                        Ok(_) => {}
                        Err(e) => {
                            self.errors
                                .return_type_mismatches
                                .push(CompilerErrorContext {
                                    error: TypeMismatch {
                                        context: ReturnTypeContext(),
                                        expected: PolyType::mono(original.clone()),
                                        actual: PolyType::mono(expr_mono.clone()),
                                    },
                                    on_element: self.on_element.clone(),
                                    location: *location,
                                    compiler_code_location: loc!(),
                                });
                            return Err(());
                        }
                    }
                }
                MIRBlockFinal::EmptyReturn(location) => {
                    let MonoType::Application(ctor, _) = return_type else {
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
                                on_element: self.on_element.clone(),
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
                                on_element: self.on_element.clone(),
                                location: *location,
                                compiler_code_location: loc!(),
                            });
                        return Err(());
                    }
                }
            };
            let mut first_error = None;

            let mut new_blocks = vec![];
            for node in &block.nodes {
                match self.check_mir_block_node(&node, block.scope, scopes, type_table) {
                    Ok(_) => new_blocks.push(node),
                    Err(e) if first_error.is_none() => first_error = Some(e),
                    Err(_) => {}
                }
            }
            if let Some(e) = first_error {
                return Err(e);
            }
        }
        log!("Completed type check of {fname}");
        Ok(())
    }

    fn check_mir_block_node(
        &mut self,
        node: &MIRBlockNode,
        scope: ScopeId,
        scopes: &[MIRScope],
        type_table: &TypeTable,
    ) -> Result<(), CompilerError> {
        let mir_printer = mir_printer::MIRPrinter::new(&self.type_db);
        let mir_str = mir_printer.print_mir_block_node(&node, type_table);
        log!("Checking MIR node: {mir_str}");
        let fname = self.on_element.get_name();

        match node {
            MIRBlockNode::Assign {
                path,
                expression,
                location,
            } => {
                let path_lvalue = MIRExpr::LValue(path.clone());
                self.typecheck(&path_lvalue, type_table)?;
                self.typecheck(&expression, type_table)?;

                let expr_type = expression.get_type();

                match path {
                    MIRExprLValue::Variable(var_name, ty, _) => {
                        let vname = var_name;
                        log!("Checking ASSIGN to variable `{vname}` in {fname}");
                        //let variable_type =
                        //    self.find_variable_and_get_type(var_name, scope, scopes);

                        let mono_expr_type = &type_table[expr_type].mono;
                        let mono_ty = &type_table[ty].mono;

                        log!("Types being unified: {:?}, {:?}", mono_expr_type, mono_ty);

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
                                    on_element: self.on_element.clone(),
                                    location: *location,
                                    compiler_code_location: loc!(),
                                });
                                return Err(());
                            }
                        }
                        Ok(())
                    }
                    MIRExprLValue::MemberAccess(_, member, ty, location) => {
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
                                    on_element: self.on_element.clone(),
                                    location: *location,
                                    compiler_code_location: loc!(),
                                });
                                return Err(());
                            }
                        };
                        Ok(())
                    }
                    MIRExprLValue::Deref(ptr, ty, location) => {
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
                                    on_element: self.on_element.clone(),
                                    location: *location,
                                    compiler_code_location: loc!(),
                                });
                                return Err(());
                            }
                        };

                        Ok(())
                    }
                }
            }
            MIRBlockNode::FunctionCall {
                function,
                args,
                location,
                return_type,
            } => {
                //already checked at inference time
                Ok(())
            }
            MIRBlockNode::MethodCall {
                object,
                method_name,
                args,
                location,
                return_type,
            } => {
                //already checked at inference time
                Ok(())
            }
        }
    }

    fn check_if_statement_expression(
        &mut self,
        expr: &TypecheckPendingExpression,
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
                    on_element: self.on_element.clone(),
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
                    on_element: self.on_element.clone(),
                    location,
                    compiler_code_location: loc!(),
                });
            return Err(());
        }
    }
}

pub fn typecheck(
    top_nodes: &[MIRTopLevelNode],
    type_db: &TypeConstructorDatabase,
    errors: &mut TypeErrors,
) -> Result<(), CompilerError> {
    for node in top_nodes {
        match node {
            MIRTopLevelNode::DeclareFunction {
                struct_name,
                function_name,
                parameters,
                body,
                scopes,
                return_type,
                type_table,
            } => {
                let mut context = TypeCheckContext {
                    type_db,
                    errors,
                    on_element: RootElementType::Function(*function_name),
                };
                let mono_return_type = &type_table[*return_type].mono;
                context.type_check_function(body, &scopes, mono_return_type, type_table)?;
            }
            _ => {}
        }
    }
    Ok(())
}
