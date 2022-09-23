use super::hir::{HIRAstMetadata, HIRExpr, HIRExprMetadata, TrivialHIRExpr};
use crate::ast::parser::AST;
use crate::types::type_db::TypeDatabase;
use crate::types::type_db::TypeInstance;
use crate::types::type_errors::IfStatementNotBoolean;
use crate::types::type_errors::{AssignContext, CallToNonCallableType, FunctionCallArgumentCountMismatch, FunctionCallContext, ReturnTypeContext, TypeErrors, TypeMismatch};

use super::mir::{MIRBlock, MIRBlockFinal, MIRBlockNode, MIRScope, MIRTopLevelNode};
use super::name_registry::NameRegistry;

fn find_variable_and_get_type<'block, 'scope>(
    name: &str,
    current_block: &'block MIRBlock,
    scopes: &'scope [MIRScope],
    names: &'scope NameRegistry,
) -> &'scope TypeInstance {
    let mut current_scope = &scopes[current_block.scope.0];

    loop {
        let bound_name = current_scope.boundnames.iter().find(|x| x.name == name);
        if let Some(name_and_type) = bound_name {
            return &name_and_type.typename
        }
        if current_scope.index == 0 {
            break;
        }
        current_scope = &scopes[current_scope.index - 1];
    }

    //try find in the global scope
    return names.get_ref(name).expect_resolved();
}

fn check_function_arguments(
    on_function: &str,
    function_called: &FunctionName,
    function_parameters: &[TypeInstance],
    arguments_passed: &[TypeInstance],
    _type_db: &TypeDatabase,
    type_errors: &mut TypeErrors,
) {
    if function_parameters.len() != arguments_passed.len() {
        type_errors
            .function_call_argument_count
            .push(FunctionCallArgumentCountMismatch {
                on_function: on_function.to_string(),
                called_function_name: function_called.clone(),
                expected_count: function_parameters.len(),
                passed_count: arguments_passed.len(),
            });
        return;
    }

    let zipped = function_parameters.iter().zip(arguments_passed.iter());

    for (number, (expected, passed)) in zipped.enumerate() {
        if passed != expected {
            type_errors.function_call_mismatches.push(TypeMismatch {
                on_function: on_function.to_string(),
                expected: expected.clone(),
                actual: passed.clone(),
                context: FunctionCallContext {
                    called_function_name: function_called.clone(),
                    argument_position: number,
                },
            });
        }
    }
}

fn all_paths_return_values_of_correct_type(
    function_name: &str,
    body: &[MIRBlock],
    return_type: &TypeInstance,
    type_db: &TypeDatabase,
    errors: &mut TypeErrors,
) {
    for body_node in body {
        if let MIRBlockFinal::Return(return_expr, ..) = &body_node.finish {
            let expr_type = return_expr.get_expr_type().expect_resolved();
            if !return_type.is_compatible(return_expr.get_expr_type().expect_resolved(), type_db) {
                errors.return_type_mismatches.push(TypeMismatch {
                    context: ReturnTypeContext(),
                    on_function: function_name.to_string(),
                    expected: return_type.clone(),
                    actual: expr_type.clone(),
                });
            }
        }
        if let MIRBlockFinal::EmptyReturn = &body_node.finish {
            if return_type != &type_db.special_types.void {
                errors.return_type_mismatches.push(TypeMismatch {
                    context: ReturnTypeContext(),
                    on_function: function_name.to_string(),
                    expected: return_type.clone(),
                    actual: type_db.special_types.void.clone(),
                });
            }
        }
    }
}

fn all_assignments_correct_type(
    function_name: &str,
    body: &[MIRBlock],
    scopes: &[MIRScope],
    names: &NameRegistry,
    _type_db: &TypeDatabase,
    type_errors: &mut TypeErrors,
) {
    for body_node in body {
        for block_node in &body_node.block {
            match block_node {
                MIRBlockNode::Assign {
                    path, expression, ..
                } if path.len() == 1 => {
                    let var = path.first().unwrap();
                    //find variable
                    let variable_type = find_variable_and_get_type(var, body_node, scopes, names);
                    let expr_type = expression.get_expr_type().expect_resolved();

                    if variable_type != expr_type {
                        type_errors.assign_mismatches.push(TypeMismatch {
                            on_function: function_name.to_string(),
                            context: AssignContext {
                                target_variable_name: var.to_string(),
                            },
                            expected: variable_type.clone(),
                            actual: expr_type.clone(),
                        });
                    }
                }
                MIRBlockNode::Assign { path, .. } if path.len() != 1 => {
                    todo!("Typecheck for path assignments of length > 1 not implemented yet")
                }
                _ => {}
            }
        }
    }
}


fn if_statement_exprs_read_from_bool_variable(
    function_name: &str,
    body: &[MIRBlock],
    type_db: &TypeDatabase,
    type_errors: &mut TypeErrors,
) {
    for body_node in body {
        match &body_node.finish {
            MIRBlockFinal::If(expr, _, _, _) => {
                let expr_type = expr.1.expect_resolved();
                if expr_type != &type_db.special_types.bool {
                    type_errors.if_statement_unexpected_type.push(IfStatementNotBoolean {
                        on_function: function_name.to_string(),
                        actual_type: expr_type.clone()
                    })
                }
            },
            _ => {}
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionName {
    Function(String),
    IndexAccess,
    #[allow(dead_code)] Method {
        function_name: String,
        type_name: String,
    },
}

fn get_actual_function_name_with_details(
    function_name: &str,
    meta_ast: &HIRAstMetadata,
    meta_expr: &HIRExprMetadata,
) -> FunctionName {
    if meta_ast.is_none() && meta_expr.is_none() {
        return FunctionName::Function(function_name.to_string());
    }

    let expr = match meta_ast.as_ref() {
        Some(AST::StandaloneExpr(expr)) => expr,
        _ => meta_expr.as_ref().unwrap(),
    };

    match expr {
        crate::ast::parser::Expr::FunctionCall(function_name, _) => {
            match &**function_name {
                crate::ast::parser::Expr::Variable(str) => {
                    return FunctionName::Function(str.to_string())
                }
                crate::ast::parser::Expr::MemberAccess(_, member) => {
                    return FunctionName::Function(member.to_string())
                }
                _ => {}
            };
        }
        crate::ast::parser::Expr::IndexAccess(_, _) => return FunctionName::IndexAccess,
        _ => {}
    };

    FunctionName::Function(function_name.to_string())
}

fn function_calls_are_actually_callable_and_parameters_are_correct_type(
    body: &[MIRBlock],
    scopes: &[MIRScope],
    names: &NameRegistry,
    function_name: &str,
    type_db: &TypeDatabase,
    type_errors: &mut TypeErrors,
) {
    //
    for body_node in body {
        for block_node in &body_node.block {
            match block_node {
                MIRBlockNode::Assign {
                    path,
                    expression,
                    meta_ast,
                    meta_expr: _,
                } => {
                    assert!(path.len() <= 1, "Assign to path len > 2 not supported in type checker yet!");

                    //on assigns, we need to check if's a function call.
                    let HIRExpr::FunctionCall(call_expr, args, return_type, expr_metadata) = expression else {
                        continue; //other cases handled elsewhere
                    };
                    //if it is a function call, check that the arguments match (return type and arguments passed)
                    let TypeInstance::Function(func_args_types, func_return_type) = call_expr.1.expect_resolved() else {
                        type_errors.call_non_callable.push(CallToNonCallableType {
                            on_function: function_name.to_string(),
                            actual_type: call_expr.1.expect_resolved().clone()
                        });
                        continue;
                    };

                    let TrivialHIRExpr::Variable(called_function) = &call_expr.0 else {
                        panic!("Cannot call function that is not named: anonymous functions not supported yet");
                    };

                    assert!(!(return_type.expect_resolved() != func_return_type.as_ref()), "Return type of function is {func_return_type:#?} but expression return type is {return_type:#?}. This should not happen. This is a type inference bug, and something is inconsistent!");

                    let passed_types = args
                        .iter()
                        .map(|x| x.1.expect_resolved().clone())
                        .collect::<Vec<_>>();
                    let actual_function_name = get_actual_function_name_with_details(
                        called_function,
                        meta_ast,
                        expr_metadata,
                    );
                    check_function_arguments(
                        function_name,
                        &actual_function_name,
                        func_args_types,
                        &passed_types,
                        type_db,
                        type_errors,
                    );
                }
                MIRBlockNode::FunctionCall {
                    function,
                    args,
                    meta_ast,
                } => {
                    let function_type = find_variable_and_get_type(function, body_node, scopes, names);
                    match function_type {
                        TypeInstance::Function(argument_types, _) => {
                            let passed = args
                                .iter()
                                .map(|x| x.1.expect_resolved().clone())
                                .collect::<Vec<_>>();
                            let actual_function_name =
                                get_actual_function_name_with_details(function, meta_ast, &None);
                            check_function_arguments(
                                function_name,
                                &actual_function_name,
                                argument_types,
                                &passed,
                                type_db,
                                type_errors,
                            );
                        }
                        _ => {
                            type_errors.call_non_callable.push(CallToNonCallableType {
                                on_function: function_name.to_string(),
                                actual_type: function_type.clone(),
                            });
                        }
                    }
                }
            }
        }
    }
}

fn type_check_function(
    function_name: &str,
    body: &[MIRBlock],
    scopes: &[MIRScope],
    return_type: &TypeInstance,
    globals: &NameRegistry,
    type_db: &TypeDatabase,
    type_errors: &mut TypeErrors,
) {
    all_paths_return_values_of_correct_type(function_name, body, return_type, type_db, type_errors);
    all_assignments_correct_type(function_name, body, scopes, globals, type_db, type_errors);
    function_calls_are_actually_callable_and_parameters_are_correct_type(
        body,
        scopes,
        globals,
        function_name,
        type_db,
        type_errors,
    );
    if_statement_exprs_read_from_bool_variable(function_name, body, type_db, type_errors);
}

pub fn check_type(
    top_nodes: &[MIRTopLevelNode],
    type_db: &TypeDatabase,
    names: &NameRegistry,
) -> TypeErrors {
    let mut type_errors = TypeErrors::new();

    for node in top_nodes {
        match node {
            MIRTopLevelNode::DeclareFunction {
                function_name,
                parameters: _,
                body,
                scopes,
                return_type,
            } => {
                type_check_function(
                    function_name,
                    body,
                    scopes,
                    return_type,
                    names,
                    type_db,
                    &mut type_errors,
                );
            }
            MIRTopLevelNode::StructDeclaration { .. } => {
                todo!("Not done yet!")
            }
        }
    }
    type_errors
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        ast::parser::{Parser, AST},
        semantic::{mir_printer, mir::hir_to_mir}, types::type_errors::TypeErrorPrinter,
    };
    use pretty_assertions::assert_eq;

    pub struct TestContext {
        mir: Vec<MIRTopLevelNode>,
        database: TypeDatabase,
        globals: NameRegistry,
    }

    fn prepare(source: &str) -> TestContext {
        let tokenized = crate::ast::lexer::Tokenizer::new(source)
            .tokenize()
            .ok()
            .unwrap();
        let mut parser = Parser::new(tokenized);
        let ast = AST::Root(parser.parse_ast());
        let analysis_result = crate::semantic::analysis::do_analysis(&ast);
        let mir = hir_to_mir(&analysis_result.final_mir, &analysis_result.type_db);

        println!("{:#?}", &mir);
        println!("{}", mir_printer::print_mir(&mir, &analysis_result.type_db));

        TestContext {
            mir,
            database: analysis_result.type_db,
            globals: analysis_result.globals,
        }
    }

    //Parses a single expression
    fn run_test(ctx: &TestContext) -> (TypeErrors, &TypeDatabase) {
        let errors = check_type(&ctx.mir, &ctx.database, &ctx.globals);
        if errors.count() > 0 {
            println!("{}", TypeErrorPrinter::new(&errors, &ctx.database));
        } else {
            println!("No errors found!");
        }
        (errors, &ctx.database)
    }

    #[test]
    fn return_from_void_func_is_correct() {
        let ctx = prepare(
            "
def main():
    return
",
        );

        let (err, _) = run_test(&ctx);
        assert_eq!(0, err.count());
    }

    #[test]
    fn return_int_from_void_is_not_correct() {
        let ctx = prepare(
            "
def main():
    return 1
",
        );

        let (err, db) = run_test(&ctx);

        assert_eq!(1, err.count());
        assert_eq!(1, err.return_type_mismatches.len());
        assert_eq!(
            err.return_type_mismatches[0].expected,
            db.special_types.void
        );
        assert_eq!(
            err.return_type_mismatches[0].actual,
            TypeInstance::Simple(db.expect_find_by_name("i32").id)
        );
    }

    #[test]
    fn return_int_from_int_func_is_correct() {
        let ctx = prepare(
            "
def main() -> i32:
    return 1
",
        );
        let (err, _) = run_test(&ctx);
        assert_eq!(0, err.count());
    }

    #[test]
    fn return_void_from_int_func_is_not_correct() {
        let ctx = prepare(
            "
def main() -> i32:
    return
    ",
        );
        let (err, db) = run_test(&ctx);
        assert_eq!(1, err.count());
        assert_eq!(1, err.return_type_mismatches.len());
        assert_eq!(err.return_type_mismatches[0].actual, db.special_types.void);
        assert_eq!(
            err.return_type_mismatches[0].expected,
            TypeInstance::Simple(db.expect_find_by_name("i32").id)
        );
    }

    #[test]
    fn assign_incorrect_type_literal() {
        let ctx = prepare(
            "
def main():
    x: i32 = \"some str\"
",
        );

        let (err, db) = run_test(&ctx);
        assert_eq!(1, err.count());
        assert_eq!(1, err.assign_mismatches.len());
        assert_eq!(
            err.assign_mismatches[0].actual,
            TypeInstance::Simple(db.expect_find_by_name("str").id)
        );
        assert_eq!(
            err.assign_mismatches[0].expected,
            TypeInstance::Simple(db.expect_find_by_name("i32").id)
        );
    }

    #[test]
    fn type_check_function_call_no_args_correct_types() {
        let ctx = prepare(
            "
def test() -> i32:
    return 1

def main():
    x: i32 = test()
",
        );
        let (err, _) = run_test(&ctx);
        assert_eq!(0, err.count());
    }

    #[test]
    fn type_check_wrong_type_function_call_return_incompatible() {
        let ctx = prepare(
            "
def test() -> i32:
    return 1

def main():
    x: f32 = test()
",
        );

        let (err, db) = run_test(&ctx);

        assert_eq!(1, err.count());
        assert_eq!(1, err.assign_mismatches.len());
        assert_eq!(
            err.assign_mismatches[0].actual,
            TypeInstance::Simple(db.expect_find_by_name("i32").id)
        );
        assert_eq!(
            err.assign_mismatches[0].expected,
            TypeInstance::Simple(db.expect_find_by_name("f32").id)
        );
    }

    #[test]
    fn type_check_binary_expr_result_wrong_type() {
        let ctx = prepare(
            "
def test() -> i32:
    return 1

def main():
    x: f32 = test() + 1
",
        );

        let (err, db) = run_test(&ctx);

        assert_eq!(1, err.count());
        assert_eq!(1, err.assign_mismatches.len());
        assert_eq!(
            err.assign_mismatches[0].actual,
            TypeInstance::Simple(db.expect_find_by_name("i32").id)
        );
        assert_eq!(
            err.assign_mismatches[0].expected,
            TypeInstance::Simple(db.expect_find_by_name("f32").id)
        );
    }

    #[test]
    fn pass_correct_type_to_function_single_args() {
        let ctx = prepare(
            "
def test(i: i32) -> i32:
    return i + 1

def main():
    test(1)
",
        );

        let (err, _db) = run_test(&ctx);

        assert_eq!(0, err.count());
    }

    #[test]
    fn pass_correct_type_to_function_two_args() {
        let ctx = prepare(
            "
def test(i: i32, f: f32) -> i32:
    return i + 1

def main():
    test(1, 1.0)
",
        );
        let (err, _db) = run_test(&ctx);
        assert_eq!(0, err.count());
    }

    #[test]
    fn pass_correct_type_to_function_two_args_from_vars() {
        let ctx = prepare(
            "
def test(i: i32, f: f32) -> i32:
    return i + 1

def main():
    i = 1
    f = 1.0
    test(i, f)
",
        );

        let (err, _db) = run_test(&ctx);

        assert_eq!(0, err.count());
    }

    #[test]
    fn pass_wrong_type_to_function_single_arg() {
        let ctx = prepare(
            "
def test(i: i32) -> i32:
    return i

def main():
    s = \"abc\"
    test(s)
",
        );

        let (err, db) = run_test(&ctx);

        assert_eq!(1, err.count());
        assert_eq!(1, err.function_call_mismatches.len());
        assert_eq!(
            err.function_call_mismatches[0].actual,
            TypeInstance::Simple(db.expect_find_by_name("str").id)
        );
        assert_eq!(
            err.function_call_mismatches[0].expected,
            TypeInstance::Simple(db.expect_find_by_name("i32").id)
        );
    }

    #[test]
    fn pass_wrong_type_to_function_two_args_both_wrong() {
        let ctx = prepare(
            "
def test(i: i32, f: f32) -> f32:
    return f

def main():
    s = \"abc\"
    i = 100
    test(s, i)
",
        );

        let (err, db) = run_test(&ctx);

        assert_eq!(2, err.count());
        assert_eq!(2, err.function_call_mismatches.len());
        assert_eq!(
            err.function_call_mismatches[0].actual,
            TypeInstance::Simple(db.expect_find_by_name("str").id)
        );
        assert_eq!(
            err.function_call_mismatches[0].expected,
            TypeInstance::Simple(db.expect_find_by_name("i32").id)
        );

        assert_eq!(
            err.function_call_mismatches[1].actual,
            TypeInstance::Simple(db.expect_find_by_name("i32").id)
        );
        assert_eq!(
            err.function_call_mismatches[1].expected,
            TypeInstance::Simple(db.expect_find_by_name("f32").id)
        );
    }

    #[test]
    fn assign_incorrect_type_literal_errormsg() {
        let ctx = prepare(
            "
def main():
    x: i32 = \"some str\"
",
        );
        let (err, db) = run_test(&ctx);
        let printer = TypeErrorPrinter::new(&err, db);
        let error_msg = format!("{}", printer);
        let expected = "Assigned type mismatch: In function main, assignment to variable x: variable has type i32 but got assigned a value of type str\n";
        assert_eq!(error_msg, expected);
    }

    #[test]
    fn assign_to_variable_wrong_type_after_declaration() {
        let ctx = prepare(
            "
def main():
    x: i32 = 1
    x = \"abc\"
",
        );
        let (err, db) = run_test(&ctx);
        let printer = TypeErrorPrinter::new(&err, db);
        let error_msg = format!("{}", printer);
        let expected = "Assigned type mismatch: In function main, assignment to variable x: variable has type i32 but got assigned a value of type str\n";
        assert_eq!(error_msg, expected);
    }

    #[test]
    fn args_array_string_error_on_index_operator_refers_to_index_accessor() {
        let ctx = prepare(
            "
def main(args: array<str>):
    i : str = args[\"lol\"]
",
        );
        let (err, db) = run_test(&ctx);
        let printer = TypeErrorPrinter::new(&err, db);
        let error_msg = format!("{}", printer);
        let expected = "Function argument type mismatch: In function main, on index operator, parameter on position 0 has incorrect type: Expected u32 but passed str\n";
        assert_eq!(error_msg, expected);
    }
}
