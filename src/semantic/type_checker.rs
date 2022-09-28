use super::hir::{HIRAstMetadata, HIRExpr, HIRExprMetadata, TrivialHIRExpr};
use crate::ast::parser::{AST, Expr};
use crate::types::type_errors::IfStatementNotBoolean;
use crate::types::type_errors::{AssignContext, CallToNonCallableType, FunctionCallArgumentCountMismatch, FunctionCallContext, ReturnTypeContext, TypeErrors, TypeMismatch};
use crate::types::type_instance_db::{TypeInstanceId, TypeInstanceManager};

use super::mir::{MIRBlock, MIRBlockFinal, MIRBlockNode, MIRScope, MIRTopLevelNode};
use super::name_registry::NameRegistry;

fn find_variable_and_get_type(
    name: &str,
    current_block: &MIRBlock,
    scopes: &[MIRScope],
    names: &NameRegistry,
) -> TypeInstanceId {
    let mut current_scope = &scopes[current_block.scope.0];

    loop {
        let bound_name = current_scope.boundnames.iter().find(|x| x.name == name);
        if let Some(name_and_type) = bound_name {
            return name_and_type.typename
        }
        if current_scope.index == 0 {
            break;
        }
        current_scope = &scopes[current_scope.index - 1];
    }

    //try find in the global scope
    return *names.get(name).unwrap();
}

fn check_function_arguments_match_param_types(
    on_function: &str,
    function_called: &FunctionName,
    function_parameters: &[TypeInstanceId],
    arguments_passed: &[TypeInstanceId],
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

    for (number, (parameter_type, argument_type)) in zipped.enumerate() {
        if argument_type != parameter_type {
            type_errors.function_call_mismatches.push(TypeMismatch {
                on_function: on_function.to_string(),
                expected: *parameter_type,
                actual: *argument_type,
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
    return_type: TypeInstanceId,
    type_db: &TypeInstanceManager,
    errors: &mut TypeErrors,
) {
    for body_node in body {
        if let MIRBlockFinal::Return(return_expr, ..) = &body_node.finish {
            let expr_type = return_expr.get_type();
            if return_type != return_expr.get_type() {
                errors.return_type_mismatches.push(TypeMismatch {
                    context: ReturnTypeContext(),
                    on_function: function_name.to_string(),
                    expected: return_type,
                    actual: expr_type,
                });
            }
        }
        if let MIRBlockFinal::EmptyReturn = &body_node.finish {
            if return_type != type_db.common_types.void {
                errors.return_type_mismatches.push(TypeMismatch {
                    context: ReturnTypeContext(),
                    on_function: function_name.to_string(),
                    expected: return_type,
                    actual: type_db.common_types.void,
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
                    let expr_type = expression.get_type();

                    if variable_type != expr_type {
                        type_errors.assign_mismatches.push(TypeMismatch {
                            on_function: function_name.to_string(),
                            context: AssignContext {
                                target_variable_name: var.to_string(),
                            },
                            expected: variable_type,
                            actual: expr_type,
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
    type_db: &TypeInstanceManager,
    type_errors: &mut TypeErrors,
) {
    for body_node in body {
        if let MIRBlockFinal::If(expr, _, _, _) = &body_node.finish {
            let expr_type = expr.get_type();
            if expr_type != type_db.common_types.bool {
                type_errors.if_statement_unexpected_type.push(IfStatementNotBoolean {
                    on_function: function_name.to_string(),
                    actual_type: expr_type
                });
            }
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
   //@TODO return FunctionName method variant
    //Prioritize AST standalone expr
    let expr = match meta_ast {
        AST::StandaloneExpr(expr) => expr,
        _ => meta_expr,
    };

    match expr {
        Expr::FunctionCall(function_name, _) => {
            match &**function_name {
                Expr::Variable(str) => {
                    return FunctionName::Function(str.to_string())
                }
                Expr::MemberAccess(_, member) => {
                    return FunctionName::Function(member.to_string())
                }
                _ => {}
            };
        }
        Expr::IndexAccess(_, _) => return FunctionName::IndexAccess,
        _ => {}
    };

    FunctionName::Function(function_name.to_string())
}

fn function_calls_are_actually_callable_and_parameters_are_correct_type(
    body: &[MIRBlock],
    scopes: &[MIRScope],
    names: &NameRegistry,
    function_name: &str,
    type_db: &TypeInstanceManager,
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
                    let HIRExpr::FunctionCall(call_expr, args, _return_type, expr_metadata) = expression else {
                        continue; //other cases handled elsewhere
                    };

                    let type_id =  call_expr.get_type();
                    let type_data = type_db.get_instance(type_id);
                    if !type_data.is_function {
                        type_errors.call_non_callable.push(CallToNonCallableType {
                            on_function: function_name.to_string(),
                            actual_type: call_expr.get_type()
                        });
                        continue;
                    }
                  
                    let HIRExpr::Trivial(TrivialHIRExpr::Variable(called_function), ..) = &**call_expr else {
                        panic!("Cannot call function that is not named: anonymous functions not supported yet")
                    };

                    let passed_types = args
                        .iter()
                        .map(HIRExpr::<TypeInstanceId>::get_type)
                        .collect::<Vec<_>>();
                    let actual_function_name = get_actual_function_name_with_details(
                        called_function,
                        meta_ast.into(),
                        expr_metadata.into(),
                    );
                    check_function_arguments_match_param_types(
                        function_name,
                        &actual_function_name,
                        &type_data.function_args,
                        &passed_types,
                        type_errors,
                    );
                }
                MIRBlockNode::FunctionCall {
                    function,
                    args,
                    meta_ast,
                    meta_expr
                } => {
                    let function_type = find_variable_and_get_type(function, body_node, scopes, names);
                    let type_data = type_db.get_instance(function_type);
                   
                    if !type_data.is_function {
                        type_errors.call_non_callable.push(CallToNonCallableType {
                            on_function: function_name.to_string(),
                            actual_type: function_type,
                        });
                        continue;
                    }
                    
                    let passed = args
                        .iter()
                        .map(HIRExpr::<TypeInstanceId>::get_type)
                        .collect::<Vec<_>>();

                    let actual_function_name =
                        get_actual_function_name_with_details(function, meta_ast.into(), meta_expr.into());
                        
                    check_function_arguments_match_param_types(
                        function_name,
                        &actual_function_name,
                        &type_data.function_args,
                        &passed,
                        type_errors
                    );
                }
            }
        }
    }
}

fn methods_receive_parameters_of_correct_type(
    body: &[MIRBlock],
    function_name: &str,
    type_db: &TypeInstanceManager,
    type_errors: &mut TypeErrors,
) {
    //
    for body_node in body {
        for block_node in &body_node.block {
            if let MIRBlockNode::Assign {
                                path,
                                expression,
                                meta_ast,
                                meta_expr: _,
                            } = block_node {
                assert!(path.len() <= 1, "Assign to path len > 2 not supported in type checker yet!");

                //on assigns, we need to check if's a function call.
                let HIRExpr::MethodCall(object_expr, method, args, _, expr_metadata) = expression else {
                    continue; //other cases handled elsewhere
                };

                let object_expr_type = object_expr.get_type(); 
                let object_type_data = type_db.get_instance(object_expr_type);

                let method = object_type_data.methods.iter().find(|m| &m.name == method).unwrap();

                let method_function_type_data = type_db.get_instance(method.function_type);

                let passed_types = args
                    .iter()
                    .map(HIRExpr::<TypeInstanceId>::get_type)
                    .collect::<Vec<_>>();

                let actual_function_name = get_actual_function_name_with_details(
                    &method_function_type_data.name,
                    meta_ast.into(),
                    expr_metadata.into(),
                );

                check_function_arguments_match_param_types(
                    function_name,
                    &actual_function_name,
                    &method_function_type_data.function_args,
                    &passed_types,
                    type_errors,
                );
            }
        }
    }
}


fn type_check_function(
    function_name: &str,
    body: &[MIRBlock],
    scopes: &[MIRScope],
    return_type: TypeInstanceId,
    globals: &NameRegistry,
    type_db: &TypeInstanceManager,
    type_errors: &mut TypeErrors,
) {
    all_paths_return_values_of_correct_type(function_name, body, return_type, type_db, type_errors);
    all_assignments_correct_type(function_name, body, scopes, globals, type_errors);
    function_calls_are_actually_callable_and_parameters_are_correct_type(
        body,
        scopes,
        globals,
        function_name,
        type_db,
        type_errors,
    );
    methods_receive_parameters_of_correct_type(
        body,
        function_name,
        type_db,
        type_errors,
    );
    
    if_statement_exprs_read_from_bool_variable(function_name, body, type_db, type_errors);
}

pub fn check_type(
    top_nodes: &[MIRTopLevelNode],
    type_db: &TypeInstanceManager,
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
                    *return_type,
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
        database: TypeInstanceManager,
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
        let mir = hir_to_mir(&analysis_result.hir);

        println!("{}", mir_printer::print_mir(&mir, &analysis_result.type_db));

        TestContext {
            mir,
            database: analysis_result.type_db,
            globals: analysis_result.globals,
        }
    }

    //Parses a single expression
    fn run_test(ctx: &TestContext) -> (TypeErrors, &TypeInstanceManager) {
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
            db.common_types.void
        );
        assert_eq!(
            err.return_type_mismatches[0].actual,
            db.common_types.i32
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
        assert_eq!(err.return_type_mismatches[0].actual, db.common_types.void);
        assert_eq!(
            err.return_type_mismatches[0].expected,
            db.common_types.i32
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
            db.common_types.string
        );
        assert_eq!(
            err.assign_mismatches[0].expected,
            db.common_types.i32
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
            db.common_types.i32
        );
        assert_eq!(
            err.assign_mismatches[0].expected,
            db.common_types.f32
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
            db.common_types.i32
        );
        assert_eq!(
            err.assign_mismatches[0].expected,
            db.common_types.f32
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
            db.common_types.string
        );
        assert_eq!(
            err.function_call_mismatches[0].expected,
            db.common_types.i32
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
            db.common_types.string
        );
        assert_eq!(
            err.function_call_mismatches[0].expected,
            db.common_types.i32
        );

        assert_eq!(
            err.function_call_mismatches[1].actual,
            db.common_types.i32
        );
        assert_eq!(
            err.function_call_mismatches[1].expected,
            db.common_types.f32
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
