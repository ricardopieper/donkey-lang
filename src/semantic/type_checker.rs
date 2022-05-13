use std::fmt::Display;

use super::hir::*;

use super::mir::*;
use super::name_registry::NameRegistry;
use super::type_db::TypeDatabase;

pub trait TypeErrorDisplay {
    fn fmt_err(&self, type_db: &TypeDatabase, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

pub struct TypeMismatch<TContext> {
    pub on_function: String,
    pub context: TContext,
    pub expected: TypeInstance,
    pub actual: TypeInstance,
}

pub struct AssignContext {
    pub target_variable_name: String,
}

impl TypeErrorDisplay for TypeMismatch<AssignContext> {
    fn fmt_err(&self, type_db: &TypeDatabase, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let var_type_str = self.expected.as_string(type_db);
        let expr_type_str = self.actual.as_string(type_db);

        write!(f,  "Assigned type mismatch: In function {on_function}, assignment to variable {var}: variable has type {var_type_str} but got assigned a value of type {expr_type_str}",
            on_function = self.on_function,
            var = self.context.target_variable_name
        )
    }
}

pub struct ReturnTypeContext();

impl TypeErrorDisplay for TypeMismatch<ReturnTypeContext> {
    fn fmt_err(&self, type_db: &TypeDatabase, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let passed_name = self.actual.as_string(type_db);
        let expected_name = self.expected.as_string(type_db);
        write!(f,  "Return type mismatch: Function {on_function} returns {return_type_name} but expression returns {expr_return_type_name}",
            on_function = self.on_function,
            return_type_name = expected_name,
            expr_return_type_name = passed_name,
        )
    }
}

pub struct FunctionCallContext {
    pub called_function_name: String,
    pub argument_position: usize,
}

impl TypeErrorDisplay for TypeMismatch<FunctionCallContext> {
    fn fmt_err(&self, type_db: &TypeDatabase, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let passed_name = self.actual.as_string(type_db);
        let expected_name = self.expected.as_string(type_db);
        write!(f,  "Function argument type mismatch: In function {on_function}, call to function {function_called} parameter on position {position} has incorrect type: Expected {expected_name} but passed {passed_name}",
            on_function = self.on_function,
            function_called = self.context.called_function_name,
            position = self.context.argument_position
        )
    }
}

pub struct FunctionCallArgumentCountMismatch {
    pub on_function: String,
    pub called_function_name: String,
    pub expected_count: usize,
    pub passed_count: usize,
}

impl TypeErrorDisplay for FunctionCallArgumentCountMismatch {
    fn fmt_err(&self, type_db: &TypeDatabase, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,  "Argument count mismatch: In function {on_function}, call to function {function_called} expects {expected_args} arguments, but {passed_args} were passed",
            on_function = self.on_function,
            function_called = self.called_function_name,
            expected_args = self.expected_count,
            passed_args = self.passed_count,
        )
    }
}

pub struct CallToNonCallableType {
    pub on_function: String,
    pub actual_type: TypeInstance,
}

impl TypeErrorDisplay for CallToNonCallableType {
    fn fmt_err(&self, type_db: &TypeDatabase, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "In function {on_function}, call to non-callable type {non_callable_type_name}",
            on_function = self.on_function,
            non_callable_type_name = self.actual_type.as_string(type_db),
        )
    }
}

pub struct TypeErrors {
    pub assign_mismatches: Vec<TypeMismatch<AssignContext>>,
    pub return_type_mismatches: Vec<TypeMismatch<ReturnTypeContext>>,
    pub function_call_mismatches: Vec<TypeMismatch<FunctionCallContext>>,
    pub function_call_argument_count: Vec<FunctionCallArgumentCountMismatch>,
    pub call_non_callable: Vec<CallToNonCallableType>,
}

pub struct TypeErrorPrinter<'errors, 'type_db> {
    pub errors: &'errors TypeErrors,
    pub type_db: &'type_db TypeDatabase,
}

impl<'errors, 'type_db> TypeErrorPrinter<'errors, 'type_db> {
    pub fn new(
        errors: &'errors TypeErrors,
        type_db: &'type_db TypeDatabase,
    ) -> TypeErrorPrinter<'errors, 'type_db> {
        TypeErrorPrinter { errors, type_db }
    }
}
impl TypeErrors {
    pub fn new() -> TypeErrors {
        TypeErrors {
            assign_mismatches: vec![],
            return_type_mismatches: vec![],
            function_call_mismatches: vec![],
            function_call_argument_count: vec![],
            call_non_callable: vec![],
        }
    }

    pub fn count(&self) -> usize {
        self.assign_mismatches.len()
            + self.return_type_mismatches.len()
            + self.function_call_mismatches.len()
            + self.function_call_argument_count.len()
            + self.call_non_callable.len()
    }
}

impl<'errors, 'type_db> Display for TypeErrorPrinter<'errors, 'type_db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.errors.count() == 0 {
            return Ok(());
        }

        for err in &self.errors.assign_mismatches {
            err.fmt_err(self.type_db, f)?;
            write!(f, "\n")?;
        }
        for err in &self.errors.return_type_mismatches {
            err.fmt_err(self.type_db, f)?;
            write!(f, "\n")?;
        }
        for err in &self.errors.function_call_mismatches {
            err.fmt_err(self.type_db, f)?;
            write!(f, "\n")?;
        }
        for err in &self.errors.function_call_argument_count {
            err.fmt_err(self.type_db, f)?;
            write!(f, "\n")?;
        }
        for err in &self.errors.call_non_callable {
            err.fmt_err(self.type_db, f)?;
            write!(f, "\n")?;
        }

        return Ok(());
    }
}

fn find_variable<'block, 'scope>(
    name: &str,
    current_block: &'block MIRBlock,
    scopes: &'scope [MIRScope],
    names: &'scope NameRegistry,
) -> Option<&'scope TypeInstance> {
    let mut current_scope = &scopes[current_block.scope.0];

    loop {
        let bound_name = current_scope.boundnames.iter().find(|x| x.name == name);
        match bound_name {
            Some(name_and_type) => return Some(&name_and_type.typename),
            None => {
                if current_scope.index == 0 {
                    break;
                } else {
                    current_scope = &scopes[current_scope.index - 1];
                }
            }
        };
    }

    //try find in the global scope
    return Some(names.get_ref(name).expect_resolved());
}

fn expect_find_variable<'block, 'scope>(
    name: &str,
    current_block: &'block MIRBlock,
    scopes: &'scope [MIRScope],
    names: &'scope NameRegistry,
) -> &'scope TypeInstance {
    let variable = find_variable(name, current_block, scopes, names);
    match variable {
        Some(x) => return x,
        None => panic!("Variable not found: {name}"),
    }
}

fn check_function_arguments(
    on_function: &str,
    function_called: &str,
    function_parameters: &[TypeInstance],
    arguments_passed: &[TypeInstance],
    type_db: &TypeDatabase,
    type_errors: &mut TypeErrors,
) {
    if function_parameters.len() != arguments_passed.len() {
        type_errors
            .function_call_argument_count
            .push(FunctionCallArgumentCountMismatch {
                on_function: on_function.to_string(),
                called_function_name: function_called.to_string(),
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
                    called_function_name: function_called.to_string(),
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
        if let MIRBlockFinal::Return(return_expr) = &body_node.finish {
            let expr_type = return_expr.get_expr_type().expect_resolved();
            if !return_type.is_compatible(&return_expr.get_expr_type().expect_resolved(), type_db) {
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
    type_db: &TypeDatabase,
    type_errors: &mut TypeErrors,
) {
    for body_node in body {
        for block_node in body_node.block.iter() {
            match block_node {
                MIRBlockNode::Assign { path, expression } if path.len() == 1 => {
                    let var = path.first().unwrap();
                    //find variable
                    let variable_type = find_variable(var, body_node, scopes, names);
                    match variable_type {
                        Some(variable_found_type) => {
                            let expr_type = expression.get_expr_type().expect_resolved();

                            if variable_found_type != expr_type {
                                type_errors.assign_mismatches.push(TypeMismatch {
                                    on_function: function_name.to_string(),
                                    context: AssignContext {
                                        target_variable_name: var.to_string(),
                                    },
                                    expected: variable_found_type.clone(),
                                    actual: expr_type.clone(),
                                });
                            }
                        }
                        None => {
                            panic!("Variable not found: {var}")
                        }
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
        for block_node in body_node.block.iter() {
            match block_node {
                MIRBlockNode::Assign { path, expression } => {
                    if path.len() > 1 {
                        panic!("Assign to path len > 2 not supported in type checker yet!");
                    }

                    //on assigns, we need to check if's a function call.
                    let HIRExpr::FunctionCall(call_expr, args, return_type) = expression else {
                        continue; //handled elsewhere
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

                    if return_type.expect_resolved() != func_return_type.as_ref() {
                        panic!("Return type of function is {func_return_type:#?} but expression return type is {return_type:#?}. This should not happen. This is a type inference bug, and something is inconsistent!");
                    }

                    let variable_type =
                        expect_find_variable(path[0].as_str(), body_node, scopes, names);

                    if func_return_type.as_ref() != variable_type {
                        type_errors.assign_mismatches.push(TypeMismatch {
                            on_function: function_name.to_string(),
                            context: AssignContext {
                                target_variable_name: path[0].to_string(),
                            },
                            expected: variable_type.clone(),
                            actual: *func_return_type.clone(),
                        });
                    }

                    let passed_types = args
                        .iter()
                        .map(|x| x.1.expect_resolved().clone())
                        .collect::<Vec<_>>();

                    check_function_arguments(
                        function_name,
                        &called_function,
                        &func_args_types,
                        &passed_types,
                        type_db,
                        type_errors,
                    );
                }
                MIRBlockNode::FunctionCall { function, args } => {
                    let function_type = expect_find_variable(function, body_node, scopes, names);
                    match function_type {
                        TypeInstance::Function(argument_types, _) => {
                            let passed = args
                                .iter()
                                .map(|x| x.1.expect_resolved().clone())
                                .collect::<Vec<_>>();

                            check_function_arguments(
                                function_name,
                                &function,
                                &argument_types,
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
}

pub fn check_type<'type_db>(
    top_nodes: &[MIRTopLevelNode],
    type_db: &'type_db TypeDatabase,
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
    return type_errors;
}

#[cfg(test)]
mod tests {

    use crate::ast::parser::{Parser, AST};
    use super::*;
    use pretty_assertions::assert_eq;

    //Parses a single expression
    fn run_test(source: &str) -> (TypeErrors, TypeDatabase) {
        let tokenized = crate::ast::lexer::Tokenizer::new(source)
            .tokenize()
            .ok()
            .unwrap();
        let mut parser = Parser::new(tokenized);
        let ast = AST::Root(parser.parse_ast().ok().unwrap());
        let analysis_result = crate::semantic::analysis::do_analysis(&ast);
        let mir = hir_to_mir(&analysis_result.final_mir, &analysis_result.type_db);
        let errors = check_type(&mir, &analysis_result.type_db, &analysis_result.globals);
        if errors.count() > 0 {
            println!(
                "{}",
                TypeErrorPrinter::new(&errors, &analysis_result.type_db)
            );
        } else {
            println!("No errors found!");
        }
        return (errors, analysis_result.type_db);
    }

    #[test]
    fn return_from_void_func_is_correct() {
        let (err, _) = run_test(
            "
def main():
    return
",
        );
        assert_eq!(0, err.count());
    }

    #[test]
    fn return_int_from_void_is_not_correct() {
        let (err, db) = run_test(
            "
def main():
    return 1
",
        );
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
        let (err, _) = run_test(
            "
def main() -> i32:
    return 1
",
        );
        assert_eq!(0, err.count());
    }

    #[test]
    fn return_void_from_int_func_is_not_correct() {
        let (err, db) = run_test(
            "
def main() -> i32:
    return
",
        );
        assert_eq!(1, err.count());
        assert_eq!(1, err.return_type_mismatches.len());
        assert_eq!(
            err.return_type_mismatches[0].actual,
            db.special_types.void
        );
        assert_eq!(
            err.return_type_mismatches[0].expected,
            TypeInstance::Simple(db.expect_find_by_name("i32").id)
        );
    }

    #[test]
    fn assign_incorrect_type_literal() {
        let (err, db) = run_test(
            "
def main():
    x: i32 = \"some str\"
",
        );
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
    fn assign_incorrect_type_literal_errormsg() {
        let (err, db) = run_test(
            "
def main():
    x: i32 = \"some str\"
",
        );
        let printer = TypeErrorPrinter::new(&err, &db);
        let error_msg = format!("{}", printer);
        let expected = "Assigned type mismatch: In function main, assignment to variable x: variable has type i32 but got assigned a value of type str\n";
        assert_eq!(error_msg, expected);
        
    }
}
