use crate::semantic::hir::{HIR, ast_to_hir};
use crate::semantic::{first_assignments, name_registry, type_inference, undeclared_vars};
use crate::types::type_db::TypeDatabase;
use crate::{ast::parser::{AST}, types::type_errors::TypeErrors};

use super::name_registry::NameRegistry;

pub struct AnalysisResult {
    pub initial_mir: Vec<HIR>,
    pub after_make_declarations_mir: Vec<HIR>,
    pub final_mir: Vec<HIR>,
    pub type_db: TypeDatabase,
    pub globals: NameRegistry,
    pub type_errors: TypeErrors,
}

pub fn do_analysis(ast: &AST) -> AnalysisResult {
    let mut hir = vec![];
    ast_to_hir(ast, 0, &mut hir);

    let initial_mir = hir.clone();
    let type_db = TypeDatabase::new();

    let mut globals = name_registry::build_name_registry(&type_db, &hir);

    hir = first_assignments::transform_first_assignment_into_declaration(hir);
    let after_make_declarations_mir = hir.clone();
    undeclared_vars::detect_undeclared_vars_and_redeclarations(&globals, &hir);

    //println!("Before type inference:\n{}", print_hir(&hir, &type_db));

    let mut errors = TypeErrors::new();

    hir = type_inference::infer_types(&mut globals, &type_db, hir, &mut errors);

    AnalysisResult {
        initial_mir,
        after_make_declarations_mir,
        final_mir: hir,
        type_db,
        globals,
        type_errors: errors,
    }
}

#[cfg(test)]
mod tests {

    #[cfg(test)]
    use pretty_assertions::assert_eq;

    use crate::{ast::{lexer::Operator, parser::Parser}, semantic::hir_printer};

    use super::*;

    //Parses a single expression
    fn hir(source: &str) -> AnalysisResult {
        let tokenized = crate::ast::lexer::Tokenizer::new(source)
            .tokenize()
            .ok()
            .unwrap();
        let mut parser = Parser::new(tokenized);
        let ast = AST::Root(parser.parse_ast().ok().unwrap());
        do_analysis(&ast)
    }

    #[test]
    fn simple_assign_decl() {
        let analyzed = hir("
def my_function():
    x = 1");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);

        let expected = "
def my_function() -> Void:
    x : i32 = 1";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn standalone_call_to_builtin_function() {
        let analyzed = hir("
def my_function() -> i32:
    x: f64 = 1.0
    pow(x)");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", result);
        //1 + 2 / 3.2 + func(1, 5);
        /*
        $0 : i32 = 1 + 2
        $1 : i32 = func(1, 5)
        $2 : f32 = 3.2 + $1
        $3 : <erro> = $0 + $2


        */
        let expected = "
def my_function() -> i32:
    x : f64 = 1.0
    pow(x)";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn expr_call_to_builtin_function() {
        let analyzed = hir("
def my_function() -> i32:
    x: f64 = sqrt(16.0)
");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", result);

        let expected = "
def my_function() -> i32:
    x : f64 = sqrt(16.0)";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn alternative_test() {
        let analyzed = hir("
def my_function(arg1: i32, arg2: i32) -> i32:
    return arg1 * arg2 / (arg2 - arg1)");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);

        let expected = "
def my_function(arg1: i32, arg2: i32) -> i32:
    $0 : i32 = arg1 * arg2
    $1 : i32 = arg2 - arg1
    return $0 / $1";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn default_void_return() {
        let analyzed = hir("
def main(args: array<str>):
    print(10)");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);

        let expected = "
def main(args: array<str>) -> Void:
    print(10)";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn infer_variable_type_as_int() {
        let analyzed = hir("
def main(args: array<str>):
    my_var = 10
    print(my_var)");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);

        let expected = "
def main(args: array<str>) -> Void:
    my_var : i32 = 10
    print(my_var)";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn infer_generic_type_as_str() {
        let analyzed = hir("
def main(args: array<str>):
    my_var = args[0]
    print(my_var)");

        let final_result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", final_result);
        let expected = "
def main(args: array<str>) -> Void:
    $0 : fn (u32) -> str = args.__index__
    my_var : str = $0(0)
    print(my_var)";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn infer_builtin_function_return_type() {
        let analyzed = hir("
def my_function() -> i32:
    x =  1.3 + ((sqrt_f32(16.0 / 4.0 + 2.0 * 2.1) / 2.0) * 4.0) + (3.0 * pow_f32(2.0, 2.0))
");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", result);

        let expected = "
def my_function() -> i32:
    $0 : f32 = 16.0 / 4.0
    $1 : f32 = 2.0 * 2.1
    $2 : f32 = $0 + $1
    $3 : f32 = sqrt_f32($2)
    $4 : f32 = $3 / 2.0
    $5 : f32 = $4 * 4.0
    $6 : f32 = 1.3 + $5
    $7 : f32 = pow_f32(2.0, 2.0)
    $8 : f32 = 3.0 * $7
    x : f32 = $6 + $8
";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn infer_defined_function_return_type() {
        let analyzed = hir("
def sum(x: i32, y: i32) -> i32:
    return x + y

def main():
    my_var = sum(1, 2)
    print(my_var)");

        let final_result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", final_result);
        let expected = "
def sum(x: i32, y: i32) -> i32:
    return x + y
def main() -> Void:
    my_var : i32 = sum(1, 2)
    print(my_var)";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn infer_defined_function_generic_param() {
        let analyzed = hir("
def id(x: array<str>) -> str:
    return x[0]

def main():
    my_var = id(1)
    print(my_var)");

        let final_result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", final_result);
        let expected = "
def id(x: array<str>) -> str:
    $0 : fn (u32) -> str = x.__index__
    return $0(0)
def main() -> Void:
    my_var : str = id(1)
    print(my_var)";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn semi_first_class_functions() {
        let analyzed = hir("
def id(x: array<str>) -> str:
    return x[0]

def main():
    my_func = id
    my_var = my_func(1)
    print(my_var)");

        let final_result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", final_result);
        let expected = "
def id(x: array<str>) -> str:
    $0 : fn (u32) -> str = x.__index__
    return $0(0)
def main() -> Void:
    my_func : fn (array<str>) -> str = id
    my_var : str = my_func(1)
    print(my_var)";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn access_property_of_struct_and_infer_type() {
        let analyzed = hir("
def main():
    my_array = [1, 2, 3]
    my_array_length = my_array.length
    print(my_array_length)");

        let final_result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", final_result);
        let expected = "
def main() -> Void:
    my_array : array<i32> = [1, 2, 3]
    my_array_length : u32 = my_array.length
    print(my_array_length)";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn return_expr() {
        let analyzed = hir("
def main(x: i32) -> i32:
    y = 0
    return x + y
");

        let final_result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", final_result);
        let expected = "
def main(x: i32) -> i32:
    y : i32 = 0
    return x + y";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn self_decl_read() {
        let result = std::panic::catch_unwind(|| {
            hir("
def main(x: i32) -> i32:
    y = y + 1
");
        });
        let err = result.unwrap_err();
        let as_str = err.downcast_ref::<String>().unwrap();
        assert_eq!(as_str, "Could not find a name for y");
    }

    #[test]
    fn self_decl_read_expr() {
        let result = std::panic::catch_unwind(|| {
            hir("
def main(x: i32) -> i32:
    a = 1
    b = 2
    y = (a + b * (x / y)) / 2
");
        });
        let err = result.unwrap_err();
        let as_str = err.downcast_ref::<String>().unwrap();
        assert_eq!(as_str, "Variable y not found, function: main");
    }

    #[test]
    fn if_return_both_branches() {
        let analyzed = hir("
def main(x: i32) -> i32:
    if x == 0:
        return 1
    else:
        return 2
");

        let final_result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", final_result);
        let expected = "
def main(x: i32) -> i32:
    $0 : bool = x == 0
    if $0:
        return 1
    else:
        return 2";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_more_branches() {
        let analyzed = hir("
def main(x: i32) -> i32:
    if x == 0:
        return 1
    else:
        if x == 2:
            return 2
        else:
            return x
");

        let final_result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", final_result);
        let expected = "
def main(x: i32) -> i32:
    $0 : bool = x == 0
    if $0:
        return 1
    else:
        $1 : bool = x == 2
        if $1:
            return 2
        else:
            return x";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_no_return_in_one_branch() {
        let analyzed = hir("
def main(x: i32) -> i32:
    if x == 0:
        print(x)
    else:
        if x == 2:
            return 2
        else:
            return x
");

        let final_result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", final_result);
        let expected = "
def main(x: i32) -> i32:
    $0 : bool = x == 0
    if $0:
        print(x)
    else:
        $1 : bool = x == 2
        if $1:
            return 2
        else:
            return x";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_statements_decls_inside_branches() {
        let analyzed = hir("
def main() -> i32:
    x = 0
    if True:
        y = x + 1
        return y
    else:
        x = 1
        y = 2 + x
        return x + y
");

        let final_result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", final_result);
        let expected = "
def main() -> i32:
    x : i32 = 0
    if True:
        y : i32 = x + 1
        return y
    else:
        x = 1
        y : i32 = 2 + x
        return x + y";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_nested_branch_but_some_do_not_return() {
        let analyzed = hir("
def main() -> i32:
    if True:
        x = 1
        if 1 == 1:
            x = x + 3
            return x
        else:
            x = x + 1
            print(x)
        print(\"nice\")
    else:
        y = 3
        if 2 == 2:
            y = y + 1
            print(y)
        else:
            return 4 * y
");

        let final_result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", final_result);
        let expected = "
def main() -> i32:
    if True:
        x : i32 = 1
        $0 : bool = 1 == 1
        if $0:
            x = x + 3
            return x
        else:
            x = x + 1
            print(x)
        print(\"nice\")
    else:
        y : i32 = 3
        $0 : bool = 2 == 2
        if $0:
            y = y + 1
            print(y)
        else:
            return 4 * y";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn type_error_operator_not_found() {
        let analyzed = hir("
def my_function():
    x = 1 + \"abc\"");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", result);
        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.binary_op_not_found.len(), 1);

        assert_eq!(
            analyzed.type_errors.binary_op_not_found[0]
                .lhs
                .as_string(&analyzed.type_db),
            "i32"
        );

        assert_eq!(
            analyzed.type_errors.binary_op_not_found[0]
                .rhs
                .as_string(&analyzed.type_db),
            "str"
        );

        assert_eq!(
            analyzed.type_errors.binary_op_not_found[0].operator,
            Operator::Plus
        );
    }

    #[test]
    fn field_ddoes_not_exist() {
        let analyzed = hir("
def my_function():
    x = [1,2,3]
    y = x.sizee");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", result);
        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.field_or_method_not_found.len(), 1);

        assert_eq!(
            analyzed.type_errors.field_or_method_not_found[0].field_or_method,
            "sizee"
        );
        assert_eq!(
            analyzed.type_errors.field_or_method_not_found[0]
                .object_type
                .as_string(&analyzed.type_db),
            "array<i32>"
        );
        assert_eq!(
            analyzed.type_errors.field_or_method_not_found[0].on_function,
            "my_function"
        );
    }

    #[test]
    fn method_does_not_exist() {
        let analyzed = hir("
def my_function():
    x = [1,2,3]
    y = x.reevert()");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", result);
        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.field_or_method_not_found.len(), 1);

        assert_eq!(
            analyzed.type_errors.field_or_method_not_found[0].field_or_method,
            "reevert"
        );
        assert_eq!(
            analyzed.type_errors.field_or_method_not_found[0]
                .object_type
                .as_string(&analyzed.type_db),
            "array<i32>"
        );
        assert_eq!(
            analyzed.type_errors.field_or_method_not_found[0].on_function,
            "my_function"
        );
    }

    #[test]
    fn type_not_found() {
        let analyzed = hir("
def my_function():
    x: i65 = 1");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", result);
        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.type_not_found.len(), 1);

        assert_eq!(
            analyzed.type_errors.type_not_found[0].type_name.to_string(),
            "i65"
        );
        assert_eq!(
            analyzed.type_errors.type_not_found[0].on_function,
            "my_function"
        );
    }

    #[test]
    fn unexpected_type_found_in_binary_expression_lhs() {
        let analyzed = hir("
def my_function():
    x: array<str> = [\"1\",\"2\",\"3\"]
    y: i32 = x[0].as_i32 + 1");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", result);
        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.unexpected_types.len(), 1);

        assert_eq!(
            analyzed.type_errors.unexpected_types[0]
                .type_def
                .as_string(&analyzed.type_db),
            "fn (str) -> i32"
        );
        assert_eq!(
            analyzed.type_errors.unexpected_types[0].on_function,
            "my_function"
        );
    }

    #[test]
    fn unexpected_type_found_in_binary_expression_rhs() {
        let analyzed = hir("
def my_function():
    x: array<str> = [\"1\",\"2\",\"3\"]
    y: i32 = 1 + x[0].as_i32");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", result);
        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.unexpected_types.len(), 1);

        assert_eq!(
            analyzed.type_errors.unexpected_types[0]
                .type_def
                .as_string(&analyzed.type_db),
            "fn (str) -> i32"
        );
        assert_eq!(
            analyzed.type_errors.unexpected_types[0].on_function,
            "my_function"
        );
    }

    #[test]
    fn unary_operator_not_found() {
        let analyzed = hir("
def my_function():
    x = \"1\"
    y = +x");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", result);
        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.unary_op_not_found.len(), 1);

        assert_eq!(
            analyzed.type_errors.unary_op_not_found[0]
                .rhs
                .as_string(&analyzed.type_db),
            "str"
        );
        assert_eq!(
            analyzed.type_errors.unary_op_not_found[0].operator,
            Operator::Plus
        );
        assert_eq!(
            analyzed.type_errors.unary_op_not_found[0].on_function,
            "my_function"
        );
    }

    #[test]
    fn insufficient_array_type_info() {
        let analyzed = hir("
def my_function():
    x = []");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", result);
        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.insufficient_array_type_info.len(), 1);
        assert_eq!(
            analyzed.type_errors.insufficient_array_type_info[0].on_function,
            "my_function"
        );
    }

    #[test]
    fn call_to_non_callable() {
        let analyzed = hir("
def my_function():
    x: array<i32> = []
    y = x()");

        let result = hir_printer::print_hir(&analyzed.final_mir, &analyzed.type_db);
        println!("{}", result);
        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.call_non_callable.len(), 1);
        println!(
            "{:?}",
            analyzed.type_errors.call_non_callable[0].actual_type
        );

        assert_eq!(
            analyzed.type_errors.call_non_callable[0]
                .actual_type
                .as_string(&analyzed.type_db),
            "array<i32>"
        );
    }
}
