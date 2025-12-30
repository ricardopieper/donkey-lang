#[cfg(test)]
use crate::semantic::{hir_printer::HIRPrinter, monomorph::MonomorphizedStruct};

#[cfg(test)]
use crate::{
    semantic::{hir::MetaTable, typer::Typer},
    types::{diagnostics::TypeErrorPrinter, type_constructor_db::TypeConstructorDatabase},
};
#[cfg(test)]
use super::context::test_utils;

#[cfg(test)]
use super::hir::ast_globals_to_hir;

#[cfg(test)]
use super::context::Source;

#[cfg(test)]
use super::hir::HIRRoot;

#[cfg(test)]
use crate::types::diagnostics::TypeErrors;

#[cfg(test)]
fn setup(
    src: &'static str,
) -> (
    TypeConstructorDatabase,
    MetaTable,
    Source,
    Vec<HIRRoot>,
    String,
    Result<(), ()>,
    TypeErrors,
) {
    let mut type_db = TypeConstructorDatabase::new();
    let mut meta_table = MetaTable::new();
    let original_src = test_utils::parse_no_stdlib(src);
    let mut parsed = ast_globals_to_hir(&original_src.file_table[0].ast, &type_db, &mut meta_table);

    let (compiler_errors, tc_result) = {
        let mut typer = Typer::new(&mut type_db);
        if src.starts_with("#TEST_DISABLE_TYPECHECK") {
            (typer.compiler_errors, Ok(()))
        } else {
            let tc_result = typer.assign_types(&mut parsed);
            (typer.compiler_errors, tc_result)
        }
    };

    let hir_string = {
        let printer = HIRPrinter::new(true, &type_db);
        printer.print_hir(&parsed)
    };

    let printer = TypeErrorPrinter::new(
        &compiler_errors,
        &type_db,
        &meta_table,
        &original_src.file_table,
    );
    println!("{printer}");

    (
        type_db,
        meta_table,
        original_src,
        parsed,
        hir_string,
        tc_result,
        compiler_errors,
    )
}

#[cfg(test)]
fn setup_mono(
    src: &'static str,
) -> (
    TypeConstructorDatabase,
    MetaTable,
    Source,
    Vec<HIRRoot>,
    String,
    Result<(), ()>,
    TypeErrors,
) {
    

    let mut type_db = TypeConstructorDatabase::new();
    let mut meta_table = MetaTable::new();
    let original_src = test_utils::parse_no_stdlib(src);

    let parsed = ast_globals_to_hir(&original_src.file_table[0].ast, &type_db, &mut meta_table);

    //Run type check + monomorphization
    let (compiler_errors, new_hir, tc_result, ..) =
        run_type_checker(&mut type_db, parsed, true);


    let hir_string = {
        let printer = HIRPrinter::new(true, &type_db);
        let s = printer.print_hir(&new_hir);

        s
    };
    let printer = TypeErrorPrinter::new(
        &compiler_errors,
        &type_db,
        &meta_table,
        &original_src.file_table,
    );
    println!("{printer}");

    (
        type_db,
        meta_table,
        original_src,
        new_hir,
        hir_string,
        tc_result,
        compiler_errors,
    )
}

#[cfg(test)]
fn run_type_checker(
    type_db: &mut TypeConstructorDatabase,
    mut parsed: Vec<HIRRoot>,
    monomorphize: bool,
) -> (
    TypeErrors,
    Vec<HIRRoot>,
    Result<(), ()>,
    Vec<MonomorphizedStruct>,
) {
    use crate::semantic::monomorph::Monomorphizer;
    use crate::semantic::uniformizer;

    let mut typer = Typer::new(type_db);
    typer.forgive_skolem_mismatches();

    let tc_result = typer.assign_types(&mut parsed);

    let compiler_errors = typer.compiler_errors;
    if !monomorphize {
        return (compiler_errors, parsed, tc_result, vec![]);
    }

    let mut mono = Monomorphizer::new(type_db);
    mono.run(&parsed).unwrap();
    let (mut new_parsed, monomorphized_structs) = mono.get_result();


    let replacements = uniformizer::uniformize(type_db, &mut new_parsed, &monomorphized_structs);

    let mut final_typer = Typer::new(type_db);
    final_typer.set_monomorphized_versions(replacements);
    let tc_result_2 = final_typer.assign_types(&mut new_parsed);
    let full_compiler_errors = final_typer.compiler_errors;

    //uniformize again - typer will redo some types
    uniformizer::uniformize(type_db, &mut new_parsed, &monomorphized_structs);

     (
         full_compiler_errors,
         new_parsed,
         tc_result_2,
         monomorphized_structs,
    )
}

#[test]
fn test_assign_variables_simple() {
    let (.., result, _, _) = setup(
        "
def foo() -> i32:
    x = 1
    return x
",
    );

    let expected = "
def foo() -> i32 (return inferred: i32):
    x : i32 = {1: i32} [synth]
    return {x: i32}
    ";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_assign_variables_with_sum2() {
    let parsed = test_utils::parse_no_stdlib(
        "
def foo() -> i32:
    x = 1 + 2
    return x
",
    );

    let mut type_db = TypeConstructorDatabase::new();
    let mut meta_table = MetaTable::new();
    let mut parsed = ast_globals_to_hir(&parsed.file_table[0].ast, &type_db, &mut meta_table);
    let mut typer = Typer::new(&mut type_db);

    typer.assign_types(&mut parsed).unwrap();
    let printer = HIRPrinter::new(true, &type_db);

    let expected = "
def foo() -> i32 (return inferred: i32):
    x : i32 = {{1: i32} + {2: i32}: i32} [synth]
    return {x: i32}
    ";

    let result = printer.print_hir(&parsed);
    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_assign_variables_with_sum() {
    let (.., result, typing_result, errors) = setup(
        "
def foo() -> i32:
    x = 1 + 2
    return x
",
    );

    typing_result.expect("Typing failed");
    assert_eq!(errors.len(), 0);

    let expected = "
def foo() -> i32 (return inferred: i32):
    x : i32 = {{1: i32} + {2: i32}: i32} [synth]
    return {x: i32}
    ";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_generic_params() {
    let (.., result, _, _) = setup(
        "
def foo<T>(i: T, p: ptr<T>) -> T:
    x = i + 2
    y = x * 98
    z = y - *p
    return x + z
",
    );

    let expected = "
def foo<T>(i: T (inferred: T), p: ptr<T> (inferred: ptr<T>)) -> T (return inferred: T):
    x : 't1 = {{i: T} + {2: i32}: 't1} [synth]
    y : 't3 = {{x: 't1} * {98: i32}: 't3} [synth]
    z : 't6 = {{y: 't3} - {*{p: ptr<T>}: T}: 't6} [synth]
    return {{x: 't1} + {z: 't6}: T}
    ";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_generic_param_with_int() {
    let (.., result, typecheck_result, errors) = setup(
        "
def foo<T>(i: T) -> i32:
    x = i + 2
    return x * 3
",
    );

    typecheck_result.expect("Typing failed");

    assert_eq!(errors.len(), 0);

    let expected = "
def foo<T>(i: T (inferred: T)) -> i32 (return inferred: i32):
    x : 't1 = {{i: T} + {2: i32}: 't1} [synth]
    return {{x: 't1} * {3: i32}: i32}
    ";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_call_function_right_params() {
    let (.., result, _, _) = setup(
        "
def foo(x: i32) -> i32:
    return x

def bar(y: i32) -> i32:
    return foo(y)
",
    );

    let expected = "
def foo(x: i32 (inferred: i32)) -> i32 (return inferred: i32):
    return {x: i32}
def bar(y: i32 (inferred: i32)) -> i32 (return inferred: i32):
    return {{foo: (i32) -> i32}({y: i32}): i32}
    ";

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_call_function_different_type() {
    let (.., result, typing_result, errors) = setup(
        "
def foo(x: f32) -> f32:
    return x

def bar(y: i32) -> i32:
    return foo(y)
",
    );
    typing_result.expect_err("Expect type error");

    assert_eq!(errors.len(), 1);
    assert_eq!(errors.function_call_mismatches.len(), 1);

    let expected = "
def foo(x: f32 (inferred: f32)) -> f32 (return inferred: f32):
    return {x: f32}
def bar(y: i32 (inferred: i32)) -> i32 (return inferred: i32):
    return {{foo: 't0}({y: i32}): 't1}
    ";
    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_call_generic() {
    let (.., result, _, _) = setup(
        "
def id<T>(x: T) -> T:
    return x

def bar(y: i32) -> i32:
    return id<i32>(y)
",
    );

    let expected = "
def id<T>(x: T (inferred: T)) -> T (return inferred: T):
    return {x: T}
def bar(y: i32 (inferred: i32)) -> i32 (return inferred: i32):
    return {{id: (i32) -> i32}<i32>({y: i32}): i32}
    ";

    println!("{result}");
    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_call_ptr_t_id() {
    let (.., result, typing_result, errors) = setup(
        "
def id<T>(x: ptr<T>) -> T:
    return *x

def bar() -> i32:
    x = 1.0
    y = &x
    return id<f32>(y)
",
    );

    typing_result.expect_err("Expect type error");

    assert_eq!(errors.len(), 1);
    assert_eq!(errors.unify_error.len(), 1);
    assert_eq!(&errors.unify_error[0].error.context, "On return statement");

    let expected = "
def id<T>(x: ptr<T> (inferred: ptr<T>)) -> T (return inferred: T):
    return {*{x: ptr<T>}: T}
def bar() -> i32 (return inferred: i32):
    x : f32 = {1.0: f32} [synth]
    y : ptr<f32> = {&{x: f32}: ptr<f32>} [synth]
    return {{id: (ptr<f32>) -> f32}<f32>({y: ptr<f32>}): f32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_call_ptr_t_id_without_passing_type_params_let_inference_run() {
    let (.., result, _, _) = setup(
        "
def id<T>(x: ptr<T>) -> T:
    return *x

def bar() -> i32:
    x = 1
    y = &x
    return id(y)
",
    );

    let expected = "
def id<T>(x: ptr<T> (inferred: ptr<T>)) -> T (return inferred: T):
    return {*{x: ptr<T>}: T}
def bar() -> i32 (return inferred: i32):
    x : i32 = {1: i32} [synth]
    y : ptr<i32> = {&{x: i32}: ptr<i32>} [synth]
    return {{id: (ptr<i32>) -> i32}<i32>({y: ptr<i32>}): i32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_if_statement_boolean_expr() {
    let (.., result, _, _) = setup(
        "
def foo() -> i32:
    if True:
        return 1
    else:
        return 2
",
    );

    let expected = "
def foo() -> i32 (return inferred: i32):
    if {True: bool}:
        return {1: i32}
    else:
        return {2: i32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_if_non_bool() {
    let (.., result, typing_result, errors) = setup(
        "
def foo() -> i32:
    if 0:
        return 1
    else:
        return 2
",
    );

    typing_result.expect_err("Expect type error");
    assert_eq!(errors.len(), 1);
    assert_eq!(errors.unify_error.len(), 1);
    assert_eq!(
        &errors.unify_error[0].error.context,
        "On if expression, condition type mismatch"
    );

    let expected = "
def foo() -> i32 (return inferred: i32):
    if {0: i32}:
        return {1: i32}
    else:
        return {2: i32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_return_type_invalid() {
    let (.., result, typing_result, errors) = setup(
        "
def foo() -> i32:
    return 1.0
",
    );

    typing_result.expect_err("Expected type error");
    assert_eq!(errors.len(), 1);
    assert_eq!(errors.unify_error.len(), 1);
    assert_eq!(&errors.unify_error[0].error.context, "On return statement");

    let expected = "
def foo() -> i32 (return inferred: i32):
    return {1.0: f32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn pointer_deref() {
    let (.., result, typing_result, errors) = setup(
        "
def foo<T>(p: ptr<T>):
    x = *p
",
    );

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def foo<T>(p: ptr<T> (inferred: ptr<T>)) -> Void (return inferred: Void):
    x : T = {*{p: ptr<T>}: T} [synth]
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn return_ref() {
    let (.., result, typing_result, errors) = setup(
        "
def foo<T>(x: T) -> ptr<T>:
    return &x
",
    );

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def foo<T>(x: T (inferred: T)) -> ptr<T> (return inferred: ptr<T>):
    return {&{x: T}: ptr<T>}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn pointers_everywhere() {
    let (.., result, typing_result, errors) = setup(
        "
def foo<T>(p: ptr<ptr<T>>) -> T:
    return **p

def bar() -> i32:
    x = 1
    y = &x
    z = &y
    return foo<i32>(z)
",
    );

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def foo<T>(p: ptr<ptr<T>> (inferred: ptr<ptr<T>>)) -> T (return inferred: T):
    return {*{*{p: ptr<ptr<T>>}: ptr<T>}: T}
def bar() -> i32 (return inferred: i32):
    x : i32 = {1: i32} [synth]
    y : ptr<i32> = {&{x: i32}: ptr<i32>} [synth]
    z : ptr<ptr<i32>> = {&{y: ptr<i32>}: ptr<ptr<i32>>} [synth]
    return {{foo: (ptr<ptr<i32>>) -> i32}<i32>({z: ptr<ptr<i32>>}): i32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn inference_of_return_should_not_change_signature() {
    let (.., result, typing_result, errors) = setup(
        "
def foo<T>(t: T) -> T:
    return 1
",
    );

    typing_result.expect_err("Expected error");
    assert_eq!(errors.len(), 1);
    assert_eq!(errors.unify_error.len(), 1);
    assert_eq!(&errors.unify_error[0].error.context, "On return statement");

    let expected = "
def foo<T>(t: T (inferred: T)) -> T (return inferred: T):
    return {1: i32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn call_another_generic_function_same_type_args_names() {
    let (.., result, typing_result, errors) = setup(
        "
def bar<T>(u: T) -> T:
    return u

def foo<T>(t: T) -> T:
    return bar(t)
",
    );

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def bar<T>(u: T (inferred: T)) -> T (return inferred: T):
    return {u: T}
def foo<T>(t: T (inferred: T)) -> T (return inferred: T):
    return {{bar: (T) -> T}<T>({t: T}): T}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn call_another_generic_function_different_type_args_names() {
    let (.., result, typing_result, errors) = setup(
        "
def bar<U>(u: U) -> U:
    return u

def foo<T>(t: T) -> T:
    return bar(t)
",
    );

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def bar<U>(u: U (inferred: U)) -> U (return inferred: U):
    return {u: U}
def foo<T>(t: T (inferred: T)) -> T (return inferred: T):
    return {{bar: (T) -> T}<T>({t: T}): T}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn generic_args_tests2() {
    let (.., result, typing_result, errors) = setup(
        "
def baz<T, U>(y: ptr<ptr<T>>, x: ptr<U>) -> T:
    return **y

def bar<U, T>(u: U, t: T) -> U:
    x = &u
    return baz(&x, x)
",
    );
    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def baz<T, U>(y: ptr<ptr<T>> (inferred: ptr<ptr<T>>), x: ptr<U> (inferred: ptr<U>)) -> T (return inferred: T):
    return {*{*{y: ptr<ptr<T>>}: ptr<T>}: T}
def bar<U, T>(u: U (inferred: U), t: T (inferred: T)) -> U (return inferred: U):
    x : ptr<U> = {&{u: U}: ptr<U>} [synth]
    return {{baz: (ptr<ptr<U>>, ptr<U>) -> U}<U, U>({&{x: ptr<U>}: ptr<ptr<U>>}, {x: ptr<U>}): U}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn generic_args_tests3_fails_because_needs_more_ptr() {
    use crate::types::diagnostics::FunctionName;
    let (.., result, typing_result, errors) = setup(
        "
def baz<T, U>(x: ptr<ptr<T>>, y: ptr<U>) -> U:
    return *y

def bar<U, T>(u: U, t: T) -> U:
    x = &u
    return baz(x, &x)
",
    );

    typing_result.expect_err("Expected type error");
    assert_eq!(errors.len(), 1);
    assert_eq!(errors.function_call_mismatches.len(), 1);
    assert_eq!(
        errors.function_call_mismatches[0]
            .error
            .context
            .called_function_name,
        FunctionName::Function("baz".into())
    );

    let expected = "
def baz<T, U>(x: ptr<ptr<T>> (inferred: ptr<ptr<T>>), y: ptr<U> (inferred: ptr<U>)) -> U (return inferred: U):
    return {*{y: ptr<U>}: U}
def bar<U, T>(u: U (inferred: U), t: T (inferred: T)) -> U (return inferred: U):
    x : ptr<U> = {&{u: U}: ptr<U>} [synth]
    return {{baz: 't2}({x: ptr<U>}, {&{x: ptr<U>}: ptr<ptr<U>>}): 't4}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn missing_type_in_args_list_user_can_inform_types() {
    let (.., result, typing_result, errors) = setup(
        "
def convert<TIn, TOut>(input: TIn) -> TOut:
    external

def main() -> i32:
    x = 1.0
    return convert<f32, i32>(x)
",
    );

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def convert<TIn, TOut>(input: TIn (inferred: TIn)) -> TOut (return inferred: TOut):
def main() -> i32 (return inferred: i32):
    x : f32 = {1.0: f32} [synth]
    return {{convert: (f32) -> i32}<f32, i32>({x: f32}): i32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn method_call_infer_types() {
    let (.., result, typing_result, errors) = setup(
        "
def main(x: ptr<i32>):
    y = x.read(0)
    x.write(0, 99)
",
    );

    println!("{result}");

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def main(x: ptr<i32> (inferred: ptr<i32>)) -> Void (return inferred: Void):
    y : i32 = {{x: ptr<i32>}.read({0: u64}): i32} [synth]
    {x: ptr<i32>}.write({0: u64}, {99: i32})
";

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn struct_inference() {
    let (.., result, typing_result, errors) = setup(
        "
struct SomeStruct:
    x: i32

def main():
    x = SomeStruct()
",
    );

    println!("{result}");

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
struct SomeStruct:
    x: i32
def main() -> Void (return inferred: Void):
    x : SomeStruct = {SomeStruct(): SomeStruct} [synth]
";
    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn struct_set_field() {
    let (.., result, typing_result, errors) = setup(
        "
struct SomeStruct:
    x: i32

def main():
    my_struct = SomeStruct()
    my_struct.x = 1
",
    );

    println!("{result}");

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
struct SomeStruct:
    x: i32
def main() -> Void (return inferred: Void):
    my_struct : SomeStruct = {SomeStruct(): SomeStruct} [synth]
    {{my_struct: SomeStruct}.x: i32} = {1: i32}
";
    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn generic_struct_set_field_generic_works() {
    let (.., result, typing_result, errors) = setup(
        "
struct SomeStruct<T>:
    x: ptr<T>

struct SomeOtherStruct<T>:
    x: T

def main():
    x = 1
    some_struct = SomeStruct<i32>()
    some_struct.x = &x
    other_struct = SomeOtherStruct<ptr<i32>>()
    other_struct.x = some_struct.x
",
    );

    println!("{result}");

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
struct SomeStruct:
    x: ptr<T>
struct SomeOtherStruct:
    x: T
def main() -> Void (return inferred: Void):
    x : i32 = {1: i32} [synth]
    some_struct : SomeStruct<i32> = {SomeStruct<i32>(): SomeStruct<i32>} [synth]
    {{some_struct: SomeStruct<i32>}.x: ptr<i32>} = {&{x: i32}: ptr<i32>}
    other_struct : SomeOtherStruct<ptr<i32>> = {SomeOtherStruct<ptr<i32>>(): SomeOtherStruct<ptr<i32>>} [synth]
    {{other_struct: SomeOtherStruct<ptr<i32>>}.x: ptr<i32>} = {{some_struct: SomeStruct<i32>}.x: ptr<i32>}
";
    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn generic_struct_set_field_generic_does_not_typecheck() {
    let (.., result, typing_result, errors) = setup(
        "
struct SomeStruct<T>:
    x: ptr<T>

struct SomeOtherStruct<T>:
    x: T

def main():
    x = 1
    some_struct = SomeStruct<i32>()
    some_struct.x = &x
    other_struct = SomeOtherStruct<ptr<f64>>()
    other_struct.x = some_struct.x
",
    );

    println!("{result}");

    typing_result.expect_err("Expected typing error");
    assert_eq!(errors.len(), 1);

    let expected = "
struct SomeStruct:
    x: ptr<T>
struct SomeOtherStruct:
    x: T
def main() -> Void (return inferred: Void):
    x : i32 = {1: i32} [synth]
    some_struct : SomeStruct<i32> = {SomeStruct<i32>(): SomeStruct<i32>} [synth]
    {{some_struct: SomeStruct<i32>}.x: ptr<i32>} = {&{x: i32}: ptr<i32>}
    other_struct : SomeOtherStruct<ptr<f64>> = {SomeOtherStruct<ptr<f64>>(): SomeOtherStruct<ptr<f64>>} [synth]
    {{other_struct: SomeOtherStruct<ptr<f64>>}.x: ptr<f64>} = {{some_struct: SomeStruct<i32>}.x: ptr<i32>}
";
    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn do_not_accept_non_existing_variables() {
    let (.., result, typing_result, errors) = setup(
        "
struct SomeStruct<T>:
    x: ptr<T>

def main():
    some_struct = SomeStruct<i32>()
    some_struct.x = &value
",
    );

    println!("{result}");

    typing_result.expect_err("Expected typing error");
    assert_eq!(errors.len(), 1);
    assert_eq!(errors.variable_not_found.len(), 1);

    let expected = "
struct SomeStruct:
    x: ptr<T>
def main() -> Void (return inferred: Void):
    some_struct : SomeStruct<i32> = {SomeStruct<i32>(): SomeStruct<i32>} [synth]
    {{some_struct: SomeStruct<i32>}.x: ptr<i32>} = {&{value: 't4}: 't5}
";
    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn dont_confuse_type_parameters_struct_and_caller_ptr() {
    let (.., result, typing_result, errors) = setup(
        "
def make_ptr<TPtr>(val: TPtr) -> ptr<TPtr>:
    return &val

def main():
    float_ptr = make_ptr(1.0)
",
    );

    println!("{result}");

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def make_ptr<TPtr>(val: TPtr (inferred: TPtr)) -> ptr<TPtr> (return inferred: ptr<TPtr>):
    return {&{val: TPtr}: ptr<TPtr>}
def main() -> Void (return inferred: Void):
    float_ptr : ptr<f32> = {{make_ptr: (f32) -> ptr<f32>}<f32>({1.0: f32}): ptr<f32>} [synth]
";
    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn accept_user_declared_structs_in_function_params() {
    let (.., result, typing_result, errors) = setup(
        "
struct SomeStruct:
    x: i32

def main(val: SomeStruct):
    val.x = 1
",
    );

    println!("{result}");

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
struct SomeStruct:
    x: i32
def main(val: SomeStruct (inferred: SomeStruct)) -> Void (return inferred: Void):
    {{val: SomeStruct}.x: i32} = {1: i32}
";
    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn accept_user_declared_structs_in_function_params_after_the_function() {
    let (.., result, typing_result, errors) = setup(
        "
def main(val: SomeStruct):
    val.x = 1

struct SomeStruct:
    x: i32
",
    );

    println!("{result}");

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def main(val: SomeStruct (inferred: SomeStruct)) -> Void (return inferred: Void):
    {{val: SomeStruct}.x: i32} = {1: i32}
struct SomeStruct:
    x: i32
";
    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn return_ptr_to_struct_ptr() {
    let (.., result, typing_result, errors) = setup(
        "

def main(val: ptr<SomeStruct>) -> ptr<i32>:
    return &(*val).x

struct SomeStruct:
    x: i32
",
    );

    println!("{result}");

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def main(val: ptr<SomeStruct> (inferred: ptr<SomeStruct>)) -> ptr<i32> (return inferred: ptr<i32>):
    return {&{{*{val: ptr<SomeStruct>}: SomeStruct}.x: i32}: ptr<i32>}
struct SomeStruct:
    x: i32
";
    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn monomprthization_test_simple() {
    let (.., result, typing_result, errors) = setup_mono(
        "
def foo<T>(t: T) -> i32:
    return 1

def bar() -> i32:
    x = 1
    return foo(x)
",
    );

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def bar() -> i32 (return inferred: i32):
    x : i32 = {1: i32} [synth]
    return {{foo[i32]: (i32) -> i32}({x: i32}): i32}
def foo[i32](t: T (inferred: i32)) -> i32 (return inferred: i32):
    return {1: i32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn monomorphization_test_harder() {
    let (.., result, typing_result, errors) = setup_mono(
        "
def foo<T, U>(t: T, u: U) -> i32:
    return 1 + u + t

def bar() -> i32:
    x = 1
    return foo(x, x)
",
    );

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def bar() -> i32 (return inferred: i32):
    x : i32 = {1: i32} [synth]
    return {{foo[i32,i32]: (i32, i32) -> i32}({x: i32}, {x: i32}): i32}
def foo[i32,i32](t: T (inferred: i32), u: U (inferred: i32)) -> i32 (return inferred: i32):
    return {{{1: i32} + {u: i32}: i32} + {t: i32}: i32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn monomorphization_test_multiple() {
    let (.., result, typing_result, errors) = setup_mono(
        "
def foo<T, U>(t: T, u: U) -> i32:
    return 1 + t

def bar() -> i32:
    x = 1
    y = 3.14
    r1 = foo(x, y)
    r2 = foo(y, 'c')
",
    );

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def bar() -> i32 (return inferred: i32):
    x : i32 = {1: i32} [synth]
    y : f32 = {3.14: f32} [synth]
    r1 : i32 = {{foo[i32,f32]: (i32, f32) -> i32}({x: i32}, {y: f32}): i32} [synth]
    r2 : i32 = {{foo[f32,char]: (f32, char) -> i32}({y: f32}, {'c': char}): i32} [synth]
def foo[f32,char](t: T (inferred: f32), u: U (inferred: char)) -> i32 (return inferred: i32):
    return {{1: i32} + {t: f32}: i32}
def foo[i32,f32](t: T (inferred: i32), u: U (inferred: f32)) -> i32 (return inferred: i32):
    return {{1: i32} + {t: i32}: i32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn monomorphization_test_indirect() {
    let (.., result, typing_result, errors) = setup_mono(
        "
def foo<T, U>(t: T, u: U) -> i32:
    return baz(u) + t

def baz<T>(t: T) -> i32:
    return 1 + t

def bar() -> i32:
    foo(1, 3)
",
    );

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def bar() -> i32 (return inferred: i32):
    {foo[i32,i32]: (i32, i32) -> i32}({1: i32}, {3: i32})
def foo[i32,i32](t: T (inferred: i32), u: U (inferred: i32)) -> i32 (return inferred: i32):
    return {{{baz[i32]: (i32) -> i32}({u: i32}): i32} + {t: i32}: i32}
def baz[i32](t: T (inferred: i32)) -> i32 (return inferred: i32):
    return {{1: i32} + {t: i32}: i32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn monomorphization_tes2_indirect_second_typechecking_run_should_accept_it() {
    let (mut ty_db, meta, source, mut hir, result, typing_result, errors) = setup_mono(
        "
def foo<T, U>(t: T, u: U) -> i32:
    return baz(u) + t

def baz<T>(t: T) -> i32:
    return 1 + t

def bar() -> i32:
    foo(1, 3)
",
    );

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def bar() -> i32 (return inferred: i32):
    {foo[i32,i32]: (i32, i32) -> i32}({1: i32}, {3: i32})
def foo[i32,i32](t: T (inferred: i32), u: U (inferred: i32)) -> i32 (return inferred: i32):
    return {{{baz[i32]: (i32) -> i32}({u: i32}): i32} + {t: i32}: i32}
def baz[i32](t: T (inferred: i32)) -> i32 (return inferred: i32):
    return {{1: i32} + {t: i32}: i32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());

    let mut typer = Typer::new(&mut ty_db);
    typer.assign_types(&mut hir).expect("Typing error");

    let printer = TypeErrorPrinter::new(&typer.compiler_errors, &ty_db, &meta, &source.file_table);
    println!("ERRORS: \n{printer}");
}

#[test]
fn monomorphization_test_full_generic_indirect_second_typechecking_run_should_accept_it() {
    let (mut ty_db, meta, source, mut hir, result, typing_result, errors) = setup_mono(
        "
def foo<T, U>(t: T, u: U) -> T:
    return baz(u) + t

def baz<T>(t: T) -> T:
    return 1 + t

def bar() -> i32:
    foo(1, 3)
",
    );

    //ignorable error
    typing_result.expect("Compiler should be forgiving skolem mismatches for now");
    assert_eq!(errors.len(), 0);

    let expected = "
def bar() -> i32 (return inferred: i32):
    {foo[i32,i32]: (i32, i32) -> i32}({1: i32}, {3: i32})
def foo[i32,i32](t: T (inferred: i32), u: U (inferred: i32)) -> T (return inferred: i32):
    return {{{baz[i32]: (i32) -> i32}({u: i32}): i32} + {t: i32}: i32}
def baz[i32](t: T (inferred: i32)) -> T (return inferred: i32):
    return {{1: i32} + {t: i32}: i32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());

    let mut typer = Typer::new(&mut ty_db);
    typer.assign_types(&mut hir).expect("Typing error");
    let printer = TypeErrorPrinter::new(&typer.compiler_errors, &ty_db, &meta, &source.file_table);
    println!("ERRORS: \n{printer}");
}

#[test]
fn struct_usage_monomorphization() {
    let (.., result, typing_result, errors) = setup_mono(
        "
struct Box<T>:
    x: T

def box<T>(item: T) -> Box<T>:
    b = Box<T>()
    b.x = item
    return b

def main():
    b1 = box(1)
    b2 = box(3.14)
    b3 = box('c')
",
    );

    //ignorable error
    typing_result.expect("Compiler should be forgiving skolem mismatches for now");
    assert_eq!(errors.len(), 0);

    let expected = "
def main() -> Void (return inferred: Void):
    b1 : Box[i32] = {{box[i32]: (i32) -> Box[i32]}({1: i32}): Box[i32]} [synth]
    b2 : Box[f32] = {{box[f32]: (f32) -> Box[f32]}({3.14: f32}): Box[f32]} [synth]
    b3 : Box[char] = {{box[char]: (char) -> Box[char]}({'c': char}): Box[char]} [synth]
def box[char](item: T (inferred: char)) -> Box<T> (return inferred: Box[char]):
    b : Box[char] = {Box[char](): Box[char]} [synth]
    {{b: Box[char]}.x: char} = {item: char}
    return {b: Box[char]}
struct Box[char]:
    x: char
def box[f32](item: T (inferred: f32)) -> Box<T> (return inferred: Box[f32]):
    b : Box[f32] = {Box[f32](): Box[f32]} [synth]
    {{b: Box[f32]}.x: f32} = {item: f32}
    return {b: Box[f32]}
struct Box[f32]:
    x: f32
def box[i32](item: T (inferred: i32)) -> Box<T> (return inferred: Box[i32]):
    b : Box[i32] = {Box[i32](): Box[i32]} [synth]
    {{b: Box[i32]}.x: i32} = {item: i32}
    return {b: Box[i32]}
struct Box[i32]:
    x: i32
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn struct_generic() {
    let (.., result, typing_result, errors) = setup_mono(
        "
struct Box<T>:
    x: T

def box<T>(item: T) -> Box<T>:
    return Box<T>()

def main():
    y = 1
    x = box(&y)
    x.x = &y
",
    );
    println!("{result}");

    //ignorable error
    typing_result.expect("Compiler should be forgiving skolem mismatches for now");
    assert_eq!(errors.len(), 0);

    let expected = "
def main() -> Void (return inferred: Void):
    y : i32 = {1: i32} [synth]
    x : Box[ptr<i32>] = {{box[ptr<i32>]: (ptr<i32>) -> Box[ptr<i32>]}({&{y: i32}: ptr<i32>}): Box[ptr<i32>]} [synth]
    {{x: Box[ptr<i32>]}.x: ptr<i32>} = {&{y: i32}: ptr<i32>}
def box[ptr<i32>](item: T (inferred: ptr<i32>)) -> Box<T> (return inferred: Box[ptr<i32>]):
    return {Box[ptr<i32>](): Box[ptr<i32>]}
struct Box[ptr<i32>]:
    x: ptr<i32>
";

    pretty_assertions::assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_intrinsic() {
    let (mut ty_db, meta, source, mut hir, result, typing_result, errors) = setup_mono(
        "
def print(x: i32):
    intrinsic

def main() -> i32:
    x = 0
    if x == 0:
        print(1)
    else:
        return x
    return x
",
    );

    //ignorable error
    typing_result.expect("Compiler should be forgiving skolem mismatches for now");
    assert_eq!(errors.len(), 0);

    let expected = "
def print(x: i32 (inferred: i32)) -> Void (return inferred: Void):
def main() -> i32 (return inferred: i32):
    x : i32 = {0: i32} [synth]
    if {{x: i32} == {0: i32}: bool}:
        {print: (i32) -> Void}({1: i32})
    else:
        return {x: i32}
    return {x: i32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());

    let mut typer = Typer::new(&mut ty_db);
    typer
        .assign_types(&mut hir)
        .expect("Should have NOT gotten an error");
    let printer = TypeErrorPrinter::new(&typer.compiler_errors, &ty_db, &meta, &source.file_table);
    println!("ERRORS: \n{printer}");
}

#[test]
fn list_test_fully_inferred() {
    let (.., result, typing_result, errors) = setup_mono(
        "
struct List<T>:
    buf: ptr<T>

# Creates a new list
def list_new<T>(i: T) -> List<T>:
    return List<T>()

def main():
    list = list_new(99)
",
    );

    //ignorable error
    typing_result.expect("Compiler should be forgiving skolem mismatches for now");
    assert_eq!(errors.len(), 0);

    let expected = "
def main() -> Void (return inferred: Void):
    list : List[i32] = {{list_new[i32]: (i32) -> List[i32]}({99: i32}): List[i32]} [synth]
def list_new[i32](i: T (inferred: i32)) -> List<T> (return inferred: List[i32]):
    return {List[i32](): List[i32]}
struct List[i32]:
    buf: ptr<i32>
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn list_test_with_impl() {
    let (.., result, typing_result, errors) = setup_mono(
        "
struct List<T>:
    buf: ptr<T>
    len: u64
    cap: u64

# Creates a new list
def list_new<T>() -> List<T>:
    list = List<T>()
    list.len = 0
    list.cap = 0

    return list

def mem_alloc<T>(size: u64) -> ptr<T>:
    intrinsic

def mem_free<T>(ptr: ptr<T>):
    intrinsic

impl List<T>:
    def add(self, item: T):
        len = (*self).len
        cap = (*self).cap
        if len == cap:
            if len == 0:
                (*self).cap = 4
            else:
                (*self).cap = cap * 2

            new_allocation = mem_alloc<T>((*self).cap)
            if len > 0:
                i: u64 = 0
                while i < len:
                    new_allocation[i] = (*self).buf[i]
                    i = i + 1
                mem_free((*self).buf)

            (*self).buf = new_allocation

        (*self).buf[len] = item
        (*self).len = len + 1

    def get(self, index: u64) -> T:
        return (*self).buf[index]

    def __index_ptr__(self, index: u64) -> ptr<T>:
        return &(*self).buf[index]

def main():
    list = list_new()
    list.add(1)
    x = list.get(0)
    y = list[0]
",
    );

    //ignorable error
    typing_result.expect("Compiler should be forgiving skolem mismatches for now");
    assert_eq!(errors.len(), 0);

    let expected = "
def main() -> Void (return inferred: Void):
    list : List[i32] = {{list_new[i32]: () -> List[i32]}(): List[i32]} [synth]
    {list: List[i32]}.add({1: i32})
    x : i32 = {{list: List[i32]}.get({0: u64}): i32} [synth]
    y : i32 = {*{{list: List[i32]}.__index_ptr__({0: u64}): ptr<i32>}: i32} [synth]
def mem_free[i32](ptr: ptr<T> (inferred: ptr<i32>)) -> Void (return inferred: Void):
def mem_alloc[i32](size: u64 (inferred: u64)) -> ptr<T> (return inferred: ptr<i32>):
def list_new[i32]() -> List<T> (return inferred: List[i32]):
    list : List[i32] = {List[i32](): List[i32]} [synth]
    {{list: List[i32]}.len: u64} = {0: u64}
    {{list: List[i32]}.cap: u64} = {0: u64}
    return {list: List[i32]}
struct List[i32]:
    buf: ptr<i32>
    len: u64
    cap: u64
impl List[i32]:
    def __index_ptr__(self,     index: u64 (inferred: u64)) -> ptr<T> (return inferred: ptr<i32>):
        return {&{*{{{*{self: ptr<List[i32]>}: List[i32]}.buf: ptr<i32>}.__index_ptr__({index: u64}): ptr<i32>}: i32}: ptr<i32>}
    def get(self,     index: u64 (inferred: u64)) -> T (return inferred: i32):
        return {*{{{*{self: ptr<List[i32]>}: List[i32]}.buf: ptr<i32>}.__index_ptr__({index: u64}): ptr<i32>}: i32}
    def add(self,     item: T (inferred: i32)) -> Void (return inferred: Void):
        len : u64 = {{*{self: ptr<List[i32]>}: List[i32]}.len: u64} [synth]
        cap : u64 = {{*{self: ptr<List[i32]>}: List[i32]}.cap: u64} [synth]
        if {{len: u64} == {cap: u64}: bool}:
            if {{len: u64} == {0: i32}: bool}:
                {{*{self: ptr<List[i32]>}: List[i32]}.cap: u64} = {4: u64}
            else:
                {{*{self: ptr<List[i32]>}: List[i32]}.cap: u64} = {{cap: u64} * {2: u64}: u64}
            new_allocation : ptr<i32> = {{mem_alloc[i32]: (u64) -> ptr<i32>}({{*{self: ptr<List[i32]>}: List[i32]}.cap: u64}): ptr<i32>} [synth]
            if {{len: u64} > {0: i32}: bool}:
                i : u64 = {0: u64} (inferred: u64)
                while {{i: u64} < {len: u64}: bool}:
                    {*{{new_allocation: ptr<i32>}.__index_ptr__({i: u64}): ptr<i32>}: i32} = {*{{{*{self: ptr<List[i32]>}: List[i32]}.buf: ptr<i32>}.__index_ptr__({i: u64}): ptr<i32>}: i32}
                    {i: u64} = {{i: u64} + {1: u64}: u64}
                {mem_free[i32]: (ptr<i32>) -> Void}({{*{self: ptr<List[i32]>}: List[i32]}.buf: ptr<i32>})
            else:
                pass
            {{*{self: ptr<List[i32]>}: List[i32]}.buf: ptr<i32>} = {new_allocation: ptr<i32>}
        else:
            pass
        {*{{{*{self: ptr<List[i32]>}: List[i32]}.buf: ptr<i32>}.__index_ptr__({len: u64}): ptr<i32>}: i32} = {item: i32}
        {{*{self: ptr<List[i32]>}: List[i32]}.len: u64} = {{len: u64} + {1: u64}: u64}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn index_ptr_test() {
    let (.., result, typing_result, errors) = setup_mono(
        "
struct List:
    cap: i32

impl List:
    def __index_ptr__(self, index: u64) -> ptr<i32>:
        return &0

def main():
    list = List()
    y = list[0]
",
    );

    //ignorable error
    typing_result.expect("Compiler should be forgiving skolem mismatches for now");
    assert_eq!(errors.len(), 0);

    let expected = "
struct List:
    cap: i32
impl List:
    def __index_ptr__(self,     index: u64 (inferred: u64)) -> ptr<i32> (return inferred: ptr<i32>):
        return {&{0: i32}: ptr<i32>}
def main() -> Void (return inferred: Void):
    list : List = {List(): List} [synth]
    y : i32 = {*{{list: List}.__index_ptr__({0: u64}): ptr<i32>}: i32} [synth]
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn array_numbers_test() {
    let (.., result, typing_result, errors) = setup_mono(
        "
def main():
    x = [1,2,3]
",
    );

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def main() -> Void (return inferred: Void):
    x : array<i32> = {[{1: i32}, {2: i32}, {3: i32}]: array<i32>} [synth]
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}
/**
 * #[test]
fn unary_works() {
    no_errors!(
        "
def main():
    x = 1
    y = -x
    x = +x

    a = 1.3
    b = -a
    c = +a
"
    );
}

 */

#[test]
fn unary_expr() {
    let (.., result, typing_result, errors) = setup_mono(
        "
def main():
    x = 1
    y = -x
    x = +x

    a = 1.3
    b = -a
    c = +a
",
    );

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def main() -> Void (return inferred: Void):
    x : i32 = {1: i32} [synth]
    y : i32 = {-{x: i32}: i32} [synth]
    {x: i32} = {+{x: i32}: i32}
    a : f32 = {1.3: f32} [synth]
    b : f32 = {-{a: f32}: f32} [synth]
    c : f32 = {+{a: f32}: f32} [synth]
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn impl_test() {
    let (.., result, typing_result, errors) = setup_mono(
        "
struct SomeStruct:
    x: i32

impl SomeStruct:
    def foo(self) -> i32:
        return (*self).x
",
    );

    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
struct SomeStruct:
    x: i32
impl SomeStruct:
    def foo(self) -> i32 (return inferred: i32):
        return {{*{self: ptr<SomeStruct>}: SomeStruct}.x: i32}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn impl_generic_different_args_test() {
    let (.., result, typing_result, errors) = setup_mono(
        "
struct SomeStruct<T>:
    x: T

impl SomeStruct<F>:
    def foo(self) -> F:
        return (*self).x

def main():
    s = SomeStruct<i32>()
    s.x = 1
    y = s.foo()
",
    );
    println!("{result}");
    typing_result.expect("Typing error");
    assert_eq!(errors.len(), 0);

    let expected = "
def main() -> Void (return inferred: Void):
    s : SomeStruct[i32] = {SomeStruct[i32](): SomeStruct[i32]} [synth]
    {{s: SomeStruct[i32]}.x: i32} = {1: i32}
    y : i32 = {{s: SomeStruct[i32]}.foo(): i32} [synth]
struct SomeStruct[i32]:
    x: i32
impl SomeStruct[i32]:
    def foo(self) -> F (return inferred: i32):
        return {{*{self: ptr<SomeStruct[i32]>}: SomeStruct[i32]}.x: i32}
";

    assert_eq!(expected.trim(), result.trim());
}

/**
 * This is because errors of this kind are caught in MIR type checker.
 */
#[test]
fn no_type_errors_on_hir_after_mono() {
    let (.., result, _, errors) = setup_mono(
        "
struct ToyotaCorolla:
    year: u32

def sum<T>(i: i32, u: T) -> T:
    return i + u

def main():
    corolla = ToyotaCorolla()
    corolla.year = 2025
    sum(69, corolla)
",
    );
    assert_eq!(errors.len(), 1);

    let expected = "
struct ToyotaCorolla:
    year: u32
def main() -> Void (return inferred: Void):
    corolla : ToyotaCorolla = {ToyotaCorolla(): ToyotaCorolla} [synth]
    {{corolla: ToyotaCorolla}.year: u32} = {2025: u32}
    {sum[ToyotaCorolla]: (i32, ToyotaCorolla) -> ToyotaCorolla}({69: i32}, {corolla: ToyotaCorolla})
def sum[ToyotaCorolla](i: i32 (inferred: i32), u: T (inferred: ToyotaCorolla)) -> T (return inferred: ToyotaCorolla):
    return {{i: i32} + {u: ToyotaCorolla}: i32}
";

    println!("{result}");

    pretty_assertions::assert_str_eq!(expected.trim(), result.trim());
}
