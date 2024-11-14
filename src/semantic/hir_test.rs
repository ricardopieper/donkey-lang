#[cfg(test)]
use crate::semantic::hir_printer::HIRPrinter;

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
    x : 't2 = {{i: T} + {2: i32}: 't2} [synth]
    return {{x: 't2} * {3: i32}: i32}
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
    assert_eq!(errors.unify_error.len(), 1);
    assert_eq!(&errors.unify_error[0].error.context, "On function call");

    let expected = "
def foo(x: f32 (inferred: f32)) -> f32 (return inferred: f32):
    return {x: f32}
def bar(y: i32 (inferred: i32)) -> i32 (return inferred: i32):
    return {{foo: 't1}({y: i32}): 't2
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
    return {{id: (ptr<i32>) -> i32}({y: ptr<i32>}): i32}
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
    return {{bar: (T) -> T}({t: T}): T}
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
    return {{bar: (T) -> T}({t: T}): T}
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
    return {{baz: (ptr<ptr<U>>, ptr<U>) -> U}({&{x: ptr<U>}: ptr<ptr<U>>}, {x: ptr<U>}): U}
";

    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn generic_args_tests3_fails_because_needs_more_ptr() {
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
    assert_eq!(errors.unify_error.len(), 1);
    assert_eq!(&errors.unify_error[0].error.context, "On function call");

    let expected = "
def baz<T, U>(x: ptr<ptr<T>> (inferred: ptr<ptr<T>>), y: ptr<U> (inferred: ptr<U>)) -> U (return inferred: U):
    return {*{y: ptr<U>}: U}
def bar<U, T>(u: U (inferred: U), t: T (inferred: T)) -> U (return inferred: U):
    x : ptr<U> = {&{u: U}: ptr<U>} [synth]
    return {{baz: 't4}({x: ptr<U>}, {&{x: ptr<U>}: ptr<ptr<U>>}): 't6}
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
fn do_not_accept_hallucinated_variables() {
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

    let expected = "
struct SomeStruct:
    x: ptr<T>
def main() -> Void (return inferred: Void):
    some_struct : SomeStruct<i32> = {SomeStruct<i32>(): SomeStruct<i32>} [synth]
    {{some_struct: SomeStruct<i32>}.x: ptr<'t4>} = {&{value: 't4}: ptr<'t4>}
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
    float_ptr : ptr<f32> = {{make_ptr: (f32) -> ptr<f32>}({1.0: f32}): ptr<f32>} [synth]
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
