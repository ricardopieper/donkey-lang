use crate::semantic::{hir::MetaTable, typer::Typer};
#[cfg(test)]
use crate::{
    interner::{InternedString, StringInterner},
    semantic::hir_printer::HIRPrinter,
    types::type_constructor_db::TypeConstructorDatabase,
};

#[cfg(test)]
use super::context::test_utils;

#[cfg(test)]
use super::hir::ast_globals_to_hir;

#[test]
fn test_assign_variables() {
    let parsed = test_utils::parse_no_stdlib(
        "
def foo() -> i32:
    x = 1
    return x
",
    );

    let type_db = TypeConstructorDatabase::new();
    let mut meta_table = MetaTable::new();
    let parsed = ast_globals_to_hir(&parsed.file_table[0].ast, &type_db, &mut meta_table);
    let printer = HIRPrinter::new(true, &type_db);
    let expected = "
def foo() -> i32 (return inferred: i32):
    x : i32 = {1: i32} [synth]
    return {x: i32}
    ";

    let result = printer.print_hir(&parsed);
    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_assign_variables_with_sum() {
    let parsed = test_utils::parse_no_stdlib(
        "
def foo() -> i32:
    x = 1 + 2
    return x
",
    );

    let mut type_db = TypeConstructorDatabase::new();
    let mut meta_table = MetaTable::new();
    let parsed = ast_globals_to_hir(&parsed.file_table[0].ast, &type_db, &mut meta_table);
    let mut typer = Typer::new(&mut type_db);

    let typed = typer.assign_types(parsed);
    let printer = HIRPrinter::new(true, &type_db);

    let expected = "
def foo() -> i32 (return inferred: i32):
    x : i32 = {{1: i32} + {2: i32}: i32} [synth]
    return {x: i32}
    ";

    let result = printer.print_hir(&typed);
    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_generic_params() {
    let parsed = test_utils::parse_no_stdlib(
        "
def foo<T>(i: T, p: ptr<T>) -> T:
    x = i + 2
    y = x * 98
    z = y - *p
    return x + z
",
    );

    let mut type_db = TypeConstructorDatabase::new();
    let mut meta_table = MetaTable::new();
    let parsed = ast_globals_to_hir(&parsed.file_table[0].ast, &type_db, &mut meta_table);
    let mut typer = Typer::new(&mut type_db);
    let typed = typer.assign_types(parsed);

    let printer = HIRPrinter::new(true, &type_db);

    let expected = "
def foo<T>(i: T (inferred: T), p: ptr<T> (inferred: ptr<T>)) -> T (return inferred: T):
    x : 't1 = {{i: T} + {2: i32}: 't1} [synth]
    y : 't3 = {{x: 't1} * {98: i32}: 't3} [synth]
    z : 't6 = {{y: 't3} - {*{p: ptr<T>}: 't5}: 't6} [synth]
    return {{x: 't1} + {z: 't6}: 't7}
    ";

    let result = printer.print_hir(&typed);
    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_call_function() {
    let parsed = test_utils::parse_no_stdlib(
        "
def foo(x: i32) -> i32:
    return x

def bar(y: i32) -> i32:
    return foo(y)
",
    );

    let mut type_db = TypeConstructorDatabase::new();
    let mut meta_table = MetaTable::new();
    let parsed = ast_globals_to_hir(&parsed.file_table[0].ast, &type_db, &mut meta_table);
    let mut typer = Typer::new(&mut type_db);
    let typed = typer.assign_types(parsed);
    let printer = HIRPrinter::new(true, &type_db);
    let expected = "
def foo(x: i32 (inferred: i32)) -> i32 (return inferred: i32):
    return {x: i32}
def bar(y: i32 (inferred: i32)) -> i32 (return inferred: i32):
    return {{foo: (i32) -> i32}({y: i32}): i32}
    ";

    let result = printer.print_hir(&typed);
    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_call_function_different_type() {
    let parsed = test_utils::parse_no_stdlib(
        "
def foo(x: f32) -> f32:
    return x

def bar(y: i32) -> i32:
    return foo(y)
",
    );

    let mut type_db = TypeConstructorDatabase::new();
    let mut meta_table = MetaTable::new();
    let parsed = ast_globals_to_hir(&parsed.file_table[0].ast, &type_db, &mut meta_table);
    let mut typer = Typer::new(&mut type_db);
    let typed = typer.assign_types(parsed);
    let printer = HIRPrinter::new(true, &type_db);

    //won't type check
    let expected = "
def foo(x: f32 (inferred: f32)) -> f32 (return inferred: f32):
    return {x: f32}
def bar(y: i32 (inferred: i32)) -> i32 (return inferred: i32):
    return {{foo: (f32) -> f32}({y: i32}): f32}
    ";

    let result = printer.print_hir(&typed);
    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_call_generic() {
    let parsed = test_utils::parse_no_stdlib(
        "
def id<T>(x: T) -> T:
    return x

def bar(y: i32) -> i32:
    return id<i32>(y)
",
    );

    let mut type_db = TypeConstructorDatabase::new();
    let mut meta_table = MetaTable::new();
    let parsed = ast_globals_to_hir(&parsed.file_table[0].ast, &type_db, &mut meta_table);
    let mut typer = Typer::new(&mut type_db);
    let typed = typer.assign_types(parsed);
    let printer = HIRPrinter::new(true, &type_db);

    //won't type check
    let expected = "
def id<T>(x: T (inferred: T)) -> T (return inferred: T):
    return {x: T}
def bar(y: i32 (inferred: i32)) -> i32 (return inferred: i32):
    return {{id: (i32) -> i32}<i32>({y: i32}): i32}
    ";

    let result = printer.print_hir(&typed);
    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_call_ptr_t_id() {
    let parsed = test_utils::parse_no_stdlib(
        "
def id<T>(x: ptr<T>) -> T:
    return *x

def bar() -> i32:
    x = 1.0
    y = &x
    return id<f32>(y)
",
    );

    let mut type_db = TypeConstructorDatabase::new();
    let mut meta_table = MetaTable::new();
    let parsed = ast_globals_to_hir(&parsed.file_table[0].ast, &type_db, &mut meta_table);
    let mut typer = Typer::new(&mut type_db);
    let typed = typer.assign_types(parsed);
    let printer = HIRPrinter::new(true, &type_db);

    //won't type check
    let expected = "
def id<T>(x: ptr<T> (inferred: ptr<T>)) -> T (return inferred: T):
    return {*{x: ptr<T>}: 't0}
def bar() -> i32 (return inferred: i32):
    x : f32 = {1.0: f32} [synth]
    y : ptr<f32> = {&{x: f32}: ptr<f32>} [synth]
    return {{id: (ptr<f32>) -> f32}<f32>({y: ptr<f32>}): f32}
";

    let result = printer.print_hir(&typed);
    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}

#[test]
fn test_call_ptr_t_id_without_passing_type_params_let_inference_run() {
    let parsed = test_utils::parse_no_stdlib(
        "
def id<T>(x: ptr<T>) -> T:
    return *x

def bar() -> i32:
    x = 1
    y = &x
    return id(y)
",
    );

    let mut type_db = TypeConstructorDatabase::new();
    let mut meta_table = MetaTable::new();
    let parsed = ast_globals_to_hir(&parsed.file_table[0].ast, &type_db, &mut meta_table);
    let mut typer = Typer::new(&mut type_db);
    let typed = typer.assign_types(parsed);
    let printer = HIRPrinter::new(true, &type_db);

    let expected = "
def id<T>(x: ptr<T> (inferred: ptr<T>)) -> T (return inferred: T):
    return {*{x: ptr<T>}: 't0}
def bar() -> i32 (return inferred: i32):
    x : i32 = {1: i32} [synth]
    y : ptr<i32> = {&{x: i32}: ptr<i32>} [synth]
    return {{id: (ptr<i32>) -> i32}({y: ptr<i32>}): i32}
";

    let result = printer.print_hir(&typed);
    println!("{result}");

    assert_eq!(expected.trim(), result.trim());
}
