#[cfg(test)]
use crate::{
    semantic::{context::Source, mir_printer},
    types::diagnostics::TypeErrorPrinter,
};

#[cfg(test)]
use super::hir::{HIRRoot, MetaTable};

#[cfg(test)]
use crate::{
    semantic::{mir::hir_to_mir, monomorph::Monomorphizer},
    types::{diagnostics::TypeErrors, type_constructor_db::TypeConstructorDatabase},
};

#[cfg(test)]
use crate::semantic::{context::test_utils, hir::ast_globals_to_hir};

#[cfg(test)]
use crate::semantic::{hir_printer::HIRPrinter, typer::Typer};

#[cfg(test)]
fn setup(
    src: &'static str,
) -> (
    Vec<super::mir::MIRTopLevelNode>,
    String,
    TypeConstructorDatabase,
    MetaTable,
    Source,
    Vec<HIRRoot>,
    String,
    Result<(), ()>,
    TypeErrors,
) {
    use crate::semantic::type_checker;

    let mut type_db = TypeConstructorDatabase::new();
    let mut meta_table = MetaTable::new();

    let basic_types = test_utils::parse_basic_types();

    for file in basic_types.file_table.iter() {
        let mut parsed = ast_globals_to_hir(&file.ast, &type_db, &mut meta_table);
        let mut typer = Typer::new(&mut type_db);
        typer.forgive_skolem_mismatches();
        if typer.assign_types(&mut parsed).is_err() {
            panic!("Failed to assign types while processing basic types, this is a bug");
        }
        //discarded - not needed
        let mut mono = Monomorphizer::new(&mut type_db);
        mono.run(&mut parsed).unwrap();
    }

    let original_src = test_utils::parse_no_stdlib(src);

    let mut parsed = ast_globals_to_hir(&original_src.file_table[0].ast, &type_db, &mut meta_table);

    let (mut compiler_errors, tc_result) = {
        let tc_result = {
            let mut typer = Typer::new(&mut type_db);
            typer.forgive_skolem_mismatches();
            let tc_result = typer.assign_types(&mut parsed);
            (typer.compiler_errors, tc_result)
        };
        let mut mono = Monomorphizer::new(&mut type_db);

        mono.run(&mut parsed).unwrap();
        parsed = mono.get_result();
        tc_result
    };

    let hir_string = {
        let printer = HIRPrinter::new(true, &type_db);
        printer.print_hir(&parsed)
    };

    let mir = hir_to_mir(parsed.clone(), &mut compiler_errors).unwrap();
    let printed_mir = mir_printer::print_mir(&mir, &type_db);

    let _ = type_checker::typecheck(&mir, &type_db, &mut compiler_errors);

    let printer = TypeErrorPrinter::new(
        &compiler_errors,
        &type_db,
        &meta_table,
        &original_src.file_table,
    );
    println!("{printer}");

    (
        mir,
        printed_mir,
        type_db,
        meta_table,
        original_src,
        parsed,
        hir_string,
        tc_result,
        compiler_errors,
    )
}

//create a macro for the pattern where no errors are expected
#[cfg(test)]
macro_rules! no_errors {
    ($code:expr) => {
        let (.., compiler_errors) = setup($code);
        assert_eq!(0, compiler_errors.len());
    };
}

#[test]
fn return_int_from_int_func_is_correct() {
    no_errors!(
        "
def main() -> i32:
    return 1
    "
    );
}

#[test]
fn return_from_void_func_is_correct() {
    no_errors!(
        "
def main():
    return
"
    );
}

#[test]
fn return_int_from_void_is_not_correct() {
    let (.., err) = setup(
        "
def main():
    return 1
",
    );
    assert!(err.len() >= 1);
}

#[test]
fn return_void_from_int_func_is_not_correct() {
    let (.., err) = setup(
        "
def main() -> i32:
    return
    ",
    );

    assert!(err.len() >= 1);
}

#[test]
fn assign_incorrect_type_literall() {
    let (.., err) = setup(
        "
def main():
    x: i32 = 1.0
",
    );

    assert!(err.len() >= 1);
}

#[test]
fn type_check_function_call_no_args_correct_types() {
    no_errors!(
        "
def test() -> i32:
    return 1

def main():
    x: i32 = test()
"
    );
}

#[test]
fn type_check_wrong_type_function_call_return_incompatible() {
    let (.., err) = setup(
        "
def test() -> i32:
    return 1

def main():
    x: f32 = test()
",
    );

    assert_eq!(1, err.len());
    assert_eq!(1, err.unify_error.len());
}

#[test]
fn type_check_binary_expr_result_wrong_type() {
    let (.., err) = setup(
        "
def test() -> i32:
    return 1

def main():
    x: f32 = test() + 1
",
    );

    assert_eq!(1, err.len());
    assert_eq!(1, err.unify_error.len());
}

#[test]
fn pass_correct_type_to_function_single_args() {
    no_errors!(
        "
def test(i: i32) -> i32:
    return i + 1

def main():
    test(1)
"
    );
}

#[test]
fn pass_correct_type_to_function_two_args() {
    no_errors!(
        "
def test(i: i32, f: f32) -> i32:
    return i + 1

def main():
    test(1, 1.0)
"
    );
}

#[test]
fn pass_correct_type_to_function_two_args_from_vars() {
    no_errors!(
        "
def test(i: i32, f: f32) -> i32:
    return i + 1

def main():
    i = 1
    f = 1.0
    test(i, f)
"
    );
}

#[test]
fn pass_wrong_type_to_function_single_arg() {
    let (.., err) = setup(
        "
def test(i: i32) -> i32:
    return i

def main():
    s = \"abc\"
    test(s)
",
    );

    assert_eq!(1, err.len());
    assert_eq!(1, err.function_call_mismatches.len());
}

#[test]
fn pass_wrong_type_to_function_two_args_both_wrong() {
    let (.., err) = setup(
        "
def test(i: i32, f: f32) -> f32:
    return f

def main():
    s = \"abc\"
    i = 100
    test(s, i)
",
    );

    assert_eq!(2, err.len());
    assert_eq!(2, err.function_call_mismatches.len());
}

#[test]
fn can_ref_variable() {
    no_errors!(
        "
def main():
    x = 1
    y = &x
"
    );
}

#[test]
fn can_ref_array_index() {
    no_errors!(
        "
def main():
    x = [1,2,3]
    y = &x[0]
"
    );
}

#[test]
fn ref_array_index_is_ptr_type() {
    no_errors!(
        "
def main():
    x = [1,2,3]
    y: ptr<i32> = &x[0]
"
    );
}

#[test]
fn ref_to_deref_works_in_type_system() {
    no_errors!(
        "
def main():
    x = [1,2,3]
    y: ptr<i32> = &*&x[0]
"
    );
}

//this "works in type system" means we accept it as an input but we might remove the &* and *& because they cancel out
#[test]
fn deref_to_ref_works_in_type_system() {
    no_errors!(
        "
def main():
    x = [1,2,3]
    y: i32 = *&x[0]
"
    );
}

#[test]
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
