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
    use crate::semantic::{mir::hir_to_mir, mir_printer, monomorph::Monomorphizer};
    use crate::semantic::uniformizer;
    

    let mut type_db = TypeConstructorDatabase::new();
    let mut meta_table = MetaTable::new();
    let original_src = test_utils::parse_no_stdlib(src);

    let mut parsed = ast_globals_to_hir(&original_src.file_table[0].ast, &type_db, &mut meta_table);

    let (mut compiler_errors, tc_result) = 'tc: {

        let mut typer = Typer::new(&mut type_db);
        typer.forgive_skolem_mismatches();
        let tc_result = typer.assign_types(&mut parsed);

        if tc_result.is_err() {
            //return here on the let (mut compiler_errors, tc_result) = { line
            break 'tc (typer.compiler_errors, tc_result)
        }

        let mut mono = Monomorphizer::new(&type_db);

        mono.run(&parsed).unwrap();
        let (mut monomorphized, mono_structs) = mono.get_result();

        let replacements = uniformizer::uniformize(&mut type_db, &mut monomorphized, &mono_structs);

        let mut final_typer = Typer::new(&mut type_db);
        final_typer.set_monomorphized_versions(replacements);
        let tc_result_2 = final_typer.assign_types(&mut monomorphized);
        let full_compiler_errors = final_typer.compiler_errors;

        //uniformize again - typer will redo some types
        uniformizer::uniformize(&mut type_db, &mut monomorphized, &mono_structs);
        parsed = monomorphized;
        (full_compiler_errors, tc_result_2)
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

    let mir = hir_to_mir(parsed.clone(), &mut compiler_errors).unwrap();
    let printed_mir = mir_printer::print_mir(&mir, &type_db);
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

#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn simplest_case() {
    let (_, final_result, ..) = setup(
        "
def main() -> i32:
    return 1",
    );

    println!("{final_result}");
    let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defblock 0:
        usescope 0
        return 1";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn set_variable() {
    let (_, final_result, ..) = setup(
        "
def main():
    x = 1",
    );

    println!("{final_result}");
    let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
        x : i32
    defblock 0:
        usescope 0
        x = 1
        return";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn just_return_parameter() {
    let (_, final_result, ..) = setup(
        "
def main(x: i32) -> i32:
    return x",
    );

    println!("{final_result}");
    let expected = "
def main(x: i32) -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defblock 0:
        usescope 0
        return x";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn many_parameters_are_added_to_scope() {
    let (_, final_result, ..) = setup(
        "
def main(x: i32, y: i64, z: f64, name: u32) -> i32:
    return x",
    );

    println!("{final_result}");
    let expected = "
def main(x: i32, y: i64, z: f64, name: u32) -> i32:
    defscope 0:
        inheritscope 0
        x : i32
        y : i64
        z : f64
        name : u32
    defblock 0:
        usescope 0
        return x";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn simple_expression() {
    let (_, final_result, ..) = setup(
        "
def main(x: i32) -> i32:
    return x + 1",
    );

    println!("{final_result}");
    let expected = "
def main(x: i32) -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defblock 0:
        usescope 0
        return x + 1";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn create_variable() {
    //@TODO replace by new result
    let (_, final_result, ..) = setup(
        "
def main(x: i32) -> i32:
    y = 0
    return x + y
",
    );

    println!("{final_result}");
    let expected = "
def main(x: i32) -> i32:
    defscope 0:
        inheritscope 0
        x : i32
        y : i32
    defscope 1:
        inheritscope 0
    defblock 0:
        usescope 0
        y = 0
        gotoblock 1
    defblock 1:
        usescope 1
        return x + y";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn multiple_variables_and_expressions() {
    let (_, final_result, ..) = setup(
        "
def main(x: i32) -> i32:
    y = 1
    z: i32 = 2 + x
    return x / (y + z)",
    );

    println!("{final_result}");
    let expected = "
def main(x: i32) -> i32:
    defscope 0:
        inheritscope 0
        x : i32
        y : i32
    defscope 1:
        inheritscope 0
        z : i32
    defscope 2:
        inheritscope 1
    defblock 0:
        usescope 0
        y = 1
        gotoblock 1
    defblock 1:
        usescope 1
        z = 2 + x
        gotoblock 2
    defblock 2:
        usescope 2
        return x / y + z";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn set_same_variable_multiple_times() {
    let (_, final_result, ..) = setup(
        "
def main() -> i32:
    y = 1
    y = 2
    y = 3
    y = 4",
    );

    println!("{final_result}");
    let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
        y : i32
    defscope 1:
        inheritscope 0
    defblock 0:
        usescope 0
        y = 1
        gotoblock 1
    defblock 1:
        usescope 1
        y = 2
        y = 3
        y = 4
        return
    ";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn set_and_use() {
    let (_, final_result, ..) = setup(
        "
def main():
    y = 1
    x = y + 1",
    );

    println!("{final_result}");
    let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
        y : i32
    defscope 1:
        inheritscope 0
        x : i32
    defblock 0:
        usescope 0
        y = 1
        gotoblock 1
    defblock 1:
        usescope 1
        x = y + 1
        return
    ";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn if_statement() {
    let (_, final_result, ..) = setup(
        "
def main():
    y = 1
    if y == 1:
        y = y + 1",
    );

    println!("{final_result}");
    let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
        y : i32
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 1
    defblock 0:
        usescope 0
        y = 1
        gotoblock 1
    defblock 1:
        usescope 1
        if y == 1:
            gotoblock 2
        else:
            gotoblock 3
    defblock 2:
        usescope 2
        y = y + 1
        return
    defblock 3:
        usescope 2
        return
        ";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn if_code_in_both_branches() {
    let (_, final_result, ..) = setup(
        "
def print(x: i32):
    intrinsic

def main():
    if True:
        print(1)
    else:
        print(2)",
    );

    println!("{final_result}");
    let expected = "
def print(x: i32) -> Void
def main() -> Void:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 0
    defblock 0:
        usescope 0
        if True:
            gotoblock 1
        else:
            gotoblock 2
    defblock 1:
        usescope 1
        print(1)
        return
    defblock 2:
        usescope 2
        print(2)
        return
        ";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn if_statement_return_deeply_nested() {
    let (_, final_result, ..) = setup(
        "
def main() -> i32:
    if True:
        x = 1
        if True:
            return 1
    y = 1
    return 2
",
    );

    println!("{final_result}");
    let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        y : i32
    defscope 2:
        inheritscope 1
    defscope 3:
        inheritscope 0
        x : i32
    defscope 4:
        inheritscope 3
    defscope 5:
        inheritscope 4
    defblock 0:
        usescope 0
        if True:
            gotoblock 3
        else:
            gotoblock 1
    defblock 1:
        usescope 1
        y = 1
        gotoblock 2
    defblock 2:
        usescope 2
        return 2
    defblock 3:
        usescope 3
        x = 1
        gotoblock 4
    defblock 4:
        usescope 4
        if True:
            gotoblock 5
        else:
            gotoblock 1
    defblock 5:
        usescope 5
        return 1
    ";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn if_return_in_both_branches() {
    let (_, final_result, ..) = setup(
        "
def main() -> i32:
    if True:
        return 1
    else:
        return 2",
    );

    println!("{final_result}");
    let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 0
    defblock 0:
        usescope 0
        if True:
            gotoblock 1
        else:
            gotoblock 2
    defblock 1:
        usescope 1
        return 1
    defblock 2:
        usescope 2
        return 2
    ";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn if_statements_decls_inside_branches() {
    let (_, final_result, ..) = setup(
        "
def main() -> i32:
    x = 0
    if True:
        y = x + 1
        return y
    else:
        x = 1
        y = 2 + x
        return x + y
",
    );

    println!("{final_result}");
    let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 1
        y : i32
    defscope 3:
        inheritscope 1
        y : i32
    defscope 4:
        inheritscope 2
    defscope 5:
        inheritscope 3
    defblock 0:
        usescope 0
        x = 0
        gotoblock 1
    defblock 1:
        usescope 1
        if True:
            gotoblock 2
        else:
            gotoblock 3
    defblock 2:
        usescope 2
        y = x + 1
        gotoblock 4
    defblock 3:
        usescope 3
        x = 1
        y = 2 + x
        gotoblock 5
    defblock 4:
        usescope 4
        return y
    defblock 5:
        usescope 5
        return x + y
";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn code_after_if_is_correctly_placed_true_branch_only() {
    let (_, final_result, ..) = setup(
        "
def print(x: i32):
    intrinsic

def main() -> i32:
    x = 0
    if x == 0:
        x = x + 1
    print(x)
",
    );

    println!("{final_result}");
    let expected = "
def print(x: i32) -> Void
def main() -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 1
    defscope 3:
        inheritscope 1
    defblock 0:
        usescope 0
        x = 0
        gotoblock 1
    defblock 1:
        usescope 1
        if x == 0:
            gotoblock 3
        else:
            gotoblock 2
    defblock 2:
        usescope 2
        print(x)
        return
    defblock 3:
        usescope 3
        x = x + 1
        gotoblock 2
";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn code_after_if_is_correctly_placed_return_on_false_branch() {
    let (_, final_result, ..) = setup(
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

    println!("{final_result}");
    let expected = "
def print(x: i32) -> Void
def main() -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 1
    defscope 3:
        inheritscope 1
    defscope 4:
        inheritscope 1
    defblock 0:
        usescope 0
        x = 0
        gotoblock 1
    defblock 1:
        usescope 1
        if x == 0:
            gotoblock 3
        else:
            gotoblock 4
    defblock 2:
        usescope 2
        return x
    defblock 3:
        usescope 3
        print(1)
        gotoblock 2
    defblock 4:
        usescope 4
        return x
    ";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn code_after_if_is_correctly_placed_true_and_false_branches() {
    let (_, final_result, ..) = setup(
        "
def print(x: i32):
    intrinsic

def main() -> i32:
    x = 0
    if x == 0:
        x = x + 1
    else:
        x = 2
    print(x)",
    );

    println!("{final_result}");
    let expected = "
def print(x: i32) -> Void
def main() -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 1
    defscope 3:
        inheritscope 1
    defscope 4:
        inheritscope 1
    defblock 0:
        usescope 0
        x = 0
        gotoblock 1
    defblock 1:
        usescope 1
        if x == 0:
            gotoblock 3
        else:
            gotoblock 4
    defblock 2:
        usescope 2
        print(x)
        return
    defblock 3:
        usescope 3
        x = x + 1
        gotoblock 2
    defblock 4:
        usescope 4
        x = 2
        gotoblock 2
";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn if_one_branch_does_not_return() {
    let (_, final_result, ..) = setup(
        "
def print(x: i32):
    intrinsic

def main() -> i32:
    x = 0
    if True:
        y = x + 1
        print(x)
    else:
        x = 1
        y = 2 + x
        return x + y
",
    );

    println!("{final_result}");
    let expected = "
def print(x: i32) -> Void
def main() -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 1
        y : i32
    defscope 3:
        inheritscope 1
        y : i32
    defscope 4:
        inheritscope 2
    defscope 5:
        inheritscope 3
    defblock 0:
        usescope 0
        x = 0
        gotoblock 1
    defblock 1:
        usescope 1
        if True:
            gotoblock 2
        else:
            gotoblock 3
    defblock 2:
        usescope 2
        y = x + 1
        gotoblock 4
    defblock 3:
        usescope 3
        x = 1
        y = 2 + x
        gotoblock 5
    defblock 4:
        usescope 4
        print(x)
        return
    defblock 5:
        usescope 5
        return x + y
";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn if_nested_branch_all_returns() {
    let (_, final_result, ..) = setup(
        "
def main() -> i32:
    if True:
        x = 1
        if 1 == 1:
            x = x + 3
            return x
        else:
            x = x + 1
            return x
    else:
        y = 3
        if 2 == 2:
            return y + 1
        else:
            return 4 * y
    ",
    );

    println!("{final_result}");
    let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        x : i32
    defscope 2:
        inheritscope 0
        y : i32
    defscope 3:
        inheritscope 1
    defscope 4:
        inheritscope 2
    defscope 5:
        inheritscope 3
    defscope 6:
        inheritscope 3
    defscope 7:
        inheritscope 4
    defscope 8:
        inheritscope 4
    defblock 0:
        usescope 0
        if True:
            gotoblock 1
        else:
            gotoblock 2
    defblock 1:
        usescope 1
        x = 1
        gotoblock 3
    defblock 2:
        usescope 2
        y = 3
        gotoblock 4
    defblock 3:
        usescope 3
        if 1 == 1:
            gotoblock 5
        else:
            gotoblock 6
    defblock 4:
        usescope 4
        if 2 == 2:
            gotoblock 7
        else:
            gotoblock 8
    defblock 5:
        usescope 5
        x = x + 3
        return x
    defblock 6:
        usescope 6
        x = x + 1
        return x
    defblock 7:
        usescope 7
        return y + 1
    defblock 8:
        usescope 8
        return 4 * y";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn if_nested_branch_but_one_does_not_return() {
    let (_, final_result, ..) = setup(
        "
def print(x: i32):
    intrinsic

def main() -> i32:
    if True:
        x = 1
        if 1 == 1:
            x = x + 3
            return x
        else:
            x = x + 1
            print(x)
        print(3143)
    else:
        y = 3
        if 2 == 2:
            y = y + 1
            print(y)
        else:
            return 4 * y
",
    );

    println!("{final_result}");
    let expected = "
def print(x: i32) -> Void
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        x : i32
    defscope 2:
        inheritscope 0
        y : i32
    defscope 3:
        inheritscope 1
    defscope 4:
        inheritscope 3
    defscope 5:
        inheritscope 2
    defscope 6:
        inheritscope 3
    defscope 7:
        inheritscope 3
    defscope 8:
        inheritscope 5
    defscope 9:
        inheritscope 5
    defblock 0:
        usescope 0
        if True:
            gotoblock 1
        else:
            gotoblock 2
    defblock 1:
        usescope 1
        x = 1
        gotoblock 3
    defblock 2:
        usescope 2
        y = 3
        gotoblock 5
    defblock 3:
        usescope 3
        if 1 == 1:
            gotoblock 6
        else:
            gotoblock 7
    defblock 4:
        usescope 4
        print(3143)
        return
    defblock 5:
        usescope 5
        if 2 == 2:
            gotoblock 8
        else:
            gotoblock 9
    defblock 6:
        usescope 6
        x = x + 3
        return x
    defblock 7:
        usescope 7
        x = x + 1
        print(x)
        gotoblock 4
    defblock 8:
        usescope 8
        y = y + 1
        print(y)
        return
    defblock 9:
        usescope 9
        return 4 * y";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn set_some_vars_exprs() {
    let (_, final_result, ..) = setup(
        "
def main():
    x : i32 = 15
    y : i32 = 3
    z : i32 = x + y
    result: i32 = 5 + z
    result = result + y",
    );

    println!("{final_result}");
    let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
        x : i32
    defscope 1:
        inheritscope 0
        y : i32
    defscope 2:
        inheritscope 1
        z : i32
    defscope 3:
        inheritscope 2
        result : i32
    defscope 4:
        inheritscope 3
    defblock 0:
        usescope 0
        x = 15
        gotoblock 1
    defblock 1:
        usescope 1
        y = 3
        gotoblock 2
    defblock 2:
        usescope 2
        z = x + y
        gotoblock 3
    defblock 3:
        usescope 3
        result = 5 + z
        gotoblock 4
    defblock 4:
        usescope 4
        result = result + y
        return
";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn ref_deref_pattern_is_cancelled_out() {
    let (_, final_result, ..) = setup(
        "
def main(args: array<i32>) -> ptr<i32>:
    return &args[0]",
    );

    //the return would be actually &*args.__index_ptr__(0) but we should cancel out the deref &*
    let expected = "
def main(args: array<i32>) -> ptr<i32>:
    defscope 0:
        inheritscope 0
        args : array<i32>
    defblock 0:
        usescope 0
        return args.__index_ptr__(0)
";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn deref_ref_pattern_is_cancelled_out() {
    let (_, final_result, ..) = setup(
        "
def main():
    some_var = 1
    deref_ref = *&some_var
",
    );

    println!("{final_result}");
    //the return would be actually &*args.__index_ptr__(0) but we should cancel out the deref &*
    let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
        some_var : i32
    defscope 1:
        inheritscope 0
        deref_ref : i32
    defblock 0:
        usescope 0
        some_var = 1
        gotoblock 1
    defblock 1:
        usescope 1
        deref_ref = some_var
        return
";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn multiple_ref_deref_cancellation() {
    let (_, final_result, ..) = setup(
        "
def main(args: array<i32>) -> ptr<i32>:
    return &*&*&args[0]",
    );

    let expected = "
def main(args: array<i32>) -> ptr<i32>:
    defscope 0:
        inheritscope 0
        args : array<i32>
    defblock 0:
        usescope 0
        return args.__index_ptr__(0)
";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn multiple_deref_ref_cancellation() {
    let (_, final_result, ..) = setup(
        "
def main(args: array<i32>) -> i32:
    return *&*&args[0]",
    );

    let expected = "
def main(args: array<i32>) -> i32:
    defscope 0:
        inheritscope 0
        args : array<i32>
    defblock 0:
        usescope 0
        return *args.__index_ptr__(0)
";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn while_loop() {
    let (_, final_result, ..) = setup(
        "
def main():
    i = 10
    while i > 0:
        x = i + 1
        i = i - 1
",
    );

    println!("{final_result}");
    let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
        i : i32
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 1
        x : i32
    defscope 3:
        inheritscope 2
    defblock 0:
        usescope 0
        i = 10
        gotoblock 1
    defblock 1:
        usescope 1
        if i > 0:
            gotoblock 2
        else:
            gotoblock 3
    defblock 2:
        usescope 2
        x = i + 1
        gotoblock 4
    defblock 3:
        usescope 2
        return
    defblock 4:
        usescope 3
        i = i - 1
        gotoblock 1
";
    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn assign_to_second_variable_inside_block() {
    //this tests that the block scopes are inheriting correctly
    let (_, final_result, ..) = setup(
        "
def main() -> i32:
    x = 0
    y = 0
    if x < 99:
        y = y + 1
    return y
",
    );

    println!("{final_result}");
    let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defscope 1:
        inheritscope 0
        y : i32
    defscope 2:
        inheritscope 1
    defscope 3:
        inheritscope 2
    defscope 4:
        inheritscope 2
    defblock 0:
        usescope 0
        x = 0
        gotoblock 1
    defblock 1:
        usescope 1
        y = 0
        gotoblock 2
    defblock 2:
        usescope 2
        if x < 99:
            gotoblock 4
        else:
            gotoblock 3
    defblock 3:
        usescope 3
        return y
    defblock 4:
        usescope 4
        y = y + 1
        gotoblock 3
";
    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn simple_struct() {
    let (_, final_result, ..) = setup(
        "
struct MyStruct:
    x: i32

def main():
    s = MyStruct()
    s.x = 1",
    );

    println!("{final_result}");
    let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
        s : MyStruct
    defscope 1:
        inheritscope 0
    defblock 0:
        usescope 0
        s = MyStruct()
        gotoblock 1
    defblock 1:
        usescope 1
        s.x = 1
        return";

    assert_eq!(expected.trim(), final_result.trim());
}

#[test]
fn simple_struct_impl() {
    let (_, final_result, ..) = setup(
        "
struct MyStruct:
    x: i32

impl MyStruct:
    def get_x(self) -> i32:
        return (*self).x

def main():
    s = MyStruct()
    s.x = 1
    s.get_x()
",
    );

    println!("{final_result}");
    let expected = "
def MyStruct::get_x() -> i32:
    defscope 0:
        inheritscope 0
    defblock 0:
        usescope 0
        return *self.x
def main() -> Void:
    defscope 0:
        inheritscope 0
        s : MyStruct
    defscope 1:
        inheritscope 0
    defblock 0:
        usescope 0
        s = MyStruct()
        gotoblock 1
    defblock 1:
        usescope 1
        s.x = 1
        s.get_x()
        return";

    assert_eq!(expected.trim(), final_result.trim());
}



#[test]
fn generic_struct_impl() {
    let (_, final_result, ..) = setup(
        "
struct MyStruct<T>:
    x: T

impl MyStruct<T>:
    def get_x(self) -> T:
        return (*self).x

def main():
    s = MyStruct<i32>()
    s.x = 1
    s.get_x()
",
    );

    println!("{final_result}");
    let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
        s : MyStruct[i32]
    defscope 1:
        inheritscope 0
    defblock 0:
        usescope 0
        s = MyStruct[i32]()
        gotoblock 1
    defblock 1:
        usescope 1
        s.x = 1
        s.get_x()
        return
def MyStruct[i32]::get_x() -> i32:
    defscope 0:
        inheritscope 0
    defblock 0:
        usescope 0
        return *self.x";

    assert_eq!(expected.trim(), final_result.trim());
}
