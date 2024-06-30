use core::panic;
use std::fs;

use super::hir::{ast_globals_to_hir, InferredTypeHIRRoot, MonomorphizedHIRRoot};
use super::mir::{hir_to_mir, MIRTopLevelNode};
use super::monomorph::Monomorphizer;
use super::struct_instantiations;
use super::top_level_decls::NameRegistry;
use super::type_checker::typecheck;

use crate::ast::lexer::{TokenSpanIndex, TokenTable};
use crate::ast::parser::{print_errors, AstSpan, Parser};
use crate::ast::{lexer, parser};
use crate::interner::InternedString;
use crate::semantic::{first_assignments, mir_printer, top_level_decls, type_inference};
use crate::types::diagnostics::TypeErrorPrinter;
use crate::types::type_constructor_db::{TypeConstructParams, TypeKind};
use crate::types::type_instance_db::{TypeInstanceId, TypeInstanceManager};
use crate::{ast::parser::AST, types::diagnostics::TypeErrors};

#[derive(Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub struct FileTableIndex(pub usize);

#[derive(Debug)]
pub struct FileTableEntry {
    pub path: String,
    pub index: FileTableIndex,
    pub ast: AST,
    pub contents: &'static str,
    pub token_table: TokenTable,
}

#[derive(Debug)]
pub struct Source {
    pub file_table: Vec<FileTableEntry>,
}

impl Source {
    pub fn new() -> Source {
        Source { file_table: vec![] }
    }
}
impl Source {
    #[allow(dead_code)]
    pub fn load_str_ref(self: &mut Source, file_name: &str, source: &str) -> bool {
        self.load(file_name.to_string(), source.to_string())
    }

    pub fn load(self: &mut Source, file_name: String, source: String) -> bool {
        let prev_count = self.file_table.len();
        let mut file_table_entry = FileTableEntry {
            path: file_name,
            index: FileTableIndex(prev_count),
            ast: AST::Break(AstSpan {
                //this gets replaced
                start: TokenSpanIndex {
                    file: FileTableIndex(0),
                    index: 0,
                },
                end: TokenSpanIndex {
                    file: FileTableIndex(0),
                    index: 0,
                },
            }),
            contents: source.leak(),
            token_table: TokenTable {
                tokens: vec![],
                spans: vec![],
            },
        };
        file_table_entry.token_table =
            lexer::tokenize(FileTableIndex(prev_count), file_table_entry.contents).unwrap();

        self.file_table.push(file_table_entry);
        let last = &self.file_table.last().unwrap();
        let mut parser = Parser::new(&last.token_table);
        let result = parser.parse_ast();

        if !parser.errors.is_empty() {
            print_errors(&parser, &self.file_table, &last.token_table);
            return false;
        }

        //println!("{}\n", print_ast(&ast, &self.interner));
        let root = parser::AST::Root(result);
        self.file_table.last_mut().unwrap().ast = root;

        return true;
    }

    pub fn load_file(&mut self, file_location: &str) -> bool {
        //println!("LOADING FILE {file_location}");
        let input = fs::read_to_string(file_location)
            .unwrap_or_else(|_| panic!("Could not read file {file_location}"));
        self.load(file_location.to_string(), input)
    }

    #[allow(dead_code)]
    pub fn load_stdlib(&mut self) -> bool {
        self.load_file("./stdlib/c_lib.dk");
        self.load_file("./stdlib/llvm_intrinsics.dk");
        self.load_file("./stdlib/asserts.dk");
        return true;
    }
}

pub struct Analyzer<'source> {
    pub mir: Vec<MIRTopLevelNode<'source>>,
    pub unchecked_mir: Vec<MIRTopLevelNode<'source>>,
    pub type_db: TypeInstanceManager,
    pub type_errors: TypeErrors<'source>,
    pub hir: Vec<InferredTypeHIRRoot<'source>>,
    pub hir_monomorphized: Vec<MonomorphizedHIRRoot<'source>>,
    source: &'source Source,
}

impl<'source> Analyzer<'source> {
    pub fn new(source: &'source Source) -> Analyzer<'source> {
        Analyzer {
            type_db: TypeInstanceManager::new(),
            type_errors: TypeErrors::new(),
            mir: vec![],
            hir: vec![],
            unchecked_mir: vec![],
            hir_monomorphized: vec![],
            source,
        }
    }

    pub fn last_hir(&self, n: usize) -> &[InferredTypeHIRRoot<'source>] {
        &self.hir[self.hir.len() - n..]
    }

    pub fn last_hir_mono(&self, n: usize) -> &[MonomorphizedHIRRoot<'source>] {
        &self.hir_monomorphized[self.hir_monomorphized.len() - n..]
    }

    #[allow(dead_code)]
    pub fn print_errors(&self) {
        println!(
            "{}",
            TypeErrorPrinter::new(&self.type_errors, &self.type_db, &self.source.file_table)
        )
    }

    pub fn ensure_instantiate_builtin_type(&mut self, name: &str) {
        //check if instance already exists
        let type_data_type = self.type_db.find_by_name(name);

        match type_data_type {
            Some(_) => {}
            None => {
                let type_data_type_constructor = self
                    .type_db
                    .constructors
                    .find_by_name(InternedString::new(name));

                match type_data_type_constructor {
                    Some(type_data_type_constructor) => {
                        self.type_db
                            .construct_type(type_data_type_constructor.id, &[])
                            .unwrap();
                    }
                    None => {
                        //TODO: Fix: navigate through all types and roots before running inference,
                        //otherwise we can't use types in stdlib
                        log!("Type {} not found in stdlib", name);
                    }
                }
            }
        }
    }

    #[allow(dead_code)]
    pub fn generate_mir_without_typecheck(&mut self, source: &'source Source) {
        self.generate_mir_and_typecheck(source, false);
    }
    pub fn analyze(&mut self, source: &'source Source) {
        self.generate_mir_and_typecheck(source, true);
    }
    pub fn generate_mir_and_typecheck(&mut self, source: &'source Source, do_typecheck: bool) {
        //stage 1: conversion to HIR
        let mut globals: NameRegistry<TypeConstructParams> = NameRegistry::new();
        let all_hir = source
            .file_table
            .iter()
            .flat_map(|file| ast_globals_to_hir(&file.ast))
            .collect::<Vec<_>>();

        let inferred_globals_hir = match top_level_decls::build_name_registry_and_resolve_signatures(
            &mut self.type_db,
            &mut globals,
            &mut self.type_errors,
            all_hir,
        ) {
            Ok(hir) => hir,
            Err(e) => {
                let printer =
                    TypeErrorPrinter::new(&self.type_errors, &self.type_db, &source.file_table);
                panic!("build_name_registry_and_resolve_signatures {e:?}\n{printer}")
            }
        };

        let first_assignment_hir =
            first_assignments::transform_first_assignment_into_declaration(inferred_globals_hir);

        let struct_instantiations = struct_instantiations::construct_struct_instantiations(
            first_assignment_hir,
            &mut self.type_db,
        );

        let type_inferred = type_inference::infer_types(
            &mut globals,
            &mut self.type_db,
            struct_instantiations,
            &mut self.type_errors,
        );

        let type_inferred_hir = match type_inferred {
            Ok(hir) => hir,
            Err(e) => {
                println!(
                    "{e:?}\nType errors:\n{}",
                    TypeErrorPrinter::new(&self.type_errors, &self.type_db, &source.file_table,)
                );
                return;
            }
        };

        self.hir = type_inferred_hir.clone();

        let mut monomorphizer = Monomorphizer::new(&mut self.type_db, &mut self.type_errors);

        let mono_result = monomorphizer.run(type_inferred_hir);

        match mono_result {
            Ok(_) => {}
            Err(e) => {
                println!(
                    "{e:?}\nType errors:\n{}",
                    TypeErrorPrinter::new(&self.type_errors, &self.type_db, &source.file_table,)
                );
                return;
            }
        }

        let (monomorphized_hir, new_top_level_decls) = monomorphizer.get_result();



        self.hir_monomorphized = monomorphized_hir.clone();

        let mir = hir_to_mir(monomorphized_hir, &mut self.type_errors);

        let mut top_level_decls_instantiated = NameRegistry::<TypeInstanceId>::new();

        top_level_decls_instantiated.include(&new_top_level_decls);

        for (k, v) in globals.names.iter() {
            match v {
                TypeConstructParams::Parameterized(tc, args) if args.len() == 0 => {

                    let type_data = self.type_db.constructors.find(*tc);

                    if type_data.kind == TypeKind::Function && type_data.type_params.len() > 0 {
                        continue;
                    }

                    let instance = self
                        .type_db
                        .construct_type(*tc, &[])
                        .expect(&format!("Failed to construct type for name {}", k));
                    top_level_decls_instantiated.insert(k.clone(), instance);
                },
                _ => {}
            }
        }

        if let Ok(mir) = mir {
            let printed_mir = mir_printer::print_mir(&mir, &self.type_db);
            log!("MIR being typechecked: {printed_mir}");
            new_top_level_decls.print(&self.type_db);

            if do_typecheck {
                let typechecked =
                    typecheck(mir, &self.type_db, &top_level_decls_instantiated, &mut self.type_errors);

                if self.type_errors.count() == 0 {
                    self.mir.extend(typechecked.unwrap());
                }
            } else {
                self.unchecked_mir.extend(mir);
            }
        }
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{
        interner::InternedString, semantic::mir::MIRTopLevelNode,
        types::type_instance_db::TypeInstanceManager,
    };

    use super::{Analyzer, Source};

    pub fn istr(str: &'static str) -> InternedString {
        InternedString::new(str)
    }

    pub struct AnalysisWithoutTypecheckResult<'source> {
        pub mir: Vec<MIRTopLevelNode<'source>>,
        pub type_db: TypeInstanceManager,
    }

    pub fn parse<'a, 'f: 'a>(s: &'a str) -> Source {
        let mut source = Source::new();
        source.load_stdlib();
        source.load_str_ref("test", s);
        source
    }

    pub fn parse_no_std<'a, 'f: 'a>(s: &'a str) -> Source {
        let mut source = Source::new();
        source.load_str_ref("test", s);
        source
    }

    pub fn do_analysis(source: &Source) -> Analyzer {
        let mut ctx = Analyzer::new(source);
        ctx.analyze(source);
        ctx
    }

    pub fn do_analysis_no_typecheck(source: &Source) -> AnalysisWithoutTypecheckResult {
        let mut ctx = Analyzer::new(source);
        ctx.generate_mir_without_typecheck(source);
        AnalysisWithoutTypecheckResult {
            mir: ctx.unchecked_mir,
            type_db: ctx.type_db,
        }
    }
}

#[cfg(test)]
mod tests {

    #[cfg(test)]
    use pretty_assertions::assert_eq;

    use crate::{
        ast::lexer::Operator,
        interner::InternedString,
        semantic::{
            context::test_utils::{do_analysis, parse, parse_no_std},
            hir::HIRTypeDisplayer,
            hir_printer::HIRPrinter,
            mir_printer::print_mir, type_name_printer::TypeNamePrinter,
        },
    };

    use super::*;

    fn print_hir(hir: &[InferredTypeHIRRoot], analyzer: &Analyzer) -> String {
        HIRPrinter::new(&analyzer.type_db, false).print_hir(hir)
    }

    fn print_hir_mono(hir: &[MonomorphizedHIRRoot], analyzer: &Analyzer) -> String {
        HIRPrinter::new(&analyzer.type_db, false).print_hir(hir)
    }

    fn print_hir_mono_verbose(hir: &[MonomorphizedHIRRoot], analyzer: &Analyzer) -> String {
        HIRPrinter::new(&analyzer.type_db, true).print_hir(hir)
    }

    #[test]
    fn simple_assign_decl() {
        let parsed = parse_no_std(
            "
def my_function():
    x = 1",
        );

        println!("Parsed: {parsed:?}");

        let analyzed = do_analysis(&parsed);
        assert_eq!(analyzed.type_errors.count(), 0);

        let result = print_hir(analyzed.last_hir(1), &analyzed);

        let expected = "
def my_function() -> Void:
    x : i32 = 1";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn standalone_call_to_builtin_function() {
        let parsed = parse(
            "
def my_function():
    x = 1.0
    powf(x, 2.0)",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();

        assert_eq!(analyzed.type_errors.count(), 0);

        let result = print_hir(analyzed.last_hir(1), &analyzed);
        println!("{result}");

        let expected = "
def my_function() -> Void:
    x : f32 = 1.0
    powf(x, 2.0)";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn use_undeclared_var_in_expr() {
        let parsed = parse(
            "
def my_function():
    x = 1.0
    y = xx + 20",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();

        assert_eq!(analyzed.type_errors.count(), 1);
    }

    #[test]
    fn expr_call_to_builtin_function() {
        let parsed = parse(
            "
def my_function():
    x: f32 = powf(16.0, 2.0)
",
        );
        let analyzed = do_analysis(&parsed);
        assert_eq!(analyzed.type_errors.count(), 0);
        let result = print_hir(analyzed.last_hir(1), &analyzed);
        println!("{result}");

        let expected = "
def my_function() -> Void:
    x : f32 = powf(16.0, 2.0)";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn alternative_test() {
        let parsed = parse(
            "
def my_function(arg1: i32, arg2: i32) -> i32:
    return arg1 * arg2 / (arg2 - arg1)",
        );
        let analyzed = do_analysis(&parsed);
        assert_eq!(analyzed.type_errors.count(), 0);

        let result = print_hir(analyzed.last_hir(1), &analyzed);
        println!("Result: {result} {:#?}", analyzed.hir);

        let expected = "
def my_function(arg1: i32, arg2: i32) -> i32:
    return arg1 * arg2 / arg2 - arg1";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn default_void_return() {
        let parsed = parse(
            "
def main(args: array<str>):
    print_int(10)",
        );
        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        assert_eq!(analyzed.type_errors.count(), 0);
        let result = print_hir(analyzed.last_hir(1), &analyzed);

        let expected = "
def main(args: array<str>) -> Void:
    print_int(10)";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn infer_variable_type_as_int() {
        let parsed = parse(
            "
def main(args: array<str>):
    my_var = 10
    print_int(my_var)",
        );
        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        assert_eq!(analyzed.type_errors.count(), 0);
        let result = print_hir(analyzed.last_hir(1), &analyzed);

        let expected = "
def main(args: array<str>) -> Void:
    my_var : i32 = 10
    print_int(my_var)";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn infer_generic_type_as_str() {
        let parsed = parse(
            "
def main(args: array<str>):
    my_var = args[0]
    print(my_var)",
        );
        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        assert_eq!(analyzed.type_errors.count(), 0);
        //println!("Here is the hir: {:#?}", analyzed.hir.last().unwrap());
        let final_result = print_hir(analyzed.last_hir(1), &analyzed);
        println!("{final_result}");
        let expected = "
def main(args: array<str>) -> Void:
    my_var : str = *args.__index_ptr__(0)
    print(my_var)";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn infer_builtin_function_return_type() {
        let parsed = parse(
            "
def my_function() -> f32:
    x = 1.3 + ((sqrtf(16.0 / 4.0 + 2.0 * 2.1) / 2.0) * 4.0) + (3.0 * powf(2.0, 2.0))
    return x
",
        );
        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();

        let result = print_hir(analyzed.last_hir(1), &analyzed);

        println!("Result: \n{result}\n====end result");

        let expected = "
def my_function() -> f32:
    x : f32 = 1.3 + sqrtf(16.0 / 4.0 + 2.0 * 2.1) / 2.0 * 4.0 + 3.0 * powf(2.0, 2.0)
    return x
";

        let result_mir = print_mir(&analyzed.mir, &analyzed.type_db);

        println!("Result mir: \n{result_mir}\n====end result mir");

        assert_eq!(analyzed.type_errors.count(), 0);

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn infer_defined_function_return_type() {
        let parsed = parse(
            "
def sum(x: i32, y: i32) -> i32:
    return x + y

def main():
    my_var = sum(1, 2)
    print_int(my_var)",
        );
        let analyzed = do_analysis(&parsed);
        assert_eq!(analyzed.type_errors.count(), 0);
        let final_result = print_hir(analyzed.last_hir(2), &analyzed);
        println!("{final_result}");
        let expected = "
def sum(x: i32, y: i32) -> i32:
    return x + y
def main() -> Void:
    my_var : i32 = sum(1, 2)
    print_int(my_var)";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test] //#ignored
    fn integer_promotion_for_wildly_different_type() {
        let parsed = parse(
            "
def id(x: array<str>) -> str:
    first_item = x[0]
    return first_item

def main():
    my_var = id(1)
    print(my_var)",
        );
        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();

        assert_eq!(analyzed.type_errors.count(), 1);
    }

    #[test]
    fn functions_assignable_to_variables() {
        let parsed = parse(
            "
def id(x: i32) -> i32:
    return x

def main():
    my_func = id
    my_var = my_func(1)",
        );
        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();

        assert_eq!(analyzed.type_errors.count(), 0);
        let final_result = print_hir(analyzed.last_hir(2), &analyzed);
        println!("{final_result}");
        let expected = "
def id(x: i32) -> i32:
    return x
def main() -> Void:
    my_func : fn(i32) -> i32 = id
    my_var : i32 = my_func(1)";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn access_property_of_struct_and_infer_type() {
        let parsed = parse_no_std(
            "
def print_uint(x: u32):
    intrinsic

def main():
    my_array = [1, 2, 3]
    my_array_length = my_array.length
    print_uint(my_array_length)",
        );
        println!("Doing analysis");

        let analyzed = do_analysis(&parsed);

        assert_eq!(analyzed.type_errors.count(), 0);
        let final_result = print_hir(analyzed.last_hir(1), &analyzed);

        println!("HIR printed:\n{final_result}");
        let expected = "
def main() -> Void:
    my_array : array<i32> = [1, 2, 3]
    my_array_length : u32 = my_array.length
    print_uint(my_array_length)";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn return_expr() {
        let parsed = parse_no_std(
            "
def main(x: i32) -> i32:
    y = 0
    return x + y
",
        );
        let analyzed = do_analysis(&parsed);
        assert_eq!(analyzed.type_errors.count(), 0);
        let final_result = print_hir(analyzed.last_hir(1), &analyzed);
        println!("{final_result}");
        let expected = "
def main(x: i32) -> i32:
    y : i32 = 0
    return x + y";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn self_decl_read() {
        let parsed = parse(
            "
def main(x: i32) -> i32:
    y = y + 1
",
        );
        let analyzed = do_analysis(&parsed);
        let err = &analyzed.type_errors.variable_not_found[0];
        assert_eq!(err.error.variable_name, InternedString::new("y"));
    }

    #[test]
    fn self_decl_read_expr() {
        let parsed = parse_no_std(
            "
def main(x: i32) -> i32:
    a = 1
    b = 2
    y = (a + b * (x / y)) / 2
",
        );
        let analyzed = do_analysis(&parsed);
        let err = &analyzed.type_errors.variable_not_found[0];
        assert_eq!(err.error.variable_name, InternedString::new("y"));
    }

    #[test]
    fn if_return_both_branches() {
        let parsed = parse_no_std(
            "
def main(x: i32) -> i32:
    if x == 0:
        return 1
    else:
        return 2
",
        );
        let analyzed = do_analysis(&parsed);
        assert_eq!(analyzed.type_errors.count(), 0);
        let final_result = print_hir(analyzed.last_hir(1), &analyzed);
        println!("{final_result}");
        let expected = "
def main(x: i32) -> i32:
    if x == 0:
        return 1
    else:
        return 2";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_more_branches() {
        let parsed = parse_no_std(
            "
def main(x: i32) -> i32:
    if x == 0:
        return 1
    else:
        if x == 2:
            return 2
        else:
            return x
",
        );
        let analyzed = do_analysis(&parsed);
        assert_eq!(analyzed.type_errors.count(), 0);
        let final_result = print_hir(analyzed.last_hir(1), &analyzed);
        println!("{final_result}");
        let expected = "
def main(x: i32) -> i32:
    if x == 0:
        return 1
    else:
        if x == 2:
            return 2
        else:
            return x";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_no_return_in_one_branch() {
        let parsed = parse(
            "
def main(x: i32) -> i32:
    if x == 0:
        print(x)
    else:
        if x == 2:
            return 2
        else:
            return x
",
        );
        let analyzed = do_analysis(&parsed);
        assert_eq!(analyzed.type_errors.count(), 1);
        let final_result = print_hir(analyzed.last_hir(1), &analyzed);
        println!("{final_result}");
        let expected = "
def main(x: i32) -> i32:
    if x == 0:
        print(x)
    else:
        if x == 2:
            return 2
        else:
            return x";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_statements_decls_inside_branches() {
        let parsed = parse_no_std(
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
        let analyzed = do_analysis(&parsed);
        assert_eq!(analyzed.type_errors.count(), 0);
        let final_result = print_hir(analyzed.last_hir(1), &analyzed);
        println!("{final_result}");
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
        let parsed = parse(
            "
def main() -> i32:
    if True:
        x = 1
        if 1 == 1:
            x = x + 3
            return x
        else:
            x = x + 1
            print_int(x)
        print(\"nice\")
    else:
        y = 3
        if 2 == 2:
            y = y + 1
            print_int(y)
        else:
            return 4 * y
",
        );
        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();

        assert_eq!(analyzed.type_errors.count(), 1);
        let final_result = print_hir(analyzed.last_hir(1), &analyzed);
        println!("{final_result}");
        let expected = "
def main() -> i32:
    if True:
        x : i32 = 1
        if 1 == 1:
            x = x + 3
            return x
        else:
            x = x + 1
            print_int(x)
        print(\"nice\")
    else:
        y : i32 = 3
        if 2 == 2:
            y = y + 1
            print_int(y)
        else:
            return 4 * y";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn type_error_operator_not_found() {
        let parsed = parse(
            "
def my_function():
    x = 1 + \"abc\"",
        );
        let analyzed = do_analysis(&parsed);

        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.binary_op_not_found_tc.len(), 1);

        assert_eq!(
            analyzed.type_errors.binary_op_not_found_tc[0]
                .error
                .lhs
                .print_name(&analyzed.type_db),
            "i32"
        );

        assert_eq!(
            analyzed.type_errors.binary_op_not_found_tc[0]
                .error
                .rhs
                .print_name(&analyzed.type_db),
            "str"
        );

        assert_eq!(
            analyzed.type_errors.binary_op_not_found_tc[0].error.operator,
            Operator::Plus
        );
    }

    #[test]
    fn field_does_not_exist() {
        let parsed = parse(
            "
def my_function():
    x = [1,2,3]
    y = x.sizee",
        );
        let analyzed = do_analysis(&parsed);

        assert_eq!(analyzed.type_errors.count(), 1);

        assert_eq!(analyzed.type_errors.field_not_found_tc.len(), 1);

        assert_eq!(
            analyzed.type_errors.field_not_found_tc[0].error.field,
            InternedString::new("sizee")
        );
        assert_eq!(
            analyzed.type_errors.field_not_found_tc[0]
                .error
                .object_type
                .print_name(&analyzed.type_db),
            "array"
        );
        assert_eq!(
            analyzed.type_errors.field_not_found_tc[0]
                .on_element
                .get_name(),
            "my_function"
        );
    }

    #[test]
    fn method_does_not_exist() {
        let parsed = parse(
            "
def my_function():
    x = [1,2,3]
    y = x.reevert()",
        );
        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        assert_eq!(analyzed.type_errors.count(), 1);

        assert_eq!(analyzed.type_errors.method_not_found.len(), 1);

        assert_eq!(
            analyzed.type_errors.method_not_found[0]
                .error
                .method
                .to_string(),
            "reevert"
        );
        assert_eq!(
            analyzed.type_errors.method_not_found[0]
                .error
                .object_type
                .to_string(&analyzed.type_db),
            "array<i32>"
        );
        assert_eq!(
            analyzed.type_errors.method_not_found[0]
                .on_element
                .get_name(),
            "my_function"
        );
    }

    #[test]
    fn type_not_found() {
        let parsed = parse(
            "
def my_function():
    x: i65 = 1",
        );

        let analyzed = do_analysis(&parsed);

        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.type_not_found.len(), 1);

        let printer =
            HIRTypeDisplayer::new(&analyzed.type_errors.type_not_found[0].error.type_name);

        assert_eq!(printer.to_string(), "i65");
        assert_eq!(
            analyzed.type_errors.type_not_found[0].on_element.get_name(),
            "my_function"
        );
    }

    #[test]
    fn unexpected_type_found_in_binary_expression_lhs() {
        let parsed = parse(
            "
def my_function():
    x: array<str> = [\"1\",\"2\",\"3\"]
    y: i32 = x[0].as_i32 + 1",
        );

        let analyzed = do_analysis(&parsed);

        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(
            analyzed.type_errors.field_not_found_tc[0].error.field,
            InternedString::new("as_i32")
        );
    }

    #[test]
    fn unexpected_type_found_in_binary_expression_rhs() {
        let parsed = parse(
            "
def my_function():
    x: array<str> = [\"1\",\"2\",\"3\"]
    y: i32 = 1 + x[0].as_i32",
        );
        let analyzed = do_analysis(&parsed);

        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(
            analyzed.type_errors.field_not_found_tc[0].error.field,
            InternedString::new("as_i32")
        );
    }

    #[test]
    fn unary_operator_not_found() {
        let parsed = parse(
            "
def my_function():
    x = \"1\"
    y = +x",
        );
        let analyzed = do_analysis(&parsed);

        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.unary_op_not_found.len(), 1);

        assert_eq!(
            analyzed.type_errors.unary_op_not_found[0]
                .error
                .rhs
                .to_string(&analyzed.type_db),
            "str"
        );
        assert_eq!(
            analyzed.type_errors.unary_op_not_found[0].error.operator,
            Operator::Plus
        );
        assert_eq!(
            analyzed.type_errors.unary_op_not_found[0]
                .on_element
                .get_name(),
            "my_function"
        );
    }

    #[test]
    fn insufficient_array_type_info() {
        let parsed = parse(
            "
def my_function():
    x = []",
        );

        let analyzed = do_analysis(&parsed);

        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.insufficient_array_type_info.len(), 1);
        assert_eq!(
            analyzed.type_errors.insufficient_array_type_info[0]
                .on_element
                .get_name(),
            "my_function"
        );
    }

    #[test]
    fn call_to_non_callable() {
        let parsed = parse(
            "
def my_function():
    x: array<i32> = []
    y = x()",
        );
        let analyzed = do_analysis(&parsed);
        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.call_non_callable_tc.len(), 1);

        assert_eq!(
            analyzed.type_errors.call_non_callable_tc[0]
                .error
                .actual_type
                .unwrap()
                .print_name(&analyzed.type_db),
            "array"
        );
    }

    #[test]
    fn instantiate_struct_and_assign_variables() {
        let parsed = parse_no_std(
            "
struct Point2D:
    x: i32
    y: i32

def main():
    my_point = Point2D()
    my_point.x = 1
    my_point.y = 2
    my_var = my_point.y
",
        );
        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        assert_eq!(analyzed.type_errors.count(), 0);
        let final_result = print_hir(analyzed.last_hir(1), &analyzed);
        println!("{final_result}");
        let expected = "
def main() -> Void:
    my_point : Point2D = Point2D()
    my_point.x = 1
    my_point.y = 2
    my_var : i32 = my_point.y
";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn mono_test_2() {
        let parsed = parse_no_std(
            "
struct MyAmazingItem<T>:
    item: T

def main():
    amazing_item = MyAmazingItem<i32>()
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
       // println!("PRINTING HIR");
       // println!("{}", print_hir(&analyzed.hir, &analyzed));
       // println!("Here is the hir: {:#?}", analyzed.hir);

        assert_eq!(analyzed.type_errors.count(), 0);
        let final_result = print_hir_mono(analyzed.last_hir_mono(1), &analyzed);
        println!("{final_result}");
        let expected = "
def main() -> Void:
    amazing_item : MyAmazingItem<i32> = MyAmazingItem<i32>()
    ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn mono_test() {
        let parsed = parse_no_std(
            "
struct MyAmazingItem<T>:
    item: T

def make_item<T>(item: T) -> MyAmazingItem<T>:
    amazing_item = MyAmazingItem<T>()
    amazing_item.item = item
    return amazing_item

def main():
    number: f32 = 1.0
    my_item = make_item<f32>(number)

",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        //assert_eq!(analyzed.type_errors.count(), 0);
        //println!("Here is the hir: {:#?}", analyzed.hir.last().unwrap());
        let final_result = print_hir_mono(analyzed.last_hir_mono(2), &analyzed);
        println!("{final_result}");
        let expected = "
def main() -> Void:
    number : f32 = 1.0
    my_item : MyAmazingItem<f32> = make_item_f32<f32>(number)
def make_item_f32(item: f32) -> MyAmazingItem<f32>:
    amazing_item : MyAmazingItem<f32> = MyAmazingItem<f32>()
    amazing_item.item = item
    return amazing_item
    ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn mono_test_access_field_type() {
        let parsed = parse_no_std(
            "
struct MyAmazingItem<T>:
    item: T

def make_item<T>(item: T) -> MyAmazingItem<T>:
    amazing_item = MyAmazingItem<T>()
    amazing_item.item = item
    return amazing_item

def main():
    amazing_item = make_item<i32>(0)
    x = amazing_item.item
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        println!("Here is the hir");
        let final_result = print_hir_mono(analyzed.last_hir_mono(2), &analyzed);
        println!("{final_result}");
        assert_eq!(analyzed.type_errors.count(), 0);

        let expected = "
def main() -> Void:
    amazing_item : MyAmazingItem<i32> = make_item_i32<i32>(0)
    x : i32 = amazing_item.item
def make_item_i32(item: i32) -> MyAmazingItem<i32>:
    amazing_item : MyAmazingItem<i32> = MyAmazingItem<i32>()
    amazing_item.item = item
    return amazing_item
    ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn mono_test_access_field_type_with_type_hint_wrong() {
        let parsed = parse_no_std(
            "
struct MyAmazingItem<T>:
    item: T

def make_item<T>(item: T) -> MyAmazingItem<T>:
    amazing_item = MyAmazingItem<T>()
    amazing_item.item = item
    return amazing_item

def main():
    amazing_item = make_item<i32>(0)
    x: u32 = amazing_item.item
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        assert_eq!(analyzed.type_errors.assign_mismatches.len(), 1);
    }

    #[test]
    fn test_more_complex_code() {
        let parsed = parse_no_std(
            "
struct List<T>:
    buf: ptr<T>
    len: u32
    cap: u32

def main():
    new_list = List<i32>()
    b = new_list.buf
    b.write(0, 1)
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        println!("Here is the hir");
        let final_result = print_hir_mono(analyzed.last_hir_mono(1), &analyzed);
        println!("{final_result}");
        assert_eq!(analyzed.type_errors.count(), 0);

        let expected = "
def main() -> Void:
    new_list : List<i32> = List<i32>()
    b : ptr<i32> = new_list.buf
    b.write(0, 1)
    ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn test_more_complex_code2() {
        let parsed = parse_no_std(
            "
struct List<T>:
    buf: ptr<T>
    len: u64

def list_add<T>(list: List<T>, item: T):
    b = list.buf
    b.write(list.len, item)

def main():
    new_list = List<i32>()
    list_add<i32>(new_list, 1)
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        println!("Here is the hir");
        let final_result = print_hir_mono(analyzed.last_hir_mono(2), &analyzed);
        println!("{final_result}");
        assert_eq!(analyzed.type_errors.count(), 0);

        let expected = "
def main() -> Void:
    new_list : List<i32> = List<i32>()
    list_add_i32(new_list, 1)
def list_add_i32(list: List<i32>, item: i32) -> Void:
    b : ptr<i32> = list.buf
    b.write(list.len, item)
    ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn test_more_complex_code3() {
        let parsed = parse_no_std(
            "
struct List<T>:
    buf: ptr<T>
    len: u64
    cap: u64

def list_new<T>() -> List<T>:
    list = List<T>()
    list.len = 0
    list.cap = 0

    return list

def list_add<T>(list: List<T>, item: T):
    if list.len == list.cap:
        if list.len == 0:
            list.cap = 4
        else:
            list.cap = list.cap * 2
        
        buf_u8 = reinterpret_ptr<T, u8>(list.buf)
        buf_u8 = realloc(buf_u8, list.cap * 4)
        buf_t = reinterpret_ptr<u8, T>(buf_u8)
        list.buf = buf_t
    list.buf.write(list.len, item)
    list.len = list.len + 1

def reinterpret_ptr<T, U>(p: ptr<T>) -> ptr<U>:
    intrinsic

def realloc(buf: ptr<u8>, size: u64) -> ptr<u8>:
    intrinsic

def main():
    new_list = list_new<i32>()
    list_add<i32>(new_list, 1)
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        println!("Here is the hir");
        let final_result = print_hir_mono(analyzed.last_hir_mono(6), &analyzed);
        println!("{final_result}");
        assert_eq!(analyzed.type_errors.count(), 0);

        let expected = "
def realloc(buf: ptr<u8>, size: u64) -> ptr<u8>:
def main() -> Void:
    new_list : List<i32> = list_new_i32<i32>()
    list_add_i32(new_list, 1)
def list_add_i32(list: List<i32>, item: i32) -> Void:
    if list.len == list.cap:
        if list.len == 0:
            list.cap = 4
        else:
            list.cap = list.cap * 2
        buf_u8 : ptr<u8> = reinterpret_ptr_i32_u8<i32, u8>(list.buf)
        buf_u8 = realloc(buf_u8, list.cap * 4)
        buf_t : ptr<i32> = reinterpret_ptr_u8_i32<u8, i32>(buf_u8)
        list.buf = buf_t
    else:
        pass
    list.buf.write(list.len, item)
    list.len = list.len + 1
def reinterpret_ptr_u8_i32(p: ptr<u8>) -> ptr<i32>:
def reinterpret_ptr_i32_u8(p: ptr<i32>) -> ptr<u8>:
def list_new_i32() -> List<i32>:
    list : List<i32> = List<i32>()
    list.len = 0
    list.cap = 0
    return list
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn nested_ptr_generics() {
        let parsed = parse_no_std(
            "
def do_something<T>(buf: ptr<ptr<T>>) -> ptr<T>:
    intrinsic

def main():
    x = 0
    y = &x
    yy = &y
    do_something<i32>(yy)
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        println!("Here is the hir");
        let final_result = print_hir_mono(analyzed.last_hir_mono(2), &analyzed);
        println!("{final_result}");
        assert_eq!(analyzed.type_errors.count(), 0);

        let expected = "
def main() -> Void:
    x : i32 = 0
    y : ptr<i32> = &x
    yy : ptr<ptr<i32>> = &y
    do_something_i32(yy)
def do_something_i32(buf: ptr<ptr<i32>>) -> ptr<i32>:
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn nested_ptr_generics2() {
        let parsed = parse_no_std(
            "
def first<T>(array_ptr: ptr<array<T>>) -> ptr<T>:
    intrinsic

def main():
    y = [1]
    yy = &y
    first<i32>(yy)
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        println!("Here is the hir");
        let final_result = print_hir_mono(analyzed.last_hir_mono(2), &analyzed);
        println!("{final_result}");
        assert_eq!(analyzed.type_errors.count(), 0);

        let expected = "
def main() -> Void:
    y : array<i32> = [1]
    yy : ptr<array<i32>> = &y
    first_i32(yy)
def first_i32(array_ptr: ptr<array<i32>>) -> ptr<i32>:
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn nested_ptr_generics3() {
        let parsed = parse_no_std(
            "
struct List<T>:
    buf: ptr<T>
    len: u32
    cap: u32

def list_new<T>() -> List<T>:
    intrinsic

def list_add<T>(list_ptr: ptr<List<T>>, item: T):
    intrinsic

def main():
    new_list = list_new<i32>()
    list_add<i32>(&new_list, 1)
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        println!("Here is the hir");
        let final_result = print_hir_mono(analyzed.last_hir_mono(3), &analyzed);
        println!("{final_result}");
        assert_eq!(analyzed.type_errors.count(), 0);

        let expected = "
def main() -> Void:
    new_list : List<i32> = list_new_i32<i32>()
    list_add_i32(&new_list, 1)
def list_add_i32(list_ptr: ptr<List<i32>>, item: i32) -> Void:
def list_new_i32() -> List<i32>:
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn list_example() {
        let parsed = parse_no_std(
            "
struct List<T>:
    buf: ptr<T>
    len: u32
    cap: u32

def list_new<T>() -> List<T>:
    list = List<T>()
    list.len = 0
    list.cap = 0

    return list

def list_add<T>(list_ptr: ptr<List<T>>, item: T):
    list = *list_ptr
    
def main():
    new_list = list_new<i32>()
    list_add<i32>(&new_list, 1)
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        println!("Here is the hir");
        let final_result = print_hir_mono(analyzed.last_hir_mono(3), &analyzed);
        println!("{final_result}");
        assert_eq!(analyzed.type_errors.count(), 0);

        let expected = "
def main() -> Void:
    new_list : List<i32> = list_new_i32<i32>()
    list_add_i32(&new_list, 1)
def list_add_i32(list_ptr: ptr<List<i32>>, item: i32) -> Void:
    list : List<i32> = *list_ptr
def list_new_i32() -> List<i32>:
    list : List<i32> = List<i32>()
    list.len = 0
    list.cap = 0
    return list";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn list_example_call_read_function_generic() {
        let parsed = parse_no_std(
            "
struct List<T>:
    buf: ptr<T>
    len: u32
    cap: u32

def list_new<T>() -> List<T>:
    intrinsic

def read<X>(p: ptr<X>) -> X:
    intrinsic

def list_add<T>(list_ptr: ptr<List<T>>, item: T):
    list = read<List<T>>(list_ptr)
    
def main():
    new_list = list_new<i32>()
    list_add<i32>(&new_list, 1)
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        println!("Here is the hir");
        let final_result = print_hir_mono(analyzed.last_hir_mono(4), &analyzed);
        println!("{final_result}");
        assert_eq!(analyzed.type_errors.count(), 0);

        let expected = "
def main() -> Void:
    new_list : List<i32> = list_new_i32<i32>()
    list_add_i32(&new_list, 1)
def list_add_i32(list_ptr: ptr<List<i32>>, item: i32) -> Void:
    list : List<i32> = read_List<i32><List<i32>>(list_ptr)
def read_List<i32>(p: ptr<List<i32>>) -> List<i32>:
def list_new_i32() -> List<i32>:
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn list_example_call_read_method_generic() {
        let parsed = parse_no_std(
            "
struct List<T>:
    buf: ptr<T>
    len: u32
    cap: u32

def list_new<T>() -> List<T>:
    intrinsic

def list_add<T>(list_ptr: ptr<List<T>>, item: T):
    list = list_ptr.read(0)
    
def main():
    new_list = list_new<i32>()
    list_add<i32>(&new_list, 1)
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        let final_result = print_hir_mono(&analyzed.hir_monomorphized, &analyzed);
        println!("{final_result}");

        let expected = "
def main() -> Void:
    new_list : List<i32> = list_new_i32<i32>()
    list_add_i32(&new_list, 1)
def list_add_i32(list_ptr: ptr<List<i32>>, item: i32) -> Void:
    list : List<i32> = list_ptr.read(0)
def list_new_i32() -> List<i32>:
        ";

        assert_eq!(expected.trim(), final_result.trim());
        assert_eq!(analyzed.type_errors.count(), 0);

    }

    #[test]
    fn cast_numeric_type() {
        let parsed = parse_no_std(
            "
def main():
    x = 1 as f32
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        let final_result = print_hir_mono(analyzed.last_hir_mono(1), &analyzed);
        println!("{final_result}");
        assert_eq!(analyzed.type_errors.count(), 0);

        let expected = "
def main() -> Void:
    x : f32 = 1 as f32
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn cast_numeric_type_to_invalid_type_results_in_error() {
        let parsed = parse_no_std(
            "
def main():
    x = 1 as ptr<i32>
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        let final_result = print_hir_mono(analyzed.last_hir_mono(1), &analyzed);
        println!("{final_result}");
        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.invalid_casts.len(), 1);

        let expected = "
def main() -> Void:
    x : ptr<i32> = 1 as ptr<i32>
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn generics_work_without_function_args() {
        let parsed = parse(
            "
def print_type_data<T>():
    type_size = T.size as i32
    print_int(type_size)

def main() -> i32:
    print_type_data<i32>()
    return 1
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        //let final_result = print_hir(analyzed.last_hir(1), &analyzed);
        let final_result = print_hir_mono_verbose(analyzed.last_hir_mono(2), &analyzed);
        println!("{final_result}");
        assert_eq!(analyzed.type_errors.count(), 0);
        let expected = "
def main() -> i32:
    {print_type_data_i32: fn () -> Void}()
    return {1: i32}
def print_type_data_i32() -> Void:
    type_size : i32 = {{{i32: TypeData}.size: u64} as i32: i32}
    {print_int: fn (i32) -> Void}({type_size: i32})
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }


    #[test]
    fn zero_arguments_bug() {
        let parsed = parse_no_std(
            "
def some_fn(s: ptr<i32>) -> i32:
    intrinsic

def another_fn() -> i32:
    x = 0
    return some_fn(&x)
",
        );

        let analyzed = do_analysis(&parsed);
        analyzed.print_errors();
        //let final_result = print_hir(analyzed.last_hir(1), &analyzed);
        let final_result = print_hir_mono_verbose(analyzed.last_hir_mono(2), &analyzed);
        println!("{final_result}");
        assert_eq!(analyzed.type_errors.count(), 0);
        let expected = "
def some_fn(s: ptr<i32>) -> i32:
def another_fn() -> i32:
    x : i32 = {0: i32}
    return {{some_fn: fn (ptr<i32>) -> i32}({&{x: i32}: ptr<i32>}): i32}
";

        assert_eq!(expected.trim(), final_result.trim());
    }
    /*



    */
}
