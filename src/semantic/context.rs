use std::fs;

use super::hir::{ast_globals_to_hir, Checked, InferredTypeHIRRoot, NotChecked};
use super::mir::{hir_to_mir, MIRTopLevelNode};
use super::name_registry::NameRegistry;
use super::type_checker::typecheck;
use crate::ast::ast_printer::print_ast;
use crate::ast::lexer::{TokenSpanIndex, TokenTable};
use crate::ast::parser::AstSpan;
use crate::ast::{lexer, parser};
use crate::interner::StringInterner;
use crate::semantic::{first_assignments, name_registry, type_inference, undeclared_vars};
use crate::types::type_errors::TypeErrorPrinter;
use crate::types::type_instance_db::TypeInstanceManager;
use crate::{ast::parser::AST, types::type_errors::TypeErrors};

#[derive(Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub struct FileTableIndex(pub usize);

pub struct FileTableEntry {
    pub path: String,
    pub index: FileTableIndex,
    pub ast: AST,
    pub contents: &'static str,
    pub token_table: TokenTable,
}

pub struct Source {
    pub interner: StringInterner,
    pub file_table: Vec<FileTableEntry>,
}

impl Source {
    pub fn new() -> Source {
        Source {
            interner: StringInterner::new(),
            file_table: vec![],
        }
    }
}
impl Source {
    #[allow(dead_code)]
    pub fn load_str_ref(self: &mut Source, file_name: &str, source: &str) {
        self.load(file_name.to_string(), source.to_string())
    }

    pub fn load(self: &mut Source, file_name: String, source: String) {
        let prev_count = self.file_table.len();
        let mut file_table_entry = FileTableEntry {
            path: file_name,
            index: FileTableIndex(prev_count),
            ast: AST::Break(AstSpan {
                start: TokenSpanIndex(0),
                end: TokenSpanIndex(0),
            }),
            contents: source.leak(),
            token_table: TokenTable {
                tokens: vec![],
                spans: vec![],
            },
        };
        file_table_entry.token_table = lexer::tokenize(
            FileTableIndex(prev_count),
            file_table_entry.contents,
            &self.interner,
        ).unwrap();

        self.file_table.push(file_table_entry);


        let (ast, _) = parser::parse_ast(&self.file_table[prev_count].token_table, &self.file_table);
        println!("{}\n", print_ast(&ast, &self.interner));
        let root = parser::AST::Root(ast);
        self.file_table.last_mut().unwrap().ast = root;
    }

    pub fn load_file(&mut self, file_location: &str) {
        let input = fs::read_to_string(file_location)
            .unwrap_or_else(|_| panic!("Could not read file {file_location}"));
        self.load(file_location.to_string(), input);
    }

    pub fn load_stdlib(&mut self) {
        self.load_file("./stdlib/llvm_intrinsics.dk");
    }
}

pub struct Analyzer<'source, 'interner> {
    pub mir: Vec<MIRTopLevelNode<'source, Checked>>,
    pub unchecked_mir: Vec<MIRTopLevelNode<'source, NotChecked>>,
    pub type_db: TypeInstanceManager<'interner>,
    pub globals: NameRegistry,
    pub type_errors: TypeErrors<'source>,
    pub hir: Vec<Vec<InferredTypeHIRRoot<'source>>>,
    source: &'interner Source,
}

impl<'interner, 'source: 'interner> Analyzer<'source, 'interner> {
    pub fn new(source: &'interner Source) -> Analyzer<'source, 'interner> {
        Analyzer {
            type_db: TypeInstanceManager::new(&source.interner),
            globals: NameRegistry::new(),
            type_errors: TypeErrors::new(),
            mir: vec![],
            hir: vec![],
            unchecked_mir: vec![],
            source,
        }
    }

    #[allow(dead_code)]
    pub fn print_errors(&self) {
        println!(
            "{}",
            TypeErrorPrinter::new(&self.type_errors, &self.type_db, &self.source.interner, &self.source.file_table)
        )
    }

    #[allow(dead_code)]
    pub fn generate_mir(&mut self, source: &'source Source) {
        self.generate_mir_and_typecheck(source, false);
    }
    pub fn analyze(&mut self, source: &'source Source) {
        self.generate_mir_and_typecheck(source, true);
    }
    pub fn generate_mir_and_typecheck(&mut self, source: &'source Source, do_typecheck: bool) {
        for file in source.file_table.iter() {
            let ast_hir = ast_globals_to_hir(&file.ast, &source.interner);
            let inferred_globals_hir =
                match name_registry::build_name_registry_and_resolve_signatures(
                    &mut self.type_db,
                    &mut self.globals,
                    &mut self.type_errors,
                    ast_hir,
                    file.index
                ) {
                    Ok(hir) => hir,
                    Err(e) => {
                        let printer = TypeErrorPrinter::new(
                            &self.type_errors,
                            &self.type_db,
                            &source.interner,
                            &source.file_table,
                        );
                        panic!(
                            "build_name_registry_and_resolve_signatures {e:?}\n{printer}"
                        );
                    }
                };

            let first_assignment_hir =
                first_assignments::transform_first_assignment_into_declaration(
                    inferred_globals_hir,
                );

            if let Err(e) = undeclared_vars::detect_undeclared_vars_and_redeclarations(
                &self.globals,
                &first_assignment_hir,
                &mut self.type_errors,
                &source.interner,
                file.index
            ) {
                panic!("detect_undeclared_vars_and_redeclarations Err: {e:#?}");
            }

            match type_inference::infer_types(
                &mut self.globals,
                &mut self.type_db,
                first_assignment_hir,
                &mut self.type_errors,
                &source.interner,
                file.index
            ) {
                Ok(_) if self.type_errors.count() > 0 => {
                    let printer =
                        TypeErrorPrinter::new(&self.type_errors, &self.type_db, &source.interner, &source.file_table);
                    panic!("{}", printer);
                }
                Ok(final_hir) => {
                    self.hir.push(final_hir.clone());
                    let mir = hir_to_mir(final_hir);
                    if do_typecheck {
                        let typechecked = typecheck(
                            mir,
                            &self.type_db,
                            &self.globals,
                            &mut self.type_errors,
                            &source.interner,
                            file.index
                        );
                        if self.type_errors.count() == 0 {
                            self.mir.extend(typechecked.unwrap());
                        }
                    } else {
                        self.unchecked_mir.extend(mir);
                    }
                }
                Err(_) => {}
            }
        }
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{
        interner::StringInterner,
        semantic::{hir::NotChecked, mir::MIRTopLevelNode},
        types::{type_errors::TypeErrorPrinter, type_instance_db::TypeInstanceManager},
    };

    use super::{Analyzer, Source};

    macro_rules! tls_interner {
        ($interner:expr) => {
            fn istr(str: &'static str) -> InternedString {
                $interner.with(|i| i.get(str))
            }
        };
    }
    pub(crate) use tls_interner;

    pub struct AnalysisWithoutTypecheckResult<'source, 'interner> {
        pub mir: Vec<MIRTopLevelNode<'source, NotChecked>>,
        pub type_db: TypeInstanceManager<'interner>,
        pub interner: &'interner StringInterner,
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

        if ctx.type_errors.count() > 0 {
            let printer = TypeErrorPrinter::new(&ctx.type_errors, &ctx.type_db, &source.interner, &source.file_table);
            println!("\nTest failed with type errors:\n{printer}");
        }

        ctx
    }

    pub fn do_analysis_no_typecheck(
        source: &Source,
    ) -> AnalysisWithoutTypecheckResult {
        let mut ctx = Analyzer::new(source);
        ctx.generate_mir(source);
        AnalysisWithoutTypecheckResult {
            mir: ctx.unchecked_mir,
            type_db: ctx.type_db,
            interner: &source.interner,
        }
    }
}

#[cfg(test)]
mod tests {

    #[cfg(test)]
    use pretty_assertions::assert_eq;

    use crate::{
        ast::lexer::Operator,
        semantic::{
            context::test_utils::{do_analysis, parse},
            hir::HIRTypeDisplayer,
            hir_printer::HIRPrinter,
        },
    };

    use super::*;

    fn print_hir(hir: &[InferredTypeHIRRoot], analyzer: &Analyzer) -> String {
        HIRPrinter::new(&analyzer.type_db, &analyzer.source.interner).print_hir(hir)
    }

    #[test]
    fn simple_assign_decl() {
        let parsed = parse(
            "
def my_function():
    x = 1",
        );
        let analyzed = do_analysis(&parsed);
        assert_eq!(analyzed.type_errors.count(), 0);

        let result = print_hir(&analyzed.hir[1], &analyzed);

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

        let result = print_hir(&analyzed.hir[1], &analyzed);
        println!("{result}");

        let expected = "
def my_function() -> Void:
    x : f32 = 1.0
    powf(x, 2.0)";

        assert_eq!(expected.trim(), result.trim());
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
        let result = print_hir(&analyzed.hir[1], &analyzed);
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

        let result = print_hir(&analyzed.hir[1], &analyzed);
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
        assert_eq!(analyzed.type_errors.count(), 0);
        let result = print_hir(&analyzed.hir[1], &analyzed);

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
        let result = print_hir(&analyzed.hir[1], &analyzed);

        let expected = "
def main(args: array<str>) -> Void:
    my_var : i32 = 10
    print_int(my_var)";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    #[ignore = "we do not have integer literal promotion yet"]
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
        let final_result = print_hir(&analyzed.hir[1], &analyzed);
        println!("{final_result}");
        let expected = "
def main(args: array<str>) -> Void:
    my_var : str = args.__index__(0)
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

        assert_eq!(analyzed.type_errors.count(), 0);
        let result = print_hir(&analyzed.hir[1], &analyzed);
        println!("{result}");

        let expected = "
def my_function() -> f32:
    x : f32 = 1.3 + sqrtf(16.0 / 4.0 + 2.0 * 2.1) / 2.0 * 4.0 + 3.0 * powf(2.0, 2.0)
    return x
";

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
        let final_result = print_hir(&analyzed.hir[1], &analyzed);
        println!("{final_result}");
        let expected = "
def sum(x: i32, y: i32) -> i32:
    return x + y
def main() -> Void:
    my_var : i32 = sum(1, 2)
    print_int(my_var)";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    #[ignore = "we do not have integer literal promotion yet"]
    fn infer_defined_function_generic_param() {
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

        assert_eq!(analyzed.type_errors.count(), 0);
        let final_result = print_hir(&analyzed.hir[1], &analyzed);
        println!("{final_result}");
        let expected = "
def id(x: array<str>) -> str:
    first_item : str = x.__index__(0)
    return first_item
def main() -> Void:
    my_var : str = id(1)
    print(my_var)";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    #[ignore = "we do not have integer literal promotion yet"]
    fn semi_first_class_functions() {
        let parsed = parse(
            "
def id(x: array<str>) -> str:
    return x[0]

def main():
    my_func = id
    my_var = my_func(1)
    print(my_var)",
        );
        let analyzed = do_analysis(&parsed);
        assert_eq!(analyzed.type_errors.count(), 0);
        let final_result = print_hir(&analyzed.hir[1], &analyzed);
        println!("{final_result}");
        let expected = "
def id(x: array<str>) -> str:
    return x.__index__(0)
def main() -> Void:
    my_func : fn (array<str>) -> str = id
    my_var : str = my_func(1)
    print(my_var)";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn access_property_of_struct_and_infer_type() {
        let parsed = parse(
            "
def main():
    my_array = [1, 2, 3]
    my_array_length = my_array.length
    print_uint(my_array_length)",
        );
        println!("Doing analysis");

        let analyzed = do_analysis(&parsed);

        assert_eq!(analyzed.type_errors.count(), 0);
        let final_result = print_hir(&analyzed.hir[1], &analyzed);

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
        let parsed = parse(
            "
def main(x: i32) -> i32:
    y = 0
    return x + y
",
        );
        let analyzed = do_analysis(&parsed);
        assert_eq!(analyzed.type_errors.count(), 0);
        let final_result = print_hir(&analyzed.hir[1], &analyzed);
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
        assert_eq!(err.error.variable_name, analyzed.source.interner.get("y"));
    }

    #[test]
    fn self_decl_read_expr() {
        let parsed = parse(
            "
def main(x: i32) -> i32:
    a = 1
    b = 2
    y = (a + b * (x / y)) / 2
",
        );
        let analyzed = do_analysis(&parsed);
        let err = &analyzed.type_errors.variable_not_found[0];
        assert_eq!(err.error.variable_name, analyzed.source.interner.get("y"));
    }

    #[test]
    fn if_return_both_branches() {
        let parsed = parse(
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
        let final_result = print_hir(&analyzed.hir[1], &analyzed);
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
        let parsed = parse(
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
        let final_result = print_hir(&analyzed.hir[1], &analyzed);
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
        let final_result = print_hir(&analyzed.hir[1], &analyzed);
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
        let parsed = parse(
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
        let final_result = print_hir(&analyzed.hir[1], &analyzed);
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
        let final_result = print_hir(&analyzed.hir[1], &analyzed);
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
        assert_eq!(analyzed.type_errors.binary_op_not_found.len(), 1);

        assert_eq!(
            analyzed.type_errors.binary_op_not_found[0]
                .error
                .lhs
                .as_string(&analyzed.type_db),
            "i32"
        );

        assert_eq!(
            analyzed.type_errors.binary_op_not_found[0]
                .error
                .rhs
                .as_string(&analyzed.type_db),
            "str"
        );

        assert_eq!(
            analyzed.type_errors.binary_op_not_found[0].error.operator,
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
        let result = print_hir(&analyzed.hir[0], &analyzed);
        println!("{result}");

        assert_eq!(analyzed.type_errors.count(), 1);

        assert_eq!(analyzed.type_errors.field_or_method_not_found.len(), 1);

        assert_eq!(
            analyzed.type_errors.field_or_method_not_found[0]
                .error
                .field_or_method,
            analyzed.source.interner.get("sizee")
        );
        assert_eq!(
            analyzed.type_errors.field_or_method_not_found[0]
                .error
                .object_type
                .as_string(&analyzed.type_db),
            "array<i32>"
        );
        assert_eq!(
            analyzed.type_errors.field_or_method_not_found[0]
                .on_element
                .get_name(&analyzed.source.interner),
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

        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.field_or_method_not_found.len(), 1);

        assert_eq!(
            analyzed.type_errors.field_or_method_not_found[0]
                .error
                .field_or_method,
            analyzed.source.interner.get("reevert")
        );
        assert_eq!(
            analyzed.type_errors.field_or_method_not_found[0]
                .error
                .object_type
                .as_string(&analyzed.type_db),
            "array<i32>"
        );
        assert_eq!(
            analyzed.type_errors.field_or_method_not_found[0]
                .on_element
                .get_name(&analyzed.source.interner),
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

        let printer = HIRTypeDisplayer::new(
            &analyzed.type_errors.type_not_found[0].error.type_name,
            &analyzed.source.interner,
        );

        assert_eq!(printer.to_string(), "i65");
        assert_eq!(
            analyzed.type_errors.type_not_found[0]
                .on_element
                .get_name(&analyzed.source.interner),
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
        let result = print_hir(&analyzed.hir[0], &analyzed);
        println!("{result}");
        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(
            analyzed.type_errors.field_or_method_not_found[0]
                .error
                .field_or_method,
            analyzed.source.interner.get("as_i32")
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
            analyzed.type_errors.field_or_method_not_found[0]
                .error
                .field_or_method,
            analyzed.source.interner.get("as_i32")
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
                .as_string(&analyzed.type_db),
            "str"
        );
        assert_eq!(
            analyzed.type_errors.unary_op_not_found[0].error.operator,
            Operator::Plus
        );
        assert_eq!(
            analyzed.type_errors.unary_op_not_found[0]
                .on_element
                .get_name(&analyzed.source.interner),
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
                .get_name(&analyzed.source.interner),
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
        let result = print_hir(&analyzed.hir[0], &analyzed);
        println!("{result}");
        assert_eq!(analyzed.type_errors.count(), 1);
        assert_eq!(analyzed.type_errors.call_non_callable.len(), 1);
        println!(
            "{:?}",
            analyzed.type_errors.call_non_callable[0].error.actual_type
        );

        assert_eq!(
            analyzed.type_errors.call_non_callable[0]
                .error
                .actual_type
                .as_string(&analyzed.type_db),
            "array<i32>"
        );
    }
}
