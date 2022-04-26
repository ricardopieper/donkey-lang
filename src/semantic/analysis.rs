use crate::ast::parser::*;
use crate::semantic::hir::*;
use crate::semantic::*;

use super::type_db::TypeDatabase;

pub struct AnalysisResult {
    pub initial_mir: Vec<HIR>,
    pub after_make_declarations_mir: Vec<HIR>,
    pub final_mir: Vec<HIR>,
    pub type_db: TypeDatabase,
}

pub fn do_analysis(ast: &AST) -> AnalysisResult {
    let mut mir = vec![];
    ast_to_hir(ast, 0, &mut mir);

    let initial_mir = mir.clone();
    let type_db = type_db::TypeDatabase::new();

    let mut globals = name_registry::build_name_registry(&type_db, &mir);

    mir = first_assignments::transform_first_assignment_into_declaration(mir);
    let after_make_declarations_mir = mir.clone();
    undeclared_vars::detect_undeclared_vars_and_redeclarations(&mir);

    mir = typing::infer_types(&mut globals, &type_db, mir);

    typing::check_types(&globals, &type_db, &mir[..]);

    return AnalysisResult {
        initial_mir,
        after_make_declarations_mir,
        final_mir: mir,
        type_db,
    };
}


#[cfg(test)]
mod tests {
    use crate::ast::lexer::Operator;
    #[cfg(test)]
    use pretty_assertions::{assert_eq};

    use super::*;

    //Parses a single expression
    fn mir(source: &str) -> AnalysisResult {
        let tokenized = crate::ast::lexer::Tokenizer::new(source).tokenize().ok().unwrap();
        let mut parser = Parser::new(tokenized);
        let ast = AST::Root(parser.parse_ast().ok().unwrap());
        super::analysis::do_analysis(&ast)
    }


    #[test]
    fn simple_test() {
        let result = mir(
            "
def my_function(arg1: i32, arg2: i32) -> i32:
    return arg1 * arg2 / (arg2 - arg1)",
        );
        let type_db = result.type_db;
        let i32_type = TypeInstance::Simple(type_db.find_by_name("i32").unwrap().id);
        
        let body = vec![
            HIR::Declare { 
                var: "$0".into(), 
                typename: HIRTypeDef::Resolved(i32_type.clone()),
                expression: HIRExpr::BinaryOperation(
                    TrivialHIRExpr::Variable("arg1".into()),
                    Operator::Multiply,
                    TrivialHIRExpr::Variable("arg2".into())
                ) 
            },
            HIR::Declare { 
                var: "$1".into(), 
                typename:  HIRTypeDef::Resolved(i32_type.clone()),
                expression: HIRExpr::BinaryOperation(
                    TrivialHIRExpr::Variable("arg2".into()),
                    Operator::Minus,
                    TrivialHIRExpr::Variable("arg1".into())
                ) 
            },
            HIR::Return(HIRExpr::BinaryOperation(
                    TrivialHIRExpr::Variable("$0".into()),
                    Operator::Divide,
                    TrivialHIRExpr::Variable("$1".into())
                )
            )
            
        ];

        let expected = vec![
            HIR::DeclareFunction{
                function_name: "my_function".into(),
                parameters: vec![
                    HIRTypedBoundName {
                        name: "arg1".into(),
                        typename: HIRTypeDef::Resolved(i32_type.clone())
                    },
                    HIRTypedBoundName {
                        name: "arg2".into(),
                        typename: HIRTypeDef::Resolved(i32_type.clone())
                    }
                ],
                body,
                return_type: HIRTypeDef::Resolved(i32_type.clone())
            },
        ];

        assert_eq!(expected, result.final_mir);
    }

    #[test]
    fn alternative_test() {
        let analyzed = mir(
            "
def my_function(arg1: i32, arg2: i32) -> i32:
    return arg1 * arg2 / (arg2 - arg1)",
        );
        
        let result = hir_printer::print_mir(&analyzed.final_mir, &analyzed.type_db);
        
        let expected = "
def my_function(arg1: i32, arg2: i32) -> i32:
    $0 : i32 = arg1 * arg2
    $1 : i32 = arg2 - arg1
    return $0 / $1";

        assert_eq!(expected.trim(), result.trim());
    }
}