#![feature(assert_matches)]
#![feature(let_else)]

mod ast;
mod commons;
mod semantic;

use std::env;
use std::fs;
use crate::ast::lexer;
use crate::ast::parser;
use crate::semantic::mir;
use crate::semantic::mir::*;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 2 {
        let input =
            fs::read_to_string(args[1].clone()).expect(&format!("Could not read file {}", args[1]));
        let tokens = lexer::tokenize(input.as_str());
        //println!("Tokens: {:?}", tokens);
        let ast = parser::parse_ast(tokens.unwrap());

        let root = parser::AST::Root(ast);
        let mut result = crate::semantic::analysis::do_analysis(&root);

        use lexer::Operator;
        fn operator_str(op: lexer::Operator) -> String {
            match op {
                Operator::Plus => "+".into(),
                Operator::Minus => "-".into(),
                Operator::Multiply => "*".into(),
                Operator::Divide => "/".into(),
                _ => "__some_op__".into()
            }
        }

        fn trivial_expr_str(expr: &TrivialMIRExpr) -> String {
            match expr {
                TrivialMIRExpr::Variable(s) => s.clone(),
                TrivialMIRExpr::FloatValue(f) => format!("{:?}", f),
                TrivialMIRExpr::IntegerValue(i) => format!("{}", i),
                TrivialMIRExpr::StringValue(s) => format!("\"{}\"", s),
                TrivialMIRExpr::BooleanValue(b) => format!("{}", b),
                TrivialMIRExpr::None => "None".into(),
            }
        }

        fn expr_str(expr: &MIRExpr) -> String {
            match expr {
                MIRExpr::Trivial(trivial) => trivial_expr_str(trivial),
                MIRExpr::FunctionCall(f, params) => {
                    let args_str = params.iter().map(|x| trivial_expr_str(x)).collect::<Vec<_>>().join(", ");
                    format!("{}({})", trivial_expr_str(f), args_str)
                }
                MIRExpr::BinaryOperation(var, op, var2) => 
                    format!("{} {} {}", trivial_expr_str(var), operator_str(*op), trivial_expr_str(var2)),
                
                MIRExpr::IndexAccess(var, index) => 
                    format!("{}[{}]", trivial_expr_str(var), trivial_expr_str(index)),
                
                MIRExpr::Array(items) => {
                    let args_str = items.iter().map(|x| trivial_expr_str(x)).collect::<Vec<_>>().join(", ");
                    format!("[{}]", args_str)
                }
                MIRExpr::UnaryExpression(op, expr) => {
                    format!("{}{}", operator_str(*op), trivial_expr_str(expr))
                }

                    e => format!("not added to expr_str: {:?}", e)
            }
        }

        fn mir_type_str(typ: &MIRTypeDef) -> String {
            match typ {
                MIRTypeDef::Pending => "UNKNOWN_TYPE".into(),
                MIRTypeDef::Unresolved(MIRType::Simple(s)) => s.clone(),
                MIRTypeDef::Unresolved(MIRType::Generic(s, g)) => format!("{}<{}>", s, 
                    g.iter().map(|x| mir_type_str(x)).collect::<Vec<_>>().join(", ")),
                MIRTypeDef::Resolved(id) => format!("RESOLVED_{}", id)
            }
        }

        fn mir_str(node: &MIR, indent: String) {
            match node {
                MIR::Assign {path, expression} => {
                    println!("{}{} = {}",indent, path.join("."), expr_str(&expression));
                },
                MIR::Declare{var, typename, expression} => {
                    println!("{}{} : {} = {}", indent, var, mir_type_str(typename), expr_str(&expression));
                },
                MIR::DeclareFunction{function_name, parameters, body, return_type} => {
                    
                    let parameters = parameters.iter().map(|param| {
                        return format!("{}{}: {}", indent, param.name, mir_type_str(&param.typename));
                    }).collect::<Vec<_>>().join(", ");
                    
                    println!("{}def {}({}) -> {}:", indent, function_name, parameters, mir_type_str(return_type));

                    for n in body {
                        mir_str(n, format!("{}  ", indent));
                    }
                    println!()

                }
                MIR::Return(expr) => {
                    println!("{}return {}",indent, expr_str(expr));
                }
                MIR::StructDeclaration { struct_name, body} => {
                    println!("{}struct {}:", indent, struct_name);

                    for field in body {
                        println!("{}  {}: {}", indent, field.name, mir_type_str(&field.typename));
                    }

                    println!()
                }
                MIR::FunctionCall { function, args} => {
                    let args_str = args.iter().map(|x| trivial_expr_str(x)).collect::<Vec<_>>().join(", ");
                  
                    println!("{}{}({})", indent, trivial_expr_str(function), args_str);
                },
                e => panic!("Code format not implemented for node {:?}", e)
            }
        }

        for node in result {
            mir_str(&node, "".into());
        }

        return;
    }
}
