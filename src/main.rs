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
use crate::semantic::type_db::TypeDatabase;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        return;
    }
    let input =
        fs::read_to_string(args[1].clone()).expect(&format!("Could not read file {}", args[1]));
    let tokens = lexer::tokenize(input.as_str());
    //println!("Tokens: {:?}", tokens);
    let ast = parser::parse_ast(tokens.unwrap());

    let root = parser::AST::Root(ast);
    let result = crate::semantic::analysis::do_analysis(&root);

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
            
            MIRExpr::Array(items) => {
                let args_str = items.iter().map(|x| trivial_expr_str(x)).collect::<Vec<_>>().join(", ");
                format!("[{}]", args_str)
            }
            MIRExpr::UnaryExpression(op, expr) => {
                format!("{}{}", operator_str(*op), trivial_expr_str(expr))
            }
            MIRExpr::MemberAccess(obj, elem) => {
                format!("{}.{}", trivial_expr_str(obj), elem)
            }
                e => format!("not added to expr_str: {:?}", e)
        }
    }

    fn type_instance_to_str(instance: &TypeInstance, type_db: &crate::semantic::type_db::TypeDatabase) -> String {
        match instance {
            TypeInstance::Simple(id) => {
                let record = type_db.find(*id);
                return record.name.clone();
            },
            TypeInstance::Generic(id, type_args) => {
                let base_record = type_db.find(*id);
                let args_str = type_args.iter().map(|x| type_instance_to_str(x, type_db)).collect::<Vec<_>>().join(", ");
                return format!("{}<{}>", base_record.name, args_str);
            },
            TypeInstance::Function(args, return_type) => {
                let args_str = args.iter().map(|x| type_instance_to_str(x, type_db)).collect::<Vec<_>>().join(", ");
                let return_type_str = type_instance_to_str(return_type, type_db);
                return format!("({}) -> {}", args_str, return_type_str);
            },
        }
    }

    fn mir_type_str(typ: &MIRTypeDef, type_db: &TypeDatabase) -> String {

        fn slice_types_str(types: &[MIRType], type_db: &TypeDatabase) -> String {
            types.iter().map(|x| mir_type_str(&MIRTypeDef::Unresolved(x.clone()), type_db)).collect::<Vec<_>>().join(", ")
        }

        match typ {
            MIRTypeDef::Pending => "UNKNOWN_TYPE".into(),
            MIRTypeDef::Unresolved(MIRType::Simple(s)) => s.clone(),
            MIRTypeDef::Unresolved(MIRType::Generic(s, g)) => format!("UNRESOLVED {}<{}>", s, slice_types_str(g, type_db)),
            MIRTypeDef::Unresolved(MIRType::Function(args, return_type)) => 
                format!("UNRESOLVED ({}) -> {}", slice_types_str(args, type_db), mir_type_str(&MIRTypeDef::Unresolved(*return_type.clone()), type_db)),
            MIRTypeDef::Resolved(instance) => {
                type_instance_to_str(instance, type_db)
            } 
        }
    }

    fn mir_str(node: &MIR, indent: String, type_db: &TypeDatabase) {
        match node {
            MIR::Assign {path, expression} => {
                println!("{}{} = {}",indent, path.join("."), expr_str(&expression));
            },
            MIR::Declare{var, typename, expression} => {
                println!("{}{} : {} = {}", indent, var, mir_type_str(typename, type_db), expr_str(&expression));
            },
            MIR::DeclareFunction{function_name, parameters, body, return_type} => {
                
                let parameters = parameters.iter().map(|param| {
                    return format!("{}{}: {}", indent, param.name, mir_type_str(&param.typename, type_db));
                }).collect::<Vec<_>>().join(", ");
                
                println!("{}def {}({}) -> {}:", indent, function_name, parameters, mir_type_str(return_type, type_db));

                for n in body {
                    mir_str(n, format!("{}  ", indent), type_db);
                }
                println!()

            }
            MIR::Return(expr) => {
                println!("{}return {}",indent, expr_str(expr));
            }
            MIR::StructDeclaration { struct_name, body} => {
                println!("{}struct {}:", indent, struct_name);

                for field in body {
                    println!("{}  {}: {}", indent, field.name, mir_type_str(&field.typename, type_db));
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

    println!("Initial MIR:");
    for node in result.initial_mir {
        mir_str(&node, "".into(), &result.type_db);
    }

    println!("After transforming into declarations:");
    for node in result.after_make_declarations_mir {
        mir_str(&node, "".into(), &result.type_db);
    }

    println!("Final MIR:");
    for node in result.final_mir {
        mir_str(&node, "".into(), &result.type_db);
    }

    return;
}
