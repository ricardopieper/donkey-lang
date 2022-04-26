use std::env;
use std::fs;
use crate::ast::lexer;
use crate::ast::parser;
use crate::semantic::hir;
use crate::semantic::hir::*;
use crate::semantic::type_db::TypeDatabase;
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

fn trivial_expr_str(expr: &TrivialHIRExpr) -> String {
    match expr {
        TrivialHIRExpr::Variable(s) => s.clone(),
        TrivialHIRExpr::FloatValue(f) => format!("{:?}", f),
        TrivialHIRExpr::IntegerValue(i) => format!("{}", i),
        TrivialHIRExpr::StringValue(s) => format!("\"{}\"", s),
        TrivialHIRExpr::BooleanValue(b) => format!("{}", b),
        TrivialHIRExpr::None => "None".into(),
    }
}

fn expr_str(expr: &HIRExpr) -> String {
    match expr {
        HIRExpr::Trivial(trivial) => trivial_expr_str(trivial),
        HIRExpr::FunctionCall(f, params) => {
            let args_str = params.iter().map(|x| trivial_expr_str(x)).collect::<Vec<_>>().join(", ");
            format!("{}({})", trivial_expr_str(f), args_str)
        }
        HIRExpr::BinaryOperation(var, op, var2) => 
            format!("{} {} {}", trivial_expr_str(var), operator_str(*op), trivial_expr_str(var2)),
        
        HIRExpr::Array(items) => {
            let args_str = items.iter().map(|x| trivial_expr_str(x)).collect::<Vec<_>>().join(", ");
            format!("[{}]", args_str)
        }
        HIRExpr::UnaryExpression(op, expr) => {
            format!("{}{}", operator_str(*op), trivial_expr_str(expr))
        }
        HIRExpr::MemberAccess(obj, elem) => {
            format!("{}.{}", trivial_expr_str(obj), elem)
        }
            e => format!("not added to expr_str: {:?}", e)
    }
}


fn mir_type_str(typ: &HIRTypeDef, type_db: &TypeDatabase) -> String {

    fn slice_types_str(types: &[HIRType], type_db: &TypeDatabase) -> String {
        types.iter().map(|x| mir_type_str(&HIRTypeDef::Unresolved(x.clone()), type_db)).collect::<Vec<_>>().join(", ")
    }

    match typ {
        HIRTypeDef::Pending => "UNKNOWN_TYPE".into(),
        HIRTypeDef::Unresolved(HIRType::Simple(s)) => format!("UNRESOLVED! {}", s.clone()),
        HIRTypeDef::Unresolved(HIRType::Generic(s, g)) => format!("UNRESOLVED {}<{}>", s, slice_types_str(g, type_db)),
        HIRTypeDef::Unresolved(HIRType::Function(args, return_type)) => 
            format!("UNRESOLVED ({}) -> {}", slice_types_str(args, type_db), mir_type_str(&HIRTypeDef::Unresolved(*return_type.clone()), type_db)),
        HIRTypeDef::Resolved(instance) => {
            instance.string(type_db)
        } 
    }
}

fn print_mir_str(node: &HIR, indent: String, type_db: &TypeDatabase) -> String {
    match node {
        HIR::Assign {path, expression} => {
            format!("{}{} = {}\n",indent, path.join("."), expr_str(&expression))
        },
        HIR::Declare{var, typename, expression} => {
            format!("{}{} : {} = {}\n", indent, var, mir_type_str(typename, type_db), expr_str(&expression))
        },
        HIR::DeclareFunction{function_name, parameters, body, return_type} => {
            
            let parameters = parameters.iter().map(|param| {
                return format!("{}{}: {}", indent, param.name, mir_type_str(&param.typename, type_db));
            }).collect::<Vec<_>>().join(", ");
            
            let mut function = format!("{}def {}({}) -> {}:\n", indent, function_name, parameters, mir_type_str(return_type, type_db));

            for n in body {
                function.push_str(&print_mir_str(n, format!("{}    ", indent), type_db));
            }
            return function;
        }   
        HIR::Return(expr) => {
            format!("{}return {}\n",indent, expr_str(expr))
        }
        HIR::StructDeclaration { struct_name, body} => {
            let mut structdecl = format!("{}struct {}:\n", indent, struct_name);

            for field in body {
                structdecl.push_str(&format!("{}  {}: {}", indent, field.name, mir_type_str(&field.typename, type_db)));
            }
            

            structdecl
        }
        HIR::FunctionCall { function, args} => {
            let args_str = args.iter().map(|x| trivial_expr_str(x)).collect::<Vec<_>>().join(", ");
            
            format!("{}{}({})\n", indent, trivial_expr_str(function), args_str)
        },
        e => panic!("Code format not implemented for node {:?}", e)
    }
}


pub fn print_mir(mir: &[HIR], type_db: &TypeDatabase) -> String {
    let mut buffer = String::new();
    for node in mir {
        buffer.push_str(&print_mir_str(&node, "".into(), type_db));
    }
    return buffer;
}
