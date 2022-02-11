use std::env;
use std::fs;
use crate::ast::lexer;
use crate::ast::parser;
use crate::semantic::mir;
use crate::semantic::mir::*;
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


fn mir_type_str(typ: &MIRTypeDef, type_db: &TypeDatabase) -> String {

    fn slice_types_str(types: &[MIRType], type_db: &TypeDatabase) -> String {
        types.iter().map(|x| mir_type_str(&MIRTypeDef::Unresolved(x.clone()), type_db)).collect::<Vec<_>>().join(", ")
    }

    match typ {
        MIRTypeDef::Pending => "UNKNOWN_TYPE".into(),
        MIRTypeDef::Unresolved(MIRType::Simple(s)) => format!("UNRESOLVED! {}", s.clone()),
        MIRTypeDef::Unresolved(MIRType::Generic(s, g)) => format!("UNRESOLVED {}<{}>", s, slice_types_str(g, type_db)),
        MIRTypeDef::Unresolved(MIRType::Function(args, return_type)) => 
            format!("UNRESOLVED ({}) -> {}", slice_types_str(args, type_db), mir_type_str(&MIRTypeDef::Unresolved(*return_type.clone()), type_db)),
        MIRTypeDef::Resolved(instance) => {
            instance.string(type_db)
        } 
    }
}

fn print_mir_str(node: &MIR, indent: String, type_db: &TypeDatabase) -> String {
    match node {
        MIR::Assign {path, expression} => {
            format!("{}{} = {}\n",indent, path.join("."), expr_str(&expression))
        },
        MIR::Declare{var, typename, expression} => {
            format!("{}{} : {} = {}\n", indent, var, mir_type_str(typename, type_db), expr_str(&expression))
        },
        MIR::DeclareFunction{function_name, parameters, body, return_type} => {
            
            let parameters = parameters.iter().map(|param| {
                return format!("{}{}: {}", indent, param.name, mir_type_str(&param.typename, type_db));
            }).collect::<Vec<_>>().join(", ");
            
            let mut function = format!("{}def {}({}) -> {}:\n", indent, function_name, parameters, mir_type_str(return_type, type_db));

            for n in body {
                function.push_str(&print_mir_str(n, format!("{}    ", indent), type_db));
            }
            return function;
        }   
        MIR::Return(expr) => {
            format!("{}return {}\n",indent, expr_str(expr))
        }
        MIR::StructDeclaration { struct_name, body} => {
            let mut structdecl = format!("{}struct {}:\n", indent, struct_name);

            for field in body {
                structdecl.push_str(&format!("{}  {}: {}", indent, field.name, mir_type_str(&field.typename, type_db)));
            }
            

            structdecl
        }
        MIR::FunctionCall { function, args} => {
            let args_str = args.iter().map(|x| trivial_expr_str(x)).collect::<Vec<_>>().join(", ");
            
            format!("{}{}({})\n", indent, trivial_expr_str(function), args_str)
        },
        e => panic!("Code format not implemented for node {:?}", e)
    }
}


pub fn print_mir(mir: &[MIR], type_db: &TypeDatabase) -> String {
    let mut buffer = String::new();
    for node in mir {
        buffer.push_str(&print_mir_str(&node, "".into(), type_db));
    }
    return buffer;
}
