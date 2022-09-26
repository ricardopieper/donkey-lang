use crate::ast::lexer;

use crate::semantic::hir::{HIR, HIRExpr, HIRType, HIRTypeDef, TrivialHIRExpr};
use crate::semantic::name_registry::TypeResolvedState;
use crate::types::type_constructor_db::TypeConstructorDatabase;
use crate::types::type_instance_db::TypeInstanceManager;
use lexer::Operator;

use super::hir::HIRTypeResolutionState;


pub fn operator_str(op: lexer::Operator) -> String {
    match op {
        Operator::Plus => "+".into(),
        Operator::Minus => "-".into(),
        Operator::Multiply => "*".into(),
        Operator::Divide => "/".into(),
        Operator::Equals => "==".into(),
        Operator::NotEquals => "!=".into(),
        Operator::Greater => ">".into(),
        Operator::GreaterEquals => ">=".into(),
        Operator::LessEquals => "<=".into(),
        Operator::Less => "<".into(),
        Operator::Mod => "%".into(),
        _ => "operator_str doesn't implement this operator".into(),
    }
}


pub fn expr_str(expr: &HIRExpr) -> String {
    match expr {
        HIRExpr::Trivial(trivial, ..) => trivial_expr_str(trivial),
        HIRExpr::FunctionCall(f, params, ..) => {
            let args_str = params
                .iter()
                .map(expr_str)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", expr_str(f), args_str)
        }
        HIRExpr::MethodCall(obj, name, params, ..) => {
            let args_str = params
                .iter()
                .map(expr_str)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}.{}({})", expr_str(obj), name, args_str)
        },

        HIRExpr::BinaryOperation(var, op, var2, ..) => format!(
            "{} {} {}",
            expr_str(var),
            operator_str(*op),
            expr_str(var2)
        ),

        HIRExpr::Array(items, ..) => {
            let args_str = items
                .iter()
                .map(expr_str)
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{}]", args_str)
        }
        HIRExpr::UnaryExpression(op, expr, ..) => {
            format!("{}{}", operator_str(*op), expr_str(expr))
        }
        HIRExpr::MemberAccess(obj, elem, ..) => {
            format!("{}.{}", expr_str(obj), elem)
        }
        HIRExpr::Cast(_, _, _) => "cast not implemented in HIR printer".to_string(),
    }
}


fn trivial_expr_str(expr: &TrivialHIRExpr) -> String {
    match &expr {
        TrivialHIRExpr::Variable(s) => s.clone(),
        TrivialHIRExpr::FloatValue(f) => format!("{:?}", f.0),
        TrivialHIRExpr::IntegerValue(i) => format!("{}", i),
        TrivialHIRExpr::StringValue(s) => format!("\"{}\"", s),
        TrivialHIRExpr::BooleanValue(true) => "True".to_string(),
        TrivialHIRExpr::BooleanValue(false) => "False".to_string(),
        TrivialHIRExpr::None => "None".into(),
    }
}



 
pub fn hir_type_def_str(typ: &HIRTypeDef, type_db: &TypeInstanceManager) -> String {
    fn slice_types_str(types: &[HIRType], type_db: &TypeInstanceManager) -> String {
        types
            .iter()
            .map(|x| hir_type_def_str(&HIRTypeDef::Unresolved(x.clone()), type_db))
            .collect::<Vec<_>>()
            .join(", ")
    }

    match typ {
        HIRTypeDef::PendingInference => "UNKNOWN_TYPE".into(),
        HIRTypeDef::Unresolved(HIRType::Simple(s)) => format!("UNRESOLVED! {}", s.clone()),
        HIRTypeDef::Unresolved(HIRType::Generic(s, g)) => {
            format!("UNRESOLVED {}<{}>", s, slice_types_str(g, type_db))
        }
        HIRTypeDef::Unresolved(HIRType::Function(args, return_type)) => format!(
            "UNRESOLVED ({}) -> {}",
            slice_types_str(args, type_db),
            hir_type_def_str(&HIRTypeDef::Unresolved(*return_type.clone()), type_db)
        ),
        HIRTypeDef::Resolved(instance) => instance.as_string(type_db),
    }
}

pub fn infer_state_type_str(typ: &HIRTypeResolutionState, type_db: &TypeInstanceManager) -> String {

    match typ {
        TypeResolvedState::Resolved(type_instance_id) => {
            type_db.get_instance(*type_instance_id).name.clone()
        },
        TypeResolvedState::Unresolved(unresolved) => {
            hir_type_def_str(&HIRTypeDef::Unresolved(unresolved.clone()), type_db)
        }
    }
}

#[allow(dead_code)]
fn print_hir_str(node: &HIR, indent: &str, type_db: &TypeInstanceManager) -> String {
   
    match node {
        HIR::Assign {
            path, expression, ..
        } => {
            format!("{}{} = {}\n", indent, path.join("."), expr_str(expression))
        }
        HIR::Declare {
            var,
            typedef: typename,
            expression,
            ..
        } => {
            format!(
                "{}{} : {} = {}\n",
                indent,
                var,
                hir_type_def_str(&typename.typedef_state, type_db),
                expr_str(expression)
            )
        }
        HIR::DeclareFunction {
            function_name,
            parameters,
            body,
            return_type,
            ..
        } => {
            let parameters = parameters
                .iter()
                .map(|param| {
                    format!(
                        "{}{}: {}",
                        indent,
                        param.name,
                        infer_state_type_str(&param.typename, type_db)
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");

            let mut function = format!(
                "{}def {}({}) -> {}:\n",
                indent,
                function_name,
                parameters,
                infer_state_type_str(return_type, type_db)
            );
            let indent_block = format!("{}    ", indent);
            for n in body {
                function.push_str(&print_hir_str(n, &indent_block, type_db));
            }
            function
        }
        HIR::Return(expr, ..) => {
            format!("{}return {}\n", indent, expr_str(expr))
        }
        HIR::EmptyReturn => {
            format!("{}return\n", indent)
        }
        HIR::StructDeclaration {
            struct_name, body, ..
        } => {
            let mut structdecl = format!("{}struct {}:\n", indent, struct_name);

            for field in body {
                structdecl.push_str(&format!(
                    "{}  {}: {}",
                    indent,
                    field.name,
                    infer_state_type_str(&field.typename, type_db)
                ));
            }

            structdecl
        }
        HIR::FunctionCall { function, args, .. } => {
            let args_str = args
                .iter()
                .map(expr_str)
                .collect::<Vec<_>>()
                .join(", ");

            format!("{}{}({})\n", indent, expr_str(function), args_str)
        }
        HIR::If(condition, true_body, false_body, ..) => {
            let condition_str = expr_str(condition);
            let mut ifdecl = format!("{}if {}:\n", indent, condition_str);
            let indent_block = format!("{}    ", indent);
            for statement in true_body {
                let statement_str = print_hir_str(statement, &indent_block, type_db);
                ifdecl.push_str(&statement_str);
            }
            ifdecl.push_str(&format!("{}else:\n", indent));
            for statement in false_body {
                let statement_str = print_hir_str(statement, &indent_block, type_db);
                ifdecl.push_str(&statement_str);
            }

            if false_body.is_empty() {
                ifdecl.push_str(&format!("{}pass\n", indent_block));
            }
            ifdecl
        }
    }
}


#[allow(dead_code)]
pub fn print_hir(mir: &[HIR], type_db: &TypeInstanceManager) -> String {
    let mut buffer = String::new();
    for node in mir {
        buffer.push_str(&print_hir_str(node, "", type_db));
    }
    buffer
}
