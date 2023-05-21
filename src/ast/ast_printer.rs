use crate::{ast::parser::ASTIfStatement, interner::StringInterner};

use super::parser::{Expr, SpanAST, AST};

pub fn print_expr(expr: &Expr, interner: &StringInterner, parenthesized: bool) -> String {
    match expr {
        Expr::IntegerValue(i, _) => format!("{i}"),
        Expr::FloatValue(f, _) => format!("{}", f.0),
        Expr::StringValue(s) => format!("{:?}", interner.borrow(s.0)),
        Expr::CharValue(c, _) => format!("{:?}", c),
        Expr::BooleanValue(b, _) => if *b { "True" } else { "False" }.to_string(),
        Expr::NoneValue(_) => "None".to_string(),
        Expr::Variable(name) => interner.get_string(name.0),
        Expr::FunctionCall(function_expr, params, _) => {
            let call_expr_str = print_expr(function_expr, interner, parenthesized);
            let args = params
                .iter()
                .map(|x| print_expr(&x.expr, interner, parenthesized))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{call_expr_str}({args})")
        }
        Expr::IndexAccess(indexed_expr, index_expr, _) => {
            let indexed_expr_str = print_expr(indexed_expr, interner, parenthesized);
            let index_expr_str = print_expr(index_expr, interner, parenthesized);
            format!("{indexed_expr_str}[{index_expr_str}]")
        }
        Expr::BinaryOperation(lhs, op, rhs) => {
            let mut lhs_str = print_expr(lhs, interner, parenthesized);
            let mut rhs_str = print_expr(rhs, interner, parenthesized);
            let op_str = op.0.to_string();
            let lhs_is_binary = matches!(lhs.as_ref(), Expr::BinaryOperation(_, _, _));
            let rhs_is_binary = matches!(rhs.as_ref(), Expr::BinaryOperation(_, _, _));

            if lhs_is_binary {
                lhs_str = format!("({lhs_str})");
            }
            if rhs_is_binary {
                rhs_str = format!("({rhs_str})");
            }

            if parenthesized {
                format!("({lhs_str} {op_str} {rhs_str})")
            } else {
                format!("{lhs_str} {op_str} {rhs_str}")
            }
        }
        Expr::Parenthesized(_) => panic!("Parenthesized expr reached print_expr"),
        Expr::UnaryExpression(op, expr) => {
            let mut expr_str = print_expr(expr, interner, parenthesized);
            let expr_is_binary = matches!(expr.as_ref(), Expr::BinaryOperation(_, _, _));

            if expr_is_binary {
                expr_str = format!("({expr_str})");
            }
            let op_str = op.0.to_string();
            if parenthesized {
                format!("{op_str} ({expr_str})")
            } else {
                format!("{op_str} {expr_str}")
            }
        }
        Expr::MemberAccess(obj, member) => {
            let obj_str = print_expr(obj, interner, parenthesized);
            let member_str = interner.borrow(member.0);
            format!("{obj_str}.{member_str}")
        }
        Expr::Array(items, _) => {
            let items_str = items
                .iter()
                .map(|x| print_expr(&x.expr, interner, parenthesized))
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{items_str}]")
        }
    }
}

fn print_ast_internal(
    ast: &AST,
    interner: &StringInterner,
    indent: &str,
    parenthesized: bool,
) -> String {
    let indent_block = format!("{indent}    ");

    match ast {
        AST::StandaloneExpr(e) => format!(
            "{indent}{e}",
            e = print_expr(&e.expr, interner, parenthesized)
        ),
        AST::Assign { path, expression } => {
            let lhs_expr = print_expr(&path.expr, interner, parenthesized);
            let expr = print_expr(&expression.expr, interner, parenthesized);
            format!("{indent}{lhs_expr} = {expr}")
        }
        AST::Declare { var, expression } => {
            let var_name = interner.borrow(var.name.0);
            let type_name = var.name_type.to_string(interner);
            let expr = print_expr(&expression.expr, interner, parenthesized);
            format!("{var_name}: {type_name} = {expr}")
        }
        AST::IfStatement {
            true_branch,
            elifs,
            final_else,
        } => {
            let mut buf = "".to_string();

            let if_expr = print_expr(&true_branch.expression.expr, interner, parenthesized);
            buf.push_str(&format!("{indent}if {if_expr}:\n"));

            let body = true_branch
                .statements
                .iter()
                .map(|x| print_ast_internal(&x.ast, interner, &indent_block, parenthesized))
                .collect::<Vec<_>>()
                .join("\n");
            buf.push_str(&body);

            for ASTIfStatement {
                expression,
                statements,
            } in elifs.iter()
            {
                let expr = print_expr(&expression.expr, interner, parenthesized);
                buf.push_str(&format!("{indent}elif {expr}:\n"));
                let body = statements
                    .iter()
                    .map(|x| print_ast_internal(&x.ast, interner, &indent_block, parenthesized))
                    .collect::<Vec<_>>()
                    .join("\n");
                buf.push_str(&body);
            }
            if let Some(final_else_statements) = final_else {
                buf.push_str(&format!("{indent}\nelse:\n"));
                let body = final_else_statements
                    .iter()
                    .map(|x| print_ast_internal(&x.ast, interner, &indent_block, parenthesized))
                    .collect::<Vec<_>>()
                    .join("\n");
                buf.push_str(&body);
            }

            buf
        }
        AST::WhileStatement { expression, body } => {
            let mut buf = "".to_string();
            let if_expr = print_expr(&expression.expr, interner, parenthesized);
            buf.push_str(&format!("{indent}while {if_expr}:\n"));

            let body = body
                .iter()
                .map(|x| print_ast_internal(&x.ast, interner, &indent_block, parenthesized))
                .collect::<Vec<_>>()
                .join("\n");
            buf.push_str(&body);

            buf
        }
        AST::ForStatement {
            item_name,
            list_expression,
            body,
        } => {
            let mut buf = "".to_string();
            let item_var = interner.borrow(item_name.0);
            let list_expr_str = print_expr(&list_expression.expr, interner, parenthesized);

            buf.push_str(&format!("{indent}for {item_var} in {list_expr_str}:\n"));
            let indent_block = format!("{indent}    ");

            let body = body
                .iter()
                .map(|x| print_ast_internal(&x.ast, interner, &indent_block, parenthesized))
                .collect::<Vec<_>>()
                .join("\n");
            buf.push_str(&body);
            buf
        }
        AST::StructDeclaration {
            struct_name,
            type_parameters,
            body,
        } => {
            let mut buf = "".to_string();
            let struct_name = interner.borrow(struct_name.0);
            if !type_parameters.is_empty() {
                let generic_args = type_parameters
                    .iter()
                    .map(|x| interner.borrow(x.0))
                    .collect::<Vec<_>>()
                    .join(", ");
                buf.push_str(&format!("{indent}struct {struct_name}<{generic_args}>:"));
            } else {
                buf.push_str(&format!("{indent}struct {struct_name}:"));
            }
            buf.push('\n');

            for b in body {
                let var_name = interner.borrow(b.name.0);
                let type_name = b.name_type.to_string(interner);
                buf.push_str(&format!("{indent}    {var_name}: {type_name}"));
                buf.push('\n');
            }
            buf
        }
        AST::DeclareFunction {
            function_name,
            parameters,
            body,
            return_type,
            is_varargs,
        } => {
            let mut buf = "".to_string();
            let function_name = interner.borrow(function_name.0);
            let params = parameters
                .iter()
                .map(|x| {
                    let var_name = interner.borrow(x.name.0);
                    let type_name = x.name_type.to_string(interner);
                    format!("{var_name}: {type_name}")
                })
                .collect::<Vec<_>>()
                .join(", ");

            let varargs = if *is_varargs { "..." } else { "" };

            let comma_if_params = if !params.is_empty() && *is_varargs {
                ", "
            } else {
                ""
            };

            match return_type {
                Some(x) => {
                    let return_type = x.to_string(interner);
                    buf.push_str(&format!(
                        "{indent}def {function_name}({params}{comma_if_params}{varargs}) -> {return_type}:"
                    ));
                }
                None => {
                    buf.push_str(&format!(
                        "{indent}def {function_name}({params}{comma_if_params}{varargs}):"
                    ));
                }
            }
            buf.push('\n');
            let indent_block = format!("{indent}    ");

            let body = body
                .iter()
                .map(|x| print_ast_internal(&x.ast, interner, &indent_block, parenthesized))
                .collect::<Vec<_>>()
                .join("\n");
            buf.push_str(&body);
            buf
        }
        AST::Break(_) => format!("{indent}break"),
        AST::Intrinsic(_) => format!("{indent}intrinsic"),
        AST::Return(_, ret_expr) => match ret_expr {
            Some(x) => {
                let expr = print_expr(&x.expr, interner, parenthesized);
                format!("{indent}return {expr}")
            }
            None => format!("{indent}return"),
        },
        AST::Root(r) => {
            return r
                .iter()
                .map(|x| print_ast_internal(&x.ast, interner, indent, parenthesized))
                .collect::<Vec<_>>()
                .join("\n");
        }
    }
}

#[allow(dead_code)]
pub fn print_ast(ast: &[SpanAST], interner: &StringInterner) -> String {
    return ast
        .iter()
        .map(|x| print_ast_internal(&x.ast, interner, "", false))
        .collect::<Vec<_>>()
        .join("\n");
}

#[allow(dead_code)]
pub fn print_fully_parenthesized_ast(ast: &[SpanAST], interner: &StringInterner) -> String {
    return ast
        .iter()
        .map(|x| print_ast_internal(&x.ast, interner, "", true))
        .collect::<Vec<_>>()
        .join("\n");
}
