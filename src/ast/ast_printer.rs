use super::parser::{ASTIfStatement, Expr, FunctionDeclaration, SpanAST, StringSpan, AST};

pub fn print_expr(expr: &Expr, parenthesized: bool) -> String {
    match expr {
        Expr::IntegerValue(i, _) => i.to_string(),
        Expr::FloatValue(f, _) => f.0.to_string(),
        Expr::StringValue(s) => format!("{:?}", s.0.to_string()),
        Expr::CharValue(c, _) => format!("{:?}", c),
        Expr::BooleanValue(b, _) => if *b { "True" } else { "False" }.to_string(),
        Expr::NoneValue(_) => "None".to_string(),
        Expr::Variable(name) => name.0.to_string(),
        Expr::FunctionCall(function_expr, type_parameters, params, _) => {
            let call_expr_str = print_expr(function_expr, parenthesized);
            let generic_args = if !type_parameters.is_empty() {
                let generic_args = type_parameters
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("<{generic_args}>")
            } else {
                "".to_string()
            };

            let args = params
                .iter()
                .map(|x| print_expr(&x.expr, parenthesized))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{call_expr_str}{generic_args}({args})")
        }
        Expr::IndexAccess(indexed_expr, index_expr, _) => {
            let indexed_expr_str = print_expr(indexed_expr, parenthesized);
            let index_expr_str = print_expr(index_expr, parenthesized);
            format!("{indexed_expr_str}[{index_expr_str}]")
        }
        Expr::BinaryOperation(lhs, op, rhs) => {
            let mut lhs_str = print_expr(lhs, parenthesized);
            let mut rhs_str = print_expr(rhs, parenthesized);
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
            let mut expr_str = print_expr(expr, parenthesized);
            let expr_is_binary = matches!(expr.as_ref(), Expr::BinaryOperation(_, _, _));

            if expr_is_binary {
                expr_str = format!("({expr_str})");
            }
            let op_str = op.0.to_string();
            if parenthesized {
                format!("({op_str} ({expr_str}))")
            } else {
                format!("{op_str} {expr_str}")
            }
        }
        Expr::MemberAccess(obj, member) => {
            let obj_str = print_expr(obj, parenthesized);
            let member_str = member.0;
            format!("{obj_str}.{member_str}")
        }
        Expr::Array(items, _) => {
            let items_str = items
                .iter()
                .map(|x| print_expr(&x.expr, parenthesized))
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{items_str}]")
        }
        Expr::Cast(expr, ty, _) => {
            let expr_str = print_expr(expr, parenthesized);
            let ty_str = ty.to_string();
            if parenthesized {
                format!("({expr_str} as {ty_str})")
            } else {
                format!("{expr_str} as {ty_str}")
            }
        }
        Expr::SelfValue(_) => "self".to_string(),
    }
}

fn print_ast_internal(ast: &AST, indent: &str, parenthesized: bool) -> String {
    let indent_block = format!("{indent}    ");

    match ast {
        AST::StandaloneExpr(e) => format!("{indent}{e}", e = print_expr(&e.expr, parenthesized)),
        AST::Assign { path, expression } => {
            let lhs_expr = print_expr(&path.expr, parenthesized);
            let expr = print_expr(&expression.expr, parenthesized);
            format!("{indent}{lhs_expr} = {expr}")
        }
        AST::Declare { var, expression } => {
            let var_name = var.name.0;
            let type_name = var.name_type.to_string();
            let expr = print_expr(&expression.expr, parenthesized);
            format!("{var_name}: {type_name} = {expr}")
        }
        AST::IfStatement {
            true_branch,
            elifs,
            final_else,
        } => {
            let mut buf = "".to_string();

            let if_expr = print_expr(&true_branch.expression.expr, parenthesized);
            buf.push_str(&format!("{indent}if {if_expr}:\n"));

            let body = true_branch
                .statements
                .iter()
                .map(|x| print_ast_internal(&x.ast, &indent_block, parenthesized))
                .collect::<Vec<_>>()
                .join("\n");
            buf.push_str(&body);

            for ASTIfStatement {
                expression,
                statements,
            } in elifs.iter()
            {
                let expr = print_expr(&expression.expr, parenthesized);
                buf.push_str(&format!("{indent}elif {expr}:\n"));
                let body = statements
                    .iter()
                    .map(|x| print_ast_internal(&x.ast, &indent_block, parenthesized))
                    .collect::<Vec<_>>()
                    .join("\n");
                buf.push_str(&body);
            }
            if let Some(final_else_statements) = final_else {
                buf.push_str(&format!("{indent}\nelse:\n"));
                let body = final_else_statements
                    .iter()
                    .map(|x| print_ast_internal(&x.ast, &indent_block, parenthesized))
                    .collect::<Vec<_>>()
                    .join("\n");
                buf.push_str(&body);
            }

            buf
        }
        AST::WhileStatement { expression, body } => {
            let mut buf = "".to_string();
            let if_expr = print_expr(&expression.expr, parenthesized);
            buf.push_str(&format!("{indent}while {if_expr}:\n"));

            let body = body
                .iter()
                .map(|x| print_ast_internal(&x.ast, &indent_block, parenthesized))
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
            let item_var = item_name.0;
            let list_expr_str = print_expr(&list_expression.expr, parenthesized);

            buf.push_str(&format!("{indent}for {item_var} in {list_expr_str}:\n"));
            let indent_block = format!("{indent}    ");

            let body = body
                .iter()
                .map(|x| print_ast_internal(&x.ast, &indent_block, parenthesized))
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
            let struct_name = struct_name.0;
            if !type_parameters.is_empty() {
                let generic_args = join_comma_sep(type_parameters);
                buf.push_str(&format!("{indent}struct {struct_name}<{generic_args}>:"));
            } else {
                buf.push_str(&format!("{indent}struct {struct_name}:"));
            }
            buf.push('\n');

            for b in body {
                let var_name = b.name.0;
                let type_name = &b.name_type;
                buf.push_str(&format!("{indent}    {var_name}: {type_name}"));
                buf.push('\n');
            }
            buf
        }
        AST::DeclareFunction(fdecl) => print_fdecl(fdecl, parenthesized, indent),
        AST::Break(_) => format!("{indent}break"),
        AST::Intrinsic(_) => format!("{indent}intrinsic"),
        AST::Return(_, ret_expr) => match ret_expr {
            Some(x) => {
                let expr = print_expr(&x.expr, parenthesized);
                format!("{indent}return {expr}")
            }
            None => format!("{indent}return"),
        },
        AST::Root(r) => {
            return r
                .iter()
                .map(|x| print_ast_internal(&x.ast, indent, parenthesized))
                .collect::<Vec<_>>()
                .join("\n");
        }
        AST::External(_) => format!("{indent}external"),
        AST::ImplDeclaration {
            struct_name,
            type_parameters,
            body,
        } => {
            let mut buf = "".to_string();
            let struct_name = struct_name.0;
            if !type_parameters.is_empty() {
                let generic_args = join_comma_sep(type_parameters);
                buf.push_str(&format!("{indent}impl {struct_name}<{generic_args}>:"));
            } else {
                buf.push_str(&format!("{indent}impl {struct_name}:"));
            }
            buf.push('\n');

            for method in body {
                let printed_fdecl = print_fdecl(method, parenthesized, &indent_block);
                buf.push_str(&printed_fdecl);
                buf.push('\n');
            }
            return buf;
        }
    }
}

fn print_fdecl(fdecl: &FunctionDeclaration, parenthesized: bool, indent: &str) -> String {
    let FunctionDeclaration {
        function_name,
        parameters,
        body,
        type_parameters,
        return_type,
        is_varargs,
        is_method,
    } = fdecl;
    let mut buf = "".to_string();
    let function_name = function_name.0;
    let mut params = parameters
        .iter()
        .map(|x| {
            let var_name = x.name.0;
            let type_name = &x.name_type;
            format!("{var_name}: {type_name}")
        })
        .collect::<Vec<_>>();

    if *is_method {
        params.insert(0, "self".to_string())
    }

    let params = params.join(", ");

    let varargs = if *is_varargs { "..." } else { "" };

    let comma_if_params = if !params.is_empty() && *is_varargs {
        ", "
    } else {
        ""
    };

    let generic_args = if !type_parameters.is_empty() {
        let generic_args = join_comma_sep(type_parameters);
        format!("<{generic_args}>")
    } else {
        "".to_string()
    };

    match return_type {
        Some(x) => {
            let return_type = x;
            buf.push_str(&format!(
                "{indent}def {function_name}{generic_args}({params}{comma_if_params}{varargs}) -> {return_type}:"
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
        .map(|x| print_ast_internal(&x.ast, &indent_block, parenthesized))
        .collect::<Vec<_>>()
        .join("\n");
    buf.push_str(&body);
    buf
}

fn join_comma_sep(type_parameters: &[StringSpan]) -> String {
    type_parameters
        .iter()
        .map(|x| x.0.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

#[allow(dead_code)]
pub fn print_ast(ast: &[SpanAST]) -> String {
    return ast
        .iter()
        .map(|x| print_ast_internal(&x.ast, "", false))
        .collect::<Vec<_>>()
        .join("\n");
}

#[allow(dead_code)]
pub fn print_fully_parenthesized_ast(ast: &[SpanAST]) -> String {
    return ast
        .iter()
        .map(|x| print_ast_internal(&x.ast, "", true))
        .collect::<Vec<_>>()
        .join("\n");
}
