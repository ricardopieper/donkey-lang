use super::hir::{HIRRoot, TypeTable};
use crate::{
    semantic::hir::{FunctionCall, HIR, HIRExpr, LiteralHIRExpr},
    types::type_constructor_db::TypeConstructorDatabase,
};

pub struct HIRExprPrinter<'type_db> {
    verbose: bool,
    type_db: &'type_db TypeConstructorDatabase,
}

impl<'type_db> HIRExprPrinter<'type_db> {
    pub fn new(verbose: bool, type_db: &'type_db TypeConstructorDatabase) -> Self {
        HIRExprPrinter { verbose, type_db }
    }

    pub fn print(&self, expr: &HIRExpr, type_table: &TypeTable) -> String {
        let expr_string = match expr {
            HIRExpr::Variable(s, ..) => s.into(),
            HIRExpr::Literal(literal, ..) => self.print_literal_expr(literal),
            HIRExpr::FunctionCall(fcall, ..) => {
                let args_str = fcall
                    .args
                    .iter()
                    .map(|x| self.print(x, type_table))
                    .collect::<Vec<_>>()
                    .join(", ");

                let type_args = if !fcall.type_args.is_empty() {
                    format!(
                        "<{}>",
                        fcall
                            .type_args
                            .iter()
                            .map(|x| x.resolved_type.print_name(type_table, self.type_db))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                } else {
                    "".into()
                };

                format!(
                    "{}{}({})",
                    self.print(&fcall.function, type_table),
                    type_args,
                    args_str
                )
            }
            HIRExpr::MethodCall(mcall, ..) => {
                let args_str = mcall
                    .args
                    .iter()
                    .map(|x| self.print(x, type_table))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "{}.{}({})",
                    self.print(&mcall.object, type_table),
                    mcall.method_name,
                    args_str
                )
            }

            HIRExpr::BinaryOperation(var, op, var2, ..) => format!(
                "{} {} {}",
                self.print(var, type_table),
                op.0.to_string(),
                self.print(var2, type_table)
            ),
            HIRExpr::Array(items, ..) => {
                let array_items_str = items
                    .iter()
                    .map(|x| self.print(x, type_table))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{array_items_str}]")
            }
            HIRExpr::UnaryExpression(op, expr, ..) => {
                format!("{}{}", op.0.to_string(), self.print(expr, type_table))
            }
            HIRExpr::MemberAccess(obj, elem, ..) => {
                format!("{}.{}", self.print(obj, type_table), elem)
            }
            HIRExpr::Cast(casted, user_type, _, ty) => {
                let ty_str = ty.print_name(type_table, self.type_db);
                let user_type_str = format!("{user_type}");
                //HACK: if the type inferred is (), then we prefer the user type... but I didn't want to do
                //a "template specialization" kinda thing here
                let chosen_ty_str = if ty_str.is_empty() {
                    user_type_str
                } else {
                    ty_str
                };
                format!(
                    "{expr} as {ty}",
                    expr = self.print(casted, type_table),
                    ty = chosen_ty_str
                )
            }
            HIRExpr::Deref(expr, ..) => format!("*{}", self.print(expr, type_table)),
            HIRExpr::Ref(expr, ..) => format!("&{}", self.print(expr, type_table)),
            HIRExpr::StructInstantiate(_, _, _, ty) => {
                //@TODO Factor this
                let struct_type_name = ty.print_name(type_table, self.type_db);
                format!("{struct_type_name}()")
            }
           /* HIRExpr::TypeName { type_variable, .. } => {
                type_variable.print_name(type_table, self.type_db).to_string()
            }*/
            HIRExpr::SelfValue(..) => "self".into(),
        };

        if self.verbose {
            let ty_name = expr.get_type();
            format!(
                "{{{}: {}}}",
                expr_string,
                ty_name.print_name(type_table, self.type_db)
            )
        } else {
            expr_string
        }
    }

    pub fn print_literal_expr(&self, expr: &LiteralHIRExpr) -> String {
        match &expr {
            LiteralHIRExpr::Float(f) => format!("{:?}", f.0),
            LiteralHIRExpr::Integer(i) => format!("{i}"),
            LiteralHIRExpr::Char(c) => format!("\'{c}\'"),
            LiteralHIRExpr::String(s) => format!("\"{}\"", s),
            LiteralHIRExpr::Boolean(true) => "True".into(),
            LiteralHIRExpr::Boolean(false) => "False".into(),
            LiteralHIRExpr::None => "None".into(),
        }
    }
}

pub struct HIRPrinter<'type_db> {
    type_db: &'type_db TypeConstructorDatabase,
    expr_printer: HIRExprPrinter<'type_db>,
}

impl<'type_db> HIRPrinter<'type_db> {
    #[allow(dead_code)]
    pub fn new(verbose: bool, type_db: &'type_db TypeConstructorDatabase) -> Self {
        HIRPrinter {
            expr_printer: HIRExprPrinter::new(verbose, type_db),
            type_db,
        }
    }

    pub fn print_hir_body_str(&self, node: &HIR, indent: &str, type_table: &TypeTable) -> String {
        match node {
            HIR::Assign {
                path, expression, ..
            } => {
                let lhs = self.expr_printer.print(path, type_table);
                format!(
                    "{}{} = {}\n",
                    indent,
                    lhs,
                    self.expr_printer.print(expression, type_table)
                )
            }
            HIR::Declare {
                var,
                typedef: typename,
                expression,
                ..
            } => {
                format!(
                    "{}{} : {} = {} (inferred: {})\n",
                    indent,
                    var,
                    typename.hir_type,
                    self.expr_printer.print(expression, type_table),
                    typename.type_variable.print_name(type_table, self.type_db)
                )
            }
            HIR::Return(expr, ..) => {
                format!(
                    "{}return {}\n",
                    indent,
                    self.expr_printer.print(expr, type_table)
                )
            }
            HIR::EmptyReturn(..) => {
                format!("{indent}return\n")
            }

            HIR::FunctionCall(FunctionCall { function, args, .. }, ..) => {
                self.print_function(args, indent, function, type_table)
            }
            HIR::If(condition, true_body, false_body, ..) => {
                let condition_str = self.expr_printer.print(condition, type_table);
                let mut ifdecl = format!("{indent}if {condition_str}:\n");
                let indent_block = format!("{indent}    ");
                for statement in true_body {
                    let statement_str =
                        self.print_hir_body_str(statement, &indent_block, type_table);
                    ifdecl.push_str(&statement_str);
                }
                ifdecl.push_str(&format!("{indent}else:\n"));
                for statement in false_body {
                    let statement_str =
                        self.print_hir_body_str(statement, &indent_block, type_table);
                    ifdecl.push_str(&statement_str);
                }

                if false_body.is_empty() {
                    ifdecl.push_str(&format!("{indent_block}pass\n"));
                }
                ifdecl
            }
            HIR::While(expr, body, ..) => {
                let expr_str = self.expr_printer.print(expr, type_table);
                let mut while_decl = format!("{indent}while {expr_str}:\n");
                let indent_block = format!("{indent}    ");
                for statement in body {
                    let statement_str =
                        self.print_hir_body_str(statement, &indent_block, type_table);
                    while_decl.push_str(&statement_str);
                }
                while_decl
            }
            HIR::MethodCall(mcall, ..) => {
                let args_str = mcall
                    .args
                    .iter()
                    .map(|x| self.expr_printer.print(x, type_table))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "{indent}{}.{}({})\n",
                    self.expr_printer.print(&mcall.object, type_table),
                    mcall.method_name,
                    args_str
                )
            }
            HIR::SyntheticDeclare {
                var,
                typedef,
                expression,
                ..
            } => {
                format!(
                    "{indent}{} : {} = {} [synth]\n",
                    var,
                    typedef.print_name(type_table, self.type_db),
                    self.expr_printer.print(expression, type_table)
                )
            }
        }
    }

    fn print_function(
        &self,
        args: &Vec<HIRExpr>,
        indent: &str,
        function: &HIRExpr,
        type_table: &TypeTable,
    ) -> String {
        let args_str = args
            .iter()
            .map(|x| self.expr_printer.print(x, type_table))
            .collect::<Vec<_>>()
            .join(", ");

        format!(
            "{}{}({})\n",
            indent,
            self.expr_printer.print(function, type_table),
            args_str
        )
    }

    pub fn print_hir_item(&self, node: &HIRRoot, indent: &str) -> String {
        match node {
            HIRRoot::DeclareFunction {
                function_name,
                parameters,
                body,
                return_type,
                type_parameters,
                type_table,
                method_of,
                ..
            } => {
                let mut parameters_printed = vec![];
                if method_of.is_some() {
                    parameters_printed.push("self".into());
                }
                parameters
                    .iter()
                    .map(|param| {
                        format!(
                            "{}{}: {} (inferred: {})",
                            indent,
                            param.name,
                            param.type_data.hir_type,
                            param
                                .type_data
                                .type_variable
                                .print_name(type_table, self.type_db)
                        )
                    })
                    .collect_into::<Vec<_>>(&mut parameters_printed);

                let parameters = parameters_printed.join(", ");

                let mut args = type_parameters
                    .iter()
                    .map(|x| x.0.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                if !args.is_empty() {
                    args = format!("<{}>", args);
                }

                let mut function = format!(
                    "{}def {}{}({}) -> {} (return inferred: {}):\n",
                    indent,
                    function_name,
                    args,
                    parameters,
                    return_type.hir_type,
                    return_type
                        .type_variable
                        .print_name(type_table, self.type_db)
                );
                let indent_block = format!("{indent}    ");
                for n in body {
                    function.push_str(&self.print_hir_body_str(n, &indent_block, type_table));
                }
                function
            }
            HIRRoot::StructDeclaration {
                struct_name,
                fields: body,
                type_table,
                ..
            } => {
                let mut structdecl = format!("{}struct {}:\n", indent, struct_name);
                let mut printed_fields = vec![];

                for field in body {
                    let ty = &type_table[field.type_data.type_variable];

                    printed_fields.push(format!(
                        "{}    {}: {}",
                        indent,
                        field.name,
                        ty.mono.print_name(self.type_db)
                    ));
                }
                structdecl.push_str(&printed_fields.join("\n"));
                structdecl.push('\n');

                structdecl
            }
            HIRRoot::ImplDeclaration {
                struct_name,
                type_parameters,
                methods,
                ..
            } => {
                let mut impldecl = format!(
                    "{}impl {}{}:\n",
                    indent,
                    struct_name,
                    type_parameters
                        .iter()
                        .map(|x| x.0.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                let indent_block = format!("{indent}    ");
                for method in methods {
                    impldecl.push_str(&self.print_hir_item(method, &indent_block));
                }
                impldecl
            }
        }
    }

    #[allow(dead_code)]
    pub fn print_hir(&self, hir: &[HIRRoot]) -> String {
        let mut buffer = String::new();
        for node in hir {
            buffer.push_str(&self.print_hir_item(node, ""));
        }
        buffer
    }
}
