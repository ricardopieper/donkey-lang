use crate::semantic::hir::{FunctionCall, HIRExpr, LiteralHIRExpr, HIR};

use crate::types::type_instance_db::TypeInstanceManager;

use super::hir::HIRRoot;
use super::type_name_printer::TypeNamePrinter;

pub struct HIRExprPrinter<'type_db> {
    type_db: &'type_db TypeInstanceManager,
    verbose: bool,
}

impl<'type_db> HIRExprPrinter<'type_db> {
    pub fn new(type_db: &'type_db TypeInstanceManager, verbose: bool) -> Self {
        HIRExprPrinter { type_db, verbose }
    }

    pub fn print(&self, expr: &HIRExpr<impl TypeNamePrinter + Clone>) -> String {
        let expr_string = match expr {
            HIRExpr::Variable(s, ..) => s.into(),
            HIRExpr::Literal(literal, ..) => self.print_literal_expr(literal),
            HIRExpr::FunctionCall(fcall) => {
                let args_str = fcall
                    .args
                    .iter()
                    .map(|x| self.print(x))
                    .collect::<Vec<_>>()
                    .join(", ");

                let type_args = if fcall.type_args.len() > 0 {
                    format!(
                        "<{}>",
                        fcall
                            .type_args
                            .iter()
                            .map(|x| x.resolved_type.print_name(self.type_db))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                } else {
                    "".into()
                };

                format!("{}{}({})", self.print(&fcall.function), type_args, args_str)
            }
            HIRExpr::MethodCall(mcall) => {
                let args_str = mcall
                    .args
                    .iter()
                    .map(|x| self.print(x))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "{}.{}({})",
                    self.print(&*mcall.object),
                    mcall.method_name,
                    args_str
                )
            }

            HIRExpr::BinaryOperation(var, op, var2, ..) => format!(
                "{} {} {}",
                self.print(var),
                op.0.to_string(),
                self.print(var2)
            ),
            HIRExpr::Array(items, ..) => {
                let array_items_str = items
                    .iter()
                    .map(|x| self.print(x))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{array_items_str}]")
            }
            HIRExpr::UnaryExpression(op, expr, ..) => {
                format!("{}{}", op.0.to_string(), self.print(expr))
            }
            HIRExpr::MemberAccess(obj, elem, ..) => {
                format!("{}.{}", self.print(obj), elem)
            }
            HIRExpr::Cast(casted, user_type, ty, _) => {
                let ty_str = ty.print_name(self.type_db);
                let user_type_str = user_type.print_name(self.type_db);
                //HACK: if the type inferred is (), then we prefer the user type... but I didn't want to do
                //a "template specialization" kinda thing here
                let chosen_ty_str = if ty_str.is_empty() {
                    user_type_str
                } else {
                    ty_str
                };
                format!(
                    "{expr} as {ty}",
                    expr = self.print(casted),
                    ty = chosen_ty_str
                )
            }
            HIRExpr::Deref(expr, ..) => format!("*{}", self.print(expr)),
            HIRExpr::Ref(expr, ..) => format!("&{}", self.print(expr)),
            HIRExpr::StructInstantiate(_, _, ty, _) => {
                //@TODO Factor this
                let struct_type_name = ty.print_name(self.type_db);
                format!("{struct_type_name}()")
            }
            HIRExpr::TypeName { type_variable, .. } => {
                format!("{}", type_variable.print_name(self.type_db))
            }
            HIRExpr::SelfValue(_, _) => "self".into(),
        };

        if self.verbose {
            let ty_name = expr.get_type();
            println!(
                "PRINTING CALL: {}: {}",
                expr_string,
                ty_name.print_name(self.type_db)
            );
            format!("{{{}: {}}}", expr_string, ty_name.print_name(self.type_db))
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
    type_db: &'type_db TypeInstanceManager,
    expr_printer: HIRExprPrinter<'type_db>,
}

impl<'type_db> HIRPrinter<'type_db> {
    #[allow(dead_code)]
    pub fn new(type_db: &'type_db TypeInstanceManager, verbose: bool) -> Self {
        HIRPrinter {
            type_db,
            expr_printer: HIRExprPrinter::new(type_db, verbose),
        }
    }

    fn print_hir_body_str<T: TypeNamePrinter + Clone, U: TypeNamePrinter + Clone>(
        &self,
        node: &HIR<T, U>,
        indent: &str,
    ) -> String {
        match node {
            HIR::Assign {
                path, expression, ..
            } => {
                let lhs = self.expr_printer.print(path);
                format!(
                    "{}{} = {}\n",
                    indent,
                    lhs,
                    self.expr_printer.print(expression)
                )
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
                    typename.print_name(self.type_db),
                    self.expr_printer.print(expression)
                )
            }
            HIR::Return(expr, ..) => {
                format!("{}return {}\n", indent, self.expr_printer.print(expr))
            }
            HIR::EmptyReturn(..) => {
                format!("{indent}return\n")
            }

            HIR::FunctionCall(FunctionCall { function, args, .. }) => {
                self.print_function(args, indent, function)
            }
            HIR::If(condition, true_body, false_body, ..) => {
                let condition_str = self.expr_printer.print(condition);
                let mut ifdecl = format!("{indent}if {condition_str}:\n");
                let indent_block = format!("{indent}    ");
                for statement in true_body {
                    let statement_str = self.print_hir_body_str(statement, &indent_block);
                    ifdecl.push_str(&statement_str);
                }
                ifdecl.push_str(&format!("{indent}else:\n"));
                for statement in false_body {
                    let statement_str = self.print_hir_body_str(statement, &indent_block);
                    ifdecl.push_str(&statement_str);
                }

                if false_body.is_empty() {
                    ifdecl.push_str(&format!("{indent_block}pass\n"));
                }
                ifdecl
            }
            HIR::While(expr, body, ..) => {
                let expr_str = self.expr_printer.print(expr);
                let mut while_decl = format!("{indent}while {expr_str}:\n");
                let indent_block = format!("{indent}    ");
                for statement in body {
                    let statement_str = self.print_hir_body_str(statement, &indent_block);
                    while_decl.push_str(&statement_str);
                }
                while_decl
            }
            HIR::MethodCall(mcall) => {
                let args_str = mcall
                    .args
                    .iter()
                    .map(|x| self.expr_printer.print(x))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "{indent}{}.{}({})\n",
                    self.expr_printer.print(&*mcall.object),
                    mcall.method_name,
                    args_str
                )
            }
        }
    }

    fn print_function<U: TypeNamePrinter + Clone>(
        &self,
        args: &Vec<HIRExpr<'_, U>>,
        indent: &str,
        function: &HIRExpr<'_, U>,
    ) -> String {
        let args_str = args
            .iter()
            .map(|x| self.expr_printer.print(x))
            .collect::<Vec<_>>()
            .join(", ");

        format!(
            "{}{}({})\n",
            indent,
            self.expr_printer.print(function),
            args_str
        )
    }

    pub fn print_hir_item<
        T: TypeNamePrinter + Clone,
        U: TypeNamePrinter + Clone,
        V: TypeNamePrinter + Clone,
        X: TypeNamePrinter + Clone,
    >(
        &self,
        node: &HIRRoot<T, HIR<U, V>, X>,
        indent: &str,
    ) -> String {
        match node {
            HIRRoot::DeclareFunction {
                function_name,
                parameters,
                body,
                return_type,
                type_parameters,
                ..
            } => {
                let parameters = parameters
                    .iter()
                    .map(|param| {
                        format!(
                            "{}{}: {}",
                            indent,
                            param.name,
                            param.type_data.print_name(self.type_db)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                let mut args = type_parameters
                    .iter()
                    .map(|x| x.0.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                if args.len() > 0 {
                    args = format!("<{}>", args);
                }

                let mut function = format!(
                    "{}def {}{}({}) -> {}:\n",
                    indent,
                    function_name,
                    args,
                    parameters,
                    return_type.print_name(self.type_db)
                );
                let indent_block = format!("{indent}    ");
                for n in body {
                    function.push_str(&self.print_hir_body_str(n, &indent_block));
                }
                function
            }
            HIRRoot::StructDeclaration {
                struct_name,
                fields: body,
                ..
            } => {
                let mut structdecl = format!("{}struct {}:\n", indent, struct_name);

                for field in body {
                    structdecl.push_str(&format!(
                        "{}  {}: {}",
                        indent,
                        field.name,
                        field.type_data.print_name(self.type_db)
                    ));
                }

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
    pub fn print_hir<
        T: TypeNamePrinter + Clone,
        U: TypeNamePrinter + Clone,
        V: TypeNamePrinter + Clone,
        X: TypeNamePrinter + Clone,
    >(
        &self,
        hir: &[HIRRoot<T, HIR<U, V>, X>],
    ) -> String {
        let mut buffer = String::new();
        for node in hir {
            buffer.push_str(&self.print_hir_item(node, ""));
        }
        buffer
    }
}
