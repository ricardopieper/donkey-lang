use crate::interner::StringInterner;

use crate::semantic::hir::{HIRExpr, LiteralHIRExpr, HIR, FunctionCall};

use crate::types::type_instance_db::TypeInstanceManager;

use super::hir::HIRRoot;
use super::type_name_printer::TypeNamePrinter;

pub struct HIRExprPrinter<'interner, 'type_db> {
    interner: &'interner StringInterner,
    type_db: &'type_db TypeInstanceManager<'interner>,
}

impl<'interner, 'type_db> HIRExprPrinter<'interner, 'type_db> {
    pub fn new(
        interner: &'interner StringInterner,
        type_db: &'type_db TypeInstanceManager<'interner>,
    ) -> Self {
        HIRExprPrinter { interner, type_db }
    }

    pub fn print<T>(&self, expr: &HIRExpr<T>) -> String {
        match expr {
            HIRExpr::Variable(s, ..) => s.to_string(self.interner),
            HIRExpr::Literal(literal, ..) => self.print_literal_expr(literal),
            HIRExpr::FunctionCall(fcall) => {
                let args_str = fcall.args
                    .iter()
                    .map(|x| self.print(x))
                    .collect::<Vec<_>>()
                    .join(", ");

                let type_args = if fcall.type_args.len() > 0 {
                    format!(
                        "<{}>",
                        fcall.type_args
                            .iter()
                            .map(|x| x.user_given_type.as_ref().unwrap().print_name(self.type_db, self.interner))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                } else {
                    "".into()
                };
                
                format!("{}{}({})", self.print(&fcall.function), type_args, args_str)
            }
            HIRExpr::MethodCall(obj, name, args, ..) => {
                let args_str = args
                    .iter()
                    .map(|x| self.print(x))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "{}.{}({})",
                    self.print(obj),
                    name.to_string(self.interner),
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
                format!("{}.{}", self.print(obj), elem.to_string(self.interner))
            }
            HIRExpr::Cast(_, _, _) => "cast not implemented in HIR printer".into(),
            HIRExpr::Deref(expr, ..) => format!("*{}", self.print(expr)),
            HIRExpr::Ref(expr, ..) => format!("&{}", self.print(expr)),
            HIRExpr::StructInstantiate(name, args, _, _) => {
                //@TODO Factor this
                let type_args = if args.len() > 0 {
                    format!(
                        "<{}>",
                        args
                            .iter()
                            .map(|x| x.print_name(self.type_db, self.interner))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                } else {
                    "".into()
                };
                
                let struct_name = name.to_string(self.interner);
                format!("{}{type_args}()", struct_name)
            }
        }
    }

    pub fn print_literal_expr(&self, expr: &LiteralHIRExpr) -> String {
        match &expr {
            LiteralHIRExpr::Float(f) => format!("{:?}", f.0),
            LiteralHIRExpr::Integer(i) => format!("{i}"),
            LiteralHIRExpr::Char(c) => format!("\'{c}\'"),
            LiteralHIRExpr::String(s) => format!("\"{}\"", self.interner.borrow(*s)),
            LiteralHIRExpr::Boolean(true) => self.interner.get("True").to_string(self.interner),
            LiteralHIRExpr::Boolean(false) => self.interner.get("False").to_string(self.interner),
            LiteralHIRExpr::None => self.interner.get("None").to_string(self.interner),
        }
    }
}

pub struct HIRPrinter<'type_db, 'interner> {
    type_db: &'type_db TypeInstanceManager<'interner>,
    interner: &'interner StringInterner,
    expr_printer: HIRExprPrinter<'interner, 'type_db>,
}

impl<'type_db, 'interner> HIRPrinter<'type_db, 'interner> {
    #[allow(dead_code)]
    pub fn new(
        type_db: &'type_db TypeInstanceManager<'interner>,
        interner: &'interner StringInterner,
    ) -> Self {
        HIRPrinter {
            type_db,
            interner,
            expr_printer: HIRExprPrinter::new(interner, type_db),
        }
    }

    fn print_hir_body_str(
        &self,
        node: &HIR<impl TypeNamePrinter, impl TypeNamePrinter>,
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
                    var.to_string(self.interner),
                    typename.print_name(self.type_db, self.interner),
                    self.expr_printer.print(expression)
                )
            }
            HIR::Return(expr, ..) => {
                format!("{}return {}\n", indent, self.expr_printer.print(expr))
            }
            HIR::EmptyReturn(..) => {
                format!("{indent}return\n")
            }

            HIR::FunctionCall(FunctionCall{ function, args, .. })=> {
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
        }
    }

    fn print_hir_str(
        &self,
        node: &HIRRoot<
            impl TypeNamePrinter,
            HIR<impl TypeNamePrinter, impl TypeNamePrinter>,
            impl TypeNamePrinter,
        >,
        indent: &str,
    ) -> String {
        match node {
            HIRRoot::DeclareFunction {
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
                            param.name.to_string(self.interner),
                            param.type_data.print_name(self.type_db, self.interner)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                let mut function = format!(
                    "{}def {}({}) -> {}:\n",
                    indent,
                    function_name.to_string(self.interner),
                    parameters,
                    return_type.print_name(self.type_db, self.interner)
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
                let mut structdecl = format!(
                    "{}struct {}:\n",
                    indent,
                    struct_name.to_string(self.interner)
                );

                for field in body {
                    structdecl.push_str(&format!(
                        "{}  {}: {}",
                        indent,
                        field.name.to_string(self.interner),
                        field.type_data.print_name(self.type_db, self.interner)
                    ));
                }

                structdecl
            }
        }
    }

    #[allow(dead_code)]
    pub fn print_hir(
        &self,
        hir: &[HIRRoot<
            impl TypeNamePrinter,
            HIR<impl TypeNamePrinter, impl TypeNamePrinter>,
            impl TypeNamePrinter,
        >],
    ) -> String {
        let mut buffer = String::new();
        for node in hir {
            buffer.push_str(&self.print_hir_str(node, ""));
        }
        buffer
    }
}
