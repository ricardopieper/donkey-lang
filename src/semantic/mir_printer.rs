use crate::types::type_instance_db::TypeInstanceManager;

use super::mir::{
    LiteralMIRExpr, MIRBlock, MIRBlockFinal, MIRBlockNode, MIRExpr, MIRExprLValue, MIRExprRValue,
    MIRScope, MIRTopLevelNode,
};

pub struct MIRExprPrinter<'interner, 'type_db> {
    interner: &'interner StringInterner,
    type_db: &'type_db TypeInstanceManager<'interner>,
}

impl<'interner, 'type_db> MIRExprPrinter<'interner, 'type_db> {
    pub fn new(
        interner: &'interner StringInterner,
        type_db: &'type_db TypeInstanceManager<'interner>,
    ) -> Self {
        Self { interner, type_db }
    }

    pub fn print<T>(&self, expr: &MIRExpr<T>) -> String {
        match expr {
            MIRExpr::LValue(expr) => self.print_lvalue(expr),

            MIRExpr::RValue(MIRExprRValue::Literal(literal, ..)) => {
                self.print_literal_expr(literal)
            }
            MIRExpr::RValue(MIRExprRValue::FunctionCall(f, args, ..)) => {
                let args_str = args
                    .iter()
                    .map(|x| self.print(x))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", self.print_lvalue(f), args_str)
            }
            MIRExpr::RValue(MIRExprRValue::StructInstantiate(ty, _)) => {
                let struct_name = ty.as_string(self.type_db);
                format!("{struct_name}()")
            }
            MIRExpr::RValue(MIRExprRValue::MethodCall(obj, name, args, ..)) => {
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
            MIRExpr::RValue(MIRExprRValue::BinaryOperation(var, op, var2, ..)) => format!(
                "{} {} {}",
                self.print(var),
                op.0.to_string(),
                self.print(var2)
            ),
            MIRExpr::RValue(MIRExprRValue::Array(items, ..)) => {
                let array_items_str = items
                    .iter()
                    .map(|x| self.print(x))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{array_items_str}]")
            }
            MIRExpr::RValue(MIRExprRValue::UnaryExpression(op, expr, ..)) => {
                format!("{}{}", op.0.to_string(), self.print(expr))
            }

            MIRExpr::RValue(MIRExprRValue::Ref(expr, ..)) => {
                format!("&{}", self.print_lvalue(expr))
            }
            MIRExpr::TypecheckTag(_) => panic!("TypecheckTag should not be in MIR"),
        }
    }

    pub fn print_lvalue<T>(&self, expr: &MIRExprLValue<T>) -> String {
        match expr {
            MIRExprLValue::Variable(s, ..) => s.to_string(self.interner),
            MIRExprLValue::MemberAccess(obj, elem, ..) => {
                format!("{}.{}", self.print(obj), elem.to_string(self.interner))
            }
            MIRExprLValue::Deref(expr, ..) => format!("*{}", self.print(expr)),
        }
    }

    pub fn print_literal_expr(&self, expr: &LiteralMIRExpr) -> String {
        match &expr {
            LiteralMIRExpr::Char(c) => format!("\'{}\'", c),
            LiteralMIRExpr::Float(f) => format!("{:?}", f.0),
            LiteralMIRExpr::Integer(i) => format!("{i}"),
            LiteralMIRExpr::String(s) => format!("\"{}\"", self.interner.borrow(*s)),
            LiteralMIRExpr::Boolean(true) => self.interner.get("True").to_string(self.interner),
            LiteralMIRExpr::Boolean(false) => self.interner.get("False").to_string(self.interner),
            //LiteralMIRExpr::None => self.interner.get("None").to_string(self.interner),
        }
    }
}

use crate::interner::StringInterner;

fn print_mir_block<T>(
    block: &MIRBlock<T>,
    interner: &StringInterner,
    type_db: &TypeInstanceManager,
) -> String {
    let mut buffer = String::new();
    let expr_printer = MIRExprPrinter::new(interner, type_db);
    buffer.push_str(&format!("    defblock {}:\n", block.index));
    buffer.push_str(&format!("        usescope {}\n", block.scope.0));

    for node in &block.nodes {
        match node {
            MIRBlockNode::Assign {
                path, expression, ..
            } => {
                buffer.push_str(&format!(
                    "        {} = {}\n",
                    expr_printer.print_lvalue(path),
                    expr_printer.print(expression)
                ));
            }
            MIRBlockNode::FunctionCall { function, args, .. } => {
                let args_str = args
                    .iter()
                    .map(|x| expr_printer.print(x))
                    .collect::<Vec<_>>()
                    .join(", ");
                buffer.push_str(&format!(
                    "        {}({})\n",
                    function.to_string(interner),
                    args_str
                ));
            }
        }
    }

    match &block.finish {
        MIRBlockFinal::If(condition, true_branch, false_branch, ..) => {
            buffer.push_str(&format!(
                "        if {}:
            gotoblock {}
        else:
            gotoblock {}
",
                expr_printer.print(condition), //if condition:
                true_branch.0,                 //gotoblock 0
                false_branch.0
            )); //gotoblock 1
        }
        MIRBlockFinal::GotoBlock(block) => {
            buffer.push_str(&format!("        gotoblock {}\n", block.0));
        }
        MIRBlockFinal::Return(expr, ..) => {
            buffer.push_str(&format!("        return {}\n", expr_printer.print(expr)));
        }
        MIRBlockFinal::EmptyReturn(..) => {
            buffer.push_str("        return\n");
        }
    }

    buffer
}

fn print_mir_scope(
    scope: &MIRScope,
    type_db: &TypeInstanceManager,
    interner: &StringInterner,
) -> String {
    let mut buffer = String::new();

    buffer.push_str(&format!("    defscope {}:\n", scope.id.0));
    buffer.push_str(&format!("        inheritscope {}\n", scope.inherit.0));

    for name in &scope.boundnames {
        buffer.push_str(&format!(
            "        {} : {}\n",
            name.name.to_string(interner),
            name.type_instance.as_string(type_db)
        ));
    }

    buffer
}

fn print_mir_str<T>(
    node: &MIRTopLevelNode<T>,
    type_db: &TypeInstanceManager,
    interner: &StringInterner,
) -> String {
    match node {
        MIRTopLevelNode::IntrinsicFunction {
            function_name,
            parameters,
            return_type,
            is_varargs,
        } => {
            let parameters = parameters
                .iter()
                .map(|param| {
                    format!(
                        "{}: {}",
                        param.name.to_string(interner),
                        param.type_instance.as_string(type_db)
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");

            let varargs = if *is_varargs { ", ..." } else { "" };
            let function = format!(
                "def {}({}{varargs}) -> {}\n",
                function_name.to_string(interner),
                parameters,
                &return_type.as_string(type_db)
            );

            function
        }
        MIRTopLevelNode::DeclareFunction {
            function_name,
            parameters,
            body,
            scopes,
            return_type,
        } => {
            let parameters = parameters
                .iter()
                .map(|param| {
                    format!(
                        "{}: {}",
                        param.name.to_string(interner),
                        param.type_instance.as_string(type_db)
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            let mut function = format!(
                "def {}({}) -> {}:\n",
                function_name.to_string(interner),
                parameters,
                &return_type.as_string(type_db)
            );

            for s in scopes {
                function.push_str(&print_mir_scope(s, type_db, interner));
            }

            for n in body {
                function.push_str(&print_mir_block(n, interner, type_db));
            }

            function
        }
    }
}

pub fn print_mir<T>(
    mir: &[MIRTopLevelNode<T>],
    type_db: &TypeInstanceManager,
    interner: &StringInterner,
) -> String {
    let mut buffer = String::new();
    for node in mir {
        buffer.push_str(&print_mir_str(node, type_db, interner));
    }
    buffer
}

#[allow(dead_code)]
pub fn print_mir_node<T>(
    mir: &MIRTopLevelNode<T>,
    type_db: &TypeInstanceManager,
    interner: &StringInterner,
) -> String {
    let mut buffer = String::new();
    buffer.push_str(&print_mir_str(mir, type_db, interner));
    buffer
}
