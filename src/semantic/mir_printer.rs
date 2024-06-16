use crate::types::type_instance_db::TypeInstanceManager;

use super::mir::{
    LiteralMIRExpr, MIRBlock, MIRBlockFinal, MIRBlockNode, MIRExpr, MIRExprLValue, MIRExprRValue,
    MIRScope, MIRTopLevelNode,
};

pub struct MIRPrinter<'type_db> {
    type_db: &'type_db TypeInstanceManager,
}

impl<'type_db> MIRPrinter<'type_db> {
    pub fn new(type_db: &'type_db TypeInstanceManager) -> Self {
        Self { type_db }
    }

    pub fn print(&self, expr: &MIRExpr) -> String {
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
                let struct_name = ty.to_string(self.type_db);
                format!("{struct_name}()")
            }
            MIRExpr::RValue(MIRExprRValue::MethodCall(obj, name, args, ..)) => {
                let args_str = args
                    .iter()
                    .map(|x| self.print(x))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}.{}({})", self.print(obj), name, args_str)
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
            MIRExpr::RValue(MIRExprRValue::Cast(expr, ty, ..)) => {
                format!(
                    "{expr} as {ty}",
                    expr = self.print(expr),
                    ty = ty.to_string(self.type_db)
                )
            }
            MIRExpr::RValue(MIRExprRValue::TypeVariable { type_variable, .. }) => {
                format!("{}", type_variable.to_string(self.type_db))
            }
        }
    }

    pub fn print_lvalue(&self, expr: &MIRExprLValue) -> String {
        match expr {
            MIRExprLValue::Variable(s, ..) => s.into(),
            MIRExprLValue::MemberAccess(obj, elem, ..) => {
                format!("{}.{}", self.print(obj), elem)
            }
            MIRExprLValue::Deref(expr, ..) => format!("*{}", self.print(expr)),
        }
    }

    pub fn print_literal_expr(&self, expr: &LiteralMIRExpr) -> String {
        match &expr {
            LiteralMIRExpr::Char(c) => format!("\'{}\'", c),
            LiteralMIRExpr::Float(f) => format!("{:?}", f.0),
            LiteralMIRExpr::Integer(i) => format!("{i}"),
            LiteralMIRExpr::String(s) => format!("\"{}\"", s),
            LiteralMIRExpr::Boolean(true) => "True".into(),
            LiteralMIRExpr::Boolean(false) => "False".into(),
            //LiteralMIRExpr::None => self.interner.get("None").into(),
        }
    }

    pub fn print_mir_block_node(&self, node: &MIRBlockNode<'_>) -> String {
        match node {
            MIRBlockNode::Assign {
                path, expression, ..
            } => {
                format!(
                    "        {} = {}\n",
                    self.print_lvalue(path),
                    self.print(expression)
                )
            }
            MIRBlockNode::FunctionCall { function, args, .. } => {
                let args_str = args
                    .iter()
                    .map(|x| self.print(x))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("        {}({})\n", function, args_str)
            }
            MIRBlockNode::MethodCall {
                object,
                method_name,
                args,
                ..
            } => {
                let args_str = args
                    .iter()
                    .map(|x| self.print(x))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "        {}.{}({})\n",
                    self.print(object),
                    method_name,
                    args_str
                )
            }
        }
    }
}

pub fn print_mir_block(block: &MIRBlock, type_db: &TypeInstanceManager) -> String {
    let mut buffer = String::new();
    let printer = MIRPrinter::new(type_db);
    buffer.push_str(&format!("    defblock {}:\n", block.index));
    buffer.push_str(&format!("        usescope {}\n", block.scope.0));

    for node in &block.nodes {
        let s = printer.print_mir_block_node(node);
        buffer.push_str(&s);
    }

    match &block.finish {
        MIRBlockFinal::If(condition, true_branch, false_branch, ..) => {
            buffer.push_str(&format!(
                "        if {}:
            gotoblock {}
        else:
            gotoblock {}
",
                printer.print(condition), //if condition:
                true_branch.0,            //gotoblock 0
                false_branch.0
            )); //gotoblock 1
        }
        MIRBlockFinal::GotoBlock(block) => {
            buffer.push_str(&format!("        gotoblock {}\n", block.0));
        }
        MIRBlockFinal::Return(expr, ..) => {
            buffer.push_str(&format!("        return {}\n", printer.print(expr)));
        }
        MIRBlockFinal::EmptyReturn(..) => {
            buffer.push_str("        return\n");
        }
    }

    buffer
}

fn print_mir_scope(scope: &MIRScope, type_db: &TypeInstanceManager) -> String {
    let mut buffer = String::new();

    buffer.push_str(&format!("    defscope {}:\n", scope.id.0));
    buffer.push_str(&format!("        inheritscope {}\n", scope.inherit.0));

    for name in &scope.boundnames {
        buffer.push_str(&format!(
            "        {} : {}\n",
            name.name,
            name.type_instance.to_string(type_db)
        ));
    }

    buffer
}

fn print_mir_str(node: &MIRTopLevelNode, type_db: &TypeInstanceManager) -> String {
    match node {
        MIRTopLevelNode::IntrinsicOrExternalFunction {
            function_name,
            parameters,
            return_type,
            is_varargs,
            is_external,
        } => {
            let parameters = parameters
                .iter()
                .map(|param| format!("{}: {}", param.name, param.type_instance.to_string(type_db)))
                .collect::<Vec<_>>()
                .join(", ");

            let varargs = if *is_varargs { ", ..." } else { "" };
            let function = format!(
                "def {}({}{varargs}) -> {}\n",
                function_name,
                parameters,
                &return_type.to_string(type_db)
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
                .map(|param| format!("{}: {}", param.name, param.type_instance.to_string(type_db)))
                .collect::<Vec<_>>()
                .join(", ");
            let mut function = format!(
                "def {}({}) -> {}:\n",
                function_name,
                parameters,
                &return_type.to_string(type_db)
            );

            for s in scopes {
                function.push_str(&print_mir_scope(s, type_db));
            }

            for n in body {
                function.push_str(&print_mir_block(n, type_db));
            }

            function
        }
    }
}

pub fn print_mir(mir: &[MIRTopLevelNode], type_db: &TypeInstanceManager) -> String {
    let mut buffer = String::new();
    for node in mir {
        buffer.push_str(&print_mir_str(node, type_db));
    }
    buffer
}

#[allow(dead_code)]
pub fn print_mir_node(mir: &MIRTopLevelNode, type_db: &TypeInstanceManager) -> String {
    let mut buffer = String::new();
    buffer.push_str(&print_mir_str(mir, type_db));
    buffer
}
