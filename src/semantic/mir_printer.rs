use std::borrow::Cow;

use crate::types::type_instance_db::TypeInstanceManager;

use super::mir::{MIRBlock, MIRBlockFinal, MIRBlockNode, MIRScope, MIRTopLevelNode};

pub trait PrintableExpression {
    fn print_expr<'source>(&'source self) -> Cow<'source, str>;
}

fn print_mir_block<T>(block: &MIRBlock<T>) -> String {
    let mut buffer = String::new();

    buffer.push_str(&format!("    defblock {}:\n", block.index));
    buffer.push_str(&format!("        usescope {}\n", block.scope.0));

    for node in &block.nodes {
        match node {
            MIRBlockNode::Assign {
                path, expression, ..
            } => {
                buffer.push_str(&format!(
                    "        {} = {}\n",
                    path.join("."),
                    expression.print_expr()
                ));
            }
            MIRBlockNode::FunctionCall { function, args, .. } => {
                let args_str = args
                    .iter()
                    .map(|x| x.print_expr())
                    .collect::<Vec<_>>()
                    .join(", ");
                buffer.push_str(&format!("        {}({})\n", function, args_str));
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
                condition.print_expr(), //if condition:
                true_branch.0,          //gotoblock 0
                false_branch.0
            )); //gotoblock 1
        }
        MIRBlockFinal::GotoBlock(block) => {
            buffer.push_str(&format!("        gotoblock {}\n", block.0));
        }
        MIRBlockFinal::Return(expr, ..) => {
            buffer.push_str(&format!("        return {}\n", expr.print_expr()));
        }
        MIRBlockFinal::EmptyReturn => {
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
            name.type_instance.as_string(type_db)
        ));
    }

    buffer
}

fn print_mir_str<T>(node: &MIRTopLevelNode<T>, type_db: &TypeInstanceManager) -> String {
    match node {
        MIRTopLevelNode::IntrinsicFunction {
            function_name,
            parameters,
            return_type,
        } => {
            let parameters = parameters
                .iter()
                .map(|param| format!("{}: {}", param.name, param.type_instance.as_string(type_db)))
                .collect::<Vec<_>>()
                .join(", ");
            let function = format!(
                "def {}({}) -> {}\n",
                function_name,
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
                .map(|param| format!("{}: {}", param.name, param.type_instance.as_string(type_db)))
                .collect::<Vec<_>>()
                .join(", ");
            let mut function = format!(
                "def {}({}) -> {}:\n",
                function_name,
                parameters,
                &return_type.as_string(type_db)
            );

            for s in scopes {
                function.push_str(&print_mir_scope(s, type_db));
            }

            for n in body {
                function.push_str(&print_mir_block(n));
            }

            function
        }
    }
}

pub fn print_mir<T>(mir: &[MIRTopLevelNode<T>], type_db: &TypeInstanceManager) -> String {
    let mut buffer = String::new();
    for node in mir {
        buffer.push_str(&print_mir_str(node, type_db));
    }
    buffer
}

pub fn print_mir_node<T>(mir: &MIRTopLevelNode<T>, type_db: &TypeInstanceManager) -> String {
    let mut buffer = String::new();
    buffer.push_str(&print_mir_str(mir, type_db));
    buffer
}
