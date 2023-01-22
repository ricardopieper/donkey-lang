use crate::{interner::JoinableInternedStringSlice, types::type_instance_db::TypeInstanceManager};

use super::{
    hir_printer::HIRExprPrinter,
    mir::{MIRBlock, MIRBlockFinal, MIRBlockNode, MIRScope, MIRTopLevelNode},
};

use crate::interner::StringInterner;

fn print_mir_block<T>(block: &MIRBlock<T>, interner: &StringInterner) -> String {
    let mut buffer = String::new();
    let expr_printer = HIRExprPrinter::new(interner);
    buffer.push_str(&format!("    defblock {}:\n", block.index));
    buffer.push_str(&format!("        usescope {}\n", block.scope.0));

    for node in &block.nodes {
        match node {
            MIRBlockNode::Assign {
                path, expression, ..
            } => {
                buffer.push_str(&format!(
                    "        {} = {}\n",
                    path.join_interned(interner, "."),
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
        MIRBlockFinal::EmptyReturn => {
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
            let function = format!(
                "def {}({}) -> {}\n",
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
                function.push_str(&print_mir_block(n, interner));
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
