use super::hir::{HIR, HIRAstMetadata, HIRExpr, HIRTypeDef, HIRTypedBoundName, TrivialHIRExpr};

use crate::ast::parser::{AST, Expr};

use crate::types::type_db::TypeDatabase;
use crate::types::type_db::TypeInstance;

/*
The MIR is a representation of the HIR but in "code blocks". At this level we only have gotos
and block definitions, not much else. All other features in the language will be reduced
to this.

This representation might make it easier to perform type checking.
This still uses expressions in the HIR tree, they are low level enough

Also, at this point we have inferred all types.
*/

/*
A MIRTopLevelNode is a top-level declaration. They are not executable per se (don't do anything on the CPU)
but represent the main parts of the program.
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRTopLevelNode {
    DeclareFunction {
        function_name: String,
        parameters: Vec<MIRTypedBoundName>,
        body: Vec<MIRBlock>,
        scopes: Vec<MIRScope>,
        return_type: TypeInstance,
    },
    #[allow(dead_code)] StructDeclaration {
        struct_name: String,
        body: Vec<HIRTypedBoundName>,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ScopeId(pub usize);

/*
A MIRFunctionNode represents nodes inside a block. They can be executed within the context of
a scope and a block.
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRBlockNode {
    Assign {
        path: Vec<String>,
        expression: HIRExpr,
        meta_ast: Option<AST>,
        meta_expr: Option<Expr>,
    },
    FunctionCall {
        /*
        This is just a function call that is not used in an expression, just as a standalone call.
        Like print(x).

        We always assign a function to a variable, even if it's a function
        stored in a map. The HIR expr reduction extracts it out to a variable.
        */
        function: String,
        args: Vec<HIRExpr>,
        meta_ast: Option<AST>,
    },
}

/**
 A `MIRTypedBoundName` can be understood as a variable name and type, but it can be used as arguments of a function,
 and struct fields as well as plain old variable declarations as well.

 Everything that needs a name and a type can use a `MIRTypedBoundName`.
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRTypedBoundName {
    pub name: String,
    pub typename: TypeInstance,
}

/*
  MIRScope represents a scope inside a function. Scopes can inherit larger scopes.
  You can think of a scope as an "indentation level", but this is too simplistic: each variable declaration
  can introduce a new scope, and you don't have access to variables declared after the fact.
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRScope {
    pub index: usize,
    pub inherit: ScopeId,
    pub boundnames: Vec<MIRTypedBoundName>,
}

/*MIRBlockFinal specifies how a block ends: in a goto, branch, or a return. */
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRBlockFinal {
    //expression, true, else, meta
    If(HIRExpr, BlockId, BlockId, HIRAstMetadata),
    GotoBlock(BlockId),
    Return(HIRExpr, HIRAstMetadata),
    EmptyReturn,
}

/*MIRBlock is the definition of an executable chunk of code.
  Blocks are composed of executable high-level instructions, the scope it uses,
  and how it ends. Every block has to end.

  A block never has a loop inside itself. Infinite loops can only be achieved by multiple blocks.

*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRBlock {
    pub index: usize,
    pub scope: ScopeId,
    pub finish: MIRBlockFinal,
    pub block: Vec<MIRBlockNode>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRMaybeUnfinishedBlock {
    pub index: usize,
    pub scope: ScopeId,
    pub finish: Option<MIRBlockFinal>,
    pub block: Vec<MIRBlockNode>,
}

struct MIRFunctionEmitter {
    current_scope: ScopeId,
    current_block: BlockId,
    blocks: Vec<MIRMaybeUnfinishedBlock>,
    scopes: Vec<MIRScope>,
}

impl MIRFunctionEmitter {
    fn new() -> Self {
        MIRFunctionEmitter {
            current_block: BlockId(0),
            current_scope: ScopeId(0),
            blocks: vec![],
            scopes: vec![],
        }
    }

    fn emit(&mut self, node: MIRBlockNode) {
        self.blocks[self.current_block.0].block.push(node);
    }

    fn create_scope(&mut self, parent_scope: ScopeId) -> ScopeId {
        let current_len = self.scopes.len();
        let new_scope = MIRScope {
            index: current_len,
            inherit: parent_scope,
            boundnames: vec![],
        };
        self.scopes.push(new_scope);
        ScopeId(current_len)
    }

    fn new_block(&mut self, block_scope: ScopeId) -> BlockId {
        let current_len = self.blocks.len();
        let new_block = MIRMaybeUnfinishedBlock {
            finish: None,
            scope: block_scope,
            index: current_len,
            block: vec![],
        };
        self.blocks.push(new_block);
        BlockId(current_len)
    }

    fn scope_add_variable(&mut self, scope_id: ScopeId, var: String, typedef: TypeInstance) {
        let scope = &mut self.scopes[scope_id.0];
        scope.boundnames.push(MIRTypedBoundName {
            name: var,
            typename: typedef,
        });
    }

    fn finish_with_goto_block(&mut self, goto: BlockId) {
        self.blocks[self.current_block.0].finish = Some(MIRBlockFinal::GotoBlock(goto));
    }

    fn set_current_block(&mut self, current: BlockId) {
        self.current_block = current;
    }
    fn set_current_scope(&mut self, current: ScopeId) {
        self.current_scope = current;
    }

    fn finish_with_branch(
        &mut self,
        condition: HIRExpr,
        true_branch: BlockId,
        false_branch: BlockId,
        meta_ast: HIRAstMetadata,
    ) {
        self.blocks[self.current_block.0].finish = Some(MIRBlockFinal::If(
            condition,
            true_branch,
            false_branch,
            meta_ast,
        ));
    }

    fn finish_with_return(&mut self, expr: HIRExpr, meta_ast: HIRAstMetadata) {
        self.blocks[self.current_block.0].finish = Some(MIRBlockFinal::Return(expr, meta_ast));
    }

    fn finish_with_empty_return(&mut self) {
        self.blocks[self.current_block.0].finish = Some(MIRBlockFinal::EmptyReturn);
    }

    fn finish(self) -> (Vec<MIRScope>, Vec<MIRBlock>) {
        let blocks = self
            .blocks
            .into_iter()
            .map(|x| {
                let finisher = match x.finish {
                    Some(f) => f,
                    None => panic!("Found unfinished MIR block! {:#?}", x),
                };
                MIRBlock {
                    index: x.index,
                    scope: x.scope,
                    finish: finisher,
                    block: x.block,
                }
            })
            .collect::<Vec<_>>();

        (self.scopes, blocks)
    }

    fn check_if_block_is_finished(&self, block_id: BlockId) -> bool {
        self.blocks[block_id.0].finish.is_some()
    }
}


//returns the "root block" that this execution generated (or started with)
#[allow(clippy::too_many_lines)] //too lazy for now, and I think it's ok here
fn process_body(emitter: &mut MIRFunctionEmitter, body: &[HIR]) {
    for hir in body {
        match hir {
            HIR::DeclareFunction { .. } => {
                panic!("Cannot declare function inside another function yet!")
            }
            HIR::StructDeclaration { .. } => {
                panic!("Cannot declare struct inside a function yet!")
            }
            HIR::Assign {
                path,
                expression,
                meta_ast,
                meta_expr,
            } => {
                emitter.emit(MIRBlockNode::Assign {
                    path: path.clone(),
                    expression: expression.clone(),
                    meta_ast: meta_ast.clone(),
                    meta_expr: meta_expr.clone(),
                });
            }
            HIR::Declare {
                var,
                typedef,
                expression,
                meta_ast,
                meta_expr,
            } => {
                let HIRTypeDef::Resolved(actual_type) = typedef else {
                    panic!("An unresolved, uninferred type has reached the MIR stage. This is a type inference failure. Node: {:?}", typedef);
                };

                //we need to finalize the block we currently are,
                //define a new scope inheriting the current one,
                //declare a new variable in the new scope,
                //then emit the declaration.

                let current_scope = emitter.current_scope; //scope 0

                let new_scope = emitter.create_scope(current_scope); //defscope 1
                let new_block = emitter.new_block(new_scope); //defblock 1

                //on block 0 make it so that the current block goes to this new one (block 1)
                emitter.finish_with_goto_block(new_block);

                //now we add the variable to scope 1
                emitter.scope_add_variable(new_scope, var.clone(), actual_type.clone());

                //go to block 1
                emitter.set_current_block(new_block);
                //and *finally* assign the variable
                emitter.emit(MIRBlockNode::Assign {
                    path: vec![var.clone()],
                    expression: expression.clone(),
                    meta_ast: meta_ast.clone(),
                    meta_expr: meta_expr.clone(),
                });

                //allow other blocks to read and write from scope:
                let after_creation_variable_scope = emitter.create_scope(new_scope); //defscope 1
                let after_creation_variable_block =
                    emitter.new_block(after_creation_variable_scope); //defblock 1

                emitter.finish_with_goto_block(after_creation_variable_block);
                emitter.set_current_block(after_creation_variable_block);
                emitter.set_current_scope(after_creation_variable_scope);
            }
            HIR::FunctionCall {
                function,
                args,
                meta,
            } => {
                match &function {
                    HIRExpr::Trivial(TrivialHIRExpr::Variable(var), ..) => {
                        emitter.emit(MIRBlockNode::FunctionCall {
                            function: var.clone(),
                            args: args.clone(),
                            meta_ast: meta.clone(),
                        });
                    }
                    other => panic!("{:?} is not a function!", other),
                };
            }
            HIR::If(condition, true_branch_hir, false_branch_hir, ast) => {
                let HIRTypeDef::Resolved(_actual_condition_type) = &condition.get_expr_type() else {
                    panic!("Unresolved condition type reached MIR, this might be a type inference bug");
                };

                //we need to finalize the block we currently are,
                //define a new scope inheriting the current one,
                //generate both sides of the branch
                //but also need to create a fallback block so that we know where to go when there is no "else" code
                //or when the if does not return

                let current_scope = emitter.current_scope;
                let current_block = emitter.current_block;

                //we will generate the fallback block only if necessary
                let mut fallback_scope_and_block = None;

                let has_else_code = !false_branch_hir.is_empty();

                let (true_block, true_branch_returns) = {
                    assert!(!true_branch_hir.is_empty(), "Empty true branch on if statement reached MIR");
                    //create a block for the true branch
                    let true_branch_scope = emitter.create_scope(current_scope);
                    let true_branch_block = emitter.new_block(true_branch_scope);
                    //go to the true banch block
                    emitter.set_current_block(true_branch_block);
                    process_body(emitter, true_branch_hir);

                    //does the branch have a return instruction, or is already finished
                    let emitted_block_returns =
                        emitter.check_if_block_is_finished(true_branch_block);

                    if !emitted_block_returns {
                        if fallback_scope_and_block.is_none() {
                            let fallback_scope = emitter.create_scope(current_scope);
                            let fallback_block = emitter.new_block(fallback_scope);
                            fallback_scope_and_block = Some((fallback_block, fallback_scope));
                        }
                        let (fallback_block, _) = fallback_scope_and_block.unwrap();
                        //if no then goto the fallback block
                        //make sure we are emitting on the true branch block we generated
                        emitter.set_current_block(true_branch_block);
                        emitter.finish_with_goto_block(fallback_block);
                        emitter.set_current_block(fallback_block);
                    }

                    (true_branch_block, emitted_block_returns)
                };

                if has_else_code {
                    let false_branch_scope = emitter.create_scope(current_scope);
                    let false_branch_block = emitter.new_block(false_branch_scope);

                    //go to the false branch block
                    emitter.set_current_block(false_branch_block);

                    process_body(emitter, false_branch_hir);

                    let emitted_block_returns =
                        emitter.check_if_block_is_finished(false_branch_block);

                    //ok go back for a second
                    emitter.set_current_block(current_block);

                    //emit the branch
                    emitter.finish_with_branch(
                        condition.clone(),
                        true_block,
                        false_branch_block,
                        ast.clone(),
                    );

                    if !emitted_block_returns {
                        if fallback_scope_and_block.is_none() {
                            let fallback_scope = emitter.create_scope(current_scope);
                            let fallback_block = emitter.new_block(fallback_scope);
                            fallback_scope_and_block = Some((fallback_block, fallback_scope));
                        }
                        let (fallback_block, _) = fallback_scope_and_block.unwrap();
                        emitter.set_current_block(false_branch_block);
                        emitter.finish_with_goto_block(fallback_block);
                        emitter.set_current_block(fallback_block);
                    } else if !true_branch_returns {
                        emitter.set_current_block(fallback_scope_and_block.unwrap().0);
                    }
                } else {
                    //no else code, go to the fallback
                    //generate the branch, but the false branch just jumps to the fallback block
                    emitter.set_current_block(current_block);

                    if fallback_scope_and_block.is_none() {
                        let fallback_scope = emitter.create_scope(current_scope);
                        let fallback_block = emitter.new_block(fallback_scope);
                        fallback_scope_and_block = Some((fallback_block, fallback_scope));
                    }
                    let (fallback_block, _) = fallback_scope_and_block.unwrap();
                    emitter.finish_with_branch(
                        condition.clone(),
                        true_block,
                        fallback_block,
                        ast.clone(),
                    );

                    //in this case the function continues in the fallback block
                    emitter.set_current_block(fallback_block);
                }
            }
            HIR::Return(expr, typedef, meta_ast) => {
                let HIRTypeDef::Resolved(_resolved_type) = typedef else {
                    panic!("Unresolved return type reached MIR, this might be a bug in type inference");
                };
                emitter.finish_with_return(expr.clone(), meta_ast.clone());
            }
            HIR::EmptyReturn => {
                emitter.finish_with_empty_return();
            }
        }
    }
}

pub fn process_hir_funcdecl(
    function_name: &str,
    parameters: &[HIRTypedBoundName],
    body: &[HIR],
    return_type: &HIRTypeDef,
    _type_db: &TypeDatabase,
) -> MIRTopLevelNode {
    let mut emitter = MIRFunctionEmitter::new();

    //create a new block for the main function decl node
    let function_zero_scope = emitter.create_scope(ScopeId(0));
    let function_zero_block = emitter.new_block(function_zero_scope);
    emitter.set_current_block(function_zero_block);

    for param in parameters.iter() {
        let actual_type = param.typename.expect_resolved();

        emitter.scope_add_variable(function_zero_scope, param.name.clone(), actual_type.clone());
    }

    process_body(&mut emitter, body);

    //check for blocks with no returns (like fallback blocks or blocks that the user didnt specify a return)
    for block_id in 0..emitter.blocks.len() {
        let block = &emitter.blocks[block_id];
        if block.finish.is_none() {
            emitter.set_current_block(BlockId(block.index));
            emitter.finish_with_empty_return();
        }
    }

    let (scopes, body) = emitter.finish();

    let type_def = return_type.expect_resolved().clone();

    return MIRTopLevelNode::DeclareFunction {
        function_name: function_name.to_string(),
        parameters: parameters
            .iter()
            .map(|x| MIRTypedBoundName {
                name: x.name.clone(),
                typename: x.typename.expect_resolved().clone(),
            })
            .collect::<Vec<_>>(),
        body,
        scopes,
        return_type: type_def,
    };
}

pub fn hir_to_mir(hir_nodes: &[HIR], type_db: &TypeDatabase) -> Vec<MIRTopLevelNode> {
    let mut top_levels = vec![];
    for hir in hir_nodes {
        match hir {
            HIR::DeclareFunction {
                function_name,
                parameters,
                body,
                return_type,
                meta: _,
            } => {
                let fdecl =
                    process_hir_funcdecl(function_name, parameters, body, return_type, type_db);
                top_levels.push(fdecl);
            }
            _ => {
                panic!("Top-level HIR unsupported: {:?}", hir)
            }
        }
    }
    top_levels
}

#[cfg(test)]
#[allow(clippy::too_many_lines)]
mod tests {
    use crate::{semantic::mir_printer, ast::parser::Parser};
    #[cfg(test)]
    use pretty_assertions::assert_eq;

    use super::*;

    //Parses a single expression
    fn mir(source: &str) -> (Vec<MIRTopLevelNode>, TypeDatabase) {
        let tokenized = crate::ast::lexer::Tokenizer::new(source)
            .tokenize()
            .ok()
            .unwrap();
        let mut parser = Parser::new(tokenized);
        let ast = AST::Root(parser.parse_ast());
        println!("AST: {:?}", &ast);
        let analysis_result = crate::semantic::analysis::do_analysis(&ast);
        println!("HIR: {:?}", &analysis_result.final_mir);
        (
            hir_to_mir(&analysis_result.final_mir, &analysis_result.type_db),
            analysis_result.type_db,
        )
    }

    #[test]
    fn simplest_case() {
        let (mir, type_db) = mir("
def main():
    return 1");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
    defblock 0:
        usescope 0
        return 1";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn set_variable() {
        let (mir, type_db) = mir("
def main():
    x = 1");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        x : i32
    defscope 2:
        inheritscope 1
    defblock 0:
        usescope 0
        gotoblock 1
    defblock 1:
        usescope 1
        x = 1
        gotoblock 2
    defblock 2:
        usescope 2
        return";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn just_return_parameter() {
        let (mir, type_db) = mir("
def main(x: i32) -> i32:
    return x");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main(x: i32) -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defblock 0:
        usescope 0
        return x";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn many_parameters_are_added_to_scope() {
        let (mir, type_db) = mir("
def main(x: i32, y: i64, z: f64, name: str) -> i32:
    return x");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main(x: i32, y: i64, z: f64, name: str) -> i32:
    defscope 0:
        inheritscope 0
        x : i32
        y : i64
        z : f64
        name : str
    defblock 0:
        usescope 0
        return x";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn simple_expression() {
        let (mir, type_db) = mir("
def main(x: i32) -> i32:
    return x + 1");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main(x: i32) -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defblock 0:
        usescope 0
        return x + 1";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn create_variable() {
        let (mir, type_db) = mir("
def main(x: i32) -> i32:
    y = 0
    return x + y
");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main(x: i32) -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defscope 1:
        inheritscope 0
        y : i32
    defscope 2:
        inheritscope 1
    defblock 0:
        usescope 0
        gotoblock 1
    defblock 1:
        usescope 1
        y = 0
        gotoblock 2
    defblock 2:
        usescope 2
        return x + y";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn multiple_variables_and_expressions() {
        let (mir, type_db) = mir("
def main(x: i32) -> i32:
    y = 1
    z: i32 = 2 + x
    return x / (y + z)");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main(x: i32) -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defscope 1:
        inheritscope 0
        y : i32
    defscope 2:
        inheritscope 1
    defscope 3:
        inheritscope 2
        z : i32
    defscope 4:
        inheritscope 3
    defblock 0:
        usescope 0
        gotoblock 1
    defblock 1:
        usescope 1
        y = 1
        gotoblock 2
    defblock 2:
        usescope 2
        gotoblock 3
    defblock 3:
        usescope 3
        z = 2 + x
        gotoblock 4
    defblock 4:
        usescope 4
        return x / y + z
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn set_same_variable_multiple_times() {
        let (mir, type_db) = mir("
def main() -> i32:
    y = 1
    y = 2
    y = 3
    y = 4");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        y : i32
    defscope 2:
        inheritscope 1
    defblock 0:
        usescope 0
        gotoblock 1
    defblock 1:
        usescope 1
        y = 1
        gotoblock 2
    defblock 2:
        usescope 2
        y = 2
        y = 3
        y = 4
        return
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn set_and_use() {
        let (mir, type_db) = mir("
def main():
    y = 1
    x = y + 1");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        y : i32
    defscope 2:
        inheritscope 1
    defscope 3:
        inheritscope 2
        x : i32
    defscope 4:
        inheritscope 3
    defblock 0:
        usescope 0
        gotoblock 1
    defblock 1:
        usescope 1
        y = 1
        gotoblock 2
    defblock 2:
        usescope 2
        gotoblock 3
    defblock 3:
        usescope 3
        x = y + 1
        gotoblock 4
    defblock 4:
        usescope 4
        return
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_statement() {
        let (mir, type_db) = mir("
def main():
    y = 1
    if y == 1:
        print(y)");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        y : i32
    defscope 2:
        inheritscope 1
    defscope 3:
        inheritscope 2
    defscope 4:
        inheritscope 2
    defblock 0:
        usescope 0
        gotoblock 1
    defblock 1:
        usescope 1
        y = 1
        gotoblock 2
    defblock 2:
        usescope 2
        if y == 1:
            gotoblock 3
        else:
            gotoblock 4
    defblock 3:
        usescope 3
        print(y)
        gotoblock 4
    defblock 4:
        usescope 4
        return
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_code_in_both_branches() {
        let (mir, type_db) = mir("
def main():
    if True:
        print(1)
    else:
        print(2)");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 0
    defscope 3:
        inheritscope 0
    defblock 0:
        usescope 0
        if True:
            gotoblock 1
        else:
            gotoblock 3
    defblock 1:
        usescope 1
        print(1)
        gotoblock 2
    defblock 2:
        usescope 2
        return
    defblock 3:
        usescope 3
        print(2)
        gotoblock 2
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_return_in_both_branches() {
        let (mir, type_db) = mir("
def main() -> i32:
    if True:
        return 1
    else:
        return 2");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 0
    defblock 0:
        usescope 0
        if True:
            gotoblock 1
        else:
            gotoblock 2
    defblock 1:
        usescope 1
        return 1
    defblock 2:
        usescope 2
        return 2
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_statements_decls_inside_branches() {
        let (mir, type_db) = mir("
def main() -> i32:
    x = 0
    if True:
        y = x + 1
        return y
    else:
        x = 1
        y = 2 + x
        return x + y
    ");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        x : i32
    defscope 2:
        inheritscope 1
    defscope 3:
        inheritscope 2
    defscope 4:
        inheritscope 2
        y : i32
    defscope 5:
        inheritscope 4
    defscope 6:
        inheritscope 2
    defscope 7:
        inheritscope 5
        y : i32
    defscope 8:
        inheritscope 7
    defblock 0:
        usescope 0
        gotoblock 1
    defblock 1:
        usescope 1
        x = 0
        gotoblock 2
    defblock 2:
        usescope 2
        if True:
            gotoblock 3
        else:
            gotoblock 6
    defblock 3:
        usescope 3
        gotoblock 4
    defblock 4:
        usescope 4
        y = x + 1
        gotoblock 5
    defblock 5:
        usescope 5
        return y
    defblock 6:
        usescope 6
        x = 1
        gotoblock 7
    defblock 7:
        usescope 7
        y = 2 + x
        gotoblock 8
    defblock 8:
        usescope 8
        return x + y";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn code_after_if_is_correctly_placed_true_branch_only() {
        let (mir, type_db) = mir("
def main() -> i32:
    x = 0
    if x == 0:
        x = x + 1
    print(x)
    ");
        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        x : i32
    defscope 2:
        inheritscope 1
    defscope 3:
        inheritscope 2
    defscope 4:
        inheritscope 2
    defblock 0:
        usescope 0
        gotoblock 1
    defblock 1:
        usescope 1
        x = 0
        gotoblock 2
    defblock 2:
        usescope 2
        if x == 0:
            gotoblock 3
        else:
            gotoblock 4
    defblock 3:
        usescope 3
        x = x + 1
        gotoblock 4
    defblock 4:
        usescope 4
        print(x)
        return";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn code_after_if_is_correctly_placed_return_on_false_branch() {
        let (mir, type_db) = mir("
def main() -> i32:
    x = 0
    if x == 0:
        print(1)
    else:
        return x
    print(x)
    ");
        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        x : i32
    defscope 2:
        inheritscope 1
    defscope 3:
        inheritscope 2
    defscope 4:
        inheritscope 2
    defscope 5:
        inheritscope 2
    defblock 0:
        usescope 0
        gotoblock 1
    defblock 1:
        usescope 1
        x = 0
        gotoblock 2
    defblock 2:
        usescope 2
        if x == 0:
            gotoblock 3
        else:
            gotoblock 5
    defblock 3:
        usescope 3
        print(1)
        gotoblock 4
    defblock 4:
        usescope 4
        print(x)
        return
    defblock 5:
        usescope 5
        return x";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn code_after_if_is_correctly_placed_true_and_false_branches() {
        let (mir, type_db) = mir("
def main() -> i32:
    x = 0
    if x == 0:
        x = x + 1
    else:
        x = 2
    print(x)");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        x : i32
    defscope 2:
        inheritscope 1
    defscope 3:
        inheritscope 2
    defscope 4:
        inheritscope 2
    defscope 5:
        inheritscope 2
    defblock 0:
        usescope 0
        gotoblock 1
    defblock 1:
        usescope 1
        x = 0
        gotoblock 2
    defblock 2:
        usescope 2
        if x == 0:
            gotoblock 3
        else:
            gotoblock 5
    defblock 3:
        usescope 3
        x = x + 1
        gotoblock 4
    defblock 4:
        usescope 4
        print(x)
        return
    defblock 5:
        usescope 5
        x = 2
        gotoblock 4
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_one_branch_does_not_return() {
        let (mir, type_db) = mir("
def main() -> i32:
    x = 0
    if True:
        y = x + 1
        print(x)
    else:
        x = 1
        y = 2 + x
        return x + y
    ");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        x : i32
    defscope 2:
        inheritscope 1
    defscope 3:
        inheritscope 2
    defscope 4:
        inheritscope 2
        y : i32
    defscope 5:
        inheritscope 4
    defscope 6:
        inheritscope 2
    defscope 7:
        inheritscope 5
        y : i32
    defscope 8:
        inheritscope 7
    defblock 0:
        usescope 0
        gotoblock 1
    defblock 1:
        usescope 1
        x = 0
        gotoblock 2
    defblock 2:
        usescope 2
        if True:
            gotoblock 3
        else:
            gotoblock 6
    defblock 3:
        usescope 3
        gotoblock 4
    defblock 4:
        usescope 4
        y = x + 1
        gotoblock 5
    defblock 5:
        usescope 5
        print(x)
        return
    defblock 6:
        usescope 6
        x = 1
        gotoblock 7
    defblock 7:
        usescope 7
        y = 2 + x
        gotoblock 8
    defblock 8:
        usescope 8
        return x + y";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_nested_branch_all_returns() {
        let (mir, type_db) = mir("
def main() -> i32:
    if True:
        x = 1
        if 1 == 1:
            x = x + 3
            return x
        else:
            x = x + 1
            return x
    else:
        y = 3
        if 2 == 2:
            return y + 1
        else:
            return 4 * y
    ");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 0
        x : i32
    defscope 3:
        inheritscope 2
    defscope 4:
        inheritscope 3
    defscope 5:
        inheritscope 3
    defscope 6:
        inheritscope 0
    defscope 7:
        inheritscope 3
        y : i32
    defscope 8:
        inheritscope 7
    defscope 9:
        inheritscope 8
    defscope 10:
        inheritscope 8
    defblock 0:
        usescope 0
        if True:
            gotoblock 1
        else:
            gotoblock 6
    defblock 1:
        usescope 1
        gotoblock 2
    defblock 2:
        usescope 2
        x = 1
        gotoblock 3
    defblock 3:
        usescope 3
        if 1 == 1:
            gotoblock 4
        else:
            gotoblock 5
    defblock 4:
        usescope 4
        x = x + 3
        return x
    defblock 5:
        usescope 5
        x = x + 1
        return x
    defblock 6:
        usescope 6
        gotoblock 7
    defblock 7:
        usescope 7
        y = 3
        gotoblock 8
    defblock 8:
        usescope 8
        if 2 == 2:
            gotoblock 9
        else:
            gotoblock 10
    defblock 9:
        usescope 9
        return y + 1
    defblock 10:
        usescope 10
        return 4 * y";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_nested_branch_but_one_does_not_return() {
        let (mir, type_db) = mir("
def main() -> i32:
    if True:
        x = 1
        if 1 == 1:
            x = x + 3
            return x
        else:
            x = x + 1
            print(x)
        print(\"nice\")
    else:
        y = 3
        if 2 == 2:
            y = y + 1
            print(y)
        else:
            return 4 * y
    ");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 0
        x : i32
    defscope 3:
        inheritscope 2
    defscope 4:
        inheritscope 3
    defscope 5:
        inheritscope 3
    defscope 6:
        inheritscope 3
    defscope 7:
        inheritscope 0
    defscope 8:
        inheritscope 3
        y : i32
    defscope 9:
        inheritscope 8
    defscope 10:
        inheritscope 9
    defscope 11:
        inheritscope 9
    defscope 12:
        inheritscope 9
    defblock 0:
        usescope 0
        if True:
            gotoblock 1
        else:
            gotoblock 7
    defblock 1:
        usescope 1
        gotoblock 2
    defblock 2:
        usescope 2
        x = 1
        gotoblock 3
    defblock 3:
        usescope 3
        if 1 == 1:
            gotoblock 4
        else:
            gotoblock 5
    defblock 4:
        usescope 4
        x = x + 3
        return x
    defblock 5:
        usescope 5
        x = x + 1
        print(x)
        gotoblock 6
    defblock 6:
        usescope 6
        print(\"nice\")
        return
    defblock 7:
        usescope 7
        gotoblock 8
    defblock 8:
        usescope 8
        y = 3
        gotoblock 9
    defblock 9:
        usescope 9
        if 2 == 2:
            gotoblock 10
        else:
            gotoblock 12
    defblock 10:
        usescope 10
        y = y + 1
        print(y)
        gotoblock 11
    defblock 11:
        usescope 11
        return
    defblock 12:
        usescope 12
        return 4 * y";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn set_some_vars_exprssimplest_case() {
        let (mir, type_db) = mir("
def main():
    x : i32 = 15
    y : i32 = 3
    z : i32 = x + y
    result: i32 = 5 + z
    result = result + y");

        let final_result = mir_printer::print_mir(&mir, &type_db);
        println!("{}", final_result);
        let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        x : i32
    defscope 2:
        inheritscope 1
    defscope 3:
        inheritscope 2
        y : i32
    defscope 4:
        inheritscope 3
    defscope 5:
        inheritscope 4
        z : i32
    defscope 6:
        inheritscope 5
    defscope 7:
        inheritscope 6
        result : i32
    defscope 8:
        inheritscope 7
    defblock 0:
        usescope 0
        gotoblock 1
    defblock 1:
        usescope 1
        x = 15
        gotoblock 2
    defblock 2:
        usescope 2
        gotoblock 3
    defblock 3:
        usescope 3
        y = 3
        gotoblock 4
    defblock 4:
        usescope 4
        gotoblock 5
    defblock 5:
        usescope 5
        z = x + y
        gotoblock 6
    defblock 6:
        usescope 6
        gotoblock 7
    defblock 7:
        usescope 7
        result = 5 + z
        gotoblock 8
    defblock 8:
        usescope 8
        result = result + y
        return";

        assert_eq!(expected.trim(), final_result.trim());
    }
}
