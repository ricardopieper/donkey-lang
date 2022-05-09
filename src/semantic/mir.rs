use super::hir::*;
use crate::ast::lexer::*;
use crate::ast::parser::*;
use crate::commons::float::*;

use super::type_db::TypeDatabase;
use std::collections::VecDeque;
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
    StructDeclaration {
        struct_name: String,
        body: Vec<HIRTypedBoundName>,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BlockId(pub usize);
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
    },
    FunctionCall {
        /*
        This is just a function call that is not used in an expression, just as a standalone call.
        Like print(x).

        We always assign a function to a variable, even if it's a function
        stored in a map. The HIR expr reduction extracts it out to a variable.
        */
        function: String,
        args: Vec<TrivialHIRExpr>,
    },
}

/**
 A MIRTypedBoundName can be understood as a variable name and type, but it can be used as arguments of a function,
 and struct fields as well as plain old variable declarations as well.

 Everything that needs a name and a type can use a MIRTypedBoundName.
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
    If(TrivialHIRExpr, BlockId, BlockId),
    GotoBlock(BlockId),
    Return(HIRExpr),
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
    fallback_returns: VecDeque<BlockId>
}

impl MIRFunctionEmitter {
    fn new() -> Self {
        MIRFunctionEmitter {
            current_block: BlockId(0),
            current_scope: ScopeId(0),
            blocks: vec![],
            scopes: vec![],
            fallback_returns: VecDeque::new()
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
        return ScopeId(current_len);
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
        return BlockId(current_len);
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

    fn finish_with_branch(&mut self, condition: TrivialHIRExpr, true_branch: BlockId, false_branch: BlockId) {
        self.blocks[self.current_block.0].finish = Some(MIRBlockFinal::If(condition, true_branch, false_branch));
    }

    fn finish_with_return(&mut self, expr: HIRExpr) {
        self.blocks[self.current_block.0].finish = Some(MIRBlockFinal::Return(expr));
    }

    fn finish_with_empty_return(&mut self) {
        self.blocks[self.current_block.0].finish = Some(MIRBlockFinal::EmptyReturn);
    }

    fn finish(self) -> (Vec<MIRScope>, Vec<MIRBlock>) {
        let blocks = self.blocks.into_iter().map(|x| {
            let finisher = match x.finish {
                Some(f) => f,
                None => panic!("Found unfinished MIR block! {:?}", x),
            };
            MIRBlock {
                index: x.index,
                scope: x.scope,
                finish: finisher,
                block: x.block,
            }
        }).collect::<Vec<_>>();

        (self.scopes, blocks)
    }

    fn check_if_block_is_finished(&self, block_id: BlockId) -> bool {
        self.blocks[block_id.0].finish.is_some()
    }
}

//returns the "root block" that this execution generated (or started with)
fn process_body(emitter: &mut MIRFunctionEmitter, body: &[HIR]) {
    for hir in body {
        println!("Processing HIR {:?}", hir);
        match hir {
            HIR::DeclareFunction { .. } => {
                panic!("Cannot declare function inside another function yet!")
            }
            HIR::StructDeclaration { struct_name, body } => {
                panic!("Cannot declare struct inside a function yet!")
            }
            HIR::Assign { path, expression } => {
                //assignments do not introduce new variables, they just mutate them
                emitter.emit(MIRBlockNode::Assign {
                    path: path.clone(),
                    expression: expression.clone(),
                });
            }
            HIR::Declare {
                var,
                typedef,
                expression,
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
                });
                //allow other blocks to read and write from scope:
                let after_creation_variable_scope = emitter.create_scope(new_scope); //defscope 1
                let after_creation_variable_block = emitter.new_block(after_creation_variable_scope); //defblock 1

                emitter.finish_with_goto_block(after_creation_variable_block);
                emitter.set_current_block(after_creation_variable_block);
            }
            HIR::FunctionCall { function, args } => {
                match function {
                    TrivialHIRExpr::Variable(var) => {
                        emitter.emit(MIRBlockNode::FunctionCall {
                            function: var.clone(),
                            args: args.clone(),
                        });
                    }
                    other => panic!("{:?} is not a function!", other),
                };
            }
            HIR::If(condition, true_branch_hir, false_branch_hir) => {
                //we need to finalize the block we currently are,
                //define a new scope inheriting the current one,
                //generate both sides of the branch
                //but also need to create a fallback block so that we know where to go when there is no "else" code
                //or when the if does not return
                

                let current_scope = emitter.current_scope;
                let current_block = emitter.current_block;

                //we will generate the fallback block only if necessary
                let mut fallback_scope_and_block = None;

                let has_else_code = false_branch_hir.len() > 0;

                let true_block = {
                    if true_branch_hir.len() == 0 {
                        panic!("Empty true branch on if statement reached MIR");
                    }
                    //create a block for the true branch
                    let true_branch_scope = emitter.create_scope(current_scope);
                    let true_branch_block = emitter.new_block(true_branch_scope);
                    //go to the true banch block
                    emitter.set_current_block(true_branch_block);
                    process_body(emitter, true_branch_hir);
                    
                    //does the branch have a return instruction, or is already finished
                    let emitted_block_returns = emitter.check_if_block_is_finished(true_branch_block);

                    if !emitted_block_returns {
                        if let None = fallback_scope_and_block {
                            let fallback_scope = emitter.create_scope(current_scope);
                            let fallback_block = emitter.new_block(fallback_scope);
                            fallback_scope_and_block = Some((fallback_block, fallback_scope));
                        }
                        let (fallback_block, _) = fallback_scope_and_block.unwrap();
                        //if no then goto the fallback block
                        //make sure we are emitting on the true branch block we generated
                        emitter.set_current_block(true_branch_block); 
                        emitter.finish_with_goto_block(fallback_block);
                    }

                    true_branch_block
                };

                
                if has_else_code {
                    let false_branch_scope = emitter.create_scope(current_scope);
                    let false_branch_block = emitter.new_block(false_branch_scope);
                   
                    //go to the false branch block
                    emitter.set_current_block(false_branch_block);
                   
                    process_body(emitter, false_branch_hir);

                    let emitted_block_returns = emitter.check_if_block_is_finished(false_branch_block);

                    if !emitted_block_returns {
                        if let None = fallback_scope_and_block {
                            let fallback_scope = emitter.create_scope(current_scope);
                            let fallback_block = emitter.new_block(fallback_scope);
                            fallback_scope_and_block = Some((fallback_block, fallback_scope));
                        }
                        let (fallback_block, _) = fallback_scope_and_block.unwrap();
                        emitter.set_current_block(false_branch_block); 
                        emitter.finish_with_goto_block(fallback_block);
                    }

                    //ok go back for a second
                    emitter.set_current_block(current_block);
                    
                    //emit the branch
                    emitter.finish_with_branch(condition.clone(), true_block, false_branch_block);

                } else { 
                    //no else code, go to the fallback
                    //generate the branch, but the false branch just jumps to the fallback block
                    emitter.set_current_block(current_block);

                    if let None = fallback_scope_and_block {
                        let fallback_scope = emitter.create_scope(current_scope);
                        let fallback_block = emitter.new_block(fallback_scope);
                        fallback_scope_and_block = Some((fallback_block, fallback_scope));
                    }
                    let (fallback_block, _) = fallback_scope_and_block.unwrap();
                    emitter.finish_with_branch(condition.clone(), true_block, fallback_block);

                    //in this case the function continues in the fallback block
                    emitter.set_current_block(fallback_block);
                    
                }

            }
            HIR::Return(expr) => {
                emitter.finish_with_return(expr.clone());
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
) -> MIRTopLevelNode {
    let mut emitter = MIRFunctionEmitter::new();

    //create a new block for the main function decl node
    let function_zero_scope = emitter.create_scope(ScopeId(0));
    let function_zero_block = emitter.new_block(function_zero_scope);
    emitter.set_current_block(function_zero_block);

    for param in parameters.iter() {

        let actual_type = param.typename.expect_resolved();

        emitter.scope_add_variable(
            function_zero_scope,
            param.name.clone(),
            actual_type.clone(),
        );
    }

    process_body(&mut emitter, body);

    //check if the last emitted block has no return and add one
    let block = emitter.blocks.len() - 1;
    emitter.set_current_block(BlockId(block));
    let block = &emitter.blocks[emitter.current_block.0];
    if block.finish.is_none() {
        emitter.finish_with_empty_return();
    }

    let (scopes, body) = emitter.finish();

    let type_def = return_type.expect_resolved().clone();

    return MIRTopLevelNode::DeclareFunction {
        function_name: function_name.to_string(),
        parameters: parameters.iter().map(|x| {
            MIRTypedBoundName {
                name: x.name.clone(),
                typename: x.typename.expect_resolved().clone()
            }
        }).collect::<Vec<_>>().clone(),
        body,
        scopes,
        return_type: type_def,
    };
}

pub fn hir_to_mir(hir_nodes: &[HIR]) -> Vec<MIRTopLevelNode> {
    let mut top_levels = vec![];
    for hir in hir_nodes {
        match hir {
            HIR::DeclareFunction {
                function_name,
                parameters,
                body,
                return_type,
            } => {
                let fdecl = process_hir_funcdecl(function_name, parameters, body, return_type);
                top_levels.push(fdecl);
            }
            _ => {
                panic!("Top-level HIR unsupported: {:?}", hir)
            }
        }
    }
    return top_levels;
}



#[cfg(test)]
mod tests {
    use crate::semantic::mir_printer;
    #[cfg(test)]
    use pretty_assertions::{assert_eq};

    use super::*;

    //Parses a single expression
    fn mir(source: &str) -> (Vec<MIRTopLevelNode>, TypeDatabase) {
        let tokenized = crate::ast::lexer::Tokenizer::new(source).tokenize().ok().unwrap();
        let mut parser = Parser::new(tokenized);
        let ast = AST::Root(parser.parse_ast().ok().unwrap());
        println!("AST: {:?}", &ast);
        let analysis_result = crate::semantic::analysis::do_analysis(&ast);
        println!("HIR: {:?}", &analysis_result.final_mir);
        return (hir_to_mir(&analysis_result.final_mir), analysis_result.type_db)
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
        inheritscope 0
        z : i32
    defscope 4:
        inheritscope 3
    defscope 5:
        inheritscope 0
        $0 : i32
    defscope 6:
        inheritscope 5
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
        gotoblock 5
    defblock 5:
        usescope 5
        $0 = y + z
        gotoblock 6
    defblock 6:
        usescope 6
        return x / $0
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
        inheritscope 0
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

}