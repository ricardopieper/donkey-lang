use std::borrow::Cow;

use super::{
    hir::{
        Checked, HIRAstMetadata, HIRExpr, HIRRoot, HIRTypedBoundName, InferredTypeHIR,
        InferredTypeHIRRoot, NotChecked, HIR,
    },
    hir_printer::expr_str,
    mir_printer::PrintableExpression,
};

use crate::{
    ast::{
        lexer::SourceString,
        parser::{Expr, AST},
    },
    types::type_instance_db::TypeInstanceId,
};

pub type TypecheckPendingExpression<'source> = HIRExpr<'source, TypeInstanceId, NotChecked>;
pub type TypecheckedExpression<'source> = HIRExpr<'source, TypeInstanceId, Checked>;

impl<'source, T> PrintableExpression for HIRExpr<'source, TypeInstanceId, T> {
    fn print_expr(&self) -> Cow<'source, str> {
        expr_str(self)
    }
}

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
pub enum MIRTopLevelNode<'source, TTypecheckStateExpr> {
    IntrinsicFunction {
        function_name: SourceString<'source>,
        parameters: Vec<MIRTypedBoundName<'source>>,
        return_type: TypeInstanceId,
    },
    DeclareFunction {
        function_name: SourceString<'source>,
        parameters: Vec<MIRTypedBoundName<'source>>,
        body: Vec<MIRBlock<'source, TTypecheckStateExpr>>,
        scopes: Vec<MIRScope<'source>>,
        return_type: TypeInstanceId,
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
pub enum MIRBlockNode<'source, TTypecheckState> {
    Assign {
        path: Vec<SourceString<'source>>,
        expression: HIRExpr<'source, TypeInstanceId, TTypecheckState>,
        meta_ast: &'source AST<'source>,
        meta_expr: &'source Expr<'source>,
    },
    FunctionCall {
        function: SourceString<'source>,
        args: Vec<HIRExpr<'source, TypeInstanceId, TTypecheckState>>,
        meta_ast: &'source AST<'source>,
        meta_expr: &'source Expr<'source>,
        return_type: TypeInstanceId,
    },
    //@TODO Add method call here
}

/**
 A `MIRTypedBoundName` can be understood as a variable name and type, but it can be used as arguments of a function,
 and struct fields as well as plain old variable declarations as well.

 Everything that needs a name and a type can use a `MIRTypedBoundName`.
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRTypedBoundName<'source> {
    pub name: SourceString<'source>,
    pub type_instance: TypeInstanceId,
}

/*
  MIRScope represents a scope inside a function. Scopes can inherit larger scopes.
  You can think of a scope as an "indentation level", but this is too simplistic: each variable declaration
  can introduce a new scope, and you don't have access to variables declared after the fact.
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRScope<'source> {
    pub id: ScopeId,
    pub inherit: ScopeId,
    pub boundnames: Vec<MIRTypedBoundName<'source>>,
}

/*MIRBlockFinal specifies how a block ends: in a goto, branch, or a return. */
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRBlockFinal<'source, TTypecheckState> {
    //expression, true, else, meta
    If(
        HIRExpr<'source, TypeInstanceId, TTypecheckState>,
        BlockId,
        BlockId,
        HIRAstMetadata<'source>,
    ),
    GotoBlock(BlockId),
    Return(
        HIRExpr<'source, TypeInstanceId, TTypecheckState>,
        HIRAstMetadata<'source>,
    ),
    EmptyReturn,
}

/*MIRBlock is the definition of an executable chunk of code.
  Blocks are composed of executable high-level instructions, the scope it uses,
  and how it ends. Every block has to end.

  A block never has a loop inside itself. Infinite loops can only be achieved by multiple blocks.

*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRBlock<'source, TTypecheckState> {
    pub index: usize,
    pub scope: ScopeId,
    pub finish: MIRBlockFinal<'source, TTypecheckState>,
    pub nodes: Vec<MIRBlockNode<'source, TTypecheckState>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRMaybeUnfinishedBlock<'source> {
    pub index: usize,
    pub scope: ScopeId,
    pub finish: Option<MIRBlockFinal<'source, NotChecked>>,
    pub block: Vec<MIRBlockNode<'source, NotChecked>>,
}

pub type TypecheckedMIRBlock<'source> = MIRBlock<'source, Checked>;

struct MIRFunctionEmitter<'source> {
    current_scope: ScopeId,
    current_block: BlockId,
    blocks: Vec<MIRMaybeUnfinishedBlock<'source>>,
    scopes: Vec<MIRScope<'source>>,
    goto_stack: Vec<BlockId>,
}

impl<'source> MIRFunctionEmitter<'source> {
    fn new() -> Self {
        MIRFunctionEmitter {
            current_block: BlockId(0),
            current_scope: ScopeId(0),
            blocks: vec![],
            scopes: vec![],
            goto_stack: vec![],
        }
    }

    fn emit(&mut self, node: MIRBlockNode<'source, NotChecked>) {
        self.blocks[self.current_block.0].block.push(node);
    }

    fn create_scope(&mut self, parent_scope: ScopeId) -> ScopeId {
        let current_len = self.scopes.len();
        let new_scope = MIRScope {
            id: ScopeId(current_len),
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

    fn scope_add_variable(
        &mut self,
        scope_id: ScopeId,
        var: SourceString<'source>,
        typedef: TypeInstanceId,
    ) {
        let scope = &mut self.scopes[scope_id.0];
        scope.boundnames.push(MIRTypedBoundName {
            name: var,
            type_instance: typedef,
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
        condition: HIRExpr<'source, TypeInstanceId>,
        true_branch: BlockId,
        false_branch: BlockId,
        meta_ast: HIRAstMetadata<'source>,
    ) {
        self.blocks[self.current_block.0].finish = Some(MIRBlockFinal::If(
            condition,
            true_branch,
            false_branch,
            meta_ast,
        ));
    }

    fn finish_with_return(
        &mut self,
        expr: HIRExpr<'source, TypeInstanceId>,
        meta_ast: HIRAstMetadata<'source>,
    ) {
        self.blocks[self.current_block.0].finish = Some(MIRBlockFinal::Return(expr, meta_ast));
    }

    fn finish_with_empty_return(&mut self) {
        self.blocks[self.current_block.0].finish = Some(MIRBlockFinal::EmptyReturn);
    }

    fn finish(self) -> (Vec<MIRScope<'source>>, Vec<MIRBlock<'source, NotChecked>>) {
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
                    nodes: x.block,
                }
            })
            .collect::<Vec<_>>();

        (self.scopes, blocks)
    }

    fn check_if_block_is_finished(&self, block_id: BlockId) -> bool {
        self.blocks[block_id.0].finish.is_some()
    }

    fn push_goto_block(&mut self, block: BlockId) {
        self.goto_stack.push(block)
    }

    fn pop_goto_block(&mut self) {
        self.goto_stack.pop();
    }

    fn peek_goto_block(&self) -> Option<&BlockId> {
        self.goto_stack.last()
    }
}

//returns the "root block" that this execution generated (or started with)
#[allow(clippy::too_many_lines)] //too lazy for now, and I think it's ok here
fn process_body<'source>(
    emitter: &mut MIRFunctionEmitter<'source>,
    body: Vec<InferredTypeHIR<'source>>,
) {
    for hir in body {
        match hir {
            HIR::Assign {
                path,
                expression,
                meta_ast,
                meta_expr,
            } => {
                emitter.emit(MIRBlockNode::Assign {
                    path,
                    expression,
                    meta_ast,
                    meta_expr,
                });
            }
            HIR::Declare {
                var,
                typedef,
                expression,
                meta_ast,
                meta_expr,
            } => {
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
                emitter.scope_add_variable(new_scope, var, typedef);

                //go to block 1
                emitter.set_current_block(new_block);
                //and *finally* assign the variable
                emitter.emit(MIRBlockNode::Assign {
                    path: vec![var],
                    expression,
                    meta_ast,
                    meta_expr,
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
                meta_ast,
                meta_expr,
            } => {
                match &function {
                    HIRExpr::Variable(var, function_type, ..) => {
                        let pending_args = args.iter().map(std::clone::Clone::clone).collect();

                        emitter.emit(MIRBlockNode::FunctionCall {
                            function: var,
                            args: pending_args,
                            meta_ast,
                            meta_expr,
                            return_type: *function_type,
                        });
                    }
                    other => panic!("{:?} is not a function!", other),
                };
            }
            HIR::While(expr, body, meta_ast) => {
                let current_scope = emitter.current_scope;
                let current_block = emitter.current_block;

                assert!(
                    !body.is_empty(),
                    "Empty body on while statement reached MIR"
                );

                //create a block for the while branch
                let while_body_scope = emitter.create_scope(current_scope);
                let while_body_block = emitter.new_block(while_body_scope);

                let fallback_block = generate_or_get_fallback_block(emitter, current_scope);

                //the code inside the the body needs to know where to return when their scope ends
                //so we add this goto stack block for the body, otherwise it will make the function return
                //so we make them return to the current block, reevaluate the condition and loop again
                emitter.push_goto_block(current_block);

                //go to the while block
                emitter.set_current_block(while_body_block);
                process_body(emitter, body);

                emitter.pop_goto_block();

                //do the loop
                //ensure we are in the while block first
                emitter.set_current_block(while_body_block);

                //check if the user returned. Kinda weird to do it directly in the while loop
                //but ok
                let emitted_block_returns = emitter.check_if_block_is_finished(while_body_block);

                //if they don't return, go back to the beginning of the loop
                if !emitted_block_returns {
                    emitter.finish_with_goto_block(current_block);
                }

                emitter.set_current_block(current_block);
                emitter.finish_with_branch(
                    expr,
                    while_body_block, //return to the block
                    fallback_block,   //go to the fallback block
                    meta_ast,
                );

                emitter.set_current_block(fallback_block);
            }

            HIR::If(condition, true_branch_hir, false_branch_hir, ast) => {
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
                    assert!(
                        !true_branch_hir.is_empty(),
                        "Empty true branch on if statement reached MIR"
                    );
                    //create a block for the true branch
                    let true_branch_scope = emitter.create_scope(current_scope);
                    let true_branch_block = emitter.new_block(true_branch_scope);
                    //go to the true branch block
                    emitter.set_current_block(true_branch_block);
                    process_body(emitter, true_branch_hir);

                    //does the branch have a return instruction, or is already finished
                    let emitted_block_returns =
                        emitter.check_if_block_is_finished(true_branch_block);

                    if !emitted_block_returns {
                        if fallback_scope_and_block.is_none() {
                            let fallback_block =
                                generate_or_get_fallback_block(emitter, current_scope);
                            fallback_scope_and_block = Some(fallback_block);
                        }
                        let fallback_block = fallback_scope_and_block.unwrap();
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
                    emitter.finish_with_branch(condition, true_block, false_branch_block, ast);

                    if !emitted_block_returns {
                        if fallback_scope_and_block.is_none() {
                            let fallback_block =
                                generate_or_get_fallback_block(emitter, current_scope);
                            fallback_scope_and_block = Some(fallback_block);
                        }
                        let fallback_block = fallback_scope_and_block.unwrap();
                        emitter.set_current_block(false_branch_block);
                        emitter.finish_with_goto_block(fallback_block);
                        emitter.set_current_block(fallback_block);
                    } else if !true_branch_returns {
                        emitter.set_current_block(fallback_scope_and_block.unwrap());
                    }
                } else {
                    //no else code, go to the fallback
                    //generate the branch, but the false branch just jumps to the fallback block
                    emitter.set_current_block(current_block);

                    if fallback_scope_and_block.is_none() {
                        let fallback_block = generate_or_get_fallback_block(emitter, current_scope);
                        fallback_scope_and_block = Some(fallback_block);
                    }
                    let fallback_block = fallback_scope_and_block.unwrap();
                    emitter.finish_with_branch(condition, true_block, fallback_block, ast);

                    //in this case the function continues in the fallback block
                    emitter.set_current_block(fallback_block);
                }
            }
            HIR::Return(expr, meta_ast) => {
                emitter.finish_with_return(expr, meta_ast);
            }
            HIR::EmptyReturn => {
                emitter.finish_with_empty_return();
            }
        }
    }
}

fn generate_or_get_fallback_block<'source>(
    emitter: &mut MIRFunctionEmitter<'source>,
    current_scope: ScopeId,
) -> BlockId {
    let fallback_block = if let Some(block) = emitter.peek_goto_block() {
        *block
    } else {
        //create a fallback block, for after the while statement
        let fallback_scope = emitter.create_scope(current_scope);
        emitter.new_block(fallback_scope)
    };
    fallback_block
}

pub fn process_hir_funcdecl<'source>(
    function_name: SourceString<'source>,
    parameters: Vec<HIRTypedBoundName<'source, TypeInstanceId>>,
    body: Vec<InferredTypeHIR<'source>>,
    return_type: TypeInstanceId,
    is_intrinsic: bool,
) -> MIRTopLevelNode<'source, NotChecked> {
    if is_intrinsic {
        return MIRTopLevelNode::IntrinsicFunction {
            function_name,
            parameters: parameters
                .iter()
                .map(|x| MIRTypedBoundName {
                    name: x.name,
                    type_instance: x.typename,
                })
                .collect::<Vec<_>>(),
            return_type,
        };
    }

    let mut emitter = MIRFunctionEmitter::new();

    //create a new block for the main function decl node
    let function_zero_scope = emitter.create_scope(ScopeId(0));
    let function_zero_block = emitter.new_block(function_zero_scope);
    emitter.set_current_block(function_zero_block);

    for param in parameters.iter() {
        emitter.scope_add_variable(function_zero_scope, param.name, param.typename);
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
    return MIRTopLevelNode::DeclareFunction {
        function_name,
        parameters: parameters
            .iter()
            .map(|x| MIRTypedBoundName {
                name: x.name,
                type_instance: x.typename,
            })
            .collect::<Vec<_>>(),
        body,
        scopes,
        return_type,
    };
}

pub fn hir_to_mir<'source>(
    hir_nodes: Vec<InferredTypeHIRRoot<'source>>,
) -> Vec<MIRTopLevelNode<'source, NotChecked>> {
    let mut top_levels = vec![];
    for hir in hir_nodes {
        match hir {
            HIRRoot::DeclareFunction {
                function_name,
                parameters,
                body,
                return_type,
                meta: _,
                is_intrinsic,
            } => {
                let fdecl = process_hir_funcdecl(
                    function_name,
                    parameters,
                    body,
                    return_type,
                    is_intrinsic,
                );
                top_levels.push(fdecl);
            }
            HIRRoot::StructDeclaration {
                struct_name: _,
                type_parameters: _,
                fields: _,
                meta: _,
            } => {
                //ignored, useless in MIR because the type db should be used instead
            }
        }
    }
    top_levels
}

#[cfg(test)]
#[allow(clippy::too_many_lines)]
mod tests {
    use crate::{
        ast::parser::Parser, semantic::mir_printer, types::type_instance_db::TypeInstanceManager,
    };
    #[cfg(test)]
    use pretty_assertions::assert_eq;

    use super::*;

    fn prepare(source: &str) -> AST {
        let tokenized = crate::ast::lexer::Tokenizer::new(source)
            .tokenize()
            .ok()
            .unwrap();
        let mut parser = Parser::new(tokenized);
        let ast = AST::Root(parser.parse_ast());
        println!("AST: {:?}", &ast);
        ast
    }

    //Parses a single expression
    fn mir<'source>(
        ast: &'source AST<'source>,
    ) -> (
        Vec<MIRTopLevelNode<'source, NotChecked>>,
        TypeInstanceManager<'source>,
    ) {
        let analysis_result = crate::semantic::analysis::do_analysis(ast);
        println!("HIR: {:?}", &analysis_result.hir);
        (hir_to_mir(analysis_result.hir), analysis_result.type_db)
    }

    #[test]
    fn simplest_case() {
        let ast = prepare(
            "
def main():
    return 1",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main():
    x = 1",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main(x: i32) -> i32:
    return x",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main(x: i32, y: i64, z: f64, name: str) -> i32:
    return x",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main(x: i32) -> i32:
    return x + 1",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main(x: i32) -> i32:
    y = 0
    return x + y
",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main(x: i32) -> i32:
    y = 1
    z: i32 = 2 + x
    return x / (y + z)",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main() -> i32:
    y = 1
    y = 2
    y = 3
    y = 4",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main():
    y = 1
    x = y + 1",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main():
    y = 1
    if y == 1:
        print(y)",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main():
    if True:
        print(1)
    else:
        print(2)",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main() -> i32:
    if True:
        return 1
    else:
        return 2",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main() -> i32:
    x = 0
    if True:
        y = x + 1
        return y
    else:
        x = 1
        y = 2 + x
        return x + y
    ",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main() -> i32:
    x = 0
    if x == 0:
        x = x + 1
    print(x)
    ",
        );
        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main() -> i32:
    x = 0
    if x == 0:
        print(1)
    else:
        return x
    print(x)
    ",
        );
        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main() -> i32:
    x = 0
    if x == 0:
        x = x + 1
    else:
        x = 2
    print(x)",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main() -> i32:
    x = 0
    if True:
        y = x + 1
        print(x)
    else:
        x = 1
        y = 2 + x
        return x + y
    ",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
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
    ",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
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
    ",
        );

        let (mir, type_db) = mir(&ast);
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
        let ast = prepare(
            "
def main():
    x : i32 = 15
    y : i32 = 3
    z : i32 = x + y
    result: i32 = 5 + z
    result = result + y",
        );

        let (mir, type_db) = mir(&ast);
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
