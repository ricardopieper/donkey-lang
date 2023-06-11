use std::collections::VecDeque;

use super::compiler_errors::CompilerError;
use super::context::FileTableIndex;
use super::hir::{
    HIRAstMetadata, HIRExpr, HIRRoot, HIRTypedBoundName, MonomorphizedHIR,
    LiteralHIRExpr, HIR, MonomorphizedHIRRoot, FunctionCall,
};
use super::hir_type_resolution::RootElementType;
use crate::ast::parser::SpannedOperator;
use crate::commons::float::FloatLiteral;
use crate::interner::InternedString;

use crate::types::type_errors::{DerefOnNonPointerError, ContextualizedCompilerError, TypeErrors};
use crate::{
    ast::parser::{Expr, AST},
    types::type_instance_db::TypeInstanceId,
};

pub type TypecheckPendingExpression<'source> = MIRExpr<'source>;
pub type TypecheckedExpression<'source> = MIRExpr<'source>;

pub type MIRExprMetadata<'source> = &'source Expr;
pub type MIRAstMetadata<'source> = &'source AST;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralMIRExpr {
    Integer(i128),
    Float(FloatLiteral),
    Char(char),
    String(InternedString),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRExprLValue<'source> {
    Variable(InternedString, TypeInstanceId, MIRExprMetadata<'source>),
    MemberAccess(
        Box<MIRExpr<'source>>,
        InternedString,
        TypeInstanceId,
        MIRExprMetadata<'source>,
    ),
    Deref(
        Box<MIRExpr<'source>>,
        //This type is the dereferenced type, i.e. if the original is ptr<u8>, this is u8
        TypeInstanceId,
        MIRExprMetadata<'source>,
    ),
}
impl MIRExprLValue<'_> {
    pub fn get_type(&self) -> TypeInstanceId {
        match self {
            MIRExprLValue::Variable(_, type_id, _) => *type_id,
            MIRExprLValue::MemberAccess(_, _, type_id, _) => *type_id,
            MIRExprLValue::Deref(_, type_id, _) => *type_id,
        }
    }
}

//@TODO This is a big one! Maybe this idea of scopes wasn't really that useful. I created it in order to evaluate whether variables are in scope, but this is already being done by
//the HIR phase. I need it to judge whether declarations must be created from first assignments.
//Maybe we can just nuke it.

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRExprRValue<'source> {
    Literal(LiteralMIRExpr, TypeInstanceId, MIRExprMetadata<'source>),
    BinaryOperation(
        Box<MIRExpr<'source>>,
        SpannedOperator,
        Box<MIRExpr<'source>>,
        TypeInstanceId,
        MIRExprMetadata<'source>,
    ),
    //obj_expr, method_name, args:type, return type, metadata
    MethodCall(
        Box<MIRExpr<'source>>,
        InternedString,
        Vec<MIRExpr<'source>>,
        TypeInstanceId,
        MIRExprMetadata<'source>,
    ),
    FunctionCall(
        Box<MIRExprLValue<'source>>,
        Vec<MIRExpr<'source>>,
        TypeInstanceId,
        MIRExprMetadata<'source>,
    ),
    StructInstantiate(TypeInstanceId, MIRExprMetadata<'source>),
    Ref(
        //you can only ref an lvalue
        Box<MIRExprLValue<'source>>,
        TypeInstanceId,
        MIRExprMetadata<'source>,
    ),
    UnaryExpression(
        SpannedOperator,
        Box<MIRExpr<'source>>,
        TypeInstanceId,
        MIRExprMetadata<'source>,
    ),
    Array(
        Vec<MIRExpr<'source>>,
        TypeInstanceId,
        MIRExprMetadata<'source>,
    ),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRExpr<'source> {
    RValue(MIRExprRValue<'source>),
    LValue(MIRExprLValue<'source>),
}

impl MIRExpr<'_> {
    pub fn get_type(&self) -> TypeInstanceId {
        match self {
            MIRExpr::RValue(rvalue_expr) => match rvalue_expr {
                MIRExprRValue::Literal(_, type_id, _) => *type_id,
                MIRExprRValue::BinaryOperation(_, _, _, type_id, _) => *type_id,
                MIRExprRValue::MethodCall(_, _, _, type_id, _) => *type_id,
                MIRExprRValue::FunctionCall(_, _, type_id, _) => *type_id,
                MIRExprRValue::Ref(_, type_id, _) => *type_id,
                MIRExprRValue::UnaryExpression(_, _, type_id, _) => *type_id,
                MIRExprRValue::Array(_, type_id, _) => *type_id,
                MIRExprRValue::StructInstantiate(type_id, _) => *type_id,
            },
            MIRExpr::LValue(lvalue) => lvalue.get_type(),
        }
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
pub enum MIRTopLevelNode<'source> {
    IntrinsicFunction {
        function_name: InternedString,
        parameters: Vec<MIRTypedBoundName>,
        return_type: TypeInstanceId,
        is_varargs: bool,
    },
    DeclareFunction {
        function_name: InternedString,
        parameters: Vec<MIRTypedBoundName>,
        body: Vec<MIRBlock<'source>>,
        scopes: Vec<MIRScope>,
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
pub enum MIRBlockNode<'source> {
    Assign {
        path: MIRExprLValue<'source>,
        expression: MIRExpr<'source>,
        meta_ast: &'source AST,
        meta_expr: &'source Expr,
    },
    FunctionCall {
        //@TODO maybe it should be an lvalue expr...
        function: InternedString,
        args: Vec<MIRExpr<'source>>,
        meta_ast: &'source AST,
        meta_expr: &'source Expr,
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
pub struct MIRTypedBoundName {
    pub name: InternedString,
    pub type_instance: TypeInstanceId,
}

/*
  MIRScope represents a scope inside a function. Scopes can inherit larger scopes.
  You can think of a scope as an "indentation level", but this is too simplistic: each variable declaration
  can introduce a new scope, and you don't have access to variables declared after the fact.
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRScope {
    pub id: ScopeId,
    pub inherit: ScopeId,
    pub boundnames: Vec<MIRTypedBoundName>,
}

/*MIRBlockFinal specifies how a block ends: in a goto, branch, or a return. */
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRBlockFinal<'source> {
    //expression, true, else, meta
    If(
        MIRExpr<'source>,
        BlockId,
        BlockId,
        MIRAstMetadata<'source>,
    ),
    GotoBlock(BlockId),
    Return(MIRExpr<'source>, MIRAstMetadata<'source>),
    EmptyReturn(MIRAstMetadata<'source>),
}

/*MIRBlock is the definition of an executable chunk of code.
  Blocks are composed of executable high-level instructions, the scope it uses,
  and how it ends. Every block has to end.

  A block never has a loop inside itself. Infinite loops can only be achieved by multiple blocks.

*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRBlock<'source> {
    pub index: usize,
    pub scope: ScopeId,
    pub finish: MIRBlockFinal<'source>,
    pub nodes: Vec<MIRBlockNode<'source>>,
}

pub type TypecheckedMIRBlock<'source> = MIRBlock<'source>;

//returns the simplified expression, and whether the expression was simplified at all. Can be called until no simplification is possible.
//In theory this could do constant folding, but we're just removing &* and *&
fn simplify_expression(
    expr: HIRExpr<'_, TypeInstanceId>,
) -> (HIRExpr<'_, TypeInstanceId>, bool) {
    match expr {
        HIRExpr::Literal(literal, ty, meta) => (HIRExpr::Literal(literal, ty, meta), false),
        HIRExpr::StructInstantiate(expr, type_args, ty, meta) => (HIRExpr::StructInstantiate(expr, type_args, ty, meta), false),
        HIRExpr::Variable(var_name, ty, meta) => (HIRExpr::Variable(var_name, ty, meta), false),
        //@TODO casting a literal to a type can be optimized here, just change the inferred type instead of generating an actual cast
        HIRExpr::Cast(expr, ty, meta) => {
            let (expr, simplified) = simplify_expression(*expr);
            (HIRExpr::Cast(Box::new(expr), ty, meta), simplified)
        }
        HIRExpr::BinaryOperation(lhs, op, rhs, ty, meta) => {
            let (lhs, simplified_lhs) = simplify_expression(*lhs);
            let (rhs, simplified_rhs) = simplify_expression(*rhs);
            let simplified = simplified_lhs || simplified_rhs;
            (
                HIRExpr::BinaryOperation(lhs.into(), op, rhs.into(), ty, meta),
                simplified,
            )
        }
        HIRExpr::MethodCall(object, name, args, ty, meta) => {
            let (object, simplified_object) = simplify_expression(*object);
            let (args, simplified_args): (_, Vec<_>) =
                args.into_iter().map(|arg| simplify_expression(arg)).unzip();
            let simplified =
                simplified_object || simplified_args.iter().any(|simplified| *simplified);
            (
                HIRExpr::MethodCall(object.into(), name, args, ty, meta),
                simplified,
            )
        }
        HIRExpr::FunctionCall(call) => {
            let FunctionCall { function, args, type_args, return_type, meta_expr, meta_ast } = *call;
            let (function, simplified_function) = simplify_expression(function);
            let (args, simplified_args): (Vec<_>, Vec<_>) =
                args.into_iter().map(|arg| simplify_expression(arg)).unzip();
            let simplified =
                simplified_function || simplified_args.iter().any(|simplified| *simplified);
            (
                //HIRExpr::FunctionCall(function.into(), ty_args, args, ty, meta),
                HIRExpr::FunctionCall(FunctionCall { function, args, type_args, return_type, meta_expr, meta_ast }.into()),
                simplified,
            )
        }
        HIRExpr::Deref(derefed_expr, ty, meta) => match *derefed_expr {
            HIRExpr::Ref(refed_expr, _, _) => {
                //we can remove the deref and the ref
                let (refed_expr, simplified) = simplify_expression(*refed_expr);
                //we might lose some metadata here but it's okay I guess
                (refed_expr, simplified)
            }
            _ => {
                let (derefed_expr, simplified) = simplify_expression(*derefed_expr);
                (HIRExpr::Deref(Box::new(derefed_expr), ty, meta), simplified)
            }
        },
        HIRExpr::Ref(refed_expr, ty, meta) => match *refed_expr {
            //do the inverse as the code above
            HIRExpr::Deref(derefed_expr, _, _) => {
                let (derefed_expr, simplified) = simplify_expression(*derefed_expr);
                (derefed_expr, simplified)
            }
            _ => {
                let (refed_expr, simplified) = simplify_expression(*refed_expr);
                (HIRExpr::Ref(Box::new(refed_expr), ty, meta), simplified)
            }
        },
        HIRExpr::UnaryExpression(op, rhs, ty, meta) => {
            let (rhs, simplified) = simplify_expression(*rhs);
            (
                HIRExpr::UnaryExpression(op, rhs.into(), ty, meta),
                simplified,
            )
        }
        HIRExpr::MemberAccess(obj, member, ty, meta) => {
            let (obj, simplified) = simplify_expression(*obj);
            (
                HIRExpr::MemberAccess(obj.into(), member, ty, meta),
                simplified,
            )
        }
        HIRExpr::Array(items, ty, meta) => {
            let (items, simplified_items): (Vec<_>, Vec<_>) = items
                .into_iter()
                .map(|item| simplify_expression(item))
                .unzip();
            let simplified = simplified_items.iter().any(|simplified| *simplified);
            (HIRExpr::Array(items, ty, meta), simplified)
        }
    }
}

fn hir_expr_to_mir<'a>(
    file: FileTableIndex,
    element: RootElementType,
    expr: HIRExpr<'a, TypeInstanceId>,
    errors: &mut TypeErrors<'a>,
) -> Result<MIRExpr<'a>, CompilerError> {
    let (mut current_expr, mut simplified) = simplify_expression(expr);
    loop {
        if !simplified {
            return convert_expr_to_mir(file, element, current_expr, errors);
        }
        (current_expr, simplified) = simplify_expression(current_expr);
    }
}

macro_rules! expect_lvalue {
    ($element:expr, $file:expr, $expr:expr, $errors:expr, $meta:expr) => {
        match $expr {
            MIRExpr::RValue(_) => {
                return $errors.invalid_derefed_type.push(
                    DerefOnNonPointerError {
                        attempted_type: $expr.get_type(),
                    }
                    .at_spanned($element, $file, $meta, loc!()),
                ).as_type_check_error();
            }
            MIRExpr::LValue(lvalue) => lvalue
        }
    };
}

fn convert_expr_to_mir<'a>(
    file: FileTableIndex,
    element: RootElementType,
    expr: HIRExpr<'a, TypeInstanceId>,
    errors: &mut TypeErrors<'a>,
) -> Result<MIRExpr<'a>, CompilerError> {
    match expr {
        HIRExpr::Literal(literal, ty, meta) => {
            let mir_literal: LiteralMIRExpr = match literal {
                LiteralHIRExpr::Integer(i) => LiteralMIRExpr::Integer(i),
                LiteralHIRExpr::Float(f) => LiteralMIRExpr::Float(f),
                LiteralHIRExpr::Char(c) => LiteralMIRExpr::Char(c),
                LiteralHIRExpr::String(s) => LiteralMIRExpr::String(s),
                LiteralHIRExpr::Boolean(b) => LiteralMIRExpr::Boolean(b),
                LiteralHIRExpr::None => todo!("None not implemented in MIR"),
            };
            Ok(MIRExpr::RValue(MIRExprRValue::Literal(
                mir_literal,
                ty,
                meta,
            )))
        }
        HIRExpr::Variable(name, ty, meta) => {
            Ok(MIRExpr::LValue(MIRExprLValue::Variable(name, ty, meta)))
        }
        HIRExpr::Cast(..) => todo!(),
        HIRExpr::BinaryOperation(lhs, op, rhs, ty, meta) => {
            let lhs = hir_expr_to_mir(file, element, *lhs, errors)?;
            let rhs = hir_expr_to_mir(file, element, *rhs, errors)?;
            Ok(MIRExpr::RValue(MIRExprRValue::BinaryOperation(
                lhs.into(),
                op,
                rhs.into(),
                ty,
                meta,
            )))
        }
        HIRExpr::MethodCall(obj, method_name, args, ty, meta) => {
            let obj = hir_expr_to_mir(file, element, *obj, errors)?;
            let args: Result<Vec<_>, _> = args
                .into_iter()
                .map(|arg| hir_expr_to_mir(file, element, arg, errors))
                .collect();

            Ok(MIRExpr::RValue(MIRExprRValue::MethodCall(
                obj.into(),
                method_name,
                args?,
                ty,
                meta,
            )))
        }
        HIRExpr::FunctionCall(fcall) => {
            let FunctionCall { function, args, type_args, return_type, meta_expr, meta_ast } = *fcall;
            let function_expr = hir_expr_to_mir(file, element, function, errors)?;
            let args: Result<Vec<_>, _> = args
                .into_iter()
                .map(|arg| hir_expr_to_mir(file, element, arg, errors))
                .collect();

            let function_lvalue = expect_lvalue!(element, file, function_expr, errors, meta_expr);

            Ok(MIRExpr::RValue(MIRExprRValue::FunctionCall(
                function_lvalue.into(),
                args?,
                return_type,
                meta_expr,
            )))
        }
        HIRExpr::Deref(expr, ty, meta) => {
            let expr = hir_expr_to_mir(file, element, *expr, errors)?;
            Ok(MIRExpr::LValue(MIRExprLValue::Deref(expr.into(), ty, meta)))
        }
        HIRExpr::Ref(expr, ty, meta) => {
            let expr = hir_expr_to_mir(file, element, *expr, errors)?;

            let ref_lvalue = expect_lvalue!(element, file, expr, errors, meta);

            Ok(MIRExpr::RValue(MIRExprRValue::Ref(
                ref_lvalue.into(),
                ty,
                meta,
            )))
        }
        HIRExpr::UnaryExpression(op, rhs, ty, meta) => {
            let rhs = hir_expr_to_mir(file, element, *rhs, errors)?;
            Ok(MIRExpr::RValue(MIRExprRValue::UnaryExpression(
                op,
                rhs.into(),
                ty,
                meta,
            )))
        }
        HIRExpr::MemberAccess(obj, field, ty, meta) => {
            let obj = hir_expr_to_mir(file, element, *obj, errors)?;
            Ok(MIRExpr::LValue(MIRExprLValue::MemberAccess(
                obj.into(),
                field,
                ty,
                meta,
            )))
        }
        HIRExpr::Array(items, ty, meta) => {
            let items: Result<Vec<_>, _> = items
                .into_iter()
                .map(|item| hir_expr_to_mir(file, element, item, errors))
                .collect();
            Ok(MIRExpr::RValue(MIRExprRValue::Array(items?, ty, meta)))
        }
        HIRExpr::StructInstantiate(_name, _typeargs, ty, meta) => {
            Ok(MIRExpr::RValue(MIRExprRValue::StructInstantiate(ty, meta)))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CodegenJobType<'source> {
    List(Vec<MonomorphizedHIR<'source>>),
    Group(Vec<MonomorphizedHIR<'source>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum DefaultFinish {
    Goto(BlockId),
    //@TODO: Maybe there should be always an implicit, lazily-intantiated default empty return block?
    EmptyReturn,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CodegenJob<'source> {
    block: BlockId,
    scope: ScopeId,
    job: CodegenJobType<'source>,
    default_finish: DefaultFinish,
}

pub struct MIRFunctionEmitter<'source, 'errors> {
    file: FileTableIndex,
    function_name: RootElementType,
    errors: &'errors mut TypeErrors<'source>,
    root_ast: HIRAstMetadata<'source>,
    queue: VecDeque<CodegenJob<'source>>,
    blocks: Vec<MIRBlock<'source>>,
    scopes: Vec<MIRScope>,
    empty_return_block: Option<BlockId>,
}

impl<'source, 'errors> MIRFunctionEmitter<'source, 'errors> {
    pub fn new(
        file: FileTableIndex,
        function_name: InternedString,
        errors: &'errors mut TypeErrors<'source>,
        root: HIRAstMetadata<'source>,
    ) -> Self {
        Self {
            file,
            function_name: RootElementType::Function(function_name),
            errors,
            root_ast: root,
            blocks: vec![],
            scopes: vec![],
            empty_return_block: None,
            queue: VecDeque::new(),
        }
    }

    pub fn run(
        &mut self,
        body: Vec<MonomorphizedHIR<'source>>,
        parameters: Vec<HIRTypedBoundName<TypeInstanceId>>,
    ) -> Result<(), CompilerError> {
        //the function has a starting scope, create it
        let starting_scope = self.create_scope(ScopeId(0));
        //add all parameters to the scope 0
        for param in parameters.iter() {
            self.scope_add_variable(starting_scope, param.name, param.type_data);
            //   self.scopes[0].boundnames.push(MIRTypedBoundName { name: param.name, type_instance: param.typename });
        }

        //let starting_scope = if parameters.is_empty() { starting_scope } else { self.create_scope(starting_scope) };

        //the function has a starting block, create it
        let starting_block = self.new_block(starting_scope);

        self.queue.push_back(CodegenJob {
            scope: starting_scope,
            block: starting_block,
            job: CodegenJobType::List(body),
            default_finish: DefaultFinish::EmptyReturn,
        });

        while !self.queue.is_empty() {
            let item = self.queue.pop_front().unwrap();
            self.codegen(item)?;
        }
        Ok(())
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
        let new_block = MIRBlock {
            finish: MIRBlockFinal::EmptyReturn(self.root_ast),
            scope: block_scope,
            index: current_len,
            nodes: vec![],
        };
        self.blocks.push(new_block);
        BlockId(current_len)
    }

    fn scope_add_variable(
        &mut self,
        scope_id: ScopeId,
        var: InternedString,
        typedef: TypeInstanceId,
    ) {
        let scope = &mut self.scopes[scope_id.0];
        scope.boundnames.push(MIRTypedBoundName {
            name: var,
            type_instance: typedef,
        });
    }

    fn emit(&mut self, block: BlockId, node: MIRBlockNode<'source>) {
        self.blocks[block.0].nodes.push(node);
    }

    fn finish_with(&mut self, block: BlockId, node: MIRBlockFinal<'source>) {
        self.blocks[block.0].finish = node;
    }

    fn codegen(&mut self, job: CodegenJob<'source>) -> Result<(), CompilerError> {
        let CodegenJob {
            scope,
            block,
            job,
            default_finish,
        } = job;

        match job {
            CodegenJobType::List(items) => {
                //group items by breaking into groups, where the breaking points are if statements, while statements, and declarations
                //another thing is that we need to group by scope, but this is complicated:
                //  - if it's a variable declaration, we need to create a new scope, and the next groups must use it instead, even if they are in the same indentation level.
                //  - if it's a if/while, we need to create a new scope for their inner contents

                let mut groups: Vec<Vec<MonomorphizedHIR<'source>>> = vec![];
                let mut current_group: Vec<MonomorphizedHIR<'source>> = vec![];

                for item in items {
                    match item {
                        item @ (MonomorphizedHIR::If(..)
                        | MonomorphizedHIR::While(..)
                        | MonomorphizedHIR::Declare { .. }) => {
                            current_group.push(item);
                            groups.push(current_group);
                            current_group = vec![];
                        }
                        item => {
                            current_group.push(item);
                        }
                    }
                }

                if !current_group.is_empty() {
                    groups.push(current_group);
                }

                let mut groups_blocks = vec![];

                for (i, _) in groups.iter().enumerate() {
                    if i == 0 {
                        //since we are doing a rescheduling, we can reuse the current block
                        groups_blocks.push((scope, block));
                        continue;
                    } else {
                        let new_scope = self.create_scope(scope);
                        let new_block = self.new_block(new_scope);
                        groups_blocks.push((new_scope, new_block));
                    }
                }

                //This skip(1) is because block 0 must by default flow into the next block (1) and the last one is the current default finish
                let default_finishes = groups_blocks
                    .clone()
                    .into_iter()
                    .skip(1)
                    .map(|(_, block)| DefaultFinish::Goto(block))
                    .chain(std::iter::once(default_finish.clone()));

                //schedule groups
                for ((group, (scope, block)), finish) in groups
                    .into_iter()
                    .zip(groups_blocks.into_iter())
                    .zip(default_finishes)
                {
                    self.queue.push_back(CodegenJob {
                        block,
                        scope,
                        job: CodegenJobType::Group(group),
                        default_finish: finish,
                    });
                }

                return Ok(());
            }

            //@TODO the fact is that the only operations that can be "grouped" really are only assignments and function calls, as they don't become any control flow
            //structure... maybe this should be taken more advantage of?
            CodegenJobType::Group(items) => {
                let len = items.len();
                for (i, item) in items.into_iter().enumerate() {
                    let is_last = i == len - 1;
                    match item {
                        //assigns are trivial
                        HIR::Assign {
                            path,
                            expression,
                            meta_ast,
                            meta_expr,
                        } => {
                            let mir_expr =
                                hir_expr_to_mir(self.file, self.function_name, path, self.errors)?;

                            match mir_expr {
                                MIRExpr::RValue(_) => {
                                    self.errors.invalid_derefed_type.push(
                                        DerefOnNonPointerError {
                                            attempted_type: mir_expr.get_type(),
                                        }
                                        .at_spanned(
                                            self.function_name,
                                            self.file,
                                            meta_expr,
                                            loc!()
                                        ),
                                    );
                                }
                                MIRExpr::LValue(lvalue) => {
                                    let lvalue_rhs_expr = hir_expr_to_mir(
                                        self.file,
                                        self.function_name,
                                        expression,
                                        self.errors,
                                    )?;
                                    self.emit(
                                        block,
                                        MIRBlockNode::Assign {
                                            path: lvalue,
                                            expression: lvalue_rhs_expr,
                                            meta_ast,
                                            meta_expr,
                                        },
                                    );
                                    //will return with the default finish
                                }
                            }
                        }
                        HIR::FunctionCall(FunctionCall {
                            function,
                            type_args,
                            args,
                            meta_ast,
                            meta_expr,
                            return_type
                        }) => match &function {
                            HIRExpr::Variable(var, function_type, ..) => {
                                let pending_args: Result<Vec<_>, _> = args
                                    .into_iter()
                                    .map(|x| {
                                        hir_expr_to_mir(
                                            self.file,
                                            self.function_name,
                                            x,
                                            self.errors,
                                        )
                                    })
                                    .collect();

                                self.emit(
                                    block,
                                    MIRBlockNode::FunctionCall {
                                        function: *var,
                                        args: pending_args?,
                                        meta_ast: meta_ast.unwrap(),
                                        meta_expr,
                                        return_type,
                                    },
                                );
                                //will return with the default finish
                            }
                            other => panic!("{other:?} is not a function!"),
                        },
                        HIR::Declare {
                            var,
                            typedef,
                            expression,
                            meta_ast,
                            meta_expr,
                        } => {
                            self.scope_add_variable(scope, var, typedef);
                            let expr_type = expression.get_type();
                            let declare_rhs_expr = hir_expr_to_mir(
                                self.file,
                                self.function_name,
                                expression,
                                self.errors,
                            )?;
                            self.emit(
                                block,
                                MIRBlockNode::Assign {
                                    path: MIRExprLValue::Variable(var, expr_type, meta_expr),
                                    expression: declare_rhs_expr,
                                    meta_ast,
                                    meta_expr,
                                },
                            );
                            assert!(
                                is_last,
                                "declare statements must be the last statement in a group"
                            );
                            //will return with the default finish
                        }

                        HIR::If(expr, true_branch, false_branch, meta_ast) => {
                            //create a new block for the true branch
                            let true_scope = self.create_scope(scope);
                            let true_block = self.new_block(true_scope);
                            self.queue.push_back(CodegenJob {
                                block: true_block,
                                scope: true_scope,
                                job: CodegenJobType::List(true_branch),
                                default_finish: default_finish.clone(), //this is correct: default finish will be either empty rreturn or the next block
                            });

                            //if the false branch is empty, we can just jump to the default finish
                            let false_block = if false_branch.is_empty() {
                                self.block_from_default_finish(&default_finish)
                            } else {
                                //otherwise, we need to create a new block for the false branch, schedule the codegen, etc
                                let false_scope = self.create_scope(scope);
                                let false_block = self.new_block(false_scope);
                                self.queue.push_back(CodegenJob {
                                    block: false_block,
                                    scope: false_scope,
                                    job: CodegenJobType::List(false_branch),
                                    default_finish: default_finish.clone(),
                                });
                                false_block
                            };

                            let condition_expr =
                                hir_expr_to_mir(self.file, self.function_name, expr, self.errors)?;

                            //emit the if statement
                            self.finish_with(
                                block,
                                MIRBlockFinal::If(
                                    condition_expr,
                                    true_block,
                                    false_block,
                                    meta_ast,
                                ),
                            );
                            assert!(
                                is_last,
                                "if statements must be the last statement in a group"
                            );
                            return Ok(());
                        }
                        HIR::While(expr, body, meta_ast) => {
                            //We need a block, with a new scope, for the while body
                            let while_scope = self.create_scope(scope);
                            let while_block = self.new_block(while_scope);

                            //compile the condition

                            self.queue.push_back(CodegenJob {
                                block: while_block,
                                scope: while_scope,
                                job: CodegenJobType::List(body),
                                default_finish: DefaultFinish::Goto(block), //by default the body must loop back to us
                            });

                            let condition_expr =
                                hir_expr_to_mir(self.file, self.function_name, expr, self.errors)?;

                            //the current block is the condition evaluation

                            let default_finish_block =
                                self.block_from_default_finish(&default_finish);

                            self.finish_with(
                                block,
                                MIRBlockFinal::If(
                                    condition_expr,
                                    while_block,
                                    default_finish_block,
                                    meta_ast,
                                ),
                            );
                            assert!(
                                is_last,
                                "while statements must be the last statement in a group"
                            );
                            return Ok(());
                        }
                        HIR::Return(expr, meta_ast) => {
                            //if there are multiple returns in sequence, the rest of the block is unreachable, so we can actually just finish it and return
                            let return_expr =
                                hir_expr_to_mir(self.file, self.function_name, expr, self.errors)?;
                            self.finish_with(block, MIRBlockFinal::Return(return_expr, meta_ast));
                            return Ok(());
                        }
                        HIR::EmptyReturn(meta_ast) => {
                            self.finish_with(block, MIRBlockFinal::EmptyReturn(meta_ast));
                            return Ok(());
                        }
                    }
                }

                match default_finish {
                    DefaultFinish::Goto(default_goto_block) => {
                        self.finish_with(block, MIRBlockFinal::GotoBlock(default_goto_block));
                    }
                    DefaultFinish::EmptyReturn => {
                        self.finish_with(block, MIRBlockFinal::EmptyReturn(self.root_ast));
                    }
                }

                return Ok(());
            }
        }
    }

    fn block_from_default_finish(&mut self, default_finish: &DefaultFinish) -> BlockId {
        match *default_finish {
            DefaultFinish::Goto(block) => block,
            DefaultFinish::EmptyReturn => {
                //we will lazy-create the empty return block if necessary
                if let Some(block) = self.empty_return_block {
                    block
                } else {
                    //the scope doesn't really matter.
                    let block = self.new_block(
                        self.scopes
                            .last()
                            .expect("At least one scope should've been created by now")
                            .id,
                    );
                    self.empty_return_block = Some(block);
                    block
                }
            }
        }
    }
}

pub fn hir_to_mir<'source>(
    file: FileTableIndex,
    hir_nodes: Vec<MonomorphizedHIRRoot<'source>>,
    errors: &mut TypeErrors<'source>,
) -> Result<Vec<MIRTopLevelNode<'source>>, CompilerError> {
    let mut top_levels = vec![];
    for hir in hir_nodes {
        match hir {
            HIRRoot::DeclareFunction {
                function_name,
                type_parameters,
                parameters,
                body,
                return_type,
                meta,
                is_intrinsic,
                is_varargs,
            } => {
                let mir_parameters = parameters
                    .iter()
                    .map(|x| MIRTypedBoundName {
                        name: x.name,
                        type_instance: x.type_data,
                    })
                    .collect::<Vec<_>>();

                if is_intrinsic {
                    top_levels.push(MIRTopLevelNode::IntrinsicFunction {
                        function_name,
                        parameters: mir_parameters,
                        return_type,
                        is_varargs,
                    });
                    continue;
                } else {
                    let mut mir_emitter =
                        MIRFunctionEmitter::new(file, function_name, errors, meta);

                    mir_emitter.run(body, parameters)?;

                    let result = MIRTopLevelNode::DeclareFunction {
                        function_name,
                        parameters: mir_parameters,
                        body: mir_emitter.blocks,
                        scopes: mir_emitter.scopes,
                        return_type,
                    };
                    top_levels.push(result);
                }
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
    Ok(top_levels)
}

#[cfg(test)]
#[allow(clippy::too_many_lines)]
mod tests {
    use crate::semantic::context::test_utils::{do_analysis_no_typecheck, parse, parse_no_std};
    use crate::semantic::mir_printer;

    #[cfg(test)]
    use pretty_assertions::assert_eq;

    #[test]
    fn simplest_case() {
        let src = parse_no_std(
            "
def main() -> i32:
    return 1",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, result.interner);
        println!("{final_result}");
        let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defblock 0:
        usescope 0
        return 1";

        assert_eq!(expected.trim(), final_result.trim());
    }

    /* */
    #[test]
    fn set_variable() {
        let src = parse_no_std(
            "
def main():
    x = 1",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
        x : i32
    defblock 0:
        usescope 0
        x = 1
        return";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn just_return_parameter() {
        let src = parse_no_std(
            "
def main(x: i32) -> i32:
    return x",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
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
        let src = parse(
            "
def main(x: i32, y: i64, z: f64, name: str) -> i32:
    return x",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result =
            mir_printer::print_mir_node(result.mir.last().unwrap(), &result.type_db, &src.interner);
        println!("{final_result}");
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
        let src = parse_no_std(
            "
def main(x: i32) -> i32:
    return x + 1",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
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
        //@TODO replace by new result
        let src = parse_no_std(
            "
def main(x: i32) -> i32:
    y = 0
    return x + y
",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def main(x: i32) -> i32:
    defscope 0:
        inheritscope 0
        x : i32
        y : i32
    defscope 1:
        inheritscope 0
    defblock 0:
        usescope 0
        y = 0
        gotoblock 1
    defblock 1:
        usescope 1
        return x + y";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn multiple_variables_and_expressions() {
        let src = parse_no_std(
            "
def main(x: i32) -> i32:
    y = 1
    z: i32 = 2 + x
    return x / (y + z)",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def main(x: i32) -> i32:
    defscope 0:
        inheritscope 0
        x : i32
        y : i32
    defscope 1:
        inheritscope 0
        z : i32
    defscope 2:
        inheritscope 0
    defblock 0:
        usescope 0
        y = 1
        gotoblock 1
    defblock 1:
        usescope 1
        z = 2 + x
        gotoblock 2
    defblock 2:
        usescope 2
        return x / y + z";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn set_same_variable_multiple_times() {
        let src = parse_no_std(
            "
def main() -> i32:
    y = 1
    y = 2
    y = 3
    y = 4",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
        y : i32
    defscope 1:
        inheritscope 0
    defblock 0:
        usescope 0
        y = 1
        gotoblock 1
    defblock 1:
        usescope 1
        y = 2
        y = 3
        y = 4
        return
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn set_and_use() {
        let src = parse_no_std(
            "
def main():
    y = 1
    x = y + 1",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
        y : i32
    defscope 1:
        inheritscope 0
        x : i32
    defblock 0:
        usescope 0
        y = 1
        gotoblock 1
    defblock 1:
        usescope 1
        x = y + 1
        return
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_statement() {
        let src = parse_no_std(
            "
def main():
    y = 1
    if y == 1:
        y = y + 1",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
        y : i32
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 1
    defblock 0:
        usescope 0
        y = 1
        gotoblock 1
    defblock 1:
        usescope 1
        if y == 1:
            gotoblock 2
        else:
            gotoblock 3
    defblock 2:
        usescope 2
        y = y + 1
        return
    defblock 3:
        usescope 2
        return
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_code_in_both_branches() {
        let src = parse_no_std(
            "
def print(x: i32):
    intrinsic

def main():
    if True:
        print(1)
    else:
        print(2)",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def print(x: i32) -> Void
def main() -> Void:
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
        print(1)
        return
    defblock 2:
        usescope 2
        print(2)
        return
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_statement_return_deeply_nested() {
        let src = parse_no_std(
            "
def main() -> i32:
    if True:
        x = 1
        if True:
            return 1
    y = 1
    return 2
",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        y : i32
    defscope 2:
        inheritscope 0
    defscope 3:
        inheritscope 0
        x : i32
    defscope 4:
        inheritscope 3
    defscope 5:
        inheritscope 4
    defblock 0:
        usescope 0
        if True:
            gotoblock 3
        else:
            gotoblock 1
    defblock 1:
        usescope 1
        y = 1
        gotoblock 2
    defblock 2:
        usescope 2
        return 2
    defblock 3:
        usescope 3
        x = 1
        gotoblock 4
    defblock 4:
        usescope 4
        if True:
            gotoblock 5
        else:
            gotoblock 1
    defblock 5:
        usescope 5
        return 1
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_return_in_both_branches() {
        let src = parse_no_std(
            "
def main() -> i32:
    if True:
        return 1
    else:
        return 2",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
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
        let src = parse_no_std(
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

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 1
        y : i32
    defscope 3:
        inheritscope 1
        y : i32
    defscope 4:
        inheritscope 2
    defscope 5:
        inheritscope 3
    defblock 0:
        usescope 0
        x = 0
        gotoblock 1
    defblock 1:
        usescope 1
        if True:
            gotoblock 2
        else:
            gotoblock 3
    defblock 2:
        usescope 2
        y = x + 1
        gotoblock 4
    defblock 3:
        usescope 3
        x = 1
        y = 2 + x
        gotoblock 5
    defblock 4:
        usescope 4
        return y
    defblock 5:
        usescope 5
        return x + y
";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn code_after_if_is_correctly_placed_true_branch_only() {
        let src = parse_no_std(
            "
def print(x: i32):
    intrinsic

def main() -> i32:
    x = 0
    if x == 0:
        x = x + 1
    print(x)
    ",
        );
        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def print(x: i32) -> Void
def main() -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 0
    defscope 3:
        inheritscope 1
    defblock 0:
        usescope 0
        x = 0
        gotoblock 1
    defblock 1:
        usescope 1
        if x == 0:
            gotoblock 3
        else:
            gotoblock 2
    defblock 2:
        usescope 2
        print(x)
        return
    defblock 3:
        usescope 3
        x = x + 1
        gotoblock 2
";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn code_after_if_is_correctly_placed_return_on_false_branch() {
        let src = parse_no_std(
            "
def print(x: i32):
    intrinsic

def main() -> i32:
    x = 0
    if x == 0:
        print(1)
    else:
        return x
    return x
    ",
        );
        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def print(x: i32) -> Void
def main() -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 0
    defscope 3:
        inheritscope 1
    defscope 4:
        inheritscope 1
    defblock 0:
        usescope 0
        x = 0
        gotoblock 1
    defblock 1:
        usescope 1
        if x == 0:
            gotoblock 3
        else:
            gotoblock 4
    defblock 2:
        usescope 2
        return x
    defblock 3:
        usescope 3
        print(1)
        gotoblock 2
    defblock 4:
        usescope 4
        return x
        ";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn code_after_if_is_correctly_placed_true_and_false_branches() {
        let src = parse_no_std(
            "
def print(x: i32):
    intrinsic

def main() -> i32:
    x = 0
    if x == 0:
        x = x + 1
    else:
        x = 2
    print(x)",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def print(x: i32) -> Void
def main() -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 0
    defscope 3:
        inheritscope 1
    defscope 4:
        inheritscope 1
    defblock 0:
        usescope 0
        x = 0
        gotoblock 1
    defblock 1:
        usescope 1
        if x == 0:
            gotoblock 3
        else:
            gotoblock 4
    defblock 2:
        usescope 2
        print(x)
        return
    defblock 3:
        usescope 3
        x = x + 1
        gotoblock 2
    defblock 4:
        usescope 4
        x = 2
        gotoblock 2
";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_one_branch_does_not_return() {
        let src = parse_no_std(
            "
def print(x: i32):
    intrinsic

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

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def print(x: i32) -> Void
def main() -> i32:
    defscope 0:
        inheritscope 0
        x : i32
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 1
        y : i32
    defscope 3:
        inheritscope 1
        y : i32
    defscope 4:
        inheritscope 2
    defscope 5:
        inheritscope 3
    defblock 0:
        usescope 0
        x = 0
        gotoblock 1
    defblock 1:
        usescope 1
        if True:
            gotoblock 2
        else:
            gotoblock 3
    defblock 2:
        usescope 2
        y = x + 1
        gotoblock 4
    defblock 3:
        usescope 3
        x = 1
        y = 2 + x
        gotoblock 5
    defblock 4:
        usescope 4
        print(x)
        return
    defblock 5:
        usescope 5
        return x + y
";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_nested_branch_all_returns() {
        let src = parse_no_std(
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

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        x : i32
    defscope 2:
        inheritscope 0
        y : i32
    defscope 3:
        inheritscope 1
    defscope 4:
        inheritscope 2
    defscope 5:
        inheritscope 3
    defscope 6:
        inheritscope 3
    defscope 7:
        inheritscope 4
    defscope 8:
        inheritscope 4
    defblock 0:
        usescope 0
        if True:
            gotoblock 1
        else:
            gotoblock 2
    defblock 1:
        usescope 1
        x = 1
        gotoblock 3
    defblock 2:
        usescope 2
        y = 3
        gotoblock 4
    defblock 3:
        usescope 3
        if 1 == 1:
            gotoblock 5
        else:
            gotoblock 6
    defblock 4:
        usescope 4
        if 2 == 2:
            gotoblock 7
        else:
            gotoblock 8
    defblock 5:
        usescope 5
        x = x + 3
        return x
    defblock 6:
        usescope 6
        x = x + 1
        return x
    defblock 7:
        usescope 7
        return y + 1
    defblock 8:
        usescope 8
        return 4 * y";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn if_nested_branch_but_one_does_not_return() {
        let src = parse(
            "
def print(x: i32):
    intrinsic

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

        let result = do_analysis_no_typecheck(&src);
        let final_result =
            mir_printer::print_mir_node(result.mir.last().unwrap(), &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def main() -> i32:
    defscope 0:
        inheritscope 0
    defscope 1:
        inheritscope 0
        x : i32
    defscope 2:
        inheritscope 0
        y : i32
    defscope 3:
        inheritscope 1
    defscope 4:
        inheritscope 1
    defscope 5:
        inheritscope 2
    defscope 6:
        inheritscope 3
    defscope 7:
        inheritscope 3
    defscope 8:
        inheritscope 5
    defscope 9:
        inheritscope 5
    defblock 0:
        usescope 0
        if True:
            gotoblock 1
        else:
            gotoblock 2
    defblock 1:
        usescope 1
        x = 1
        gotoblock 3
    defblock 2:
        usescope 2
        y = 3
        gotoblock 5
    defblock 3:
        usescope 3
        if 1 == 1:
            gotoblock 6
        else:
            gotoblock 7
    defblock 4:
        usescope 4
        print(\"nice\")
        return
    defblock 5:
        usescope 5
        if 2 == 2:
            gotoblock 8
        else:
            gotoblock 9
    defblock 6:
        usescope 6
        x = x + 3
        return x
    defblock 7:
        usescope 7
        x = x + 1
        print(x)
        gotoblock 4
    defblock 8:
        usescope 8
        y = y + 1
        print(y)
        return
    defblock 9:
        usescope 9
        return 4 * y";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn set_some_vars_exprs() {
        let src = parse_no_std(
            "
def main():
    x : i32 = 15
    y : i32 = 3
    z : i32 = x + y
    result: i32 = 5 + z
    result = result + y",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result = mir_printer::print_mir(&result.mir, &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
        x : i32
    defscope 1:
        inheritscope 0
        y : i32
    defscope 2:
        inheritscope 0
        z : i32
    defscope 3:
        inheritscope 0
        result : i32
    defscope 4:
        inheritscope 0
    defblock 0:
        usescope 0
        x = 15
        gotoblock 1
    defblock 1:
        usescope 1
        y = 3
        gotoblock 2
    defblock 2:
        usescope 2
        z = x + y
        gotoblock 3
    defblock 3:
        usescope 3
        result = 5 + z
        gotoblock 4
    defblock 4:
        usescope 4
        result = result + y
        return
";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn ref_deref_pattern_is_cancelled_out() {
        let src = parse(
            "
def main(args: array<str>) -> ptr<str>:
    return &args[0]",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result =
            mir_printer::print_mir_node(result.mir.last().unwrap(), &result.type_db, &src.interner);
        //the return would be actually &*args.__index_ptr__(0) but we should cancel out the deref &*
        let expected = "
def main(args: array<str>) -> ptr<str>:
    defscope 0:
        inheritscope 0
        args : array<str>
    defblock 0:
        usescope 0
        return args.__index_ptr__(0)
";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn deref_ref_pattern_is_cancelled_out() {
        let src = parse(
            "
def main():
    some_var = 1
    deref_ref = *&some_var
",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result =
            mir_printer::print_mir_node(result.mir.last().unwrap(), &result.type_db, &src.interner);
        println!("{final_result}");
        //the return would be actually &*args.__index_ptr__(0) but we should cancel out the deref &*
        let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
        some_var : i32
    defscope 1:
        inheritscope 0
        deref_ref : i32
    defblock 0:
        usescope 0
        some_var = 1
        gotoblock 1
    defblock 1:
        usescope 1
        deref_ref = some_var
        return
";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn multiple_ref_deref_cancellation() {
        let src = parse(
            "
def main(args: array<str>) -> ptr<str>:
    return &*&*&args[0]",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result =
            mir_printer::print_mir_node(result.mir.last().unwrap(), &result.type_db, &src.interner);
        let expected = "
def main(args: array<str>) -> ptr<str>:
    defscope 0:
        inheritscope 0
        args : array<str>
    defblock 0:
        usescope 0
        return args.__index_ptr__(0)
";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn multiple_deref_ref_cancellation() {
        let src = parse(
            "
def main(args: array<str>) -> str:
    return *&*&args[0]",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result =
            mir_printer::print_mir_node(result.mir.last().unwrap(), &result.type_db, &src.interner);
        let expected = "
def main(args: array<str>) -> str:
    defscope 0:
        inheritscope 0
        args : array<str>
    defblock 0:
        usescope 0
        return *args.__index_ptr__(0)
";

        assert_eq!(expected.trim(), final_result.trim());
    }

    #[test]
    fn while_loop() {
        let src = parse(
            "
def main():
    i = 10
    while i > 0:
        x = i + 1
        i = i - 1
",
        );

        let result = do_analysis_no_typecheck(&src);
        let final_result =
            mir_printer::print_mir_node(result.mir.last().unwrap(), &result.type_db, &src.interner);
        println!("{final_result}");
        let expected = "
def main() -> Void:
    defscope 0:
        inheritscope 0
        i : i32
    defscope 1:
        inheritscope 0
    defscope 2:
        inheritscope 1
        x : i32
    defscope 3:
        inheritscope 2
    defblock 0:
        usescope 0
        i = 10
        gotoblock 1
    defblock 1:
        usescope 1
        if i > 0:
            gotoblock 2
        else:
            gotoblock 3
    defblock 2:
        usescope 2
        x = i + 1
        gotoblock 4
    defblock 3:
        usescope 2
        return
    defblock 4:
        usescope 3
        i = i - 1
        gotoblock 1
";
        assert_eq!(expected.trim(), final_result.trim());
    }
}
