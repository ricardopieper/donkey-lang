use std::collections::VecDeque;

use super::hir::{
    FunctionCall, HIR, HIRExpr, HIRRoot, HIRTypedBoundName, LiteralHIRExpr, MethodCall, NodeIndex,
    TypeIndex, TypeTable,
};
use crate::ast::parser::SpannedOperator;
use crate::commons::float::FloatLiteral;
use crate::interner::InternedString;

use crate::types::diagnostics::{
    CompilerErrorContext, DerefOnNonPointerError, RootElementType,
    TypeErrors,
};

type CompilerError = ();

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralMIRExpr {
    Integer(i128),
    Float(FloatLiteral),
    Char(char),
    String(InternedString),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRExprLValue {
    Variable(InternedString, TypeIndex, NodeIndex),
    MemberAccess(Box<MIRExpr>, InternedString, TypeIndex, NodeIndex),
    Deref(
        Box<MIRExpr>,
        //This type is the dereferenced type, i.e. if the original is ptr<u8>, this is u8
        TypeIndex,
        NodeIndex,
    ),
}
impl MIRExprLValue {
    pub fn get_type(&self) -> TypeIndex {
        match self {
            MIRExprLValue::Variable(_, type_id, _) => *type_id,
            MIRExprLValue::MemberAccess(_, _, type_id, _) => *type_id,
            MIRExprLValue::Deref(_, type_id, _) => *type_id,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PolymorphicName(InternedString);

//@TODO This is a big one! Maybe this idea of scopes wasn't really that useful. I created it in order to evaluate whether variables are in scope, but this is already being done by
//the HIR phase. I need it to judge whether declarations must be created from first assignments.
//Maybe we can just nuke it.

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRExprRValue {
    Literal(LiteralMIRExpr, TypeIndex, NodeIndex),
    BinaryOperation(
        Box<MIRExpr>,
        SpannedOperator,
        Box<MIRExpr>,
        TypeIndex,
        NodeIndex,
    ),
    //obj_expr, method_name, args:type, return type, metadata
    MethodCall(
        Box<MIRExpr>,
        InternedString,
        Vec<MIRExpr>,
        TypeIndex,
        NodeIndex,
    ),
    FunctionCall(
        Box<MIRExprLValue>,
        Vec<MIRExpr>,
        TypeIndex,
        NodeIndex,
        Option<PolymorphicName>,
    ),
    StructInstantiate(TypeIndex, NodeIndex),
    TypeVariable {
        type_variable: TypeIndex,
        type_data: TypeIndex,
        location: NodeIndex,
    },
    Ref(
        //you can only ref an lvalue
        Box<MIRExprLValue>,
        TypeIndex,
        NodeIndex,
    ),
    UnaryExpression(SpannedOperator, Box<MIRExpr>, TypeIndex, NodeIndex),
    Array(Vec<MIRExpr>, TypeIndex, NodeIndex),
    Cast(Box<MIRExpr>, TypeIndex, NodeIndex),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRExpr {
    RValue(MIRExprRValue),
    LValue(MIRExprLValue),
}

impl MIRExpr {
    pub fn get_type(&self) -> TypeIndex {
        match self {
            MIRExpr::RValue(rvalue_expr) => match rvalue_expr {
                MIRExprRValue::TypeVariable { type_data, .. } => *type_data,
                MIRExprRValue::Literal(_, type_id, _) => *type_id,
                MIRExprRValue::BinaryOperation(_, _, _, type_id, _) => *type_id,
                MIRExprRValue::MethodCall(_, _, _, type_id, _) => *type_id,
                MIRExprRValue::FunctionCall(_, _, type_id, ..) => *type_id,
                MIRExprRValue::Ref(_, type_id, _) => *type_id,
                MIRExprRValue::UnaryExpression(_, _, type_id, _) => *type_id,
                MIRExprRValue::Array(_, type_id, _) => *type_id,
                MIRExprRValue::StructInstantiate(type_id, _) => *type_id,
                MIRExprRValue::Cast(_, type_id, _) => *type_id,
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
pub enum MIRTopLevelNode {
    IntrinsicOrExternalFunction {
        function_name: InternedString,
        parameters: Vec<MIRTypedBoundName>,
        return_type: TypeIndex,
        is_varargs: bool,
        is_external: bool,
        type_table: TypeTable,
    },

    DeclareFunction {
        function_name: InternedString,
        parameters: Vec<MIRTypedBoundName>,
        body: Vec<MIRBlock>,
        scopes: Vec<MIRScope>,
        return_type: TypeIndex,
        type_table: TypeTable,
        struct_name: Option<InternedString>,
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
        path: MIRExprLValue,
        expression: MIRExpr,
        location: NodeIndex,
    },
    FunctionCall {
        //@TODO maybe it should be an lvalue expr...
        function: InternedString,
        args: Vec<MIRExpr>,
        location: NodeIndex,
        return_type: TypeIndex,
    },
    MethodCall {
        object: MIRExpr,
        method_name: InternedString,
        args: Vec<MIRExpr>,
        location: NodeIndex,
        return_type: TypeIndex,
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
    pub type_instance: TypeIndex,
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
pub enum MIRBlockFinal {
    //expression, true, else, meta
    If(MIRExpr, BlockId, BlockId, NodeIndex),
    GotoBlock(BlockId),
    Return(MIRExpr, NodeIndex),
    EmptyReturn(NodeIndex),
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
    pub nodes: Vec<MIRBlockNode>,
}

//returns the simplified expression, and whether the expression was simplified at all. Can be called until no simplification is possible.
//In theory this could do constant folding, but we're just removing &* and *&
fn simplify_expression(expr: HIRExpr) -> (HIRExpr, bool) {
    match expr {
        HIRExpr::Literal(literal, location, ty) => (HIRExpr::Literal(literal, location, ty), false),
        HIRExpr::StructInstantiate(expr, type_args, location, ty) => (
            HIRExpr::StructInstantiate(expr, type_args, location, ty),
            false,
        ),
        HIRExpr::Variable(var_name, location, ty) => {
            (HIRExpr::Variable(var_name, location, ty), false)
        }
        //@TODO casting a literal to a type can be optimized here, just change the inferred type instead of generating an actual cast
        HIRExpr::Cast(expr, user_type, location, ty) => {
            let (expr, simplified) = simplify_expression(*expr);
            (
                HIRExpr::Cast(Box::new(expr), user_type, location, ty),
                simplified,
            )
        }
        HIRExpr::BinaryOperation(lhs, op, rhs, ty, loc) => {
            let (lhs, simplified_lhs) = simplify_expression(*lhs);
            let (rhs, simplified_rhs) = simplify_expression(*rhs);
            let simplified = simplified_lhs || simplified_rhs;
            (
                HIRExpr::BinaryOperation(lhs.into(), op, rhs.into(), ty, loc),
                simplified,
            )
        }
        HIRExpr::MethodCall(mcall, location) => {
            let MethodCall {
                object,
                method_name,
                args,
                return_type,
            } = mcall;
            let (object, simplified_object) = simplify_expression(*object);
            let (args, simplified_args): (_, Vec<_>) =
                args.into_iter().map(simplify_expression).unzip();
            let simplified =
                simplified_object || simplified_args.iter().any(|simplified| *simplified);
            (
                HIRExpr::MethodCall(
                    MethodCall {
                        object: object.into(),
                        method_name,
                        args,
                        return_type,
                    },
                    location,
                ),
                simplified,
            )
        }
        HIRExpr::FunctionCall(call, location) => {
            let FunctionCall {
                function,
                args,
                type_args,
                return_type,
            } = *call;
            let (function, simplified_function) = simplify_expression(function);
            let (args, simplified_args): (Vec<_>, Vec<_>) =
                args.into_iter().map(simplify_expression).unzip();
            let simplified =
                simplified_function || simplified_args.iter().any(|simplified| *simplified);
            (
                //HIRExpr::FunctionCall(function.into(), ty_args, args, ty, meta),
                HIRExpr::FunctionCall(
                    FunctionCall {
                        function,
                        args,
                        type_args,
                        return_type,
                    }
                    .into(),
                    location,
                ),
                simplified,
            )
        }
        HIRExpr::Deref(derefed_expr, location, ty) => {
            match *derefed_expr {
                HIRExpr::Ref(refed_expr, _, _) => {
                    //we can remove the deref and the ref
                    let (refed_expr, simplified) = simplify_expression(*refed_expr);
                    //we might lose some metadata here but it's okay I guess
                    (refed_expr, simplified)
                }
                _ => {
                    let (derefed_expr, simplified) = simplify_expression(*derefed_expr);
                    (
                        HIRExpr::Deref(Box::new(derefed_expr), location, ty),
                        simplified,
                    )
                }
            }
        }
        HIRExpr::Ref(refed_expr, location, ty) => match *refed_expr {
            //do the inverse as the code above
            HIRExpr::Deref(derefed_expr, _, _) => {
                let (derefed_expr, simplified) = simplify_expression(*derefed_expr);
                (derefed_expr, simplified)
            }
            _ => {
                let (refed_expr, simplified) = simplify_expression(*refed_expr);
                (HIRExpr::Ref(Box::new(refed_expr), location, ty), simplified)
            }
        },
        HIRExpr::UnaryExpression(op, rhs, ty, location) => {
            let (rhs, simplified) = simplify_expression(*rhs);
            (
                HIRExpr::UnaryExpression(op, rhs.into(), ty, location),
                simplified,
            )
        }
        HIRExpr::MemberAccess(obj, member, location, ty) => {
            let (obj, simplified) = simplify_expression(*obj);
            (
                HIRExpr::MemberAccess(obj.into(), member, location, ty),
                simplified,
            )
        }
        HIRExpr::Array(items, location, ty) => {
            let (items, simplified_items): (Vec<_>, Vec<_>) = items
                .into_iter()
                .map(simplify_expression)
                .unzip();
            let simplified = simplified_items.iter().any(|simplified| *simplified);
            (HIRExpr::Array(items, location, ty), simplified)
        }
        HIRExpr::TypeName {
            type_variable,
            type_data,
            location,
        } => (
            HIRExpr::TypeName {
                type_variable,
                type_data,
                location,
            },
            false,
        ),
        HIRExpr::SelfValue(location, ty) => (HIRExpr::SelfValue(location, ty), false),
    }
}

fn hir_expr_to_mir(
    element: RootElementType,
    expr: HIRExpr,
    errors: &mut TypeErrors,
    type_table: &TypeTable,
) -> Result<MIRExpr, CompilerError> {
    let (mut current_expr, mut simplified) = simplify_expression(expr);
    loop {
        if !simplified {
            return convert_expr_to_mir(element, current_expr, errors, type_table);
        }
        (current_expr, simplified) = simplify_expression(current_expr);
    }
}

fn expect_lvalue(
    element: RootElementType,
    expr: MIRExpr,
    errors: &mut TypeErrors,
    location: NodeIndex,
    type_table: &TypeTable,
) -> Result<MIRExprLValue, CompilerError> {
    match expr {
        MIRExpr::RValue(_) => {
            errors.invalid_derefed_type.push(CompilerErrorContext {
                error: DerefOnNonPointerError {
                    attempted_type: type_table[expr.get_type()].clone(),
                },
                on_element: element,
                location,
                compiler_code_location: loc!(),
            });
            Err(())
        }
        MIRExpr::LValue(lvalue) => Ok(lvalue),
    }
}

fn convert_expr_to_mir(
    element: RootElementType,
    expr: HIRExpr,
    errors: &mut TypeErrors,
    type_table: &TypeTable,
) -> Result<MIRExpr, CompilerError> {
    match expr {
        HIRExpr::Literal(literal, location, ty) => {
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
                location,
            )))
        }
        HIRExpr::Variable(name, location, ty) => {
            Ok(MIRExpr::LValue(MIRExprLValue::Variable(name, ty, location)))
        }
        HIRExpr::Cast(expr, _, location, ty) => Ok(MIRExpr::RValue(MIRExprRValue::Cast(
            hir_expr_to_mir(element, *expr, errors, type_table)?.into(),
            ty,
            location,
        ))),
        HIRExpr::BinaryOperation(lhs, op, rhs, location, ty) => {
            let lhs = hir_expr_to_mir(element, *lhs, errors, type_table)?;
            let rhs = hir_expr_to_mir(element, *rhs, errors, type_table)?;
            Ok(MIRExpr::RValue(MIRExprRValue::BinaryOperation(
                lhs.into(),
                op,
                rhs.into(),
                ty,
                location,
            )))
        }
        HIRExpr::MethodCall(mcall, location) => {
            let MethodCall {
                object,
                method_name,
                args,
                return_type,
            } = mcall;
            let obj = hir_expr_to_mir(element, *object, errors, type_table)?;
            let args: Result<Vec<_>, _> = args
                .into_iter()
                .map(|arg| hir_expr_to_mir(element, arg, errors, type_table))
                .collect();

            Ok(MIRExpr::RValue(MIRExprRValue::MethodCall(
                obj.into(),
                method_name,
                args?,
                return_type,
                location,
            )))
        }
        HIRExpr::FunctionCall(fcall, location) => {
            let FunctionCall {
                function,
                args,
                type_args, //does not matter: at this point it has been monomorphized
                return_type,
            } = *fcall;

            assert!(
                type_args.is_empty(),
                "Type args should have been monomorphized and already emptied before MIR transformation"
            );

            let function_expr = hir_expr_to_mir(element, function, errors, type_table)?;
            let args: Result<Vec<_>, _> = args
                .into_iter()
                .map(|arg| hir_expr_to_mir(element, arg, errors, type_table))
                .collect();

            let function_lvalue =
                expect_lvalue(element, function_expr, errors, location, type_table)?;

            Ok(MIRExpr::RValue(MIRExprRValue::FunctionCall(
                function_lvalue.into(),
                args?,
                return_type,
                location,
                None, //@TODO: For error reporting, fill this field with the polymorphic name of the function (without the suffix name for types)
            )))
        }
        HIRExpr::Deref(expr, location, ty) => {
            let expr = hir_expr_to_mir(element, *expr, errors, type_table)?;
            Ok(MIRExpr::LValue(MIRExprLValue::Deref(
                expr.into(),
                ty,
                location,
            )))
        }
        HIRExpr::Ref(expr, location, ty) => {
            let expr = hir_expr_to_mir(element, *expr, errors, type_table)?;

            let ref_lvalue = expect_lvalue(element, expr, errors, location, type_table)?;

            Ok(MIRExpr::RValue(MIRExprRValue::Ref(
                ref_lvalue.into(),
                ty,
                location,
            )))
        }
        HIRExpr::UnaryExpression(op, rhs, location, ty) => {
            let rhs = hir_expr_to_mir(element, *rhs, errors, type_table)?;
            Ok(MIRExpr::RValue(MIRExprRValue::UnaryExpression(
                op,
                rhs.into(),
                ty,
                location,
            )))
        }
        HIRExpr::MemberAccess(obj, field, location, ty) => {
            let obj = hir_expr_to_mir(element, *obj, errors, type_table)?;
            Ok(MIRExpr::LValue(MIRExprLValue::MemberAccess(
                obj.into(),
                field,
                ty,
                location,
            )))
        }
        HIRExpr::Array(items, location, ty) => {
            let items: Result<Vec<_>, _> = items
                .into_iter()
                .map(|item| hir_expr_to_mir(element, item, errors, type_table))
                .collect();
            Ok(MIRExpr::RValue(MIRExprRValue::Array(items?, ty, location)))
        }
        HIRExpr::StructInstantiate(_name, _typeargs, location, ty) => Ok(MIRExpr::RValue(
            MIRExprRValue::StructInstantiate(ty, location),
        )),
        HIRExpr::TypeName {
            type_variable,
            type_data,
            location,
        } => Ok(MIRExpr::RValue(MIRExprRValue::TypeVariable {
            type_variable,
            type_data,
            location,
        })),
        HIRExpr::SelfValue(ty_id, location) => Ok(MIRExpr::LValue(MIRExprLValue::Variable(
            "self".into(),
            location,
            ty_id,
        ))),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CodegenJobType {
    List(Vec<HIR>),
    Group(Vec<HIR>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum DefaultFinish {
    Goto(BlockId),
    //@TODO: Maybe there should be always an implicit, lazily-intantiated default empty return block?
    EmptyReturn,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CodegenJob {
    block: BlockId,
    scope: ScopeId,
    job: CodegenJobType,
    default_finish: DefaultFinish,
}

pub struct MIRFunctionEmitter<'errors> {
    function_name: RootElementType,
    errors: &'errors mut TypeErrors,
    root_ast: NodeIndex,
    queue: VecDeque<CodegenJob>,
    blocks: Vec<MIRBlock>,
    scopes: Vec<MIRScope>,
    empty_return_block: Option<BlockId>,
    type_table: TypeTable,
}

impl<'errors> MIRFunctionEmitter<'errors> {
    pub fn new(
        function_name: InternedString,
        errors: &'errors mut TypeErrors,
        type_table: TypeTable,
        root: NodeIndex,
    ) -> Self {
        Self {
            function_name: RootElementType::Function(function_name),
            errors,
            root_ast: root,
            blocks: vec![],
            scopes: vec![],
            empty_return_block: None,
            queue: VecDeque::new(),
            type_table,
        }
    }

    pub fn run(
        &mut self,
        body: Vec<HIR>,
        parameters: Vec<HIRTypedBoundName>,
    ) -> Result<(), CompilerError> {
        //the function has a starting scope, create it
        let starting_scope = self.create_scope(ScopeId(0));
        //add all parameters to the scope 0
        for param in parameters.iter() {
            self.scope_add_variable(starting_scope, param.name, param.type_data.type_variable);
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

    fn scope_add_variable(&mut self, scope_id: ScopeId, var: InternedString, typedef: TypeIndex) {
        let scope = &mut self.scopes[scope_id.0];
        scope.boundnames.push(MIRTypedBoundName {
            name: var,
            type_instance: typedef,
        });
    }

    fn emit(&mut self, block: BlockId, node: MIRBlockNode) {
        self.blocks[block.0].nodes.push(node);
    }

    fn finish_with(&mut self, block: BlockId, node: MIRBlockFinal) {
        self.blocks[block.0].finish = node;
    }

    fn codegen(&mut self, job: CodegenJob) -> Result<(), CompilerError> {
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

                let mut groups: Vec<Vec<HIR>> = vec![];
                let mut current_group: Vec<HIR> = vec![];

                for item in items {
                    match item {
                        item @ (HIR::If(..)
                        | HIR::While(..)
                        | HIR::Declare { .. }
                        | HIR::SyntheticDeclare { .. }) => {
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
                let mut last_scope = scope;
                for (i, _) in groups.iter().enumerate() {
                    if i == 0 {
                        //since we are doing a rescheduling, we can reuse the current block
                        groups_blocks.push((scope, block));
                    } else {
                        //since the code must flow onto the next block, the current block can inherit the previous one especially
                        //when it's a declaration (i.e. if the prev block finished on a declaration then the next block must inherit it, but
                        //I think it's safe to always inherit the previous one
                        let new_scope = self.create_scope(last_scope);
                        let new_block = self.new_block(new_scope);
                        groups_blocks.push((new_scope, new_block));
                        last_scope = new_scope;
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

                Ok(())
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
                            location,
                        } => {
                            let mir_expr = hir_expr_to_mir(
                                self.function_name,
                                path,
                                self.errors,
                                &self.type_table,
                            )?;

                            match mir_expr {
                                MIRExpr::RValue(_) => {
                                    self.errors.invalid_derefed_type.push(CompilerErrorContext {
                                        error: DerefOnNonPointerError {
                                            attempted_type: self.type_table[mir_expr.get_type()]
                                                .clone(),
                                        },
                                        on_element: self.function_name,
                                        location,
                                        compiler_code_location: loc!(),
                                    });
                                }
                                MIRExpr::LValue(lvalue) => {
                                    let lvalue_rhs_expr = hir_expr_to_mir(
                                        self.function_name,
                                        expression,
                                        self.errors,
                                        &self.type_table,
                                    )?;
                                    self.emit(
                                        block,
                                        MIRBlockNode::Assign {
                                            path: lvalue,
                                            expression: lvalue_rhs_expr,
                                            location,
                                        },
                                    );
                                    //will return with the default finish
                                }
                            }
                        }
                        HIR::FunctionCall(
                            FunctionCall {
                                function,
                                type_args: _,
                                args,
                                return_type,
                            },
                            location,
                        ) => match &function {
                            HIRExpr::Variable(var, _function_type, ..) => {
                                let pending_args: Result<Vec<_>, _> = args
                                    .into_iter()
                                    .map(|x| {
                                        hir_expr_to_mir(
                                            self.function_name,
                                            x,
                                            self.errors,
                                            &self.type_table,
                                        )
                                    })
                                    .collect();

                                self.emit(
                                    block,
                                    MIRBlockNode::FunctionCall {
                                        function: *var,
                                        args: pending_args?,

                                        return_type,
                                        location,
                                    },
                                );
                                //will return with the default finish
                            }
                            other => panic!("{other:?} is not a function!"),
                        },
                        HIR::MethodCall(
                            MethodCall {
                                object,
                                method_name,
                                args,
                                return_type,
                            },
                            location,
                        ) => {
                            let object = hir_expr_to_mir(
                                self.function_name,
                                *object,
                                self.errors,
                                &self.type_table,
                            )?;

                            let args: Result<Vec<_>, _> = args
                                .into_iter()
                                .map(|x| {
                                    hir_expr_to_mir(
                                        self.function_name,
                                        x,
                                        self.errors,
                                        &self.type_table,
                                    )
                                })
                                .collect();

                            self.emit(
                                block,
                                MIRBlockNode::MethodCall {
                                    object,
                                    method_name,
                                    args: args?,
                                    return_type,
                                    location,
                                },
                            );
                            //will return with the default finish
                        }
                        HIR::Declare {
                            var,
                            typedef,
                            expression,
                            location,
                        } => {
                            self.scope_add_variable(scope, var, typedef.type_variable);
                            let expr_type = expression.get_type();
                            let declare_rhs_expr = hir_expr_to_mir(
                                self.function_name,
                                expression,
                                self.errors,
                                &self.type_table,
                            )?;
                            self.emit(
                                block,
                                MIRBlockNode::Assign {
                                    path: MIRExprLValue::Variable(var, expr_type, location),
                                    expression: declare_rhs_expr,
                                    location,
                                },
                            );
                            assert!(
                                is_last,
                                "declare statements must be the last statement in a group"
                            );
                            //will return with the default finish
                        }
                        HIR::SyntheticDeclare {
                            var,
                            typedef,
                            expression,
                            location,
                        } => {
                            self.scope_add_variable(scope, var, typedef);
                            let expr_type = expression.get_type();
                            let declare_rhs_expr = hir_expr_to_mir(
                                self.function_name,
                                expression,
                                self.errors,
                                &self.type_table,
                            )?;
                            self.emit(
                                block,
                                MIRBlockNode::Assign {
                                    path: MIRExprLValue::Variable(var, expr_type, location),
                                    expression: declare_rhs_expr,
                                    location,
                                },
                            );
                            /*assert!(
                                is_last,
                                "declare statements must be the last statement in a group"
                            );*/
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

                            let condition_expr = hir_expr_to_mir(
                                self.function_name,
                                expr,
                                self.errors,
                                &self.type_table,
                            )?;

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

                            let condition_expr = hir_expr_to_mir(
                                self.function_name,
                                expr,
                                self.errors,
                                &self.type_table,
                            )?;

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
                            let return_expr = hir_expr_to_mir(
                                self.function_name,
                                expr,
                                self.errors,
                                &self.type_table,
                            )?;
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

                Ok(())
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

pub fn hir_to_mir(
    hir_nodes: Vec<HIRRoot>,
    errors: &mut TypeErrors,
) -> Result<Vec<MIRTopLevelNode>, CompilerError> {
    let mut top_levels = vec![];
    for hir in hir_nodes {
        match hir {
            HIRRoot::DeclareFunction {
                function_name,
                type_parameters: _,
                parameters,
                body,
                return_type,
                is_intrinsic,
                is_varargs,
                is_external,
                method_of: _,
                type_table,
                has_been_monomorphized: _,
            } => {
                let mir_parameters = parameters
                    .iter()
                    .map(|x| MIRTypedBoundName {
                        name: x.name,
                        type_instance: x.type_data.type_variable,
                    })
                    .collect::<Vec<_>>();

                if is_intrinsic || is_external {
                    top_levels.push(MIRTopLevelNode::IntrinsicOrExternalFunction {
                        function_name,
                        parameters: mir_parameters,
                        return_type: return_type.type_variable,
                        is_varargs,
                        is_external,
                        type_table: type_table.clone(),
                    });
                    continue;
                } else {
                    let mut mir_emitter = MIRFunctionEmitter::new(
                        function_name,
                        errors,
                        type_table.clone(),
                        body[0].get_node_index(),
                    );

                    mir_emitter.run(body, parameters)?;

                    let result = MIRTopLevelNode::DeclareFunction {
                        function_name,
                        parameters: mir_parameters,
                        body: mir_emitter.blocks,
                        scopes: mir_emitter.scopes,
                        return_type: return_type.type_variable,
                        type_table,
                        struct_name: None,
                    };
                    top_levels.push(result);
                }
            }
            HIRRoot::StructDeclaration {
                struct_name: _,
                type_parameters: _,
                fields: _,
                type_table: _,
                has_been_monomorphized: _,
            } => {
                //ignored, useless in MIR because the type db should be used instead
            }
            HIRRoot::ImplDeclaration {
                struct_name,
                methods,
                ..
            } => {
                for method in methods.into_iter() {
                    log!("Processing method {method:?}");
                    match method {
                        HIRRoot::DeclareFunction {
                            function_name,
                            type_parameters: _,
                            parameters,
                            body,
                            return_type,
                            type_table,
                            ..
                        } => {

                            let mir_parameters = parameters
                                .iter()
                                .map(|x| MIRTypedBoundName {
                                    name: x.name,
                                    type_instance: x.type_data.type_variable,
                                })
                                .collect::<Vec<_>>();

                            let mut mir_emitter = MIRFunctionEmitter::new(
                                function_name,
                                errors,
                                type_table.clone(),
                                body[0].get_node_index(),
                            );

                            mir_emitter.run(body, parameters)?;

                            let result = MIRTopLevelNode::DeclareFunction {
                                function_name,
                                parameters: mir_parameters,
                                body: mir_emitter.blocks,
                                scopes: mir_emitter.scopes,
                                return_type: return_type.type_variable,
                                type_table: type_table.clone(),
                                struct_name: Some(struct_name),
                            };
                            top_levels.push(result);
                        }
                        _ => {
                            panic!("Unsupported")
                        }
                    }
                }
            }
        }
    }
    Ok(top_levels)
}
