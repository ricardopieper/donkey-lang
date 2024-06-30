use std::fmt::Display;
use std::fmt::Write;
use std::ops::Deref;

use crate::ast::lexer::Operator;
use crate::ast::parser::ASTIfStatement;
use crate::ast::parser::FunctionDeclaration;
use crate::ast::parser::SpanAST;
use crate::ast::parser::SpannedOperator;
use crate::ast::parser::StringSpan;
use crate::ast::parser::TypeBoundName;
use crate::ast::parser::{ASTType, Expr, AST};
use crate::commons::float::FloatLiteral;
use crate::interner::InternedString;
use crate::types::type_constructor_db::TypeConstructParams;
use crate::types::type_constructor_db::TypeParameter;
use crate::types::type_instance_db::TypeInstanceId;
use crate::types::type_instance_db::TypeInstanceManager;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LiteralHIRExpr {
    Integer(i128),
    Float(FloatLiteral),
    String(InternedString),
    Char(char),
    Boolean(bool),
    None,
}

/**
 * The HIR is a lowered form of the original AST, so it may lose some data. For instance, an indexing access
 * is just a call to __index__, or __index_ptr__ in assignments. If we don't add metadata to the tree, the compiler will report an incorrect call
 * to __index__ instead of an incorrect indexing operation, thus the error message is entirely unusable.
 *
 * Therefore, we will add some metadata to the HIR nodes referring to the raw AST and expr nodes. They will carry the context in which
 * the compiler was operating that is closer to the user, and if anything on that context fails, then that is used in the error messages.
 */
pub type HIRAstMetadata<'source> = &'source AST;
pub type HIRExprMetadata<'source> = &'source Expr;

/**
 * Instead of having just one HIR representation, we have many different representations that are appropriate for each step.
 * For instance, in the type checker, we receive a fully type-inferred HIR instead of receiving one that potentially has
 * types not yet inferred. With generics, we ensure that this is not the case. In fact I had actual problems during development
 * with unexpected uninferred types, and this eliminated them using these types.
 *
 * HIR is for the function *bodies*, while `HIRRoot` is for function declarations, struct declarations, etc.
 */

//The HIR right after AST transformation, no types. TTypeData is () because the type certainly is unknown.
//TUserSpecifiedTypeData is HIRType becase that's what the user types when they need to specify a type.
pub type UninferredHIR<'source> = HIR<'source, HIRType, ()>;
//HIR roots right after AST transformation
pub type StartingHIRRoot<'source> = HIRRoot<'source, HIRType, UninferredHIR<'source>, HIRType>;

//HIR roots after inferring and registering globals, bodies have not changed (i.e. remain uninferred)
pub type GlobalsInferredHIRRoot<'source> =
    HIRRoot<'source, TypeConstructParams, UninferredHIR<'source>, HIRType>;

//The HIR after expanding variable assignments into declarations the first time they're assigned. In this case
//we create them as PendingInference. We also wrap given type declarations by the user in a HIRTypeDef::Provided.
//TUserSpecifiedTypeData is HIRTypeDef instead of just HIRType because when we transform a first assignment into a declaration,
//the user didn't specify a type, and HIRTypeDef has a PendingInference variant. Maybe an Optional would be enough....
pub type FirstAssignmentsDeclaredHIR<'source> = HIR<'source, HIRTypeDef, ()>;
//HIR roots now with body changed, first assignments became declarations.
pub type FirstAssignmentsDeclaredHIRRoot<'source> =
    HIRRoot<'source, TypeConstructParams, FirstAssignmentsDeclaredHIR<'source>, HIRType>;

//The HIR with types inferred
pub type InferredTypeHIR<'source> = HIR<'source, TypeConstructParams, TypeConstructParams>;
//HIR roots with bodies inferred
pub type InferredTypeHIRRoot<'source> =
    HIRRoot<'source, TypeConstructParams, InferredTypeHIR<'source>, TypeConstructParams>;

pub type MonomorphizedHIR<'source> = HIR<'source, TypeInstanceId, TypeInstanceId>;
pub type MonomorphizedHIRRoot<'source> =
    HIRRoot<'source, TypeInstanceId, MonomorphizedHIR<'source>, TypeInstanceId>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRTypeDef {
    PendingInference,
    Provided(HIRType),
}

//This is for types that the user can give, like in function parameters, struct field types, or typed variable declarations.
//This type can be generic and thus not immediatelly inferrable. Thus the user given type must survive until monomorphization.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HIRUserTypeInfo<T> {
    pub user_given_type: Option<HIRType>,
    pub resolved_type: T,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionCall<'source, TTypeData> {
    pub function: HIRExpr<'source, TTypeData>,
    pub args: Vec<HIRExpr<'source, TTypeData>>,
    pub type_args: Vec<HIRUserTypeInfo<TTypeData>>,
    pub return_type: TTypeData,
    pub meta_expr: HIRExprMetadata<'source>,
    pub meta_ast: Option<HIRAstMetadata<'source>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodCall<'source, TTypeData> {
    pub object: Box<HIRExpr<'source, TTypeData>>,
    pub method_name: InternedString,
    pub args: Vec<HIRExpr<'source, TTypeData>>,
    pub return_type: TTypeData,
    pub meta_expr: HIRExprMetadata<'source>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInferenceCertainty {
    Certain,
    Uncertain,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRExpr<'source, TTypeData> {
    Literal(LiteralHIRExpr, TTypeData, HIRExprMetadata<'source>),
    Variable(InternedString, TTypeData, HIRExprMetadata<'source>),

    TypeName {
        type_variable: TTypeData,
        //Always TypeData. @TODO why even have this if it's always TypeData?
        type_data: TTypeData,
        meta: HIRExprMetadata<'source>,
    },
    Cast(
        Box<HIRExpr<'source, TTypeData>>,
        HIRType,
        TTypeData,
        HIRExprMetadata<'source>,
    ),
    SelfValue(TTypeData, HIRExprMetadata<'source>),
    BinaryOperation(
        Box<HIRExpr<'source, TTypeData>>,
        SpannedOperator,
        Box<HIRExpr<'source, TTypeData>>,
        TTypeData,
        TypeInferenceCertainty, //x + y when either x or y is generic, this will be uncertain. All the other types of expressions are certain.
        HIRExprMetadata<'source>,
    ),
    MethodCall(MethodCall<'source, TTypeData>),
    //func_expr, args:type, return type, type_args, metadata
    FunctionCall(Box<FunctionCall<'source, TTypeData>>),
    //struct name, type args, $(args:type,)*, return type, metadata
    //Only TExpr is actually needed in later stages, type args won't need to be inferred individually.
    StructInstantiate(
        InternedString,
        Vec<HIRType>,
        TTypeData,
        HIRExprMetadata<'source>,
    ),
    Deref(
        Box<HIRExpr<'source, TTypeData>>,
        TTypeData,
        HIRExprMetadata<'source>,
    ),
    Ref(
        Box<HIRExpr<'source, TTypeData>>,
        TTypeData,
        HIRExprMetadata<'source>,
    ),
    UnaryExpression(
        SpannedOperator,
        Box<HIRExpr<'source, TTypeData>>,
        TTypeData,
        TypeInferenceCertainty,
        HIRExprMetadata<'source>,
    ),
    //obj, field, result_type, metadata
    MemberAccess(
        Box<HIRExpr<'source, TTypeData>>,
        InternedString,
        TTypeData, //optional because during type inference we might not be able to infer the type of the field due to generics
        HIRExprMetadata<'source>,
    ),
    Array(
        Vec<HIRExpr<'source, TTypeData>>,
        TTypeData,
        HIRExprMetadata<'source>,
    ),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRType {
    Simple(InternedString),
    Generic(InternedString, Vec<HIRType>),
    //Function(Vec<TypeParameter>, Vec<HIRType>, Box<HIRType>, Variadic),
}

//we need to be able to represent complex stuff,
//like a function that receives a function, whose parameters are generic
//def func(another_func: Function<List<String>>)

//so we can store TypeIds, but we need it to be accompanied by more data depending on the kind of the type,
//types such as functions and generics need to be "instanced"

impl From<&ASTType> for HIRType {
    fn from(typ: &ASTType) -> Self {
        match typ {
            ASTType::Simple(name) => Self::Simple(name.0),
            ASTType::Generic(name, generics) => {
                let hir_generics = generics.iter().map(Self::from).collect::<Vec<_>>();
                HIRType::Generic(name.0, hir_generics)
            }
        }
    }
}

pub struct HIRTypeDisplayer<'source> {
    hir_type: &'source HIRType,
}

impl<'source> HIRTypeDisplayer<'source> {
    pub fn new(hir_type: &'source HIRType) -> HIRTypeDisplayer<'source> {
        HIRTypeDisplayer { hir_type }
    }
}

impl<'source> Display for HIRTypeDisplayer<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.hir_type {
            HIRType::Simple(s) => s.write_str(f),
            HIRType::Generic(s, generics) => {
                let comma_sep = generics
                    .iter()
                    .map(|ty| format!("{}", HIRTypeDisplayer::new(ty)))
                    .collect::<Vec<String>>()
                    .join(", ");
                s.write_str(f)?;
                f.write_char('<')?;
                f.write_str(&comma_sep)?;
                f.write_char('>')
            }
        }
    }
}

impl<'source, T: Clone> HIRExpr<'source, T> {
    pub fn get_type(&self) -> T {
        match self {
            HIRExpr::Literal(.., t, _)
            | HIRExpr::Cast(.., t, _)
            | HIRExpr::BinaryOperation(.., t, _, _)
            | HIRExpr::StructInstantiate(.., t, _)
            | HIRExpr::UnaryExpression(.., t, _, _)
            | HIRExpr::Deref(.., t, _)
            | HIRExpr::Ref(.., t, _)
            | HIRExpr::MemberAccess(.., t, _)
            | HIRExpr::Array(.., t, _)
            | HIRExpr::Variable(.., t, _) => t.clone(),
            HIRExpr::FunctionCall(fcall) => fcall.return_type.clone(),
            HIRExpr::MethodCall(mcall) => mcall.return_type.clone(),
            HIRExpr::TypeName { type_data, .. } => type_data.clone(),
            HIRExpr::SelfValue(t, _) => t.clone(),
        }
    }

    pub(crate) fn is_lvalue(&self, _type_db: &TypeInstanceManager) -> bool {
        match self {
            HIRExpr::Literal(..) => false,
            HIRExpr::Variable(..) => true,
            HIRExpr::Cast(..) => false,
            HIRExpr::BinaryOperation(_expr1, op, _expr2, ..)
                if op.0 == Operator::Plus || op.0 == Operator::Minus =>
            {
                todo!("Pointer arithmetic not implemented: must check if either side is a pointer")
            }
            HIRExpr::BinaryOperation(..) => false,
            HIRExpr::MethodCall(..) => {
                //I don't think this is possible, C complains about it for function calls
                false
            }
            HIRExpr::FunctionCall(..) => false,
            HIRExpr::StructInstantiate(..) => false,
            HIRExpr::Deref(..) => {
                //you can point to a value that has been dereferenced, because to dereference a value
                //means the value is a pointer or can be pointed
                true
            }
            HIRExpr::Ref(..) => false,
            HIRExpr::UnaryExpression(..) => false,
            HIRExpr::MemberAccess(obj, ..) => {
                //the only way to create a reference to a member is if the object is an lvalue too
                obj.is_lvalue(_type_db)
            }
            HIRExpr::Array(..) => false,
            HIRExpr::TypeName { .. } => false,
            HIRExpr::SelfValue(_, _) => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HIRTypedBoundName<TTypeData> {
    pub name: InternedString,
    pub type_data: TTypeData,
}

/*
The HIR expression is similar to the AST, but can have type information on every node.
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRRoot<'source, TGlobalTypes, TBodyType, TStructFieldsType> {
    DeclareFunction {
        function_name: InternedString,
        type_parameters: Vec<TypeParameter>,
        parameters: Vec<HIRTypedBoundName<TGlobalTypes>>,
        body: Vec<TBodyType>,
        return_type: TGlobalTypes,
        meta: HIRAstMetadata<'source>,
        method_of: Option<TGlobalTypes>,
        is_intrinsic: bool,
        is_external: bool,
        is_varargs: bool,
    },
    StructDeclaration {
        struct_name: InternedString,
        type_parameters: Vec<TypeParameter>,
        fields: Vec<HIRTypedBoundName<TStructFieldsType>>,
        meta: HIRAstMetadata<'source>,
    },
    ImplDeclaration {
        struct_name: InternedString,
        type_parameters: Vec<TypeParameter>,
        methods: Vec<HIRRoot<'source, TGlobalTypes, TBodyType, TStructFieldsType>>,
        meta: HIRAstMetadata<'source>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIR<'source, TUserSpecifiedTypeData, TInferredTypeData> {
    Assign {
        path: HIRExpr<'source, TInferredTypeData>,
        expression: HIRExpr<'source, TInferredTypeData>,
        meta_ast: HIRAstMetadata<'source>,
        meta_expr: HIRExprMetadata<'source>,
    },
    Declare {
        var: InternedString,
        typedef: TUserSpecifiedTypeData,
        expression: HIRExpr<'source, TInferredTypeData>,
        meta_ast: HIRAstMetadata<'source>,
        meta_expr: HIRExprMetadata<'source>,
        synthetic: bool,
    },
    FunctionCall(FunctionCall<'source, TInferredTypeData>),
    MethodCall(MethodCall<'source, TInferredTypeData>),
    //condition, true branch, false branch
    //this transforms elifs into else: \n\t if ..
    If(
        HIRExpr<'source, TInferredTypeData>,
        Vec<HIR<'source, TUserSpecifiedTypeData, TInferredTypeData>>,
        Vec<HIR<'source, TUserSpecifiedTypeData, TInferredTypeData>>,
        HIRAstMetadata<'source>,
    ),
    //condition, body
    While(
        HIRExpr<'source, TInferredTypeData>,
        Vec<HIR<'source, TUserSpecifiedTypeData, TInferredTypeData>>,
        HIRAstMetadata<'source>,
    ),
    Return(HIRExpr<'source, TInferredTypeData>, HIRAstMetadata<'source>),
    EmptyReturn(HIRAstMetadata<'source>),
}

struct IfTreeNode<'source> {
    condition: HIRExpr<'source, ()>,
    true_body: Vec<HIR<'source, HIRType, ()>>,
    body_meta: &'source AST,
}

fn convert_expr_to_hir<'source>(expr: &'source Expr) -> HIRExpr<'source, ()> {
    expr_to_hir(expr, false)
}

fn expr_to_hir<'source>(expr: &'source Expr, _is_in_assignment_lhs: bool) -> HIRExpr<'source, ()> {
    match expr {
        Expr::IntegerValue(i, _) => HIRExpr::Literal(LiteralHIRExpr::Integer(*i), (), expr),
        Expr::FloatValue(f, _) => HIRExpr::Literal(LiteralHIRExpr::Float(*f), (), expr),
        Expr::StringValue(s) => HIRExpr::Literal(LiteralHIRExpr::String(s.0), (), expr),
        Expr::CharValue(c, _) => HIRExpr::Literal(LiteralHIRExpr::Char(*c), (), expr),
        Expr::BooleanValue(b, _) => HIRExpr::Literal(LiteralHIRExpr::Boolean(*b), (), expr),
        Expr::NoneValue(_) => HIRExpr::Literal(LiteralHIRExpr::None, (), expr),
        Expr::Variable(name) => HIRExpr::Variable(name.0, (), expr),
        Expr::FunctionCall(fun_expr, type_args, args, _) => match fun_expr.as_ref() {
            var @ Expr::Variable(_) => {
                let fcall = FunctionCall {
                    function: expr_to_hir(var, false).into(),
                    type_args: type_args
                        .iter()
                        .map(|x| HIRUserTypeInfo {
                            user_given_type: Some(x.into()),
                            resolved_type: (),
                        })
                        .collect(),
                    args: args.iter().map(|x| expr_to_hir(&x.expr, false)).collect(),
                    return_type: (),
                    meta_expr: &fun_expr,
                    meta_ast: None,
                };
                HIRExpr::FunctionCall(fcall.into())
            }
            Expr::MemberAccess(obj, var_name) => HIRExpr::MethodCall(MethodCall {
                object: expr_to_hir(obj, false).into(),
                method_name: var_name.0,
                args: args
                    .iter()
                    .map(|arg| expr_to_hir(&arg.expr, false))
                    .collect(),
                return_type: (),
                meta_expr: obj,
            }),
            _ => panic!("Cannot lower function call to HIR: not variable or member access"),
        },
        Expr::IndexAccess(object, index, _) => {
            //this makes all array[index] operations take the form of:
            //*(array.__index_ptr__(index))

            let idx = InternedString::new("__index_ptr__");

            let method_call = MethodCall {
                object: Box::new(expr_to_hir(object, false)),
                method_name: idx,
                args: vec![expr_to_hir(index, false)],
                return_type: (),
                meta_expr: expr,
            };

            HIRExpr::Deref(HIRExpr::MethodCall(method_call).into(), (), expr)
        }
        Expr::BinaryOperation(lhs, op, rhs) => {
            let lhs = expr_to_hir(lhs, false);
            let rhs = expr_to_hir(rhs, false);
            HIRExpr::BinaryOperation(
                lhs.into(),
                *op,
                rhs.into(),
                (),
                TypeInferenceCertainty::Certain,
                expr,
            )
        }
        Expr::Parenthesized(_) => panic!("parenthesized not expected"),
        Expr::UnaryExpression(op, rhs) => {
            let rhs = expr_to_hir(rhs, false);

            match op.0 {
                Operator::Multiply => HIRExpr::Deref(rhs.into(), (), expr),
                Operator::Ampersand => HIRExpr::Ref(rhs.into(), (), expr),
                _ => HIRExpr::UnaryExpression(
                    *op,
                    rhs.into(),
                    (),
                    TypeInferenceCertainty::Certain,
                    expr,
                ),
            }
        }
        Expr::MemberAccess(object, member) => {
            let object = expr_to_hir(object, false);
            HIRExpr::MemberAccess(object.into(), member.0, (), expr)
        }
        Expr::Array(items, _) => {
            let items = items.iter().map(|item| expr_to_hir(item, false)).collect();
            HIRExpr::Array(items, (), expr)
        }
        Expr::Cast(casted_expr, ty, _) => {
            let ty: HIRType = ty.into();
            let casted_expr = Box::new(expr_to_hir(casted_expr, false));
            HIRExpr::Cast(casted_expr, ty, (), expr)
        }
        Expr::SelfValue(_) => HIRExpr::SelfValue((), expr),
    }
}

fn ast_to_hir<'source>(ast: &'source SpanAST, accum: &mut Vec<UninferredHIR<'source>>) {
    let ast = &ast.ast;
    match ast {
        AST::Declare { var, expression } => {
            //expr: we have to decompose the expression into HIR declarations
            //because the assigned/declared expression has to be sufficiently simple
            //to decrease complexity for the other compiler phases (typechecking, type inference)

            //maybe a way to do it is by calling reduce_expr_to_hir_declarations, and the function
            //itself returns a HIRExpr. It will also add to the HIR any declarations needed
            //for the decomposition.
            let result_expr = convert_expr_to_hir(&expression.expr);

            let typedef: HIRType = (&var.name_type).into();
            let decl_hir = HIR::Declare {
                var: var.name.0,
                typedef,
                expression: result_expr,
                meta_expr: &expression.expr,
                meta_ast: ast,
                synthetic: false,
            };

            accum.push(decl_hir);
        }
        AST::Assign { path, expression } => {
            let path_expr = expr_to_hir(&path.expr, true);
            let result_expr = convert_expr_to_hir(&expression.expr);

            let decl_hir = HIR::Assign {
                path: path_expr,
                expression: result_expr,
                meta_ast: ast,
                meta_expr: &expression.expr,
            };

            accum.push(decl_hir);
        }
        AST::Return(_, expr) => match expr {
            None => {
                accum.push(HIR::EmptyReturn(ast));
            }
            Some(e) => {
                let result_expr = convert_expr_to_hir(&e.expr);
                accum.push(HIR::Return(result_expr, ast));
            }
        },
        AST::StandaloneExpr(expr) => {
            let Expr::FunctionCall(..) = expr.deref() else {
                panic!("Can only lower function call standalone expr: {expr:#?}");
            };

            let result_expr: HIRExpr<'source, ()> = convert_expr_to_hir(expr);

            match result_expr {
                HIRExpr::MethodCall(mcall) => {
                    let mcall = MethodCall {
                        object: mcall.object,
                        method_name: mcall.method_name,
                        args: mcall.args,
                        return_type: (),
                        meta_expr: expr,
                    };
                    accum.push(HIR::MethodCall(mcall.into()));
                }
                HIRExpr::FunctionCall(mut fcall) => {
                    fcall.meta_ast = Some(ast);
                    accum.push(HIR::FunctionCall(*fcall));
                }
                _ => {
                    panic!("Lowering of function call returned invalid result: {result_expr:?}");
                }
            }
        }
        AST::IfStatement {
            true_branch,
            elifs,
            final_else,
        } => {
            ast_if_to_hir(true_branch, accum, elifs, final_else, ast);
        }
        AST::WhileStatement { expression, body } => {
            ast_while_to_hir(&expression.expr, accum, body, ast);
        }
        AST::Intrinsic(_) => panic!("Cannot use intrinsic keyword"),

        ast => todo!("Not implemented HIR for {ast:?}"),
    }
}

fn ast_decl_function_to_hir<'source>(
    body: &'source [SpanAST],
    function_name: InternedString,
    type_parameters: &'source [StringSpan],
    parameters: &'source [TypeBoundName],
    return_type: &Option<ASTType>,
    is_varargs: bool,
    ast: &'source AST,
    accum: &mut Vec<StartingHIRRoot<'source>>,
) {
    if body.len() == 1 {
        if let AST::Intrinsic(_) = &body[0].ast {
            accum.push(HIRRoot::DeclareFunction {
                function_name,
                type_parameters: type_parameters.iter().map(|x| TypeParameter(x.0)).collect(),
                parameters: create_type_bound_names(parameters),
                body: vec![],
                is_intrinsic: true,
                is_external: false,
                is_varargs,
                return_type: match return_type {
                    Some(x) => x.into(),
                    None => HIRType::Simple(InternedString::new("Void")),
                },
                meta: ast,
                method_of: None,
            });
            return;
        }

        if let AST::External(_) = &body[0].ast {
            accum.push(HIRRoot::DeclareFunction {
                function_name,
                type_parameters: type_parameters.iter().map(|x| TypeParameter(x.0)).collect(),
                parameters: create_type_bound_names(parameters),
                body: vec![],
                is_intrinsic: false,
                is_external: true,
                is_varargs,
                return_type: match return_type {
                    Some(x) => x.into(),
                    None => HIRType::Simple(InternedString::new("Void")),
                },
                meta: ast,
                method_of: None,
            });
            return;
        }
    }

    let mut function_body = vec![];
    for node in body {
        ast_to_hir(node, &mut function_body);
    }

    let decl_hir: StartingHIRRoot = HIRRoot::DeclareFunction {
        function_name,
        parameters: create_type_bound_names(parameters),
        type_parameters: type_parameters.iter().map(|x| TypeParameter(x.0)).collect(),
        body: function_body,
        is_intrinsic: false,
        is_external: false,
        is_varargs,
        return_type: match return_type {
            Some(x) => x.into(),
            None => HIRType::Simple(InternedString::new("Void")),
        },
        meta: ast,
        method_of: None,
    };
    accum.push(decl_hir);
}

fn create_type_bound_names(parameters: &[TypeBoundName]) -> Vec<HIRTypedBoundName<HIRType>> {
    parameters.iter().map(create_type_bound_name).collect()
}

fn create_type_bound_name(param: &TypeBoundName) -> HIRTypedBoundName<HIRType> {
    HIRTypedBoundName {
        name: param.name.0,
        type_data: (&param.name_type).into(),
    }
}

fn ast_if_to_hir<'source>(
    true_branch: &'source ASTIfStatement,
    accum: &mut Vec<UninferredHIR<'source>>,
    elifs: &'source [ASTIfStatement],
    final_else: &'source Option<Vec<SpanAST>>,
    ast: &'source AST,
) {
    let true_branch_result_expr = convert_expr_to_hir(&true_branch.expression.expr);

    let mut true_body_hir = vec![];
    for node in &true_branch.statements {
        ast_to_hir(node, &mut true_body_hir);
    }
    /*
      The HIR representation is simpler, but that means we have to do work to simplify it.
      HIR is only condition, true branch, false branch so the rest of the else ifs go inside the false branch.

      Let's just rewrite whatever the user wrote lmao

      First we have to handle some base cases, i.e. no more elifs ou elses, no more elifs but there is an else, etc.
    */
    if elifs.is_empty() && final_else.is_none() {
        //Just like function declarations, the intermediaries created here don't escape the context.
        //This is different than python, where all variables declared are scoped to the entire function;
        accum.push(HIR::If(true_branch_result_expr, true_body_hir, vec![], ast));
    } else if elifs.is_empty() && final_else.is_some() {
        //in this case we have a final else, just generate a false branch
        let mut false_body_hir = vec![];

        match final_else {
            None => panic!("Shouldn't happen!"),
            Some(nodes) => {
                for node in nodes.iter() {
                    ast_to_hir(node, &mut false_body_hir);
                }

                accum.push(HIR::If(
                    true_branch_result_expr,
                    true_body_hir,
                    false_body_hir,
                    ast,
                ));
            }
        }
    } else {
        //in this case we have elifs, so we build the "tree"
        //and we don't actually need to store the false body because we'll connect everything later.
        //it's not actually a tree... it's more like a linked list.

        //let mut current_if_tree = HIR::If(literal_true_branch_expr, true_body_hir, ());
        let mut nodes = vec![];

        let root_node = IfTreeNode {
            condition: true_branch_result_expr,
            true_body: true_body_hir,
            body_meta: ast,
        };

        nodes.push(root_node);

        for item in elifs {
            let elif_true_branch_result_expr = convert_expr_to_hir(&item.expression.expr);

            let mut if_node = IfTreeNode {
                condition: elif_true_branch_result_expr,
                true_body: vec![],
                body_meta: ast,
            };

            for node in &item.statements {
                ast_to_hir(node, &mut if_node.true_body);
            }
            nodes.push(if_node);
        }
        let mut final_else_body = vec![];
        if let Some(statements) = final_else {
            for node in statements.iter() {
                ast_to_hir(node, &mut final_else_body);
            }
        }

        let mut final_if_chain = None;

        if !final_else_body.is_empty() {
            let first_node = nodes.pop().unwrap(); //there MUST be a node here
            final_if_chain = Some(HIR::If(
                first_node.condition,
                first_node.true_body,
                final_else_body,
                ast,
            ));
        }

        nodes.reverse();
        //we navigate through the nodes in reverse and build the final HIR tree
        for node in nodes {
            let new_node = match final_if_chain {
                None => HIR::If(node.condition, node.true_body, vec![], node.body_meta),
                Some(current_chain) => HIR::If(
                    node.condition,
                    node.true_body,
                    vec![current_chain],
                    node.body_meta,
                ),
            };
            final_if_chain = Some(new_node);
        }
        accum.push(final_if_chain.unwrap());
    }
}

fn ast_while_to_hir<'source>(
    expression: &'source Expr,
    accum: &mut Vec<UninferredHIR<'source>>,
    body: &'source [SpanAST],
    ast: &'source AST,
) {
    let expr = convert_expr_to_hir(expression);

    let mut true_body_hir = vec![];
    for node in body {
        ast_to_hir(node, &mut true_body_hir);
    }

    accum.push(HIR::While(expr, true_body_hir, ast))
}

fn ast_impl_to_hir<'source>(
    body: &'source [FunctionDeclaration],
    struct_name: InternedString,
    type_parameters: &'source [StringSpan],
    ast: &'source AST,
    accum: &mut Vec<StartingHIRRoot<'source>>,
) {
    let mut functions = vec![];
    for node in body {
        ast_decl_function_to_hir(
            &node.body,
            node.function_name.0,
            type_parameters,
            &node.parameters,
            &node.return_type,
            node.is_varargs,
            ast,
            &mut functions,
        );
    }

    accum.push(HIRRoot::ImplDeclaration {
        struct_name,
        type_parameters: type_parameters.iter().map(|x| TypeParameter(x.0)).collect(),
        methods: functions,
        meta: ast,
    });
}

pub fn ast_globals_to_hir<'source>(ast: &'source AST) -> Vec<StartingHIRRoot<'source>> {
    let mut accum = vec![];
    match ast {
        AST::DeclareFunction(FunctionDeclaration {
            function_name,
            parameters,
            body,
            type_parameters,
            return_type,
            is_varargs,
            ..
        }) => {
            ast_decl_function_to_hir(
                body,
                function_name.0,
                type_parameters,
                parameters,
                return_type,
                *is_varargs,
                ast,
                &mut accum,
            );
        }
        AST::Root(ast_nodes) => {
            for node in ast_nodes {
                accum.extend(ast_globals_to_hir(&node.ast));
            }
        }
        AST::ImplDeclaration {
            struct_name,
            type_parameters,
            body,
        } => {
            ast_impl_to_hir(body, struct_name.0, type_parameters, ast, &mut accum);
        }
        AST::StructDeclaration {
            struct_name,
            type_parameters,
            body,
        } => {
            let fields = create_type_bound_names(body);
            accum.push(HIRRoot::StructDeclaration {
                struct_name: struct_name.0,
                fields,
                type_parameters: type_parameters.iter().map(|x| TypeParameter(x.0)).collect(),
                meta: ast,
            });
        }
        other => panic!("AST not supported: {other:?}"),
    }
    accum
}

#[cfg(test)]
mod tests {

    #[cfg(test)]
    use pretty_assertions::assert_eq;

    use super::*;

    use crate::ast::lexer::TokenSpanIndex;
    use crate::ast::parser::{parse_ast, AstSpan};
    use crate::semantic::context::{FileTableEntry, FileTableIndex};
    use crate::semantic::hir;

    use crate::semantic::hir_printer::HIRPrinter;
    use crate::types::type_instance_db::TypeInstanceManager;

    //Parses a single expression
    fn parse(source: &'static str) -> AST {
        let tokens = crate::ast::lexer::tokenize(FileTableIndex(0), source);
        let tokens_lexed = tokens.unwrap();

        let file_table = &[FileTableEntry {
            ast: AST::Break(AstSpan {
                start: TokenSpanIndex {
                    file: FileTableIndex(0),
                    index: 0,
                },
                end: TokenSpanIndex {
                    file: FileTableIndex(0),
                    index: 0,
                },
            }),
            contents: source,
            path: "hir_test".to_string(),
            index: FileTableIndex(0),
            token_table: tokens_lexed,
        }];

        let (ast, _) = parse_ast(&file_table[0].token_table, file_table);
        AST::Root(ast)
    }

    fn build_hir(parsed: AST) -> String {
        let result = hir::ast_globals_to_hir(&parsed);
        HIRPrinter::new(&TypeInstanceManager::new(), false).print_hir(&result)
    }

    #[test]
    fn complex_code() {
        let parsed = parse(
            "
def main(args: List<String>):
    minus: i32 = -my_function(99, 999)
    numbers = [1,2, -3, minus]
    r1 = my_function(1, 2)
    r2 = my_function2(3, 4)
    r3 = my_function(numbers[1], numbers[2])
    print(r1 + r2 + r3)

def my_function(arg1: i32, arg2: i32) -> i32:
    return arg1 * arg2 / (arg2 - arg1)

def my_function2(arg1: i32, arg2: i32) -> i32:
    result1: i32 = my_function(arg1, arg2 + 1)
    result2 = pow(arg1, arg2 * 9)
    return my_function(result1, result2)
",
        );
        let result = build_hir(parsed);
        println!("{result}");

        let expected = "
def main(args: UNRESOLVED List<UNRESOLVED! String>) -> UNRESOLVED! Void:
    minus : UNRESOLVED! i32 = -my_function(99, 999)
    numbers = [1, 2, -3, minus]
    r1 = my_function(1, 2)
    r2 = my_function2(3, 4)
    r3 = my_function(*numbers.__index_ptr__(1), *numbers.__index_ptr__(2))
    print(r1 + r2 + r3)
def my_function(arg1: UNRESOLVED! i32, arg2: UNRESOLVED! i32) -> UNRESOLVED! i32:
    return arg1 * arg2 / arg2 - arg1
def my_function2(arg1: UNRESOLVED! i32, arg2: UNRESOLVED! i32) -> UNRESOLVED! i32:
    result1 : UNRESOLVED! i32 = my_function(arg1, arg2 + 1)
    result2 = pow(arg1, arg2 * 9)
    return my_function(result1, result2)";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn if_statement() {
        let parsed = parse(
            "
def main(args: List<String>):
    if args[0] == 1:
        print(10)
    else:
        print(20)
",
        );
        let expected = "
def main(args: UNRESOLVED List<UNRESOLVED! String>) -> UNRESOLVED! Void:
    if *args.__index_ptr__(0) == 1:
        print(10)
    else:
        print(20)";

        let result = build_hir(parsed);
        println!("{result}");

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn if_chain() {
        let parsed = parse(
            "
def main(args: List<String>):
    arg = args[0]
    if arg == 1:
        print(10)
        if arg <= 0:
            arg = arg + 1
    else:
        if arg == 2:
            print(40)
",
        );

        let expected = "
def main(args: UNRESOLVED List<UNRESOLVED! String>) -> UNRESOLVED! Void:
    arg = *args.__index_ptr__(0)
    if arg == 1:
        print(10)
        if arg <= 0:
            arg = arg + 1
        else:
            pass
    else:
        if arg == 2:
            print(40)
        else:
            pass
";

        let result = build_hir(parsed);
        println!("{result}");

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn return_expr() {
        let parsed = parse(
            "
def main(x: i32) -> i32:
    y = 0
    return x + y
",
        );
        let expected = "
def main(x: UNRESOLVED! i32) -> UNRESOLVED! i32:
    y = 0
    return x + y";

        let result = build_hir(parsed);

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn if_statements_decls_inside_branches() {
        let parsed = parse(
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

        let result = build_hir(parsed);
        println!("{result}");
        let expected = "
def main() -> UNRESOLVED! i32:
    x = 0
    if True:
        y = x + 1
        return y
    else:
        x = 1
        y = 2 + x
        return x + y";

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn array_index_lhs() {
        let parsed = parse(
            "
def main(x: i32) -> i32:
    arr = [1, 2, 3]
    a[0] = 1
    return a[0]
",
        );
        let expected = "
def main(x: UNRESOLVED! i32) -> UNRESOLVED! i32:
    arr = [1, 2, 3]
    *a.__index_ptr__(0) = 1
    return *a.__index_ptr__(0)
";

        let result = build_hir(parsed);

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn cast_numeric() {
        let parsed = parse(
            "
def main(x: i32) -> i32:
    x = 1 as f32
",
        );
        let expected = "
def main(x: UNRESOLVED! i32) -> UNRESOLVED! i32:
    x = 1 as UNRESOLVED! f32
";

        let result = build_hir(parsed);

        assert_eq!(expected.trim(), result.trim());
    }
}
