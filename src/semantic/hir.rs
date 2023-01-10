use std::fmt::Display;
use std::fmt::Write;

use crate::ast::lexer::Operator;
use crate::ast::lexer::SourceString;
use crate::ast::parser::TypeBoundName;
use crate::ast::parser::{ASTType, Expr, AST};
use crate::commons::float::FloatLiteral;
use crate::types::type_constructor_db::GenericParameter;
use crate::types::type_constructor_db::TypeUsage;
use crate::types::type_instance_db::TypeInstanceId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LiteralHIRExpr<'source> {
    Integer(i128),
    Float(FloatLiteral),
    String(SourceString<'source>),
    Boolean(bool),
    None,
}

/**
 * The HIR is a lowered form of the original AST, so it may lose some data. For instance, an indexing access
 * is just a call to __index__. If we don't add metadata to the tree, the compiler will report an incorrect call
 * to __index__ instead of an incorrect indexing operation, thus the error message is entirely unusable.
 *
 * Therefore, we will add some metadata to the HIR nodes referring to the raw AST and expr nodes. They will carry the context in which
 * the compiler was operating that is closer to the user, and if anything on that context fails, then that is used in the error messages.
 */
pub type HIRAstMetadata<'source> = &'source AST<'source>;
pub type HIRExprMetadata<'source> = &'source Expr<'source>;

/**
 * Instead of having just one HIR representation, we have many different representations that are appropriate for each step.
 * For instance, in the type checker, we receive a fully type-inferred HIR instead of receiving one that potentially has
 * types not yet inferred. With generics, we ensure that this is not the case. In fact I had actual problems during development
 * with unexpected uninferred types, and this eliminated them using the Rust type system.
 *
 * HIR is for the function *bodies*, while `HIRRoot` is for function declarations, struct declarations, etc.
 */

//The HIR right after AST transformation, no types. HIRExpr is () because the type certainly is unknown.
pub type UninferredHIR<'source> = HIR<'source, HIRType<'source>, HIRExpr<'source, ()>>;

//The HIR after expanding variable assignments into declarations the first time they're assigned. In this case
//we create them as PendingInference. We also wrap given type declarations in a HIRTypeDef::Provided.
pub type FirstAssignmentsDeclaredHIR<'source> =
    HIR<'source, HIRTypeDef<'source>, HIRExpr<'source, ()>>;

//The HIR with types inferred already, ready for typechecking and MIR lowering
pub type InferredTypeHIR<'source> = HIR<'source, TypeInstanceId, HIRExpr<'source, TypeInstanceId>>;

//HIR roots right after AST transformation
pub type StartingHIRRoot<'source> =
    HIRRoot<'source, HIRType<'source>, UninferredHIR<'source>, HIRType<'source>>;
//HIR roots after inferring and registering globals, bodies have not changed
pub type GlobalsInferredMIRRoot<'source> =
    HIRRoot<'source, TypeInstanceId, UninferredHIR<'source>, TypeUsage<'source>>;
//HIR roots now with body changed, first assignments became declarations
pub type FirstAssignmentsDeclaredHIRRoot<'source> =
    HIRRoot<'source, TypeInstanceId, FirstAssignmentsDeclaredHIR<'source>, TypeUsage<'source>>;
//HIR roots with bodies fully inferred
pub type InferredTypeHIRRoot<'source> =
    HIRRoot<'source, TypeInstanceId, InferredTypeHIR<'source>, TypeUsage<'source>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRTypeDef<'source> {
    PendingInference,
    Provided(HIRType<'source>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NotChecked;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Checked;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRExpr<'source, TExprType, TTypechecked = NotChecked> {
    Literal(LiteralHIRExpr<'source>, TExprType, HIRExprMetadata<'source>),
    Variable(SourceString<'source>, TExprType, HIRExprMetadata<'source>),
    #[allow(dead_code)]
    Cast(
        Box<HIRExpr<'source, TExprType, TTypechecked>>,
        TExprType,
        HIRExprMetadata<'source>,
    ),
    BinaryOperation(
        Box<HIRExpr<'source, TExprType, TTypechecked>>,
        Operator,
        Box<HIRExpr<'source, TExprType, TTypechecked>>,
        TExprType,
        HIRExprMetadata<'source>,
    ),
    //obj_expr, method_name, args:type, return type, metadata
    MethodCall(
        Box<HIRExpr<'source, TExprType, TTypechecked>>,
        SourceString<'source>,
        Vec<HIRExpr<'source, TExprType, TTypechecked>>,
        TExprType,
        HIRExprMetadata<'source>,
    ),
    //func_expr, args:type, return type, metadata
    FunctionCall(
        Box<HIRExpr<'source, TExprType, TTypechecked>>,
        Vec<HIRExpr<'source, TExprType, TTypechecked>>,
        TExprType,
        HIRExprMetadata<'source>,
    ),
    UnaryExpression(
        Operator,
        Box<HIRExpr<'source, TExprType, TTypechecked>>,
        TExprType,
        HIRExprMetadata<'source>,
    ),
    //obj, field, result_type, metadata
    MemberAccess(
        Box<HIRExpr<'source, TExprType, TTypechecked>>,
        SourceString<'source>,
        TExprType,
        HIRExprMetadata<'source>,
    ),
    Array(
        Vec<HIRExpr<'source, TExprType, TTypechecked>>,
        TExprType,
        HIRExprMetadata<'source>,
    ),
    #[allow(dead_code)]
    TypecheckTag(TTypechecked),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRType<'source> {
    Simple(SourceString<'source>),
    Generic(SourceString<'source>, Vec<HIRType<'source>>),
    //Function(Vec<HIRType<'source>>, Box<HIRType<'source>>),
}

//we need to be able to represent complex stuff,
//like a function that receives a function, whose parameters are generic
//def func(another_func: Function<List<String>>)

//so we can store TypeIds, but we need it to be accompanied by more data depending on the kind of the type,
//types such as functions and generics need to be "instanced"

impl<'source> From<&ASTType<'source>> for HIRType<'source> {
    fn from(typ: &ASTType<'source>) -> Self {
        match typ {
            ASTType::Simple(name) => Self::Simple(name),
            ASTType::Generic(name, generics) => {
                let hir_generics = generics.iter().map(Self::from).collect::<Vec<_>>();
                HIRType::Generic(name, hir_generics)
            }
        }
    }
}

impl<'source> Display for HIRType<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HIRType::Simple(s) => f.write_str(s),
            HIRType::Generic(s, generics) => {
                let comma_sep = generics
                    .iter()
                    .map(HIRType::to_string)
                    .collect::<Vec<String>>()
                    .join(", ");

                f.write_str(s)?;
                f.write_char('<')?;
                f.write_str(&comma_sep)?;
                f.write_char('>')
            }
        }
    }
}

impl<'source, T> HIRExpr<'source, TypeInstanceId, T> {
    pub fn get_type(&self) -> TypeInstanceId {
        *match self {
            HIRExpr::Literal(.., t, _)
            | HIRExpr::Cast(.., t, _)
            | HIRExpr::BinaryOperation(.., t, _)
            | HIRExpr::FunctionCall(.., t, _)
            | HIRExpr::UnaryExpression(.., t, _)
            | HIRExpr::MemberAccess(.., t, _)
            | HIRExpr::Array(.., t, _)
            | HIRExpr::MethodCall(.., t, _)
            | HIRExpr::Variable(.., t, _) => t,
            HIRExpr::TypecheckTag(_) => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HIRTypedBoundName<'source, TExprType> {
    pub name: SourceString<'source>,
    pub typename: TExprType,
}

/*
The HIR expression is similar to the AST, but can have type information on every node.
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRRoot<'source, TGlobalTypes, TBodyType, TStructFieldsType> {
    DeclareFunction {
        function_name: SourceString<'source>,
        parameters: Vec<HIRTypedBoundName<'source, TGlobalTypes>>,
        body: Vec<TBodyType>,
        return_type: TGlobalTypes,
        meta: HIRAstMetadata<'source>,
        is_intrinsic: bool,
    },
    StructDeclaration {
        struct_name: SourceString<'source>,
        type_parameters: Vec<GenericParameter<'source>>,
        fields: Vec<HIRTypedBoundName<'source, TStructFieldsType>>,
        meta: HIRAstMetadata<'source>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIR<'source, TVariableDeclType, TExprType> {
    Assign {
        path: Vec<SourceString<'source>>,
        expression: TExprType,
        meta_ast: HIRAstMetadata<'source>,
        meta_expr: HIRExprMetadata<'source>,
    },
    Declare {
        var: SourceString<'source>,
        typedef: TVariableDeclType,
        expression: TExprType,
        meta_ast: HIRAstMetadata<'source>,
        meta_expr: HIRExprMetadata<'source>,
    },
    FunctionCall {
        function: TExprType,
        args: Vec<TExprType>,
        meta_ast: HIRAstMetadata<'source>,
        meta_expr: HIRExprMetadata<'source>,
    },
    //condition, true branch, false branch
    //this transforms elifs into else: \n\t if ..
    If(
        TExprType,
        Vec<HIR<'source, TVariableDeclType, TExprType>>,
        Vec<HIR<'source, TVariableDeclType, TExprType>>,
        HIRAstMetadata<'source>,
    ),
    //condition, body
    While(
        TExprType,
        Vec<HIR<'source, TVariableDeclType, TExprType>>,
        HIRAstMetadata<'source>,
    ),
    Return(TExprType, HIRAstMetadata<'source>),
    EmptyReturn,
}

struct IfTreeNode<'source, TDeclType, TExprType> {
    condition: TExprType,
    true_body: Vec<HIR<'source, TDeclType, TExprType>>,
    body_meta: &'source AST<'source>,
}

fn expr_to_hir_expr<'source>(expr: &'source Expr<'source>) -> HIRExpr<'source, (), NotChecked> {
    match expr {
        Expr::IntegerValue(i) => HIRExpr::Literal(LiteralHIRExpr::Integer(*i), (), expr),
        Expr::FloatValue(f) => HIRExpr::Literal(LiteralHIRExpr::Float(*f), (), expr),
        Expr::StringValue(s) => HIRExpr::Literal(LiteralHIRExpr::String(s), (), expr),
        Expr::BooleanValue(b) => HIRExpr::Literal(LiteralHIRExpr::Boolean(*b), (), expr),
        Expr::NoneExpr => HIRExpr::Literal(LiteralHIRExpr::None, (), expr),
        Expr::Variable(name) => HIRExpr::Variable(*name, (), expr),
        Expr::FunctionCall(fun_expr, args) => match &**fun_expr {
            var @ Expr::Variable(_) => HIRExpr::FunctionCall(
                expr_to_hir_expr(var).into(),
                args.iter().map(expr_to_hir_expr).collect(),
                (),
                expr,
            ),
            Expr::MemberAccess(obj, var_name) => HIRExpr::MethodCall(
                expr_to_hir_expr(obj).into(),
                *var_name,
                args.iter().map(expr_to_hir_expr).collect(),
                (),
                expr,
            ),
            _ => panic!("Cannot lower function call to HIR: not variable or member access"),
        },
        Expr::IndexAccess(object, index) => HIRExpr::MethodCall(
            Box::new(expr_to_hir_expr(object)),
            "__index__".into(),
            vec![expr_to_hir_expr(index)],
            (),
            expr,
        ),
        Expr::BinaryOperation(lhs, op, rhs) => {
            let lhs = expr_to_hir_expr(lhs);
            let rhs = expr_to_hir_expr(rhs);
            HIRExpr::BinaryOperation(lhs.into(), *op, rhs.into(), (), expr)
        }
        Expr::Parenthesized(_) => panic!("parenthesized not expected"),
        Expr::UnaryExpression(op, rhs) => {
            let rhs = expr_to_hir_expr(rhs);
            HIRExpr::UnaryExpression(*op, rhs.into(), (), expr)
        }
        Expr::MemberAccess(object, member) => {
            let object = expr_to_hir_expr(object);
            HIRExpr::MemberAccess(object.into(), *member, (), expr)
        }
        Expr::Array(items) => {
            let items = items.iter().map(expr_to_hir_expr).collect();
            HIRExpr::Array(items, (), expr)
        }
    }
}

fn ast_to_hir<'source>(ast: &'source AST<'source>, accum: &mut Vec<UninferredHIR<'source>>) {
    match ast {
        AST::Declare { var, expression } => {
            //expr: we have to decompose the expression into HIR declarations
            //because the assigned/declared expression has to be sufficiently simple
            //to decrease complexity for the other compiler phases (typechecking, type inference)

            //maybe a way to do it is by calling reduce_expr_to_hir_declarations, and the function
            //itself returns a HIRExpr. It will also add to the HIR any declarations needed
            //for the decomposition.
            let result_expr = expr_to_hir_expr(expression);

            let typedef: HIRType = (&var.name_type).into();
            let decl_hir = HIR::Declare {
                var: var.name,
                typedef,
                expression: result_expr,
                meta_expr: expression,
                meta_ast: ast,
            };

            accum.push(decl_hir);
        }
        AST::Assign { path, expression } => {
            let result_expr = expr_to_hir_expr(expression);

            let decl_hir = HIR::Assign {
                path: path.to_vec(),
                expression: result_expr,
                meta_ast: ast,
                meta_expr: expression,
            };

            accum.push(decl_hir);
        }
        AST::Return(expr) => match expr {
            None => {
                accum.push(HIR::EmptyReturn);
            }
            Some(e) => {
                let result_expr = expr_to_hir_expr(e);
                accum.push(HIR::Return(result_expr, ast));
            }
        },
        AST::StandaloneExpr(expr) => {
            let Expr::FunctionCall(_, _) = expr else {
                panic!("Can only lower function call standalone expr: {:#?}", expr);
            };

            let result_expr = expr_to_hir_expr(expr);
            let HIRExpr::FunctionCall(function, args, ..) = result_expr else {
                panic!("Lowering of function call returned invalid result: {:?}", result_expr);
            };

            let typed_args = args;

            accum.push(HIR::FunctionCall {
                function: *function,
                args: typed_args.to_vec(),
                meta_ast: ast,
                meta_expr: expr,
            });
        }
        AST::IfStatement {
            true_branch,
            elifs,
            final_else,
        } => {
            ast_if_to_hir(true_branch, accum, elifs, final_else, ast);
        }
        AST::WhileStatement { expression, body } => {
            ast_while_to_hir(expression, accum, body, ast);
        }
        AST::Intrinsic => panic!("Cannot use intrinsic keyword"),
        ast => panic!("Not implemented HIR for {:?}", ast),
    }
}

fn ast_decl_function_to_hir<'source>(
    body: &'source [AST<'source>],
    function_name: &SourceString<'source>,
    parameters: &'source [TypeBoundName<'source>],
    return_type: &Option<ASTType<'source>>,
    ast: &'source AST<'source>,
    accum: &mut Vec<StartingHIRRoot<'source>>,
) {
    //special case: single intrinsic

    if body.len() == 1 {
        if let AST::Intrinsic = body[0] {
            accum.push(HIRRoot::DeclareFunction {
                function_name: *function_name,
                parameters: create_type_bound_names(parameters),
                body: vec![],
                is_intrinsic: true,
                return_type: match return_type {
                    Some(x) => x.into(),
                    None => HIRType::Simple("Void".into()),
                },
                meta: ast,
            });
            return;
        }
    }

    let mut function_body = vec![];
    for node in body {
        ast_to_hir(node, &mut function_body);
    }

    let decl_hir: StartingHIRRoot = HIRRoot::DeclareFunction {
        function_name: *function_name,
        parameters: create_type_bound_names(parameters),
        body: function_body,
        is_intrinsic: false,
        return_type: match return_type {
            Some(x) => x.into(),
            None => HIRType::Simple("Void".into()),
        },
        meta: ast,
    };
    accum.push(decl_hir);
    //yes, each function declaration created the intermediares for their body to work, but they don't
    //escape the scope of the function!
}

fn create_type_bound_names<'source>(
    parameters: &'source [TypeBoundName<'source>],
) -> Vec<HIRTypedBoundName<'source, HIRType<'source>>> {
    parameters.iter().map(create_type_bound_name).collect()
}

fn create_type_bound_name<'source>(
    param: &'source TypeBoundName<'source>,
) -> HIRTypedBoundName<'source, HIRType<'source>> {
    HIRTypedBoundName {
        name: param.name,
        typename: (&param.name_type).into(),
    }
}

fn ast_if_to_hir<'source: 'source>(
    true_branch: &'source crate::ast::parser::ASTIfStatement<'source>,
    accum: &mut Vec<UninferredHIR<'source>>,
    elifs: &'source [crate::ast::parser::ASTIfStatement<'source>],
    final_else: &'source Option<Vec<AST<'source>>>,
    ast: &'source AST<'source>,
) {
    let true_branch_result_expr = expr_to_hir_expr(&true_branch.expression);

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
            let elif_true_branch_result_expr = expr_to_hir_expr(&item.expression);

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
                    &node.body_meta,
                ),
            };
            final_if_chain = Some(new_node);
        }
        accum.push(final_if_chain.unwrap());
    }
}

fn ast_while_to_hir<'source: 'source>(
    expression: &'source Expr<'source>,
    accum: &mut Vec<UninferredHIR<'source>>,
    body: &'source [AST<'source>],
    ast: &'source AST<'source>,
) {
    let expr = expr_to_hir_expr(expression);

    let mut true_body_hir = vec![];
    for node in body {
        ast_to_hir(node, &mut true_body_hir);
    }

    accum.push(HIR::While(expr, true_body_hir, ast))
}

pub fn ast_globals_to_hir<'source>(ast: &'source AST<'_>) -> Vec<StartingHIRRoot<'source>> {
    let mut accum = vec![];
    match ast {
        AST::DeclareFunction {
            function_name,
            parameters,
            body,
            return_type,
        } => {
            ast_decl_function_to_hir(
                body,
                function_name,
                parameters,
                return_type,
                ast,
                &mut accum,
            );
        }
        AST::Root(ast_nodes) => {
            for node in ast_nodes {
                accum.extend(ast_globals_to_hir(node));
            }
        }
        AST::StructDeclaration {
            struct_name,
            type_parameters,
            body,
        } => {
            let fields = create_type_bound_names(body);
            accum.push(HIRRoot::StructDeclaration {
                struct_name,
                fields,
                type_parameters: type_parameters
                    .iter()
                    .map(|x| GenericParameter(*x))
                    .collect(),
                meta: ast,
            });
        }
        other => panic!("AST not supported: {other:?}"),
    }
    accum
}

#[cfg(test)]
mod tests {

    use super::*;

    use crate::ast::parser::parse_ast;
    use crate::semantic::hir;
    use crate::semantic::hir_printer::print_hir;

    use crate::types::type_instance_db::TypeInstanceManager;

    //Parses a single expression
    fn parse<'source>(source: &str) -> AST<'_> {
        let tokens = crate::ast::lexer::tokenize(source);
        //println!("Tokens: {:?}", tokens);
        let ast = parse_ast(tokens.unwrap());
        return AST::Root(ast);
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
        let result = hir::ast_globals_to_hir(&parsed);
        let result = print_hir(&result, &TypeInstanceManager::new());
        println!("{}", result);

        let expected = "
def main(args: UNRESOLVED List<UNRESOLVED! String>) -> UNRESOLVED! Void:
    minus : UNRESOLVED! i32 = -my_function(99, 999)
    numbers = [1, 2, -3, minus]
    r1 = my_function(1, 2)
    r2 = my_function2(3, 4)
    r3 = my_function(numbers.__index__(1), numbers.__index__(2))
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
    if args.__index__(0) == 1:
        print(10)
    else:
        print(20)";

        let result = hir::ast_globals_to_hir(&parsed);
        let result = print_hir(&result, &TypeInstanceManager::new());
        println!("{}", result);

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
    arg = args.__index__(0)
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

        let result = hir::ast_globals_to_hir(&parsed);
        let final_result = print_hir(&result, &TypeInstanceManager::new());
        println!("{}", final_result);

        assert_eq!(expected.trim(), final_result.trim());
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
        let result = hir::ast_globals_to_hir(&parsed);

        let result = print_hir(&result, &TypeInstanceManager::new());

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

        let result = hir::ast_globals_to_hir(&parsed);
        let result = print_hir(&result, &TypeInstanceManager::new());
        println!("{}", result);
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
}
