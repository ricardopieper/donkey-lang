use std::fmt::Display;
use std::fmt::Write;

use crate::ast::lexer::Operator;
use crate::ast::parser::TypeBoundName;
use crate::ast::parser::{ASTType, Expr, AST};
use crate::commons::float::FloatLiteral;
use crate::types::type_constructor_db::GenericParameter;
use crate::types::type_constructor_db::TypeUsage;
use crate::types::type_instance_db::TypeInstanceId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LiteralHIRExpr {
    Integer(i128),
    Float(FloatLiteral),
    String(String),
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
pub type HIRExprMetadata = Expr;
pub type HIRAstMetadata = AST;

/**
 * Instead of having just one HIR representation, we have many different representations that are appropriate for each step.
 * For instance, in the type checker, we receive a fully type-inferred HIR instead of receiving one that potentially has
 * types not yet inferred. With generics, we ensure that this is not the case. In fact I had actual problems during development
 * with unexpected uninferred types, and this eliminated them using the Rust type system.
 *
 * HIR is for the function *bodies*, while `HIRRoot` is for function declarations, struct declarations, etc.
 */

//The HIR right after AST transformation, no types. HIRExpr is () because the type certainly is unknown.
pub type UninferredHIR = HIR<HIRType, HIRExpr<()>>;

//The HIR after expanding variable assignments into declarations the first time they're assigned. In this case
//we create them as PendingInference. We also wrap given type declarations in a HIRTypeDef::Provided.
pub type FirstAssignmentsDeclaredHIR = HIR<HIRTypeDef, HIRExpr<()>>;

//The HIR with types inferred already, ready for typechecking and MIR lowering
pub type InferredTypeHIR = HIR<TypeInstanceId, HIRExpr<TypeInstanceId>>;

//HIR roots right after AST transformation
pub type StartingHIRRoot = HIRRoot<HIRType, UninferredHIR, HIRType>;
//HIR roots after inferring and registering globals, bodies have not changed
pub type GlobalsInferredMIRRoot = HIRRoot<TypeInstanceId, UninferredHIR, TypeUsage>;
//HIR roots now with body changed, first assignments became declarations
pub type FirstAssignmentsDeclaredHIRRoot =
    HIRRoot<TypeInstanceId, FirstAssignmentsDeclaredHIR, TypeUsage>;
//HIR roots with bodies fully inferred
pub type InferredTypeHIRRoot = HIRRoot<TypeInstanceId, InferredTypeHIR, TypeUsage>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRTypeDef {
    PendingInference,
    Provided(HIRType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NotChecked;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Checked;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRExpr<TExprType, TTypechecked = NotChecked> {
    Literal(LiteralHIRExpr, TExprType, HIRExprMetadata),
    Variable(String, TExprType, HIRExprMetadata),
    #[allow(dead_code)]
    Cast(
        Box<HIRExpr<TExprType, TTypechecked>>,
        TExprType,
        HIRExprMetadata,
    ),
    BinaryOperation(
        Box<HIRExpr<TExprType, TTypechecked>>,
        Operator,
        Box<HIRExpr<TExprType, TTypechecked>>,
        TExprType,
        HIRExprMetadata,
    ),
    //obj_expr, method_name, args:type, return type, metadata
    MethodCall(
        Box<HIRExpr<TExprType, TTypechecked>>,
        String,
        Vec<HIRExpr<TExprType, TTypechecked>>,
        TExprType,
        HIRExprMetadata,
    ),
    //func_expr, args:type, return type, metadata
    FunctionCall(
        Box<HIRExpr<TExprType, TTypechecked>>,
        Vec<HIRExpr<TExprType, TTypechecked>>,
        TExprType,
        HIRExprMetadata,
    ),
    UnaryExpression(
        Operator,
        Box<HIRExpr<TExprType, TTypechecked>>,
        TExprType,
        HIRExprMetadata,
    ),
    //obj, field, result_type, metadata
    MemberAccess(
        Box<HIRExpr<TExprType, TTypechecked>>,
        String,
        TExprType,
        HIRExprMetadata,
    ),
    Array(
        Vec<HIRExpr<TExprType, TTypechecked>>,
        TExprType,
        HIRExprMetadata,
    ),
    #[allow(dead_code)]
    TypecheckTag(TTypechecked),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRType {
    Simple(String),
    Generic(String, Vec<HIRType>),
    //Function(Vec<HIRType>, Box<HIRType>),
}

//we need to be able to represent complex stuff,
//like a function that receives a function, whose parameters are generic
//def func(another_func: Function<List<String>>)

//so we can store TypeIds, but we need it to be accompanied by more data depending on the kind of the type,
//types such as functions and generics need to be "instanced"

impl From<&ASTType> for HIRType {
    fn from(typ: &ASTType) -> Self {
        match typ {
            ASTType::Simple(name) => Self::Simple(name.clone()),
            ASTType::Generic(name, generics) => {
                let hir_generics = generics.iter().map(Self::from).collect::<Vec<_>>();
                HIRType::Generic(name.clone(), hir_generics)
            }
        }
    }
}

impl Display for HIRType {
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

impl<T> HIRExpr<TypeInstanceId, T> {
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
pub struct HIRTypedBoundName<TExprType> {
    pub name: String,
    pub typename: TExprType,
}

/*
The HIR expression is similar to the AST, but can have type information on every node.
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRRoot<TGlobalTypes, TBodyType, TStructFieldsType> {
    DeclareFunction {
        function_name: String,
        parameters: Vec<HIRTypedBoundName<TGlobalTypes>>,
        body: Vec<TBodyType>,
        return_type: TGlobalTypes,
        meta: HIRAstMetadata,
        is_intrinsic: bool,
    },
    StructDeclaration {
        struct_name: String,
        type_parameters: Vec<GenericParameter>,
        fields: Vec<HIRTypedBoundName<TStructFieldsType>>,
        meta: HIRAstMetadata,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIR<TVariableDeclType, TExprType> {
    Assign {
        path: Vec<String>,
        expression: TExprType,
        meta_ast: HIRAstMetadata,
        meta_expr: HIRExprMetadata,
    },
    Declare {
        var: String,
        typedef: TVariableDeclType,
        expression: TExprType,
        meta_ast: HIRAstMetadata,
        meta_expr: HIRExprMetadata,
    },
    FunctionCall {
        function: TExprType,
        args: Vec<TExprType>,
        meta_ast: HIRAstMetadata,
        meta_expr: HIRExprMetadata,
    },
    //condition, true branch, false branch
    //this transforms elifs into else: \n\t if ..
    If(
        TExprType,
        Vec<HIR<TVariableDeclType, TExprType>>,
        Vec<HIR<TVariableDeclType, TExprType>>,
        HIRAstMetadata,
    ),
    //condition, body
    While(
        TExprType,
        Vec<HIR<TVariableDeclType, TExprType>>,
        HIRAstMetadata,
    ),
    Return(TExprType, HIRAstMetadata),
    EmptyReturn,
}

struct IfTreeNode<TDeclType, TExprType> {
    condition: TExprType,
    true_body: Vec<HIR<TDeclType, TExprType>>,
    body_meta: AST,
}

fn expr_to_hir_expr(expr: &Expr) -> HIRExpr<()> {
    match expr {
        Expr::IntegerValue(i) => HIRExpr::Literal(LiteralHIRExpr::Integer(*i), (), expr.clone()),
        Expr::FloatValue(f) => HIRExpr::Literal(LiteralHIRExpr::Float(*f), (), expr.clone()),
        Expr::StringValue(s) => {
            HIRExpr::Literal(LiteralHIRExpr::String(s.clone()), (), expr.clone())
        }
        Expr::BooleanValue(b) => HIRExpr::Literal(LiteralHIRExpr::Boolean(*b), (), expr.clone()),
        Expr::None => HIRExpr::Literal(LiteralHIRExpr::None, (), expr.clone()),
        Expr::Variable(name) => HIRExpr::Variable(name.clone(), (), expr.clone()),
        Expr::FunctionCall(fun_expr, args) => match &**fun_expr {
            var @ Expr::Variable(_) => HIRExpr::FunctionCall(
                expr_to_hir_expr(var).into(),
                args.iter().map(expr_to_hir_expr).collect(),
                (),
                expr.clone(),
            ),
            Expr::MemberAccess(obj, var_name) => HIRExpr::MethodCall(
                expr_to_hir_expr(obj).into(),
                var_name.clone(),
                args.iter().map(expr_to_hir_expr).collect(),
                (),
                expr.clone(),
            ),
            _ => panic!("Cannot lower function call to HIR: not variable or member access"),
        },
        Expr::IndexAccess(object, index) => HIRExpr::MethodCall(
            Box::new(expr_to_hir_expr(object)),
            "__index__".into(),
            vec![expr_to_hir_expr(index)],
            (),
            expr.clone(),
        ),
        Expr::BinaryOperation(lhs, op, rhs) => {
            let lhs = expr_to_hir_expr(lhs);
            let rhs = expr_to_hir_expr(rhs);
            HIRExpr::BinaryOperation(lhs.into(), *op, rhs.into(), (), expr.clone())
        }
        Expr::Parenthesized(_) => panic!("parenthesized not expected"),
        Expr::UnaryExpression(op, rhs) => {
            let rhs = expr_to_hir_expr(rhs);
            HIRExpr::UnaryExpression(*op, rhs.into(), (), expr.clone())
        }
        Expr::MemberAccess(object, member) => {
            let object = expr_to_hir_expr(object);
            HIRExpr::MemberAccess(object.into(), member.clone(), (), expr.clone())
        }
        Expr::Array(items) => {
            let items = items.iter().map(expr_to_hir_expr).collect();
            HIRExpr::Array(items, (), expr.clone())
        }
    }
}

fn ast_to_hir(ast: &AST, accum: &mut Vec<UninferredHIR>) {
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
                var: var.name.clone(),
                typedef,
                expression: result_expr,
                meta_expr: expression.clone(),
                meta_ast: ast.clone(),
            };

            accum.push(decl_hir);
        }
        AST::Assign { path, expression } => {
            let result_expr = expr_to_hir_expr(expression);

            let decl_hir = HIR::Assign {
                path: path.clone(),
                expression: result_expr,
                meta_ast: ast.clone(),
                meta_expr: expression.clone(),
            };

            accum.push(decl_hir);
        }
        AST::Return(expr) => match expr {
            None => {
                accum.push(HIR::EmptyReturn);
            }
            Some(e) => {
                let result_expr = expr_to_hir_expr(e);
                accum.push(HIR::Return(result_expr, ast.clone()));
            }
        },
        AST::StandaloneExpr(expr) => {
            let Expr::FunctionCall(_, _) = expr else {
                panic!("Can only lower function call standalone expr: {:#?}", expr);
            };

            let result_expr = expr_to_hir_expr(expr);
            let HIRExpr::FunctionCall(function, args, ..) = &result_expr else {
                panic!("Lowering of function call returned invalid result: {:?}", result_expr);
            };

            let typed_args = args.clone();

            accum.push(HIR::FunctionCall {
                function: *function.clone(),
                args: typed_args,
                meta_ast: ast.clone(),
                meta_expr: expr.clone(),
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

fn ast_decl_function_to_hir(
    body: &[AST],
    function_name: &str,
    parameters: &[crate::ast::parser::TypeBoundName],
    return_type: &Option<ASTType>,
    ast: &AST,
    accum: &mut Vec<StartingHIRRoot>,
) {
    //special case: single intrinsic

    if body.len() == 1 {
        if let AST::Intrinsic = body[0] {
            accum.push(HIRRoot::DeclareFunction {
                function_name: function_name.to_string(),
                parameters: create_type_bound_names(parameters),
                body: vec![],
                is_intrinsic: true,
                return_type: match return_type {
                    Some(x) => x.into(),
                    None => HIRType::Simple("Void".into()),
                },
                meta: ast.clone(),
            });
            return;
        }
    }

    let mut function_body = vec![];
    for node in body {
        ast_to_hir(node, &mut function_body);
    }

    let decl_hir: StartingHIRRoot = HIRRoot::DeclareFunction {
        function_name: function_name.to_string(),
        parameters: create_type_bound_names(parameters),
        body: function_body,
        is_intrinsic: false,
        return_type: match return_type {
            Some(x) => x.into(),
            None => HIRType::Simple("Void".into()),
        },
        meta: ast.clone(),
    };
    accum.push(decl_hir);
    //yes, each function declaration created the intermediares for their body to work, but they don't
    //escape the scope of the function!
}

fn create_type_bound_names(parameters: &[TypeBoundName]) -> Vec<HIRTypedBoundName<HIRType>> {
    parameters.iter().map(create_type_bound_name).collect()
}

fn create_type_bound_name(param: &TypeBoundName) -> HIRTypedBoundName<HIRType> {
    HIRTypedBoundName {
        name: param.name.clone(),
        typename: (&param.name_type).into(),
    }
}

fn ast_if_to_hir(
    true_branch: &crate::ast::parser::ASTIfStatement,
    accum: &mut Vec<UninferredHIR>,
    elifs: &[crate::ast::parser::ASTIfStatement],
    final_else: &Option<Vec<AST>>,
    ast: &AST,
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
        accum.push(HIR::If(
            true_branch_result_expr,
            true_body_hir,
            vec![],
            ast.clone(),
        ));
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
                    ast.clone(),
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
            body_meta: AST::Root(true_branch.statements.clone()),
        };

        nodes.push(root_node);

        for item in elifs {
            let elif_true_branch_result_expr = expr_to_hir_expr(&item.expression);

            let mut if_node = IfTreeNode {
                condition: elif_true_branch_result_expr.clone(),
                true_body: vec![],
                body_meta: AST::Root(item.statements.clone()),
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
                (*ast).clone(),
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

fn ast_while_to_hir(expression: &Expr, accum: &mut Vec<UninferredHIR>, body: &[AST], ast: &AST) {
    let expr = expr_to_hir_expr(expression);

    let mut true_body_hir = vec![];
    for node in body {
        ast_to_hir(node, &mut true_body_hir);
    }

    accum.push(HIR::While(expr, true_body_hir, ast.clone()))
}

pub fn ast_globals_to_hir(ast: &AST, accum: &mut Vec<StartingHIRRoot>) {
    match ast {
        AST::DeclareFunction {
            function_name,
            parameters,
            body,
            return_type,
        } => {
            ast_decl_function_to_hir(body, function_name, parameters, return_type, ast, accum);
        }
        AST::Root(ast_nodes) => {
            for node in ast_nodes {
                ast_globals_to_hir(node, accum);
            }
        }
        AST::StructDeclaration {
            struct_name,
            type_parameters,
            body,
        } => {
            let fields = create_type_bound_names(body);
            accum.push(HIRRoot::StructDeclaration {
                struct_name: struct_name.clone(),
                fields,
                type_parameters: type_parameters
                    .iter()
                    .map(|x| GenericParameter(x.to_string()))
                    .collect(),
                meta: ast.clone(),
            });
        }
        other => panic!("AST not supported: {other:?}"),
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    use crate::semantic::hir;
    use crate::semantic::hir_printer::print_hir;

    use crate::types::type_instance_db::TypeInstanceManager;

    //Parses a single expression
    fn parse(source: &str) -> Vec<StartingHIRRoot> {
        let tokens = crate::ast::lexer::tokenize(source);
        //println!("Tokens: {:?}", tokens);
        let ast = crate::ast::parser::parse_ast(tokens.unwrap());

        let root = crate::ast::parser::AST::Root(ast);
        let mut result = vec![];
        hir::ast_globals_to_hir(&root, &mut result);
        result
    }

    #[test]
    fn complex_code() {
        let result = parse(
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

        let result = print_hir(&parsed, &TypeInstanceManager::new());
        println!("{}", result);

        assert_eq!(expected.trim(), result.trim());
    }

    #[test]
    fn if_chain() {
        let result = parse(
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
        let result = print_hir(&parsed, &TypeInstanceManager::new());

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

        let result = print_hir(&parsed, &TypeInstanceManager::new());
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
