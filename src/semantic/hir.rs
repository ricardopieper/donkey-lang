
use std::fmt::Display;
use std::fmt::Write;

use crate::ast::lexer::Operator;
use crate::ast::parser::{ASTType, Expr, AST};
use crate::commons::float::FloatLiteral;
use crate::types::type_instance_db::TypeInstanceId;


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TrivialHIRExpr {
    IntegerValue(i128),
    FloatValue(FloatLiteral),
    StringValue(String),
    BooleanValue(bool),
    Variable(String),
    None,
}

/**
 * The HIR is great to work with. It simplifies a ton of stuff, reduces the tree depth, reduces recursion,
 * it's overall a good thing IMO. However, it loses some data. Actually, more data is created, more
 * intermediate steps and more structure is added. What happens is that, during type checking,
 * some checks can fail on compiler-generated data, like intermediates. The error message is thus entirely unusable.
 *
 * Therefore, we will add some metadata to the HIR nodes referring to the raw AST and expr nodes. They will carry the context in which
 * the compiler was operating that is closer to the user, and if anything on that context fails, then that is used in the error messages.
 */
pub type HIRExprMetadata = Option<Expr>;
pub type HIRAstMetadata = Option<AST>;


pub type StartingHIR = HIR<HIRType, HIRExpr<()>>;
pub type FirstAssignmentsDeclaredHIR = HIR<HIRTypeDef, HIRExpr<()>>;
pub type GlobalsInferredMIR = HIR<HIRType, HIRExpr<()>>;
pub type InferredTypeHIR = HIR<TypeInstanceId,  HIRExpr<TypeInstanceId>>;


pub type StartingHIRRoot = HIRRoot<HIRType, StartingHIR>;
pub type FirstAssignmentsDeclaredHIRRoot = HIRRoot<TypeInstanceId, FirstAssignmentsDeclaredHIR>;
pub type GlobalsInferredMIRRoot = HIRRoot<TypeInstanceId, GlobalsInferredMIR>;
pub type InferredTypeHIRRoot = HIRRoot<TypeInstanceId, InferredTypeHIR>;


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRTypeDef {
    PendingInference,
    Provided(HIRType)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRExpr<TExprType> {
    Trivial(TrivialHIRExpr, TExprType, HIRExprMetadata),
    #[allow(dead_code)]
    Cast(Box<HIRExpr<TExprType>>, TExprType, HIRExprMetadata),
    BinaryOperation(
        Box<HIRExpr<TExprType>>,
        Operator,
        Box<HIRExpr<TExprType>>,
        TExprType,
        HIRExprMetadata,
    ),
    //obj_expr, method_name, args:type, return type, metadata
    MethodCall(
        Box<HIRExpr<TExprType>>,
        String,
        Vec<HIRExpr<TExprType>>,
        TExprType,
        HIRExprMetadata,
    ),
    //func_expr, args:type, return type, metadata
    FunctionCall(Box<HIRExpr<TExprType>>, Vec<HIRExpr<TExprType>>, TExprType, HIRExprMetadata),
    UnaryExpression(Operator, Box<HIRExpr<TExprType>>, TExprType, HIRExprMetadata),
    //obj, field, result_type, metadata
    MemberAccess(Box<HIRExpr<TExprType>>, String, TExprType, HIRExprMetadata),
    Array(Vec<HIRExpr<TExprType>>, TExprType, HIRExprMetadata),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRType {
    Simple(String),
    Generic(String, Vec<HIRType>),
    //Function(Vec<HIRType>, Box<HIRType>),
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

impl HIRExpr<TypeInstanceId> {
    pub fn get_type(&self) -> TypeInstanceId {
        *match self {
            HIRExpr::Trivial(.., t, _)
            | HIRExpr::Cast(.., t, _)
            | HIRExpr::BinaryOperation(.., t, _)
            | HIRExpr::FunctionCall(.., t, _)
            | HIRExpr::UnaryExpression(.., t, _)
            | HIRExpr::MemberAccess(.., t, _)
            | HIRExpr::Array(.., t, _)
            | HIRExpr::MethodCall(.., t, _) => t,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HIRTypedBoundName<TExprType> {
    pub name: String,
    pub typename: TExprType,
}



//we need to be able to represent complex stuff,
//like a function that receives a function, whose parameters are generic
//def func(another_func: Function<List<String>>)

//so we can store TypeIds, but we need it to be accompanied by more data depending on the kind of the type,
//types such as functions and generics need to be "instanced"

impl HIRType {
    fn from_ast(typ: &ASTType) -> Self {
        match typ {
            ASTType::Simple(name) => Self::Simple(name.clone()),
            ASTType::Generic(name, generics) => {
                let hir_generics = generics.iter().map(Self::from_ast).collect::<Vec<_>>();
                HIRType::Generic(name.clone(), hir_generics)
            }
        }
    }
}

/*
The HIR expression is similar to the AST, but has type information on every node.
*/

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRRoot<TGlobalTypes, TBodyType> {
    DeclareFunction {
        function_name: String,
        parameters: Vec<HIRTypedBoundName<TGlobalTypes>>,
        body: Vec<TBodyType>,
        return_type: TGlobalTypes,
        meta: HIRAstMetadata,
    },
    StructDeclaration {
        struct_name: String,
        body: Vec<HIRTypedBoundName<TGlobalTypes>>,
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
        meta: HIRAstMetadata,
    },
    //condition, true branch, false branch
    //this transforms elifs into else: \n\t if ..
    If(TExprType, Vec<HIR<TVariableDeclType, TExprType>>, Vec<HIR<TVariableDeclType, TExprType>>, HIRAstMetadata),
    Return(TExprType, HIRAstMetadata),
    EmptyReturn,
}

struct IfTreeNode<TDeclType, TExprType> {
    condition: TExprType,
    true_body: Vec<HIR<TDeclType, TExprType>>,
    body_meta: AST,
}

pub fn expr_to_hir_expr(expr: &Expr) -> HIRExpr<()> {
    match expr {
        Expr::IntegerValue(i) => HIRExpr::Trivial(
            TrivialHIRExpr::IntegerValue(*i),
            (),
            expr.clone().into(),
        ),
        Expr::FloatValue(f) => HIRExpr::Trivial(
            TrivialHIRExpr::FloatValue(*f),
            (),
            expr.clone().into(),
        ),
        Expr::StringValue(s) => HIRExpr::Trivial(
            TrivialHIRExpr::StringValue(s.clone()),
            (),
            expr.clone().into(),
        ),
        Expr::BooleanValue(b) => HIRExpr::Trivial(
            TrivialHIRExpr::BooleanValue(*b),
            (),
            expr.clone().into(),
        ),
        Expr::None => HIRExpr::Trivial(
            TrivialHIRExpr::None,
            (),
            expr.clone().into(),
        ),
        Expr::Variable(name) => HIRExpr::Trivial(
            TrivialHIRExpr::Variable(name.clone()),
            (),
            expr.clone().into(),
        ),
        Expr::FunctionCall(fun_expr, args) => match &**fun_expr {
            var @ Expr::Variable(_) => HIRExpr::FunctionCall(
                expr_to_hir_expr(var).into(),
                args.iter().map(expr_to_hir_expr).collect(),
                (),
                expr.clone().into(),
            ),
            Expr::MemberAccess(obj, var_name) => HIRExpr::MethodCall(
                expr_to_hir_expr(obj).into(),
                var_name.clone(),
                args.iter().map(expr_to_hir_expr).collect(),
                (),
                expr.clone().into(),
            ),
            _ => panic!("Cannot lower function call to HIR: not variable or member access"),
        },
        Expr::IndexAccess(object, index) => {
           HIRExpr::MethodCall(
                Box::new(expr_to_hir_expr(object)),
                "__index__".into(),
                vec![
                    expr_to_hir_expr(index)
                ],
                (),
                Some(expr.clone()))
        }
        Expr::BinaryOperation(lhs, op, rhs) => {
            let lhs = expr_to_hir_expr(lhs);
            let rhs = expr_to_hir_expr(rhs);
            HIRExpr::BinaryOperation(
                lhs.into(),
                *op,
                rhs.into(),
                (),
                expr.clone().into(),
            )
        }
        Expr::Parenthesized(_) => panic!("parenthesized not expected"),
        Expr::UnaryExpression(op, rhs) => {
            let rhs = expr_to_hir_expr(rhs);
            HIRExpr::UnaryExpression(
                *op,
                rhs.into(),
                (),
                expr.clone().into(),
            )
        }
        Expr::MemberAccess(object, member) => {
            let object = expr_to_hir_expr(object);
            HIRExpr::MemberAccess(
                object.into(),
                member.clone(),
                (),
                expr.clone().into(),
            )
        }
        Expr::Array(items) => {
            let items = items.iter().map(expr_to_hir_expr).collect();
            HIRExpr::Array(items, (), expr.clone().into())
        }
    }
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
        AST::StructDeclaration { struct_name, body } => {
            let fields = body.iter().map(|field| HIRTypedBoundName {
                name: field.name.clone(),
                typename: HIRType::from_ast(&field.name_type),
            });
            accum.push(HIRRoot::StructDeclaration {
                struct_name: struct_name.clone(),
                body: fields.collect(),
                meta: Some(ast.clone()),
            });
        }
        other => panic!("Root AST not supported: {other:?}")
    }
}


pub fn ast_to_hir(ast: &AST, accum: &mut Vec<StartingHIR>) {
    match ast {
        AST::Declare { var, expression } => {
            //expr: we have to decompose the expression into HIR declarations
            //because the assigned/declared expression has to be sufficiently simple
            //to decrease complexity for the other compiler phases (typechecking, type inference)

            //maybe a way to do it is by calling reduce_expr_to_hir_declarations, and the function
            //itself returns a HIRExpr. It will also add to the HIR any declarations needed
            //for the decomposition.
            let result_expr = expr_to_hir_expr(expression);

            let decl_hir = HIR::Declare {
                var: var.name.clone(),
                typedef: HIRType::from_ast(&var.name_type),
                expression: result_expr,
                meta_expr: Some(expression.clone()),
                meta_ast: Some(ast.clone()),
            };

            accum.push(decl_hir);
        }
        AST::Assign { path, expression } => {
            let result_expr = expr_to_hir_expr(expression);

            let decl_hir = HIR::Assign {
                path: path.clone(),
                expression: result_expr,
                meta_ast: Some(ast.clone()),
                meta_expr: Some(expression.clone()),
            };

            accum.push(decl_hir);
        }    
        AST::Return(expr) => match expr {
            None => {
                accum.push(HIR::EmptyReturn);
            }
            Some(e) => {
                let result_expr = expr_to_hir_expr(e);
                accum.push(HIR::Return(
                    result_expr,
                    Some(ast.clone()),
                ));
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
                meta: Some(ast.clone()),
            });
        }
        AST::IfStatement {
            true_branch,
            elifs,
            final_else,
        } => {
            ast_if_to_hir(true_branch, accum, elifs, final_else, ast);
        }
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
    let mut function_body = vec![];
    for node in body {
        ast_to_hir(node, &mut function_body);
    }
    let decl_hir = HIRRoot::DeclareFunction {
        function_name: function_name.to_string(),
        parameters: parameters
            .iter()
            .map(|param| {
                let name = param.name.clone();
                HIRTypedBoundName {
                    name,
                    typename: HIRType::from_ast(&param.name_type),
                }
            })
            .collect(),
        body: function_body,
        return_type: match return_type {
            Some(x) =>HIRType::from_ast(x),
            None =>HIRType::Simple("Void".into())
        },
        meta: Some(ast.clone()),
    };
    accum.push(decl_hir);
    //yes, each function declaration created the intermediares for their body to work, but they don't
    //escape the scope of the function!
}

fn ast_if_to_hir(
    true_branch: &crate::ast::parser::ASTIfStatement,
    accum: &mut Vec<StartingHIR>,
    elifs: &[crate::ast::parser::ASTIfStatement],
    final_else: &Option<Vec<AST>>,
    ast: &AST,
) -> i32 {
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
        /* Just like function declarations, the intermediaries created here don't escape the context.
           This is different than python, where all variables declared are scoped to the entire function;
        */

        accum.push(HIR::If(
            true_branch_result_expr,
            true_body_hir,
            vec![],
            Some(ast.clone()),
        ));

        0
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
                    Some(ast.clone()),
                ));
            }
        }
        0
    } else {
        //in this case we have elifs, so we build the "tree"
        //and we don't actually need to store the false body because we'll connect everything later.
        //it's not actually a tree... it's more like a linked list.

        //let mut current_if_tree = HIR::If(trivial_true_branch_expr, true_body_hir, ());
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

        //trivialexpr, vec<hir>, vec<hir>
        let mut final_if_chain = None;

        if !final_else_body.is_empty() {
            let first_node = nodes.pop().unwrap(); //there MUST be a node here
            final_if_chain = Some(HIR::If(
                first_node.condition,
                first_node.true_body,
                final_else_body,
                (*ast).clone().into(),
            ));
        }

        nodes.reverse();
        //we navigate through the nodes in reverse and build the final HIR tree
        for node in nodes {
            let new_node = match final_if_chain {
                None => HIR::If(node.condition, node.true_body, vec![], Some(node.body_meta)),
                Some(current_chain) => HIR::If(
                    node.condition,
                    node.true_body,
                    vec![current_chain],
                    Some(node.body_meta),
                ),
            };
            final_if_chain = Some(new_node);
        }
        accum.push(final_if_chain.unwrap());

        0
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
