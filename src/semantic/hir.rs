use std::fmt::Display;
use std::fmt::Write;

use crate::ast::lexer::Operator;
use crate::ast::parser::{ASTType, Expr, AST};
use crate::commons::float::FloatLiteral;
use crate::types::type_db::TypeInstance;


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TrivialHIRExpr {
    IntegerValue(i128),
    FloatValue(FloatLiteral),
    StringValue(String),
    BooleanValue(bool),
    Variable(String),
    None,
}

impl TrivialHIRExpr {
    pub fn pending_type(&self) -> HIRExpr {
        HIRExpr::Trivial(self.clone(), HIRTypeDef::PendingInference, None)
    }
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



#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRExpr {
    Trivial(TrivialHIRExpr, HIRTypeDef, HIRExprMetadata),
    #[allow(dead_code)] Cast(Box<HIRExpr>, HIRTypeDef, HIRExprMetadata),
    BinaryOperation(
        Box<HIRExpr>,
        Operator,
        Box<HIRExpr>,
        HIRTypeDef,
        HIRExprMetadata,
    ),
    //func_expr, args:type, return type, metadata
    FunctionCall(
        Box<HIRExpr>,
        Vec<HIRExpr>,
        HIRTypeDef,
        HIRExprMetadata,
    ),
    UnaryExpression(Operator, Box<HIRExpr>, HIRTypeDef, HIRExprMetadata),
    //obj, field, result_type, metadata
    MemberAccess(Box<HIRExpr>, String, HIRTypeDef, HIRExprMetadata),
    Array(Vec<HIRExpr>, HIRTypeDef, HIRExprMetadata),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRType {
    Simple(String),
    Generic(String, Vec<HIRType>),
    Function(Vec<HIRType>, Box<HIRType>),
}

impl Display for HIRType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _str = String::new();

        match self {
            HIRType::Simple(s) => f.write_str(s),
            HIRType::Generic(s, generics) => {
                let comma_sep = generics
                    .iter()
                    .map(HIRType::to_string)
                    .collect::<Vec<String>>()
                    .join(", ");

                f.write_str(s).unwrap();
                f.write_char('<').unwrap();
                f.write_str(&comma_sep).unwrap();
                f.write_char('>')
            }
            HIRType::Function(arg_types, return_type) => {
                let comma_sep_args = arg_types
                    .iter()
                    .map(HIRType::to_string)
                    .collect::<Vec<String>>()
                    .join(", ");

                let return_type = return_type.to_string();

                f.write_str("fn(").unwrap();
                f.write_str(&comma_sep_args).unwrap();
                f.write_str(") -> ").unwrap();
                f.write_str(&return_type)
            }
        }
    }
}

impl HIRExpr {
    pub fn get_expr_type(&self) -> &HIRTypeDef {
        match self {
            HIRExpr::Trivial(.., t, _)
            | HIRExpr::Cast(.., t, _)
            | HIRExpr::BinaryOperation(.., t, _)
            | HIRExpr::FunctionCall(.., t, _)
            | HIRExpr::UnaryExpression(.., t, _)
            | HIRExpr::MemberAccess(.., t, _)
            | HIRExpr::Array(.., t, _) => t,
        }
    }

    pub fn expect_resolved(&self) -> &TypeInstance {
        let expr_type = self.get_expr_type();
        expr_type.expect_resolved()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HIRTypedBoundName {
    pub name: String,
    pub typename: HIRTypeDef, //var name, type
}

//we need to be able to represent complex stuff,
//like a function that receives a function, whose parameters are generic
//def func(another_func: Function<List<String>>)

//so we can store TypeIds, but we need it to be accompanied by more data depending on the kind of the type,
//types such as functions and generics need to be "instanced"
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRTypeDef {
    PendingInference,
    Unresolved(HIRType),
    Resolved(TypeInstance),
}

impl HIRTypeDef {
    pub fn expect_unresolved(&self) -> HIRType {
        match self {
            HIRTypeDef::PendingInference => panic!("Function parameters must have a type"),
            HIRTypeDef::Unresolved(e) => e.clone(),
            HIRTypeDef::Resolved(_) => {
                panic!("Cannot deal with resolved types at this point, this is a bug")
            }
        }
    }

    pub fn get_type(&self) -> Option<&TypeInstance> {
        match self {
            HIRTypeDef::PendingInference => None,
            HIRTypeDef::Unresolved(_) => None,
            HIRTypeDef::Resolved(r) => Some(r),
        }
    }
   
    pub fn expect_resolved(&self) -> &TypeInstance {
        match self {
            HIRTypeDef::PendingInference => panic!("Expected resolved type, but is Pending"),
            HIRTypeDef::Unresolved(e) => {
                panic!("Expected resolved type, but is Unresolved {:?}", e)
            }
            HIRTypeDef::Resolved(r) => r,
        }
    }
}

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

#[derive(Debug, Clone, PartialEq, Eq)]
/*
The HIR expression is similar to the AST, but has type information on every node.
*/
pub enum HIR {
    Assign {
        path: Vec<String>,
        expression: HIRExpr,
        meta_ast: HIRAstMetadata,
        meta_expr: HIRExprMetadata,
    },
    Declare {
        var: String,
        typedef: HIRTypeDef,
        expression: HIRExpr,
        meta_ast: HIRAstMetadata,
        meta_expr: HIRExprMetadata,
    },
    DeclareFunction {
        function_name: String,
        parameters: Vec<HIRTypedBoundName>,
        body: Vec<HIR>,
        return_type: HIRTypeDef,
        meta: HIRAstMetadata,
    },
    StructDeclaration {
        struct_name: String,
        body: Vec<HIRTypedBoundName>,
        meta: HIRAstMetadata,
    },
    FunctionCall {
        function: HIRExpr,
        args: Vec<HIRExpr>,
        meta: HIRAstMetadata,
    },
    //condition, true branch, false branch
    //this transforms elifs into else: \n\t if ..
    If(HIRExpr, Vec<HIR>, Vec<HIR>, HIRAstMetadata),
    Return(HIRExpr, HIRTypeDef, HIRAstMetadata),
    EmptyReturn,
}

fn make_intermediary(intermediary: i32) -> String {
    format!("${}", intermediary)
}

//an expression is trivial when it needs basically no effort to
//check its type. You shouldn't recurse anymore on the expr tree
fn get_trivial_hir_expr(expr: &Expr) -> Option<TrivialHIRExpr> {
    match expr {
        Expr::IntegerValue(i) => Some(TrivialHIRExpr::IntegerValue(*i)),
        Expr::FloatValue(f) => Some(TrivialHIRExpr::FloatValue(*f)),
        Expr::StringValue(s) => Some(TrivialHIRExpr::StringValue(s.clone())),
        Expr::BooleanValue(b) => Some(TrivialHIRExpr::BooleanValue(*b)),
        Expr::None => Some(TrivialHIRExpr::None),
        Expr::Variable(v) => Some(TrivialHIRExpr::Variable(v.clone())),
        Expr::Parenthesized(_) => {
            panic!("Sanity check: at this point no parenthesized expr should exist")
        }
        //member accesses are not that simple to prove trivial, let it go through check_if_reducible
        _ => None,
    }
}

fn get_trivial_pending_type_expr(expr: &Expr) -> Option<HIRExpr> {
    get_trivial_hir_expr(expr).map(|x| {
        HIRExpr::Trivial(x, HIRTypeDef::PendingInference, Some(expr.clone()))
    })
}

macro_rules! return_true_if_non_trivial {
    ($e:expr) => {
        let left_is_trivial = get_trivial_hir_expr($e).is_some();
        if !left_is_trivial {
            return true;
        }
    };
}

//If an expression is reducible, you have to call reduce_expr_to_hir_declarations
//to reduce the expression to a single variable.
fn check_if_reducible(expr: &Expr) -> bool {
    let expr_trivial = get_trivial_hir_expr(expr);
    if expr_trivial.is_some() {
        return false;
    };

    match expr {
        Expr::FunctionCall(function_call_expr, args_expr) => {
            //a function call is reducible if either side is non-trivial
            //the left side is likely trivial
            return_true_if_non_trivial!(function_call_expr);
            for node in args_expr {
                return_true_if_non_trivial!(node);
            }
            false
        }
        Expr::BinaryOperation(left, _op, right) => {
            return_true_if_non_trivial!(left);
            return_true_if_non_trivial!(right);
            false
        }
        Expr::Array(exprs) => {
            for e in exprs {
                return_true_if_non_trivial!(e);
            }
            false
        }
       
        Expr::MemberAccess(path_expr, _member) => {
            return_true_if_non_trivial!(path_expr);
            false
        }
        Expr::UnaryExpression(_operator, expr) => {
            return_true_if_non_trivial!(expr);
            false
        }
        _ => true, //index operator is lowered into __index__ call so it's marked as reducible
    }
}

//this function returns the final expression created, and the number of intermediary variables used
//In the recursive cases, this function should always return a HIRExpr::Trivial
//force_declare_intermediate_on_nonroot_exprs is a flag (yes I know flags are bad just because Uncle Bob said so) that
//forces the function to declare a new intermediate value even if the expression is irreducible, like 1 == 2.
//If you pass false the function just returns in the irreducible form without creating new variables. If you pass true
//then it will always introduce a new intermediate value, like $0 = 1 == 2 and return a variable.
fn reduce_expr_to_hir_declarations<'a>(
    expr: &'a Expr,
    mut intermediary: i32,
    accum: &mut Vec<HIR>,
    force_declare_intermediate_on_nonroot_exprs: bool,
    metadata: &'a Expr,
) -> (HIRExpr, i32) {
    let trivial_expr = get_trivial_hir_expr(expr);
    if let Some(x) = trivial_expr {
        return (HIRExpr::Trivial(x.clone(), HIRTypeDef::PendingInference, Some(expr.clone())), 0);
    }

    match expr {
        full_function_call @ Expr::FunctionCall(function_expr, args) => {
            let mut total_used_interm = 0;

            let fcall = if check_if_reducible(full_function_call) {
                /*
                Either the expr is non-trivial or the args are non-trivial, likely the args are non-trivial
                If an arg is trivial, then it should be just added to the function as-is
                Otherwise (if it's a binary op for instance) then a new variable must be created
                */

                let (lhs_expr, num_interm) = reduce_expr_to_hir_declarations(
                    function_expr,
                    intermediary,
                    accum,
                    true,
                    function_expr,
                );

                intermediary += num_interm;

                let mut args_exprs = vec![];
                let mut args_interm_used = 0;
                for node in args {
                    let (arg_expr, arg_num_interm) = reduce_expr_to_hir_declarations(
                        node,
                        intermediary,
                        accum,
                        true,
                        function_expr,
                    );
                    intermediary += arg_num_interm;
                    args_interm_used += arg_num_interm;

                    if let HIRExpr::Trivial(..) = arg_expr {
                        args_exprs.push(arg_expr);
                    } else {
                        panic!("Function call expression: after reduction, argument should be trivial!");
                    };
                }

                total_used_interm = num_interm + args_interm_used;
                let HIRExpr::Trivial(TrivialHIRExpr::Variable(_), ..) = lhs_expr else {
                    panic!("Function call expression: should be bound to a name!")
                };

                HIRExpr::FunctionCall(
                    lhs_expr.into(),
                    args_exprs,
                    HIRTypeDef::PendingInference,
                    Some(metadata.clone()),
                )
            } else {
                let args = args
                    .iter()
                    .map(|x| get_trivial_hir_expr(x).unwrap().pending_type())
                    .collect::<Vec<_>>();
                
                HIRExpr::FunctionCall(
                    Box::new(
                        HIRExpr::Trivial(
                            get_trivial_hir_expr(function_expr).expect("Function for now has to be trivial, i.e. variable"), 
                            HIRTypeDef::PendingInference, Some(*function_expr.clone()))
                    ),
                    args,
                    HIRTypeDef::PendingInference,
                    Some(full_function_call.clone()),
                )
            };

            if force_declare_intermediate_on_nonroot_exprs {
                let declare = HIR::Declare {
                    var: make_intermediary(intermediary),
                    typedef: HIRTypeDef::PendingInference,
                    expression: fcall,
                    meta_ast: None,
                    meta_expr: Some(full_function_call.clone()),
                };
                total_used_interm += 1;
                accum.push(declare);
                (
                    HIRExpr::Trivial(
                        TrivialHIRExpr::Variable(make_intermediary(intermediary)),
                        HIRTypeDef::PendingInference,
                        Some(full_function_call.clone()),
                    ),
                    total_used_interm,
                )
            } else {
                (fcall, total_used_interm)
            }
        }
        full_binop @ Expr::BinaryOperation(lhs, op, rhs) => {
            let mut total_used_interm = 0;
            let binop = if check_if_reducible(full_binop) {
                let (lhs_intermediary, lhs_num_intern) =
                    reduce_expr_to_hir_declarations(lhs, intermediary, accum, true, metadata);
                intermediary += lhs_num_intern;

                let (rhs_intermediary, rhs_num_intern) =
                    reduce_expr_to_hir_declarations(rhs, intermediary, accum, true, metadata);
                intermediary += rhs_num_intern;

                total_used_interm = lhs_num_intern + rhs_num_intern;

                HIRExpr::BinaryOperation(
                    lhs_intermediary.into(),
                    *op,
                    rhs_intermediary.into(),
                    HIRTypeDef::PendingInference,
                    Some(metadata.clone()),
                )
            } else {
                HIRExpr::BinaryOperation(
                    get_trivial_pending_type_expr(lhs).unwrap().into(),
                    *op,
                    get_trivial_pending_type_expr(rhs).unwrap().into(),
                    HIRTypeDef::PendingInference,
                    Some(metadata.clone()),
                )
            };

            if force_declare_intermediate_on_nonroot_exprs {
                let declare = HIR::Declare {
                    var: make_intermediary(intermediary),
                    typedef: HIRTypeDef::PendingInference,
                    expression: binop,
                    meta_ast: None,
                    meta_expr: Some(full_binop.clone()),
                };
                total_used_interm += 1;
                accum.push(declare);

                (
                    HIRExpr::Trivial(
                        TrivialHIRExpr::Variable(make_intermediary(intermediary)),
                        HIRTypeDef::PendingInference,
                        Some(full_binop.clone()),
                    ),
                    total_used_interm,
                )
            } else {
                (binop, total_used_interm)
            }
        }
        Expr::Variable(var) => (
            HIRExpr::Trivial(
                TrivialHIRExpr::Variable(var.clone()),
                HIRTypeDef::PendingInference,
                Some(expr.clone()),
            ),
            0,
        ),
        full_array_exp @ Expr::Array(arr_exprs) => {
            let mut total_used_interm = 0;

            let array = if check_if_reducible(full_array_exp) {
                let mut item_exprs = vec![];
                for node in arr_exprs {
                    let (item_expr, item_num_interm) =
                        reduce_expr_to_hir_declarations(node, intermediary, accum, true, node);
                    intermediary += item_num_interm;
                    total_used_interm += item_num_interm;

                    if let HIRExpr::Trivial(..) = item_expr {
                        item_exprs.push(item_expr.clone());
                    } else {
                        panic!(
                            "Array expression item: after reduction, argument should be trivial!"
                        );
                    };
                }

                HIRExpr::Array(
                    item_exprs,
                    HIRTypeDef::PendingInference,
                    Some(full_array_exp.clone()),
                )
            } else {
                let args = arr_exprs
                    .iter()
                    .map(|x| get_trivial_hir_expr(x).unwrap().pending_type())
                    .collect::<Vec<_>>();
                HIRExpr::Array(
                    args,
                    HIRTypeDef::PendingInference,
                    Some(full_array_exp.clone()),
                )
            };

            if force_declare_intermediate_on_nonroot_exprs {
                let declare = HIR::Declare {
                    var: make_intermediary(intermediary),
                    typedef: HIRTypeDef::PendingInference,
                    expression: array,
                    meta_ast: None,
                    meta_expr: Some(full_array_exp.clone()),
                };
                total_used_interm += 1;
                accum.push(declare);
                (
                    HIRExpr::Trivial(
                        TrivialHIRExpr::Variable(make_intermediary(intermediary)),
                        HIRTypeDef::PendingInference,
                        Some(full_array_exp.clone()),
                    ),
                    total_used_interm,
                )
            } else {
                (array, total_used_interm)
            }
        }
        //transforms an index access into a method call on obj
        //i.e. if obj[0], becomes obj.__index__(0)
        //i.e. if obj.map[0] becomes obj.map.__index__(0)
        //will need member access syntax support
        index_access @ Expr::IndexAccess(obj_expr, index_expr) => {
            let owned = index_expr.clone();
            let as_fcall = Expr::FunctionCall(
                Box::new(Expr::MemberAccess(obj_expr.clone(), "__index__".into())),
                vec![*owned],
            );

            reduce_expr_to_hir_declarations(
                &as_fcall,
                intermediary,
                accum,
                force_declare_intermediate_on_nonroot_exprs,
                index_access,
            )
        }
        unary_expression @ Expr::UnaryExpression(op, expr) => {
            let mut total_used_interm = 0;
            let unaryop = if check_if_reducible(unary_expression) {
                let (expr_intermediary, num_intern) = reduce_expr_to_hir_declarations(
                    expr,
                    intermediary,
                    accum,
                    true,
                    unary_expression,
                );
                intermediary += num_intern;

                total_used_interm = num_intern;

                HIRExpr::UnaryExpression(
                    *op,
                    expr_intermediary.into(),
                    HIRTypeDef::PendingInference,
                    Some(unary_expression.clone()),
                )
            } else {
                HIRExpr::UnaryExpression(
                    *op,
                    get_trivial_pending_type_expr(expr).unwrap().into(),
                    HIRTypeDef::PendingInference,
                    Some(unary_expression.clone()),
                )
            };

            if force_declare_intermediate_on_nonroot_exprs {
                let declare = HIR::Declare {
                    var: make_intermediary(intermediary),
                    typedef: HIRTypeDef::PendingInference,
                    expression: unaryop,
                    meta_ast: None,
                    meta_expr: Some(unary_expression.clone()),
                };
                total_used_interm += 1;
                accum.push(declare);

                (
                    HIRExpr::Trivial(
                        TrivialHIRExpr::Variable(make_intermediary(intermediary)),
                        HIRTypeDef::PendingInference,
                        Some(unary_expression.clone()),
                    ),
                    total_used_interm,
                )
            } else {
                (unaryop, total_used_interm)
            }
        }
        Expr::MemberAccess(obj_expr, name) => {
            let mut total_used_interm = 0;
            let member_access = if check_if_reducible(obj_expr) {
                let (expr_intermediary, num_intern) =
                    reduce_expr_to_hir_declarations(obj_expr, intermediary, accum, true, obj_expr);
                intermediary += num_intern;

                total_used_interm = num_intern;

                HIRExpr::MemberAccess(
                    expr_intermediary.into(),
                    name.clone(),
                    HIRTypeDef::PendingInference,
                    Some(expr.clone()),
                )
            } else {
                HIRExpr::MemberAccess(
                    get_trivial_pending_type_expr(obj_expr).unwrap().into(),
                    name.clone(),
                    HIRTypeDef::PendingInference,
                    Some(expr.clone()),
                )
            };

            if force_declare_intermediate_on_nonroot_exprs {
                let declare = HIR::Declare {
                    var: make_intermediary(intermediary),
                    typedef: HIRTypeDef::PendingInference,
                    expression: member_access,
                    meta_ast: None,
                    meta_expr: Some(expr.clone()),
                };
                total_used_interm += 1;
                accum.push(declare);

                (
                    HIRExpr::Trivial(
                        TrivialHIRExpr::Variable(make_intermediary(intermediary)),
                        HIRTypeDef::PendingInference,
                        Some(expr.clone()),
                    ),
                    total_used_interm,
                )
            } else {
                (member_access, total_used_interm)
            }
        }
        exprnode => panic!("Expr to HIR not implemented for {:?}", exprnode),
    }
}

struct IfTreeNode {
    condition: HIRExpr,
    true_body: Vec<HIR>,
    body_meta: AST,
}

pub fn ast_to_hir(ast: &AST, mut intermediary: i32, accum: &mut Vec<HIR>) -> i32 {
    match ast {
        AST::Declare { var, expression } => {
            //expr: we have to decompose the expression into HIR declarations
            //because the assigned/declared expression has to be sufficiently simple
            //to decrease complexity for the other compiler phases (typechecking, type inference)

            //maybe a way to do it is by calling reduce_expr_to_hir_declarations, and the function
            //itself returns a HIRExpr. It will also add to the HIR any declarations needed
            //for the decomposition.
            let (result_expr, num_intermediaries) =
                reduce_expr_to_hir_declarations(expression, intermediary, accum, false, expression);

            let decl_hir = HIR::Declare {
                var: var.name.clone(),
                typedef: HIRTypeDef::Unresolved(HIRType::from_ast(&var.name_type)),
                expression: result_expr,
                meta_expr: Some(expression.clone()),
                meta_ast: Some(ast.clone()),
            };

            accum.push(decl_hir);

            num_intermediaries
        }
        AST::Assign { path, expression } => {
            let (result_expr, num_intermediaries) =
                reduce_expr_to_hir_declarations(expression, intermediary, accum, false, expression);

            let decl_hir = HIR::Assign {
                path: path.clone(),
                expression: result_expr,
                meta_ast: Some(ast.clone()),
                meta_expr: Some(expression.clone()),
            };

            accum.push(decl_hir);
            num_intermediaries
        }
        AST::DeclareFunction {
            function_name,
            parameters,
            body,
            return_type,
        } => {
            ast_decl_function_to_hir(body, &mut intermediary, function_name, parameters, return_type, ast, accum)
        }
        AST::Root(ast_nodes) => {
            let mut sum_intermediaries = 0;
            for node in ast_nodes {
                let created_intermediaries = ast_to_hir(node, intermediary, accum);
                sum_intermediaries += created_intermediaries;
                intermediary += created_intermediaries;
            }
            sum_intermediaries
        }
        AST::Return(expr) => match expr {
            None => {
                accum.push(HIR::EmptyReturn);
                0
            }
            Some(e) => {
                let (result_expr, num_intermediaries) =
                    reduce_expr_to_hir_declarations(e, intermediary, accum, false, e);
                accum.push(HIR::Return(
                    result_expr,
                    HIRTypeDef::PendingInference,
                    Some(ast.clone()),
                ));
                num_intermediaries
            }
        },
        AST::StructDeclaration { struct_name, body } => {
            let fields = body.iter().map(|field| HIRTypedBoundName {
                name: field.name.clone(),
                typename: HIRTypeDef::Unresolved(HIRType::from_ast(&field.name_type)),
            });
            accum.push(HIR::StructDeclaration {
                struct_name: struct_name.clone(),
                body: fields.collect(),
                meta: Some(ast.clone()),
            });
            0
        }
        AST::StandaloneExpr(expr) => {
            let Expr::FunctionCall(_, _) = expr else {
                panic!("Can only lower function call standalone expr: {:#?}", expr);
            };

            let (result_expr, num_intermediaries) =
                reduce_expr_to_hir_declarations(expr, intermediary, accum, false, expr);
            let HIRExpr::FunctionCall(function, args, ..) = &result_expr else {
                panic!("Lowering of function call returned invalid result: {:?}", result_expr);
            };

            let typed_args = args.clone();

            accum.push(HIR::FunctionCall {
                function: *function.clone(),
                args: typed_args,
                meta: Some(ast.clone()),
            });
            num_intermediaries
        }
        AST::IfStatement {
            true_branch,
            elifs,
            final_else,
        } => {
            ast_if_to_hir(true_branch, intermediary, accum, elifs, final_else, ast)
        }
        ast => panic!("Not implemented HIR for {:?}", ast),
    }
}

fn ast_decl_function_to_hir(body: &[AST], intermediary: &mut i32, function_name: &str, parameters: &[crate::ast::parser::TypeBoundName], return_type: &Option<ASTType>, ast: &AST, accum: &mut Vec<HIR>) -> i32 {
    let mut function_body = vec![];
    for node in body {
        let created_intermediaries = ast_to_hir(node, *intermediary, &mut function_body);
        *intermediary += created_intermediaries;
    }
    let decl_hir = HIR::DeclareFunction {
        function_name: function_name.to_string(),
        parameters: parameters
            .iter()
            .map(|param| {
                let name = param.name.clone();
                HIRTypedBoundName {
                    name,
                    typename: HIRTypeDef::Unresolved(HIRType::from_ast(&param.name_type)),
                }
            })
            .collect(),
        body: function_body,
        return_type: match return_type {
            Some(x) => HIRTypeDef::Unresolved(HIRType::from_ast(x)),
            None => HIRTypeDef::Unresolved(HIRType::Simple("Void".into())),
        },
        meta: Some(ast.clone()),
    };
    accum.push(decl_hir);
    0
    //yes, each function declaration created the intermediares for their body to work, but they don't
    //escape the scope of the function!
}

fn ast_if_to_hir(true_branch: &crate::ast::parser::ASTIfStatement, 
    mut intermediary: i32, accum: &mut Vec<HIR>, 
    elifs: &[crate::ast::parser::ASTIfStatement], final_else: &Option<Vec<AST>>, ast: &AST) -> i32 {
    let (true_branch_result_expr, num_intermediaries) = reduce_expr_to_hir_declarations(
        &true_branch.expression,
        intermediary,
        accum,
        true,
        &true_branch.expression,
    );
    intermediary += num_intermediaries;
    let HIRExpr::Trivial(trivial_true_branch_expr,..) = &true_branch_result_expr else {
        panic!("Lowering of true branch expr returned invalid result (not trivial): {:?}", true_branch_result_expr);
    };
    let mut true_body_hir = vec![];
    for node in &true_branch.statements {
        let created_intermediaries = ast_to_hir(node, intermediary, &mut true_body_hir);
        intermediary += created_intermediaries;
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
            HIRExpr::Trivial(trivial_true_branch_expr.clone(), HIRTypeDef::PendingInference, true_branch.expression.clone().into()),
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
                    let created_intermediaries =
                        ast_to_hir(node, intermediary, &mut false_body_hir);
                    intermediary += created_intermediaries;
                }

                accum.push(HIR::If(
                    HIRExpr::Trivial(trivial_true_branch_expr.clone(), HIRTypeDef::PendingInference, true_branch.expression.clone().into()),
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
            condition: HIRExpr::Trivial(trivial_true_branch_expr.clone(), HIRTypeDef::PendingInference, true_branch.expression.clone().into()),
            true_body: true_body_hir,
            body_meta: AST::Root(true_branch.statements.clone()),
        };

        nodes.push(root_node);

        for item in elifs {
            let (elif_true_branch_result_expr, num_intermediaries) =
                reduce_expr_to_hir_declarations(
                    &item.expression,
                    intermediary,
                    accum,
                    true,
                    &item.expression,
                );
            intermediary += num_intermediaries;

            let HIRExpr::Trivial(..) = &elif_true_branch_result_expr else {
                panic!("Lowering of elif true branch expr returned invalid result: {:?}", elif_true_branch_result_expr);
            };
            let mut if_node = IfTreeNode {
                condition: elif_true_branch_result_expr.clone(),
                true_body: vec![],
                body_meta: AST::Root(item.statements.clone()),
            };

            for node in &item.statements {
                let created_intermediaries =
                    ast_to_hir(node, intermediary, &mut if_node.true_body);
                intermediary += created_intermediaries;
            }
            nodes.push(if_node);
        }
        let mut final_else_body = vec![];
        if let Some(statements) = final_else {
            for node in statements.iter() {
                let created_intermediaries =
                    ast_to_hir(node, intermediary, &mut final_else_body);
                intermediary += created_intermediaries;
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
                None => {
                    HIR::If(node.condition, node.true_body, vec![], Some(node.body_meta))
                }
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
    use crate::types::type_db::TypeDatabase;

    //Parses a single expression
    fn parse(source: &str) -> Vec<HIR> {
        let tokens = crate::ast::lexer::tokenize(source);
        //println!("Tokens: {:?}", tokens);
        let ast = crate::ast::parser::parse_ast(tokens.unwrap());

        let root = crate::ast::parser::AST::Root(ast);
        let mut result = vec![];
        hir::ast_to_hir(&root, 0, &mut result);
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
        let result = print_hir(&result, &TypeDatabase::new());
        println!("{}", result);

        let expected = "
def main(args: UNRESOLVED List<UNRESOLVED! String>) -> UNRESOLVED! Void:
    $0 : UNKNOWN_TYPE = my_function(99, 999)
    minus : UNRESOLVED! i32 = -$0
    $1 : UNKNOWN_TYPE = -3
    numbers = [1, 2, $1, minus]
    r1 = my_function(1, 2)
    r2 = my_function2(3, 4)
    $2 : UNKNOWN_TYPE = numbers.__index__
    $3 : UNKNOWN_TYPE = $2(1)
    $4 : UNKNOWN_TYPE = numbers.__index__
    $5 : UNKNOWN_TYPE = $4(2)
    r3 = my_function($3, $5)
    $6 : UNKNOWN_TYPE = r1 + r2
    $7 : UNKNOWN_TYPE = $6 + r3
    print($7)
def my_function(arg1: UNRESOLVED! i32, arg2: UNRESOLVED! i32) -> UNRESOLVED! i32:
    $0 : UNKNOWN_TYPE = arg1 * arg2
    $1 : UNKNOWN_TYPE = arg2 - arg1
    return $0 / $1
def my_function2(arg1: UNRESOLVED! i32, arg2: UNRESOLVED! i32) -> UNRESOLVED! i32:
    $0 : UNKNOWN_TYPE = arg2 + 1
    result1 : UNRESOLVED! i32 = my_function(arg1, $0)
    $1 : UNKNOWN_TYPE = arg2 * 9
    result2 = pow(arg1, $1)
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
    $0 : UNKNOWN_TYPE = args.__index__
    $1 : UNKNOWN_TYPE = $0(0)
    $2 : UNKNOWN_TYPE = $1 == 1
    if $2:
        print(10)
    else:
        print(20)";

        let result = print_hir(&parsed, &TypeDatabase::new());
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
    $0 : UNKNOWN_TYPE = args.__index__
    arg = $0(0)
    $1 : UNKNOWN_TYPE = arg == 1
    if $1:
        print(10)
        $2 : UNKNOWN_TYPE = arg <= 0
        if $2:
            arg = arg + 1
        else:
            pass
    else:
        $2 : UNKNOWN_TYPE = arg == 2
        if $2:
            print(40)
        else:
            pass        
";

        let final_result = print_hir(&result, &TypeDatabase::new());
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
        let result = print_hir(&parsed, &TypeDatabase::new());

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

        let result = print_hir(&parsed, &TypeDatabase::new());
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
