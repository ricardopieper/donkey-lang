use std::borrow::Cow;
use std::borrow::Cow::Borrowed;
use std::borrow::Cow::Owned;

use crate::ast::lexer::*;
use crate::ast::parser::*;
use crate::commons::float::*;

use super::type_db::{TypeDatabase, TypeId};



/**
 *
 * The HIR expression is not a tree, rather it's a decomposed version of the expression.
 * There is no need to do recursion over a HIR expression, it will be decomposed with more declarations
 * to make type inference easier.
 *
 * Some of the typechecking is done here, but we might have to lower yet another level
 * to do all the typechecking and other flow control validations, like checking if all paths return a value,
 * and that all returns are compatible
 *
 */

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TrivialHIRExpr {
    IntegerValue(i128),
    FloatValue(Float),
    StringValue(String),
    BooleanValue(bool),
    Variable(String),
    None,
}

impl TrivialHIRExpr {
    pub fn pending_type(&self) -> TypedTrivialHIRExpr {
        return TypedTrivialHIRExpr(self.clone(), HIRTypeDef::Pending);
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
pub struct TypedTrivialHIRExpr(pub TrivialHIRExpr, pub HIRTypeDef);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRExpr {
    Trivial(TypedTrivialHIRExpr, HIRExprMetadata),
    Cast(TypedTrivialHIRExpr, HIRTypeDef, HIRExprMetadata),
    BinaryOperation(
        TypedTrivialHIRExpr,
        Operator,
        TypedTrivialHIRExpr,
        HIRTypeDef,
        HIRExprMetadata
    ),
    FunctionCall(TypedTrivialHIRExpr, Vec<TypedTrivialHIRExpr>, HIRTypeDef, HIRExprMetadata),
    UnaryExpression(Operator, TypedTrivialHIRExpr, HIRTypeDef, HIRExprMetadata),
    MemberAccess(TypedTrivialHIRExpr, String, HIRTypeDef, HIRExprMetadata),
    Array(Vec<TypedTrivialHIRExpr>, HIRTypeDef, HIRExprMetadata),
}

/*This enum represents the type as typed in source code. This comes from the AST almost directly,
no fancy transformations are applied.
However we add a Function variant to construct a Function type where needed, but it also could be something coming from the AST in the future,
like functions receiving functions*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRType {
    Simple(String),
    Generic(String, Vec<HIRType>),
    Function(Vec<HIRType>, Box<HIRType>),
}

impl HIRExpr {
    pub fn expect_trivial(&self) -> TypedTrivialHIRExpr {
        match self {
            HIRExpr::Trivial(e, ..) => e.clone(),
            _ => panic!("Expression is not trivial {:?}", self),
        }
    }

    pub fn get_expr_type(&self) -> &HIRTypeDef {
        match self {
            HIRExpr::Trivial(t, ..) => &t.1,
            HIRExpr::Cast(_, t,..) => t,
            HIRExpr::BinaryOperation(.., t, _) => t,
            HIRExpr::FunctionCall(.., t, _) => t,
            HIRExpr::UnaryExpression(.., t, _) => t,
            HIRExpr::MemberAccess(.., t, _) => t,
            HIRExpr::Array(.., t, _) => t,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HIRTypedBoundName {
    pub name: String,
    pub typename: HIRTypeDef, //var name, type
}

#[derive(Debug, Clone, PartialEq, Eq)]
/*Represents a fully resolved type, with generics already substituted */
pub enum TypeInstance {
    Simple(TypeId),                     //Built-in types, non-generic structs, etc
    Generic(TypeId, Vec<TypeInstance>), //each TypeId in the vec is a type parameter used in this specific usage of the type, this is positional.
    //parameters, return type
    Function(Vec<TypeInstance>, Box<TypeInstance>), //In this case there is not even a base type like in generics, functions are functions
}

impl TypeInstance {
    pub fn as_string(&self, type_db: &TypeDatabase) -> String {
        match self {
            TypeInstance::Simple(id) => type_db.get_name(*id).into(),
            TypeInstance::Generic(id, args) => {
                let args_str = args
                    .iter()
                    .map(|x| x.as_string(type_db).clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                let base_str = type_db.get_name(*id);
                format!("{}<{}>", base_str, args_str)
            }
            TypeInstance::Function(args, return_type) => {
                let args_str = args
                    .iter()
                    .map(|x| x.as_string(type_db).clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                let return_type_str = return_type.as_string(type_db);
                format!("fn ({}) -> {}", args_str, return_type_str)
            }
        }
    }

    pub fn is_compatible(&self, other: &TypeInstance, type_db: &TypeDatabase) -> bool {
        //for now we just compare by equality
        return self == other;
    }
}

//we need to be able to represent complex stuff,
//like a function that receives a function, whose parameters are generic
//def func(another_func: Function<List<String>>)

//so we can store TypeIds, but we need it to be accompanied by more data depending on the kind of the type,
//types such as functions and generics need to be "instanced"
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRTypeDef {
    Pending,
    Unresolved(HIRType),
    Resolved(TypeInstance),
}

impl HIRTypeDef {
    pub fn expect_unresolved(&self) -> HIRType {
        match self {
            HIRTypeDef::Pending => panic!("Function parameters must have a type"),
            HIRTypeDef::Unresolved(e) => e.clone(),
            HIRTypeDef::Resolved(_) => {
                panic!("Cannot deal with resolved types at this point, this is a bug")
            }
        }
    }
    pub fn expect_resolved(&self) -> &TypeInstance {
        match self {
            HIRTypeDef::Pending => panic!("Expected resolved type, but is Pending"),
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
                let hir_generics = generics
                    .iter()
                    .map(|x| Self::from_ast(x))
                    .collect::<Vec<_>>();
                return HIRType::Generic(name.clone(), hir_generics);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
/*
The HIR expression is similar to the AST, but simplified. Some language features are reduced to HIR,
and types declared explicitly are resolved, as well as variables get assigned types through type inference.

Some typechecking is done during this transformation as well.

This representation can be recursive for code blocks (like the scopes for ifs, functions, etc) but expressions
are decomposed to a single reference. You won't find things like x = a + b / c * d here,
but rather a decomposed version with many intermediate declarations.

The HIR will be further decomposed into the MIR, where there is no recursion *at all*, all code blocks and scopes will be declared
and decomposed, and then they will reference each other.
*/
pub enum HIR {
    Assign {
        path: Vec<String>,
        expression: HIRExpr,
        meta_ast: HIRAstMetadata,
        meta_expr: HIRExprMetadata
    },
    Declare {
        var: String,
        typedef: HIRTypeDef,
        expression: HIRExpr,
        meta_ast: HIRAstMetadata,
        meta_expr: HIRExprMetadata
    },
    DeclareFunction {
        function_name: String,
        parameters: Vec<HIRTypedBoundName>,
        body: Vec<HIR>,
        return_type: HIRTypeDef,
        meta: HIRAstMetadata
    },
    StructDeclaration {
        struct_name: String,
        body: Vec<HIRTypedBoundName>,
        meta: HIRAstMetadata
    },
    FunctionCall {
        function: TypedTrivialHIRExpr,
        args: Vec<TypedTrivialHIRExpr>,
        meta: HIRAstMetadata
    },
    //condition, true branch, false branch
    //this transforms elifs into else: \n\t if ..
    If(TypedTrivialHIRExpr, Vec<HIR>, Vec<HIR>, HIRAstMetadata),
    Return(HIRExpr, HIRTypeDef, HIRAstMetadata),
    EmptyReturn,
}

fn make_intermediary(intermediary: i32) -> String {
    return format!("${}", intermediary);
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
            return false;
        }
        Expr::BinaryOperation(left, _op, right) => {
            return_true_if_non_trivial!(left);
            return_true_if_non_trivial!(right);
            return false;
        }
        Expr::Array(exprs) => {
            for e in exprs {
                return_true_if_non_trivial!(e);
            }
            return false;
        }
        Expr::IndexAccess(_, _) => {
            //return true so that it can be lowered to a __index__ call
            return true;
        }
        Expr::MemberAccess(path_expr, _member) => {
            return_true_if_non_trivial!(path_expr);
            return false;
        }
        Expr::UnaryExpression(_operator, expr) => {
            return_true_if_non_trivial!(expr);
            return false;
        }
        _ => true,
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
    metadata: &'a Expr
) -> (HIRExpr, i32) {
    let trivial_expr = get_trivial_hir_expr(expr);
    match trivial_expr {
        Some(x) => return (HIRExpr::Trivial(x.pending_type(), Some(expr.clone())), 0),
        None => {}
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

                let (lhs_expr, num_interm) =
                    reduce_expr_to_hir_declarations(function_expr, intermediary, accum, true, function_expr);

                intermediary += num_interm;

                let mut args_exprs = vec![];
                let mut args_interm_used = 0;
                for node in args {
                    let (arg_expr, arg_num_interm) =
                        reduce_expr_to_hir_declarations(node, intermediary, accum, true, function_expr);
                    intermediary += arg_num_interm;
                    args_interm_used += arg_num_interm;

                    if let HIRExpr::Trivial(arg, _) = arg_expr {
                        args_exprs.push(arg);
                    } else {
                        panic!("Function call expression: after reduction, argument should be trivial!");
                    };
                }

                total_used_interm = num_interm + args_interm_used;
                let call_expr = if let HIRExpr::Trivial(name, _) = lhs_expr {
                    name
                } else {
                    panic!("Function call expression: should be bound to a name!")
                };

                HIRExpr::FunctionCall(call_expr, args_exprs, HIRTypeDef::Pending, Some(metadata.clone()))
            } else {
                let args = args
                    .iter()
                    .map(|x| get_trivial_hir_expr(x).unwrap().pending_type())
                    .collect::<Vec<_>>();
                HIRExpr::FunctionCall(
                    get_trivial_hir_expr(function_expr).unwrap().pending_type(),
                    args,
                    HIRTypeDef::Pending,
                    Some(full_function_call.clone())
                )
            };

            if force_declare_intermediate_on_nonroot_exprs {
                let declare = HIR::Declare {
                    var: make_intermediary(intermediary),
                    typedef: HIRTypeDef::Pending,
                    expression: fcall.clone(),
                    meta_ast: None,
                    meta_expr: Some(full_function_call.clone())
                };
                total_used_interm += 1;
                accum.push(declare);
                return (
                    HIRExpr::Trivial(
                        TrivialHIRExpr::Variable(make_intermediary(intermediary)).pending_type(),
                        Some(full_function_call.clone())
                    ),
                    total_used_interm,
                );
            } else {
                return (fcall, total_used_interm);
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
                    lhs_intermediary.expect_trivial().clone(),
                    *op,
                    rhs_intermediary.expect_trivial().clone(),
                    HIRTypeDef::Pending,
                    Some(metadata.clone())
                )
            } else {
                HIRExpr::BinaryOperation(
                    get_trivial_hir_expr(lhs).unwrap().pending_type(),
                    *op,
                    get_trivial_hir_expr(rhs).unwrap().pending_type(),
                    HIRTypeDef::Pending,
                    Some(metadata.clone())
                )
            };

            if force_declare_intermediate_on_nonroot_exprs {
                let declare = HIR::Declare {
                    var: make_intermediary(intermediary),
                    typedef: HIRTypeDef::Pending,
                    expression: binop.clone(),
                    meta_ast: None,
                    meta_expr: Some(full_binop.clone())
                };
                total_used_interm += 1;
                accum.push(declare);

                return (
                    HIRExpr::Trivial(
                        TrivialHIRExpr::Variable(make_intermediary(intermediary)).pending_type(),
                        Some(full_binop.clone())
                    ),
                    total_used_interm,
                );
            } else {
                return (binop, total_used_interm);
            }
        }
        Expr::Variable(var) => {
            return (
                HIRExpr::Trivial(TrivialHIRExpr::Variable(var.clone()).pending_type(), Some(expr.clone())),
                0,
            );
        }
        full_array_exp @ Expr::Array(arr_exprs) => {
            let mut total_used_interm = 0;

            let array = if check_if_reducible(full_array_exp) {
                let mut item_exprs = vec![];
                for node in arr_exprs {
                    let (item_expr, item_num_interm) =
                        reduce_expr_to_hir_declarations(node, intermediary, accum, true, node);
                    intermediary += item_num_interm;
                    total_used_interm += item_num_interm;

                    if let HIRExpr::Trivial(arg, _) = item_expr {
                        item_exprs.push(arg);
                    } else {
                        panic!(
                            "Array expression item: after reduction, argument should be trivial!"
                        );
                    };
                }

                HIRExpr::Array(item_exprs, HIRTypeDef::Pending, Some(full_array_exp.clone()))
            } else {
                let args = arr_exprs
                    .iter()
                    .map(|x| get_trivial_hir_expr(x).unwrap().pending_type())
                    .collect::<Vec<_>>();
                HIRExpr::Array(args, HIRTypeDef::Pending, Some(full_array_exp.clone()))
            };

            if force_declare_intermediate_on_nonroot_exprs {
                let declare = HIR::Declare {
                    var: make_intermediary(intermediary),
                    typedef: HIRTypeDef::Pending,
                    expression: array.clone(),
                    meta_ast: None,
                    meta_expr: Some(full_array_exp.clone())
                };
                total_used_interm += 1;
                accum.push(declare);
                return (
                    HIRExpr::Trivial(
                        TrivialHIRExpr::Variable(make_intermediary(intermediary)).pending_type(),
                        Some(full_array_exp.clone())
                    ),
                    total_used_interm,
                );
            } else {
                return (array, total_used_interm);
            }
        }
        //transforms an index access into a method call on obj
        //i.e. if obj[0], becomes obj.__index__(0)
        //i.e. if obj.map[0] becomes obj.map.__index__(0)
        //will need member access syntax support
        index_access @ Expr::IndexAccess(obj_expr, index_expr) => {
            let owned = index_expr.to_owned();
            let as_fcall = Expr::FunctionCall(
                Box::new(Expr::MemberAccess(obj_expr.clone(), "__index__".into())),
                vec![*owned],
            );

            return reduce_expr_to_hir_declarations(
                &as_fcall,
                intermediary,
                accum,
                force_declare_intermediate_on_nonroot_exprs,
                &index_access
            );
        }
        unary_expression @ Expr::UnaryExpression(op, expr) => {
            let mut total_used_interm = 0;
            let unaryop = if check_if_reducible(unary_expression) {
                let (expr_intermediary, num_intern) =
                    reduce_expr_to_hir_declarations(expr, intermediary, accum, true, unary_expression);
                intermediary += num_intern;

                total_used_interm = num_intern;

                HIRExpr::UnaryExpression(
                    *op,
                    expr_intermediary.expect_trivial().clone(),
                    HIRTypeDef::Pending,
                    Some(unary_expression.clone())
                )
            } else {
                HIRExpr::UnaryExpression(
                    *op,
                    get_trivial_hir_expr(expr).unwrap().pending_type(),
                    HIRTypeDef::Pending,
                    Some(unary_expression.clone())
                )
            };

            if force_declare_intermediate_on_nonroot_exprs {
                let declare = HIR::Declare {
                    var: make_intermediary(intermediary),
                    typedef: HIRTypeDef::Pending,
                    expression: unaryop.clone(),
                    meta_ast: None,
                    meta_expr: Some(unary_expression.clone())
                };
                total_used_interm += 1;
                accum.push(declare);

                return (
                    HIRExpr::Trivial(
                        TrivialHIRExpr::Variable(make_intermediary(intermediary)).pending_type(),
                        Some(unary_expression.clone())
                    ),
                    total_used_interm,
                );
            } else {
                return (unaryop, total_used_interm);
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
                    expr_intermediary.expect_trivial().clone(),
                    name.clone(),
                    HIRTypeDef::Pending,
                    Some(expr.clone())
                )
            } else {
                HIRExpr::MemberAccess(
                    get_trivial_hir_expr(obj_expr).unwrap().pending_type(),
                    name.clone(),
                    HIRTypeDef::Pending,
                    Some(expr.clone())
                )
            };

            if force_declare_intermediate_on_nonroot_exprs {
                let declare = HIR::Declare {
                    var: make_intermediary(intermediary),
                    typedef: HIRTypeDef::Pending,
                    expression: member_access.clone(),
                    meta_ast: None,
                    meta_expr: Some(expr.clone())
                };
                total_used_interm += 1;
                accum.push(declare);

                return (
                    HIRExpr::Trivial(
                        TrivialHIRExpr::Variable(make_intermediary(intermediary)).pending_type(),
                        Some(expr.clone())
                    ),
                    total_used_interm,
                );
            } else {
                return (member_access, total_used_interm);
            }
        }
        exprnode => panic!("Expr to HIR not implemented for {:?}", exprnode),
    }
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
                meta_ast: Some(ast.clone())
            };

            accum.push(decl_hir);

            return num_intermediaries;
        }
        AST::Assign { path, expression } => {
            let (result_expr, num_intermediaries) =
                reduce_expr_to_hir_declarations(expression, intermediary, accum, false, expression);

            let decl_hir = HIR::Assign {
                path: path.clone(),
                expression: result_expr,
                meta_ast: Some(ast.clone()),
                meta_expr: Some(expression.clone())
            };

            accum.push(decl_hir);
            return num_intermediaries;
        }
        AST::DeclareFunction {
            function_name,
            parameters,
            body,
            return_type,
        } => {
            let mut function_body = vec![];

            for node in body {
                let created_intermediaries = ast_to_hir(node, intermediary, &mut function_body);
                intermediary += created_intermediaries;
            }

            let decl_hir = HIR::DeclareFunction {
                function_name: function_name.clone(),
                parameters: parameters
                    .iter()
                    .map(|param| {
                        let name = param.name.clone();
                        return HIRTypedBoundName {
                            name,
                            typename: HIRTypeDef::Unresolved(HIRType::from_ast(&param.name_type)),
                        };
                    })
                    .collect(),
                body: function_body,
                return_type: match return_type {
                    Some(x) => HIRTypeDef::Unresolved(HIRType::from_ast(x)),
                    None => HIRTypeDef::Unresolved(HIRType::Simple("Void".into())),
                },
                meta: Some(ast.clone())
            };

            accum.push(decl_hir);
            return 0; //yes, each function declaration created the intermediares for their body to work, but they don't
                      //escape the scope of the function!
        }
        AST::Root(ast_nodes) => {
            let mut sum_intermediaries = 0;
            for node in ast_nodes {
                let created_intermediaries = ast_to_hir(node, intermediary, accum);
                sum_intermediaries += created_intermediaries;
                intermediary += created_intermediaries;
            }
            return sum_intermediaries;
        }
        AST::Return(expr) => match expr {
            None => {
                accum.push(HIR::EmptyReturn);
                return 0;
            }
            Some(e) => {
                let (result_expr, num_intermediaries) =
                    reduce_expr_to_hir_declarations(e, intermediary, accum, false, e);
                accum.push(HIR::Return(result_expr, HIRTypeDef::Pending, Some(ast.clone())));
                return num_intermediaries;
            }
        },
        AST::StructDeclaration { struct_name, body } => {
            let fields = body.iter().map(|field| {
                return HIRTypedBoundName {
                    name: field.name.clone(),
                    typename: HIRTypeDef::Unresolved(HIRType::from_ast(&field.name_type)),
                };
            });
            accum.push(HIR::StructDeclaration {
                struct_name: struct_name.clone(),
                body: fields.collect(),
                meta: Some(ast.clone())
            });
            return 0;
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

            let typed_args = args.iter().map(|x| x.clone()).collect::<Vec<_>>();

            accum.push(HIR::FunctionCall {
                function: function.clone(),
                args: typed_args.clone(),
                meta: Some(ast.clone())
            });
            return num_intermediaries;
        }
        AST::IfStatement {
            true_branch,
            elifs,
            final_else,
        } => {
            let (true_branch_result_expr, num_intermediaries) =
                reduce_expr_to_hir_declarations(&true_branch.expression, intermediary, accum, true, &true_branch.expression);
            intermediary += num_intermediaries;
            let HIRExpr::Trivial(trivial_true_branch_expr, _) = &true_branch_result_expr else {
                panic!("Lowering of true branch expr returned invalid result: {:?}", true_branch_result_expr);
            };

            let mut true_body_hir = vec![];

            for node in true_branch.statements.iter() {
                let created_intermediaries = ast_to_hir(node, intermediary, &mut true_body_hir);
                intermediary += created_intermediaries;
            }

            /*
              The HIR representation is simpler, but that means we have to do work to simplify it.
              HIR is only condition, true branch, false branch so the rest of the else ifs go inside the false branch.

              Let's just rewrite whatever the user wrote lmao

              First we have to handle some base cases, i.e. no more elifs ou elses, no more elifs but there is an else, etc.
            */

            if elifs.len() == 0 && final_else.is_none() {
                /* Just like function declarations, the intermediaries created here don't escape the context.
                   This is different than python, where all variables declared are scoped to the entire function;
                */

                accum.push(HIR::If(
                    trivial_true_branch_expr.clone(),
                    true_body_hir,
                    vec![],
                    Some(ast.clone())
                ));

                return 0;
            } else if elifs.len() == 0 && final_else.is_some() {
                //in this case we have a final else, just generate a false branch
                let mut false_body_hir = vec![];

                match final_else {
                    None => panic!("Shouldn't happen!"),
                    Some(nodes) => {
                        for node in nodes.iter() {
                            let created_intermediaries =
                                ast_to_hir(&node, intermediary, &mut false_body_hir);
                            intermediary += created_intermediaries;
                        }

                        accum.push(HIR::If(
                            trivial_true_branch_expr.clone(),
                            true_body_hir,
                            false_body_hir,
                            Some(ast.clone())
                        ));
                    }
                }
                return 0;
            } else {
                //in this case we have elifs, so we build the "tree"
                //and we don't actually need to store the false body because we'll connect everything later.
                //it's not actually a tree... it's more like a linked list.

                //let mut current_if_tree = HIR::If(trivial_true_branch_expr, true_body_hir, ());
                let mut nodes = vec![];

                struct IfTreeNode {
                    condition: TypedTrivialHIRExpr,
                    true_body: Vec<HIR>,
                    body_meta: AST
                }

                let root_node = IfTreeNode {
                    condition: trivial_true_branch_expr.clone(),
                    true_body: true_body_hir,
                    body_meta: AST::Root(true_branch.statements.clone())
                };

                nodes.push(root_node);

                for item in elifs {
                    let (elif_true_branch_result_expr, num_intermediaries) =
                        reduce_expr_to_hir_declarations(
                            &item.expression,
                            intermediary,
                            accum,
                            true,
                            &item.expression
                        );
                    intermediary += num_intermediaries;

                    let HIRExpr::Trivial(elif_trivial_true_branch_result_expr, _) = &elif_true_branch_result_expr else {
                        panic!("Lowering of elif true branch expr returned invalid result: {:?}", elif_true_branch_result_expr);
                    };
                    let mut if_node = IfTreeNode {
                        condition: elif_trivial_true_branch_result_expr.clone(),
                        true_body: vec![],
                        body_meta: AST::Root(item.statements.clone())
                    };

                    for node in item.statements.iter() {
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

                if final_else_body.len() > 0 {
                    let first_node = nodes.pop().unwrap(); //there MUST be a node here
                    final_if_chain = Some(HIR::If(
                        first_node.condition,
                        first_node.true_body,
                        final_else_body,
                        Some(ast.clone())
                    ));
                }

                nodes.reverse();
                //we navigate through the nodes in reverse and build the final HIR tree
                for node in nodes {
                    let new_node = match final_if_chain {
                        None => HIR::If(node.condition, node.true_body, vec![], Some(node.body_meta)),
                        Some(current_chain) => {
                            HIR::If(node.condition, node.true_body, vec![current_chain], Some(node.body_meta))
                        }
                    };
                    final_if_chain = Some(new_node);
                }
                accum.push(final_if_chain.unwrap());

                return 0;
            }
        }
        ast => panic!("Not implemented HIR for {:?}", ast),
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::ast::lexer::*;
    use crate::ast::parser::*;
    use crate::semantic::hir_printer::print_hir;
    use crate::semantic::*;

    //Parses a single expression
    fn parse(source: &str) -> Vec<HIR> {
        let tokens = crate::ast::lexer::tokenize(source);
        //println!("Tokens: {:?}", tokens);
        let ast = crate::ast::parser::parse_ast(tokens.unwrap());

        let root = crate::ast::parser::AST::Root(ast);
        let mut result = vec![];
        hir::ast_to_hir(&root, 0, &mut result);
        return result;
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
        let result = print_hir(&result, &type_db::TypeDatabase::new());
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

        let result = print_hir(&parsed, &type_db::TypeDatabase::new());
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

        let final_result = print_hir(&result, &type_db::TypeDatabase::new());
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
        let result = print_hir(&parsed, &type_db::TypeDatabase::new());

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

        let result = print_hir(&parsed, &type_db::TypeDatabase::new());
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
