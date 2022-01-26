use crate::ast::lexer::*;
use crate::commons::float::*;
use crate::ast::parser::*;


/**
 * 
 * The MIR expression is not a tree, rather it's a decomposed version of the expression.
 * There is no need to do recursion over a MIR expression, it will be decomposed with more declarations
 * to make type checking and compilation easier
 * 
 */

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TrivialMIRExpr {
    IntegerValue(i128),
    FloatValue(Float),
    StringValue(String),
    BooleanValue(bool),
    Variable(String),
    None,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRExpr {
    Trivial(TrivialMIRExpr),
    BinaryOperation(TrivialMIRExpr, Operator, TrivialMIRExpr),
    FunctionCall(TrivialMIRExpr, Vec<TrivialMIRExpr>),
    IndexAccess(TrivialMIRExpr, TrivialMIRExpr),
    UnaryExpression(Operator, TrivialMIRExpr),
    MemberAccess(TrivialMIRExpr, String),
    Array(Vec<TrivialMIRExpr>),
}


impl MIRExpr {
    fn expect_trivial(&self) -> &TrivialMIRExpr {
        match self {
            MIRExpr::Trivial(e) => e,
            _ => panic!("Expression is not trivial {:?}", self)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRTypedBoundName {
    pub name: String,
    pub typename: MIRTypeDef //var name, type
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRType {
    Simple(String),
    Generic(String, Vec<MIRTypeDef>)
}

impl MIRType {
    fn from_ast(typ: &ASTType) -> Self {
        match typ {
            ASTType::Simple(name) => Self::Simple(name.clone()),
            ASTType::Generic(name, generics) => {
                let mir_generics = generics.iter().map(|x| MIRTypeDef::Unresolved(Self::from_ast(x))).collect::<Vec<_>>();
                return MIRType::Generic(name.clone(), mir_generics);
            } 
        }
    }
}

type TypeId = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRTypeDef {
    Pending,
    Unresolved(MIRType),
    Resolved(TypeId)
}



#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIR {
    Assign {
        path: Vec<String>,
        expression: MIRExpr,
    },
    Declare {
        var: String,
        typename: MIRTypeDef,
        expression: MIRExpr,
    },
    DeclareFunction {
        function_name: String,
        parameters: Vec<MIRTypedBoundName>,
        body: Vec<MIR>,
        return_type: MIRTypeDef
    },
    StructDeclaration {
        struct_name: String,
        body: Vec<MIRTypedBoundName>
    },
    FunctionCall {
        function: TrivialMIRExpr,
        args: Vec<TrivialMIRExpr>,
    },
    Return(MIRExpr),
    EmptyReturn
}

fn make_intermediary(intermediary: i32) -> String {
    return format!("${}", intermediary);
}

//an expression is trivial when it needs basically no effort to 
//check its type. You shouldn't recurse anymore on the expr tree 
fn get_trivial_mir_expr(expr: &Expr) -> Option<TrivialMIRExpr> {
    match expr {
        Expr::IntegerValue(i) => Some(TrivialMIRExpr::IntegerValue(*i)),
        Expr::FloatValue(f) => Some(TrivialMIRExpr::FloatValue(*f)),
        Expr::StringValue(s) => Some(TrivialMIRExpr::StringValue(s.clone())),
        Expr::BooleanValue(b) => Some(TrivialMIRExpr::BooleanValue(*b)),
        Expr::None => Some(TrivialMIRExpr::None),
        Expr::Variable(v) => Some(TrivialMIRExpr::Variable(v.clone())),
        Expr::Parenthesized(_) => panic!("Sanity check: at this point no parenthesized expr should exist"),
        //member accesses are not that simple to prove trivial, let it go through check_if_reducible
       _ => None
    }
}

macro_rules! return_true_if_non_trivial {
    ($e:expr) => {
        let left_is_trivial = get_trivial_mir_expr($e).is_some();
        if !left_is_trivial  {
            return true
        }
    };
}

//If an expression is reducible, you have to call reduce_expr_to_mir_declarations 
//to reduce the expression to a single variable.
fn check_if_reducible(expr: &Expr) -> bool {

    let expr_trivial = get_trivial_mir_expr(expr);
    if expr_trivial.is_some() { return false };


    match expr {
        Expr::FunctionCall(function_call_expr, args_expr) => {
            //a function call is reducible if either side is non-trivial
            //the left side is likely trivial
            return_true_if_non_trivial!(function_call_expr);
            for node in args_expr { return_true_if_non_trivial!(node); }
            return false;
        },
        Expr::BinaryOperation(left, _op, right) => {
            return_true_if_non_trivial!(left);
            return_true_if_non_trivial!(right);
            return false;
        },
        Expr::Array(exprs)  => {
            for e in exprs {
                return_true_if_non_trivial!(e);
            }
            return false;
        },
        Expr::IndexAccess(lhs, index_expr) => {
            return_true_if_non_trivial!(lhs);
            return_true_if_non_trivial!(index_expr);
            return false;
        },
        Expr::MemberAccess(path_expr, _member ) => {
            return_true_if_non_trivial!(path_expr);
            return false;
        },
        Expr::UnaryExpression(_operator, expr) => {
            return_true_if_non_trivial!(expr);
            return false;
        }
        _ => true
    }
}

//this function returns the final expression created, and the number of intermediary variables used
//In the recursive cases, this function should always return a MIRExpr::Trivial
fn reduce_expr_to_mir_declarations(expr: &Expr, mut intermediary: i32, accum: &mut Vec<MIR>, is_reducing: bool) -> (MIRExpr, i32) {
    let trivial_expr = get_trivial_mir_expr(expr);
    match trivial_expr {
        Some(x) => { return (MIRExpr::Trivial(x), 0) },
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

                let (lhs_expr, num_interm) = reduce_expr_to_mir_declarations(function_expr, intermediary, accum, true);
                
                intermediary += num_interm;

                let mut args_exprs = vec![];
                let mut args_interm_used = 0;
                for node in args {
                    let (arg_expr, arg_num_interm) = 
                        reduce_expr_to_mir_declarations(node, intermediary, accum, true);
                    intermediary += arg_num_interm;
                    args_interm_used += arg_num_interm;

                   if let MIRExpr::Trivial(arg) = arg_expr {
                        args_exprs.push(arg);
                    } else {
                        panic!("Function call expression: after reduction, argument should be trivial!");
                    };
                }

                total_used_interm = num_interm + args_interm_used;
                let call_expr = if let MIRExpr::Trivial(name) = lhs_expr {
                    name
                } else {
                    panic!("Function call expression: should be bound to a name!")
                };

                MIRExpr::FunctionCall(call_expr, args_exprs)
           } else {
                let args = args.iter().map(|x| get_trivial_mir_expr(x).unwrap()).collect::<Vec<_>>();
                MIRExpr::FunctionCall(get_trivial_mir_expr(function_expr).unwrap(), args)
           };

           if is_reducing {
                let declare = MIR::Declare {
                    var: make_intermediary(intermediary),
                    typename: MIRTypeDef::Pending,
                    expression: fcall.clone()
                };
                total_used_interm += 1;
                accum.push(declare);
                return (MIRExpr::Trivial(TrivialMIRExpr::Variable(make_intermediary(intermediary))), total_used_interm);
            } else {
                return (fcall, total_used_interm);
            }
           
        }
        full_binop @ Expr::BinaryOperation(lhs, op, rhs) => {
            let mut total_used_interm = 0;
            let binop = if check_if_reducible(full_binop) {
                let (lhs_intermediary, lhs_num_intern) = reduce_expr_to_mir_declarations(lhs, intermediary, accum, true);
                intermediary += lhs_num_intern;

                let (rhs_intermediary, rhs_num_intern) = reduce_expr_to_mir_declarations(rhs, intermediary, accum, true);
                intermediary += rhs_num_intern;
                
                total_used_interm = lhs_num_intern + rhs_num_intern;

                MIRExpr::BinaryOperation(
                    lhs_intermediary.expect_trivial().clone(), 
                    *op, 
                    rhs_intermediary.expect_trivial().clone())
            } else {
                MIRExpr::BinaryOperation(
                    get_trivial_mir_expr(lhs).unwrap(),
                    *op,
                    get_trivial_mir_expr(rhs).unwrap()
                )
            };
                
            if is_reducing {
                let declare = MIR::Declare {
                    var: make_intermediary(intermediary),
                    typename: MIRTypeDef::Pending,
                    expression: binop.clone()
                };
                total_used_interm += 1;
                accum.push(declare);

                return (MIRExpr::Trivial(
                    TrivialMIRExpr::Variable(make_intermediary(intermediary))), total_used_interm);
            } else {
                return (binop, total_used_interm);
            }
        },
        Expr::Variable(var) => {
            return (MIRExpr::Trivial(TrivialMIRExpr::Variable(var.clone())), 0);
        },
        full_array_exp @ Expr::Array(arr_exprs) => {

            let mut total_used_interm = 0;

            let array = if check_if_reducible(full_array_exp) {
                let mut item_exprs = vec![];
                for node in arr_exprs {
                    let (item_expr, item_num_interm) = 
                        reduce_expr_to_mir_declarations(node, intermediary, accum, true);
                    intermediary += item_num_interm;
                    total_used_interm += item_num_interm;

                    if let MIRExpr::Trivial(arg) = item_expr {
                        item_exprs.push(arg);
                    } else {
                        panic!("Array expression item: after reduction, argument should be trivial!");
                    };
                }

                MIRExpr::Array(item_exprs)
            } else {
                let args = arr_exprs.iter().map(|x| get_trivial_mir_expr(x).unwrap()).collect::<Vec<_>>();
                MIRExpr::Array(args)
            };

            if is_reducing {
                let declare = MIR::Declare {
                    var: make_intermediary(intermediary),
                    typename: MIRTypeDef::Pending,
                    expression: array.clone()
                };
                total_used_interm += 1;
                accum.push(declare);
                return (MIRExpr::Trivial(TrivialMIRExpr::Variable(make_intermediary(intermediary))), total_used_interm);
            } else {
                return (array, total_used_interm);
            }
        },
        index_access @ Expr::IndexAccess(obj_expr, index_expr) => {
            let mut total_used_interm = 0;

            let index_access_expr = if check_if_reducible(index_access) {
                
                let (obj_reduced_expr, obj_num_interm) = 
                    reduce_expr_to_mir_declarations(obj_expr, intermediary, accum, true);
                intermediary += obj_num_interm;
                total_used_interm += obj_num_interm;

                let (item_expr, item_num_interm) = 
                    reduce_expr_to_mir_declarations(index_expr, intermediary, accum, true);
                intermediary += item_num_interm;
                total_used_interm += item_num_interm;
                
                let MIRExpr::Trivial(obj_trivial_expr) = obj_reduced_expr else {
                    panic!("Index access, object expression: after reduction, expr should be trivial!");
                };
                let MIRExpr::Trivial(item_trivial_expr) = item_expr else {
                    panic!("Index access, index expression: after reduction, expr should be trivial!");
                };

                MIRExpr::IndexAccess(obj_trivial_expr, item_trivial_expr)
            } else {
                MIRExpr::IndexAccess(
                    get_trivial_mir_expr(obj_expr).unwrap(),
                    get_trivial_mir_expr(index_expr).unwrap())
            };

            if is_reducing {
                let declare = MIR::Declare {
                    var: make_intermediary(intermediary),
                    typename: MIRTypeDef::Pending,
                    expression: index_access_expr.clone()
                };
                total_used_interm += 1;
                accum.push(declare);
                return (MIRExpr::Trivial(TrivialMIRExpr::Variable(make_intermediary(intermediary))), total_used_interm);
            } else {
                return (index_access_expr, total_used_interm);
            }
        },
        unary_expression @ Expr::UnaryExpression(op, expr) => {
            let mut total_used_interm = 0;
            let unaryop = if check_if_reducible(unary_expression) {
                let (expr_intermediary, num_intern) = reduce_expr_to_mir_declarations(expr, intermediary, accum, true);
                intermediary += num_intern;

                total_used_interm = num_intern;

                MIRExpr::UnaryExpression(
                    *op, 
                    expr_intermediary.expect_trivial().clone())
            } else {
                MIRExpr::UnaryExpression(
                    *op,
                    get_trivial_mir_expr(expr).unwrap()
                )
            };
                
            if is_reducing {
                let declare = MIR::Declare {
                    var: make_intermediary(intermediary),
                    typename: MIRTypeDef::Pending,
                    expression: unaryop.clone()
                };
                total_used_interm += 1;
                accum.push(declare);

                return (MIRExpr::Trivial(
                    TrivialMIRExpr::Variable(make_intermediary(intermediary))), total_used_interm);
            } else {
                return (unaryop, total_used_interm);
            }
        }
        exprnode => panic!("Expr to mir not implemented for {:?}", exprnode)
    }
}

pub fn ast_to_mir(ast: &AST, mut intermediary: i32, accum: &mut Vec<MIR>) -> i32 {

    match ast {
        AST::Declare {var, expression} => {

            //expr: we have to decompose the expression into MIR declarations
            //because the assigned/declared expression has to be sufficiently simple
            //to decrease complexity for the other compiler phases (typechecking, type inference)

            //maybe a way to do it is by calling reduce_expr_to_mir_declarations, and the function
            //itself returns a MIRExpr. It will also add to the MIR any declarations needed
            //for the decomposition.
            let (result_expr, num_intermediaries) = reduce_expr_to_mir_declarations(expression, intermediary, accum, false);
            
            let decl_mir = MIR::Declare {
                var: var.name.clone(),
                typename: MIRTypeDef::Unresolved(MIRType::from_ast(&var.name_type)),
                expression: result_expr
            };

            accum.push(decl_mir);

            return num_intermediaries;
        },
        AST::Assign {path, expression} => {
            let (result_expr, num_intermediaries) = reduce_expr_to_mir_declarations(expression, intermediary, accum, false);
            
            let decl_mir = MIR::Assign {
                path: path.clone(),
                expression: result_expr
            };

            accum.push(decl_mir);
            return num_intermediaries;
        },
        AST::DeclareFunction { function_name, parameters, body, return_type} => {

            let mut function_body = vec![];

            for node in body {
                let created_intermediaries = ast_to_mir(node, intermediary, &mut function_body);
                intermediary += created_intermediaries;
            }

            let decl_mir = MIR::DeclareFunction {
                function_name: function_name.clone(),
                parameters: parameters.iter().map(|param| {
                    let name = param.name.clone();
                    return MIRTypedBoundName {
                        name, typename: MIRTypeDef::Unresolved(MIRType::from_ast(&param.name_type))
                    }
                }).collect(),
                body: function_body,
                return_type: match return_type {
                    Some(x) => MIRTypeDef::Unresolved(MIRType::from_ast(x)),
                    None => MIRTypeDef::Unresolved(MIRType::Simple("Void".into()))
                }
            };

            accum.push(decl_mir);
            return 0; //yes, the function decls themselves created intermediaries, but they don't 
            //escape the context
        }
        AST::Root(ast_nodes) => {
            let mut sum_intermediaries = 0;
            for node in ast_nodes {
                let created_intermediaries = ast_to_mir(node, intermediary, accum);
                sum_intermediaries += created_intermediaries;
                intermediary += created_intermediaries;
            }
            return sum_intermediaries;
        }
        AST::Return(expr) => {
            match expr {
                None => {
                    accum.push(MIR::EmptyReturn);
                    return 0;
                },
                Some(e) => {
                    let (result_expr, num_intermediaries) = reduce_expr_to_mir_declarations(e, intermediary, accum, false);
                    accum.push(MIR::Return(result_expr));
                    return num_intermediaries;
                }
            }
        }
        AST::StructDeclaration {struct_name, body} => {
            let fields = body.iter().map(|field| {
                return MIRTypedBoundName { 
                    name: field.name.clone(), 
                    typename: MIRTypeDef::Unresolved(MIRType::from_ast(&field.name_type)) };
            });
            accum.push(MIR::StructDeclaration{ struct_name: struct_name.clone(), body: fields.collect()});
            return 0;
        }
        AST::StandaloneExpr(expr) => {

            let Expr::FunctionCall(_, _) = expr else {
                panic!("Can only lower function call standalone expr");
            };

            let (result_expr, num_intermediaries) = reduce_expr_to_mir_declarations(expr, intermediary, accum, false);
            let MIRExpr::FunctionCall(function, args) = result_expr else {
                panic!("Lowering of function call returned invalid result: {:?}", result_expr);
            };
            accum.push(MIR::FunctionCall {function, args});
            return num_intermediaries;
        }
        ast => panic!("Not implemted MIR for {:?}", ast)
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::*;
    use crate::ast::parser::*;
    use crate::ast::lexer::*;

    //Parses a single expression
    fn parse(source: &str) -> Vec<MIR> {
        let tokens = crate::ast::lexer::tokenize(source);
        //println!("Tokens: {:?}", tokens);
        let ast = crate::ast::parser::parse_ast(tokens.unwrap());

        let root = crate::ast::parser::AST::Root(ast);
        let mut result = vec![];
        mir::ast_to_mir(&root, 0, &mut result);
        return result;
    }

    #[test]
    fn mir_multiline_code() {
        let result = parse(
            "
x = 'abc' + 'cde'
y = x + str(True)",
        );
        
        let expected = vec![
            MIR::Assign { 
                path: vec!["x".into()], 
                expression: MIRExpr::BinaryOperation(
                    TrivialMIRExpr::StringValue("abc".into()), 
                    Operator::Plus, 
                    TrivialMIRExpr::StringValue("cde".into())) 
            }, 
            MIR::Declare { 
                var: "$0".into(), 
                typename: None, 
                expression: MIRExpr::FunctionCall(
                    TrivialMIRExpr::Variable("str".into()), 
                    vec![TrivialMIRExpr::BooleanValue(true)]) 
            }, 
            MIR::Assign { 
                path: vec!["y".into()], 
                expression: MIRExpr::BinaryOperation(
                    TrivialMIRExpr::Variable("x".into()), 
                    Operator::Plus, 
                    TrivialMIRExpr::Variable("$0".into())) 
            }
        ];

        assert_eq!(expected, result);
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
        
        let debug_view_expected = "[DeclareFunction { function_name: \"main\", parameters: [MIRTypedBoundName { name: \"args\", typename: Generic(\"List\", [Simple(\"String\")]) }], body: [Declare { var: \"$0\", typename: None, expression: FunctionCall(Variable(\"my_function\"), [IntegerValue(99), IntegerValue(999)]) }, Declare { var: \"minus\", typename: Some(Simple(\"i32\")), expression: UnaryExpression(Minus, Variable(\"$0\")) }, Declare { var: \"$1\", typename: None, expression: UnaryExpression(Minus, IntegerValue(3)) }, Assign { path: [\"numbers\"], expression: Array([IntegerValue(1), IntegerValue(2), Variable(\"$1\"), Variable(\"minus\")]) }, Assign { path: [\"r1\"], expression: FunctionCall(Variable(\"my_function\"), [IntegerValue(1), IntegerValue(2)]) }, Assign { path: [\"r2\"], expression: FunctionCall(Variable(\"my_function2\"), [IntegerValue(3), IntegerValue(4)]) }, Declare { var: \"$2\", typename: None, expression: IndexAccess(Variable(\"numbers\"), IntegerValue(1)) }, Declare { var: \"$3\", typename: None, expression: IndexAccess(Variable(\"numbers\"), IntegerValue(2)) }, Assign { path: [\"r3\"], expression: FunctionCall(Variable(\"my_function\"), [Variable(\"$2\"), Variable(\"$3\")]) }, Declare { var: \"$4\", typename: None, expression: BinaryOperation(Variable(\"r1\"), Plus, Variable(\"r2\")) }, Declare { var: \"$5\", typename: None, expression: BinaryOperation(Variable(\"$4\"), Plus, Variable(\"r3\")) }, FunctionCall { function: Variable(\"print\"), args: [Variable(\"$5\")] }], return_type: Simple(\"Void\") }, DeclareFunction { function_name: \"my_function\", parameters: [MIRTypedBoundName { name: \"arg1\", typename: Simple(\"i32\") }, MIRTypedBoundName { name: \"arg2\", typename: Simple(\"i32\") }], body: [Declare { var: \"$0\", typename: None, expression: BinaryOperation(Variable(\"arg1\"), Multiply, Variable(\"arg2\")) }, Declare { var: \"$1\", typename: None, expression: BinaryOperation(Variable(\"arg2\"), Minus, Variable(\"arg1\")) }, Return(BinaryOperation(Variable(\"$0\"), Divide, Variable(\"$1\")))], return_type: Simple(\"i32\") }, DeclareFunction { function_name: \"my_function2\", parameters: [MIRTypedBoundName { name: \"arg1\", typename: Simple(\"i32\") }, MIRTypedBoundName { name: \"arg2\", typename: Simple(\"i32\") }], body: [Declare { var: \"$0\", typename: None, expression: BinaryOperation(Variable(\"arg2\"), Plus, IntegerValue(1)) }, Declare { var: \"result1\", typename: Some(Simple(\"i32\")), expression: FunctionCall(Variable(\"my_function\"), [Variable(\"arg1\"), Variable(\"$0\")]) }, Declare { var: \"$1\", typename: None, expression: BinaryOperation(Variable(\"arg2\"), Multiply, IntegerValue(9)) }, Assign { path: [\"result2\"], expression: FunctionCall(Variable(\"pow\"), [Variable(\"arg1\"), Variable(\"$1\")]) }, Return(FunctionCall(Variable(\"my_function\"), [Variable(\"result1\"), Variable(\"result2\")]))], return_type: Simple(\"i32\") }]";

        assert_eq!(debug_view_expected, format!("{:?}", result));
    }
}