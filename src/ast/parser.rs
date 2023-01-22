use crate::ast::lexer::{Operator, Token};
use crate::commons::float::FloatLiteral;

use super::lexer::{InternedString};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    IntegerValue(i128),
    FloatValue(FloatLiteral),
    StringValue(InternedString),
    BooleanValue(bool),
    NoneExpr,
    Variable(InternedString),
    FunctionCall(Box<Expr>, Vec<Expr>),
    IndexAccess(Box<Expr>, Box<Expr>),
    BinaryOperation(Box<Expr>, Operator, Box<Expr>),
    Parenthesized(Box<Expr>),
    UnaryExpression(Operator, Box<Expr>),
    MemberAccess(Box<Expr>, InternedString),
    Array(Vec<Expr>),
    //maybe there could be a syntax to specify the type of the array
    //ex: instead of just x = [1,2,3] it could be x = [1, 2, 3] array<i32>
    //or like sum = array<i32>[].sum() would return 0
    //x: array<i32> = [] should work too
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTIfStatement {
    pub expression: Expr,
    pub statements: Vec<AST>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ASTType {
    Simple(InternedString),
    Generic(InternedString, Vec<ASTType>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeBoundName {
    pub name: InternedString,
    pub name_type: ASTType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AST {
    StandaloneExpr(Expr),
    Assign {
        path: Vec<InternedString>,
        expression: Expr,
    },
    Declare {
        var: TypeBoundName,
        expression: Expr,
    },
    IfStatement {
        true_branch: ASTIfStatement,
        elifs: Vec<ASTIfStatement>,
        final_else: Option<Vec<AST>>,
    },
    WhileStatement {
        expression: Expr,
        body: Vec<AST>,
    },
    ForStatement {
        item_name: InternedString,
        list_expression: Expr,
        body: Vec<AST>,
    },
    StructDeclaration {
        struct_name: InternedString,
        type_parameters: Vec<InternedString>,
        body: Vec<TypeBoundName>,
    },
    DeclareFunction {
        function_name: InternedString,
        parameters: Vec<TypeBoundName>,
        body: Vec<AST>,
        return_type: Option<ASTType>,
    },
    Break,
    Intrinsic,
    Return(Option<Expr>),
    Raise(Expr),
    Root(Vec<AST>),
}

impl Expr {
    fn new_int(i: i128) -> Box<Self> {
        Box::new(Self::IntegerValue(i))
    }
    fn new_float(f: FloatLiteral) -> Box<Self> {
        Box::new(Self::FloatValue(f))
    }
}

impl From<f64> for Box<Expr> {
    fn from(w: f64) -> Self {
        Expr::new_float(w.into())
    }
}

impl From<i128> for Box<Expr> {
    fn from(w: i128) -> Self {
        Expr::new_int(w)
    }
}

fn precedence(o: Operator) -> u32 {
    match o {
        Operator::Multiply | Operator::Divide => 100,
        _ => 1,
    }
}

fn clean_parens(expr: Expr) -> Expr {
    match expr {
        Parenthesized(e) => clean_parens(*e),
        UnaryExpression(op, e) => UnaryExpression(op, Box::new(clean_parens(*e))),
        BinaryOperation(left, op, right) => {
            let left_clean = Box::new(clean_parens(*left));
            let right_clean = Box::new(clean_parens(*right));
            BinaryOperation(left_clean, op, right_clean)
        }
        _ => expr,
    }
}

pub struct Parser {
    parsing_state: Vec<ParsingState>,
    tokens: Vec<Token>,
}

struct ParsingState {
    operator_stack: Vec<Operator>,
    operand_stack: Vec<Expr>,
    index: usize,
    current_indent: usize,
}

#[derive(Debug, Clone)]
pub enum ParsingError {
    ExprError(String),
    //TypeBoundMissingTypeSpecifier,
    TypeBoundExpectedColonAfterFieldName,
}

macro_rules! parse_guard {
    ($parser:expr, $pattern:pat_param) => {
        parse_guard!($parser, $pattern, None)
    };
    ($parser:expr, $pattern:pat_param, $result:expr) => {
        if let $pattern = $parser.cur() {
            $parser.next();
            if (!$parser.can_go()) {
                return $result;
            }
        } else {
            return $result;
        }
    };
}

macro_rules! expect_token {
    ($parser:expr, $token:tt::$pattern:tt) => {
        if let $token::$pattern = $parser.cur() {
            $parser.next();
        } else {
            panic!(
                "Expected {}, got {}",
                ($token::$pattern).name(),
                $parser.cur().name()
            );
        }
    };
    ($parser:expr, $token:tt::$pattern:tt, $msg:expr) => {
        if let $token::$pattern = $parser.cur() {
            $parser.next();
        } else {
            panic!($msg, $parser.cur().name());
        }
    };
}

macro_rules! expect_identifier {
    ($parser:expr, $role:expr) => {{
        if let Token::Identifier(id) = *$parser.cur() {
            $parser.next();
            id
        } else {
            panic!("Expected {}, got {}", $role, $parser.cur().name());
        }
    }};
}

macro_rules! expect_colon_newline {
    ($parser:expr) => {
        expect_token!($parser, Token::Colon);
        expect_token!($parser, Token::NewLine);
    };
}

macro_rules! indented {
    ($parser:expr, $indented_parsing:block) => {{
        $parser.increment_expected_indent();
        let returned = $indented_parsing;
        $parser.decrement_expected_indent();
        returned
    }};
}
use Expr::*;
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            parsing_state: vec![ParsingState {
                index: 0,
                operator_stack: vec![],
                operand_stack: vec![],
                current_indent: 0,
            }],
            tokens,
        }
    }

    fn new_stack(&mut self) {
        let cur_indent = self.parsing_state.last().unwrap().current_indent;
        self.parsing_state.push(ParsingState {
            index: self.parsing_state.last().unwrap().index,
            operator_stack: vec![],
            operand_stack: vec![],
            current_indent: cur_indent,
        });
    }

    fn pop_stack(&mut self) -> ParsingState {
        self.parsing_state.pop().unwrap()
    }

    fn next(&mut self) {
        self.advance(1);
    }

    fn advance(&mut self, offset: isize) {
        let index = self.parsing_state.last().unwrap().index as isize;
        self.parsing_state.last_mut().unwrap().index = (index + offset) as usize;
    }

    fn increment_expected_indent(&mut self) {
        let indent = self.parsing_state.last().unwrap().current_indent as isize;
        self.parsing_state.last_mut().unwrap().current_indent = (indent + 1) as usize;
    }

    fn decrement_expected_indent(&mut self) {
        let indent = self.parsing_state.last().unwrap().current_indent as isize;
        self.parsing_state.last_mut().unwrap().current_indent = (indent - 1) as usize;
    }

    fn get_expected_indent(&mut self) -> usize {
        return self.parsing_state.last_mut().unwrap().current_indent;
    }

    fn set_cur(&mut self, parsing_state: &ParsingState) {
        self.parsing_state.last_mut().unwrap().index = parsing_state.index;
        self.parsing_state.last_mut().unwrap().current_indent = parsing_state.current_indent;
    }

    fn cur(&self) -> &Token {
        self.cur_offset(0)
    }

    fn prev_token(&self) -> Option<&Token> {
        self.cur_offset_opt(-1)
    }

    fn cur_offset(&self, offset: isize) -> &Token {
        return self.cur_offset_opt(offset).unwrap();
    }

    fn cur_offset_opt(&self, offset: isize) -> Option<&Token> {
        let index = self.parsing_state.last().unwrap().index as isize + offset;
        self.tokens.get(index as usize)
    }

    fn is_last(&self) -> bool {
        self.parsing_state.last().unwrap().index == self.tokens.len() - 1
    }

    fn is_not_end(&self) -> bool {
        self.parsing_state.last().unwrap().index < self.tokens.len()
    }

    fn can_go(&self) -> bool {
        self.is_not_end() && !self.cur_is_newline()
    }

    fn cur_is_newline(&self) -> bool {
        matches!(self.cur(), Token::NewLine)
    }

    fn push_operand(&mut self, token: Expr) {
        self.parsing_state
            .last_mut()
            .unwrap()
            .operand_stack
            .push(token);
    }

    fn push_operator(&mut self, operator: Operator) {
        self.parsing_state
            .last_mut()
            .unwrap()
            .operator_stack
            .push(operator);
    }

    fn operand_stack(&self) -> &[Expr] {
        return &self.parsing_state.last().unwrap().operand_stack;
    }

    fn operator_stack(&self) -> &[Operator] {
        return &self.parsing_state.last().unwrap().operator_stack;
    }

    fn operand_stack_mut(&mut self) -> &mut Vec<Expr> {
        return &mut self.parsing_state.last_mut().unwrap().operand_stack;
    }

    fn operator_stack_mut(&mut self) -> &mut Vec<Operator> {
        return &mut self.parsing_state.last_mut().unwrap().operator_stack;
    }

    pub fn parse_assign(&mut self) -> Option<AST> {
        let mut path = vec![];
        while let Token::Identifier(id) = self.cur() {
            path.push(*id);
            if self.is_last() {
                return None;
            }

            self.next();

            if let Token::MemberAccessor = self.cur() {
                self.next();
            }
        }
        if !self.can_go() {
            return None;
        }
        if let Token::Assign = self.cur() {
            self.next();
            let expr = self.parse_expr().expect("Expected expression after assign");
            Some(AST::Assign {
                path: path.into(),
                expression: expr.resulting_expr,
            })
        } else {
            None
        }
    }

    pub fn parse_assign_typed(&mut self) -> Option<AST> {
        let decl = self.parse_type_bound_name();

        if let Ok(Some(typed_var_decl)) = decl {
            //no need to do .next here, parse_type_bound_name already does a .next()
            self.next();
            let cur = self.cur();
            // println!("{:?}", cur);
            if let Token::Assign = cur {
                self.next();
                let expr = self.parse_expr().expect("Expected expression after assign");
                Some(AST::Declare {
                    var: typed_var_decl,
                    expression: expr.resulting_expr,
                })
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn parse_if_statement(&mut self) -> Option<AST> {
        parse_guard!(self, Token::IfKeyword);

        let expr = self.parse_expr().expect("Expected expr").resulting_expr;

        expect_colon_newline!(self);

        let mut if_statement = indented!(self, {
            let ast = self.parse_ast();
            AST::IfStatement {
                true_branch: ASTIfStatement {
                    expression: expr,
                    statements: ast,
                },
                elifs: vec![],
                final_else: None,
            }
        });

        self.new_stack();

        //lets try getting the else statement:
        let cur_identation = self.get_expected_indent();
        let identation_else = self.skip_whitespace_newline();

        if !self.can_go() || identation_else != cur_identation {
            self.pop_stack();
            return Some(if_statement);
        }

        if let Token::ElseKeyword = self.cur() {
            self.next();
            expect_colon_newline!(self);

            if_statement = indented!(self, {
                let ast = self.parse_ast();
                match if_statement {
                    AST::IfStatement {
                        true_branch, elifs, ..
                    } => AST::IfStatement {
                        true_branch,
                        elifs,
                        final_else: Some(ast),
                    },
                    _ => panic!("Unrecognized ast on if else parsing"),
                }
            });
        } else {
            self.pop_stack();
        }
        Some(if_statement)
    }

    pub fn parse_structdef(&mut self) -> Option<AST> {
        parse_guard!(self, Token::StructDef);

        let struct_name = expect_identifier!(self, "struct name");

        expect_colon_newline!(self);

        let struct_def = indented!(self, {
            let mut fields = vec![];

            loop {
                self.skip_whitespace_newline();
                if !self.can_go() {
                    break;
                }
                let Token::Identifier(_) = self.cur() else { break; };
                let parsed = self.parse_type_bound_name().unwrap().unwrap();
                fields.push(parsed);

                if !self.can_go() {
                    break;
                }

                self.next();
                let Token::NewLine = self.cur() else { break; };

                self.next();
                if !self.can_go() {
                    break;
                }
                //if a second newline is found, then the struct declaration is found
                if let Token::NewLine = self.cur() {
                    break;
                }
            }

            AST::StructDeclaration {
                struct_name,
                type_parameters: vec![],
                body: fields,
            }
        });

        Some(struct_def)
    }

    pub fn parse_while_statement(&mut self) -> Option<AST> {
        parse_guard!(self, Token::WhileKeyword);

        let expr = self.parse_expr().expect("Expected expr").resulting_expr;

        expect_colon_newline!(self);

        Some(indented!(self, {
            let ast = self.parse_ast();
            AST::WhileStatement {
                expression: expr,
                body: ast,
            }
        }))
    }

    pub fn parse_for_statement(&mut self) -> Option<AST> {
        parse_guard!(self, Token::ForKeyword);
        let variable_name = expect_identifier!(self, "variable name in for loop");
        expect_token!(self, Token::InKeyword);

        let expr = self
            .parse_expr()
            .expect("Expected expr after in keyword in for expression")
            .resulting_expr;

        expect_colon_newline!(self);

        Some(indented!(self, {
            let ast = self.parse_ast();

            AST::ForStatement {
                item_name: variable_name,
                list_expression: expr,
                body: ast,
            }
        }))
    }

    pub fn parse_type_name(&mut self) -> Option<ASTType> {
        let Token::Identifier(type_name) = *self.cur() else {
            return None;
        };

        if !self.can_go() {
            return Some(ASTType::Simple(type_name));
        }

        let peek_next = self.cur_offset(1);

        let Token::Operator(Operator::Less) = peek_next else {
            return Some(ASTType::Simple(type_name));
        };

        self.next(); //commits the peek_next
        self.next();

        let Token::Identifier(generic_name) = *self.cur() else {
            panic!("For now we dont have proper error handling for mistakes in generic types, cur = {:?}", self.cur())
         };
        self.next();

        if let Token::Operator(Operator::Greater) = self.cur() {
            Some(ASTType::Generic(
                type_name,
                vec![ASTType::Simple(generic_name)],
            ))
        } else {
            panic!("For now we don't suport more than 1 generic argument (i'm lazy).")
        }
    }

    //Tries to parse a bound name with its type, for instance var: i32
    //leaves cursor in the next token after the type
    pub fn parse_type_bound_name(
        &mut self,
    ) -> Result<Option<TypeBoundName>, ParsingError> {
        let Token::Identifier(name) = *self.cur() else { return Ok(None); };
        self.next();

        if !self.can_go() {
            return Ok(None);
        }

        let Token::Colon = self.cur() else {
            return Err(ParsingError::TypeBoundExpectedColonAfterFieldName);
        };
        self.next();

        let typename = self.parse_type_name();

        match typename {
            Some(x) => Ok(Some(TypeBoundName { name, name_type: x })),
            None => Ok(None),
        }
    }

    pub fn parse_def_statement(&mut self) -> Option<AST> {
        parse_guard!(self, Token::DefKeyword);
        let function_name = expect_identifier!(self, "function name");

        expect_token!(self, Token::OpenParen);

        let mut params: Vec<TypeBoundName> = vec![];

        while let Token::Identifier(_) = self.cur() {
            let param = self.parse_type_bound_name().unwrap().unwrap();

            params.push(param);
            self.next();
            if let Token::Comma = self.cur() {
                self.next();
            } else {
                break;
            }
        }

        if let Token::CloseParen = self.cur() {
            self.next();
        } else {
            panic!("Expected close paren after parameters in function declaration")
        }

        let mut return_type: Option<ASTType> = None;

        if let Token::ArrowRight = self.cur() {
            self.next();

            return_type = self.parse_type_name();
            assert!(
                return_type.is_some(),
                "Expected type name after arrow right on function declaration"
            );
            self.next();
        }

        expect_token!(
            self,
            Token::Colon,
            "Expected colon paren after parameters and return type in function declaration, got {}"
        );

        Some(indented!(self, {
            let ast = self.parse_ast();

            AST::DeclareFunction {
                function_name,
                parameters: params,
                body: ast,
                return_type,
            }
        }))
    }

    pub fn parse_break(&mut self) -> Option<AST> {
        let tok = self.cur();
        if let Token::BreakKeyword = tok {
            self.next();
            return Some(AST::Break);
        }

        None
    }

    pub fn parse_return(&mut self) -> Option<AST> {
        let tok = self.cur();
        if let Token::ReturnKeyword = tok {
            self.next();
            if self.can_go() {
                let expr = self.parse_expr().ok().unwrap();
                return Some(AST::Return(Some(expr.resulting_expr)));
            }
            return Some(AST::Return(None));
        }
        None
    }

    pub fn parse_intrinsic(&mut self) -> Option<AST> {
        let tok = self.cur();
        if let Token::IntrinsicKeyword = tok {
            self.next();
            if !self.can_go() {
                return Some(AST::Intrinsic);
            } else {
                panic!("Intrinsic must be the only content of the method if it's present")
            }
        }
        None
    }

    pub fn parse_raise(&mut self) -> Option<AST> {
        let tok = self.cur();
        if let Token::RaiseKeyword = tok {
            self.next();
            if self.can_go() {
                let expr = self.parse_expr().ok().unwrap();
                return Some(AST::Raise(expr.resulting_expr));
            }
            panic!("Must inform expression with raise keyword")
        }
        None
    }

    pub fn parse_standalone_expr(&mut self) -> Option<AST> {
        self.parse_expr()
            .ok()
            .map(|expr| AST::StandaloneExpr(expr.resulting_expr))
    }

    //returns the identation level until the first non-whitespace token
    //final state of this function is right at newline, before the identations
    fn skip_whitespace_newline(&mut self) -> usize {
        let mut identation_level = 0;
        while self.is_not_end() {
            match self.cur() {
                Token::NewLine => {
                    identation_level = 0;
                }
                Token::Indentation => identation_level += 1,
                _ => {
                    break;
                }
            }
            self.next();
        }
        identation_level
    }

    pub fn parse_ast(&mut self) -> Vec<AST> {
        let mut parsed_successfully: bool;
        let mut results = vec![];

        macro_rules! try_parse {
            ($name:expr, $parse_method:tt) => {
                if !parsed_successfully {
                    self.new_stack();
                    if let Some(parsed_result) = self.$parse_method() {
                        results.push(parsed_result);
                        parsed_successfully = true;
                        let popped = self.pop_stack();
                        //correct indentation found: commit
                        self.set_cur(&popped);
                        assert!(
                            !self.is_not_end() || self.cur_is_newline(),
                            "Newline or EOF expected after {}, got {:?}",
                            $name,
                            self.cur()
                        );
                    } else {
                        self.pop_stack();
                    }
                }
            };
        }

        loop {
            parsed_successfully = false;
            self.new_stack();

            let last_identation = self.skip_whitespace_newline();
            let expected_indent = self.get_expected_indent();

            if last_identation == expected_indent {
                let popped = self.pop_stack();
                self.set_cur(&popped);
            } else {
                self.pop_stack();
                return results;
            }

            if !self.is_not_end() {
                return results;
            }

            try_parse!("struct", parse_structdef);
            try_parse!("assign", parse_assign);
            try_parse!("assign with type", parse_assign_typed);
            try_parse!("if block", parse_if_statement);
            try_parse!("while block", parse_while_statement);
            try_parse!("for block", parse_for_statement);
            try_parse!("function definition", parse_def_statement);
            try_parse!("break", parse_break);
            try_parse!("return", parse_return);
            try_parse!("intrinsic", parse_intrinsic);
            try_parse!("raise", parse_raise);
            try_parse!("standalone expression", parse_standalone_expr);

            assert!(parsed_successfully, "Could not parse code");

            let is_end = !self.is_not_end();
            if is_end {
                break;
            };

            if self.cur_is_newline() {
                continue;
            };
            panic!(
                "is not end but is also not newline, cur = {:?}, parsed = {:?}",
                self.cur(),
                results
            );
        }

        results
    }

    fn index_access_helper(
        &mut self,
        expr_list_or_array: Expr,
    ) -> Result<Expr, ParsingError> {
        if let Token::CloseParen = self.cur() {
            panic!("Invalid syntax: must inform index value");
        }

        self.new_stack();
        let list_of_exprs = self.parse_comma_sep_list_expr();

        match list_of_exprs {
            //try parse stuff
            Ok(expressions) => {
                //commit the result
                let popped = self.pop_stack();
                let mut resulting_exprs = expressions.resulting_expr_list;
                assert!(
                    resulting_exprs.len() <= 1,
                    "Invalid syntax: must inform only one index"
                );

                let fcall = IndexAccess(
                    Box::new(expr_list_or_array),
                    Box::new(resulting_exprs.pop().unwrap()),
                );

                self.set_cur(&popped);

                Ok(fcall)
            }
            Err(e) => {
                eprintln!("Failed parsing exprssion: {:?}", e);
                Err(e)
            }
        }
    }

    fn function_call_helper(
        &mut self,
        expr_callable: Expr,
    ) -> Result<Expr, ParsingError> {
        if let Token::CloseParen = self.cur() {
            return Ok(FunctionCall(Box::new(expr_callable), vec![]));
        }

        self.new_stack();
        let list_of_exprs = self.parse_comma_sep_list_expr();

        match list_of_exprs {
            //try parse stuff
            Ok(expressions) => {
                //commit the result
                let popped = self.pop_stack();
                let resulting_exprs = expressions.resulting_expr_list;

                let fcall = FunctionCall(Box::new(expr_callable), resulting_exprs);

                self.set_cur(&popped);

                Ok(fcall)
            }
            Err(e) => {
                eprintln!("Failed parsing exprssion: {:?}", e);
                Err(e)
            }
        }
    }

    #[allow(clippy::too_many_lines)] //no patience to fix this, expr parsing is messy and I will not touch it
    pub fn parse_expr(&mut self) -> Result<ParseExpressionResult, ParsingError> {
        loop {
            if !self.can_go() {
                break;
            }
            let mut was_operand = false;
            let mut not_part_of_expr = false;
            //if there is an open paren, we collect all the tokens for this open paren
            //and parse the sub-expression recursively
            {
                let tok = self.cur();
                let prev_token = self.prev_token().cloned();
                match tok {
                    Token::OpenParen => {
                        //move to the first token, out of the OpenParen
                        //when parsing an open paren, it can either be a parenthesized expression, or a function call.
                        //A parenthesized expression can appear everywhere, and so does a function call.
                        //However, if the previous token was an operator, then the open paren can only be valid if we are
                        //parsing a parenthesized expression.
                        //A function call open paren can only appear in the following scenarios:
                        // - An expression that hasn't been fully parsed yet like `an_identifier` but next token is `(`
                        // - `"a literal token"` and then `(` is also a valid function call. It just doesn't work in vm. Same goes for int and float literals
                        // - `some_map['mapKey']` and then `(` is also valid, will load the function from map and call it
                        // - `some_array[index]()` similarly
                        // - `function_call()` and then `(` would work if the function returns another function
                        let mut could_be_fcall = true;

                        if let Some(Token::Operator(_)) = prev_token {
                            could_be_fcall = false;
                        }
                        if prev_token.is_none() {
                            could_be_fcall = false;
                        }
                        if self.operand_stack().is_empty() {
                            could_be_fcall = false;
                        }
                        if could_be_fcall {
                            //when we parse a funcion, we need to parse its arguments as well.
                            //however, the list of arguments is a list of expressions, and those expressions might be
                            //function calls as well.
                            //like fcall(fcall(fcall(1, fcall(2)), fcall2(3, fcall())))....)
                            //Unlike parenthesized expressions, in this case I cannot just fetch everything between
                            //the parens because there are commas separating the arguments. I can't also fetch all
                            //the tokens until I find a comma, because i would have a list of tokens containig
                            //[fcall(fcall(fcall(1,] as my list of tokens to parse.
                            //I will need to parse lists of expressions for other stuff as well, like array items and tuple items.
                            //Perhaps a better strategy is to make it a core function of the parser: Parse list of expressions instead of just a single expr.
                            //And we need the parse function to be more tolerant of tokens outside of expressions: if it finds something that doesn't look
                            //like it's part of an expression, then maybe we should just understand that the expression has been finished.
                            //Let's ensure that we have no operators pending
                            if !self.operand_stack().is_empty() {
                                self.next();
                                let current_expr = self.operand_stack_mut().pop().unwrap();

                                //If the top expression is a binary operation, then we need to do a little bit of surgery.
                                //The left side stays the same, but the right side needs adjustments
                                //because the parenthesis invokes a call over the result of the whole right-side expr.
                                match current_expr {
                                    BinaryOperation(left, op, right) => {
                                        let right_side_fcall = self.function_call_helper(*right)?;
                                        self.push_operand(BinaryOperation(
                                            left,
                                            op,
                                            Box::new(right_side_fcall),
                                        ));
                                        was_operand = true;
                                    }
                                    expr => {
                                        let fcall = self.function_call_helper(expr)?;
                                        self.push_operand(fcall);
                                        was_operand = true;
                                    }
                                }
                            }
                        } else {
                            self.new_stack(); //new parsing stack/state
                            self.next();
                            match self.parse_expr() {
                                //try parse stuff
                                Ok(expr_result) => {
                                    //worked
                                    //commit the result
                                    let resulting_expr = expr_result.resulting_expr;
                                    let parenthesized = Parenthesized(Box::new(resulting_expr));
                                    let popped = self.pop_stack();
                                    self.push_operand(parenthesized);
                                    self.set_cur(&popped);
                                    was_operand = true;
                                }
                                Err(e) => {
                                    eprintln!("Failed parsing exprssion: {:?}", e);
                                    return Err(e);
                                }
                            }
                        }
                    }
                    Token::OpenArrayBracket => {
                        let mut could_be_indexing = true;

                        if let Some(Token::Operator(_)) = prev_token {
                            //in this case, it could be operators being applied to 2 lists, like a concat
                            could_be_indexing = false;
                        }
                        if prev_token.is_none() {
                            //most certainly will be a new list
                            could_be_indexing = false;
                        }
                        if self.operand_stack().is_empty() {
                            //no expression to access array onto
                            could_be_indexing = false;
                        }
                        if could_be_indexing {
                            //in the future,
                            //this part of the code will also suport range slicing
                            //like list[2:3]
                            //for now it doesn't

                            if !self.operand_stack().is_empty() {
                                self.next();
                                let current_expr = self.operand_stack_mut().pop().unwrap();

                                //If the top expression is a binary operation, then we need to do a little bit of surgery.
                                //The left side stays the same, but the right side needs adjustments
                                //because the parenthesis invokes an array access
                                //over the result of the whole right-side expr.

                                match current_expr {
                                    BinaryOperation(left, op, right) => {
                                        let right_side_index_access =
                                            self.index_access_helper(*right)?;
                                        self.push_operand(BinaryOperation(
                                            left,
                                            op,
                                            Box::new(right_side_index_access),
                                        ));
                                        was_operand = true;
                                    }
                                    expr => {
                                        let index_access = self.index_access_helper(expr)?;
                                        self.push_operand(index_access);
                                        was_operand = true;
                                    }
                                }
                            }
                        } else {
                            //this is parsing an array literal!
                            self.new_stack(); //new parsing stack/state
                            self.next(); //move to the first token, out of the open array
                            if let Token::CloseArrayBracket = self.cur() {
                                self.push_operand(Array(vec![]));
                                was_operand = true;
                            } else {
                                let list_of_exprs = self.parse_comma_sep_list_expr();
                                match list_of_exprs {
                                    //try parse stuff
                                    Ok(expressions) => {
                                        //worked
                                        //commit the result
                                        let popped = self.pop_stack();
                                        let resulting_exprs = expressions.resulting_expr_list;
                                        self.push_operand(Array(resulting_exprs));
                                        self.set_cur(&popped);
                                    }
                                    Err(e) => {
                                        eprintln!("Failed parsing exprssion: {:?}", e);
                                        return Err(e);
                                    }
                                }
                            }
                        }
                    }
                    Token::Identifier(identifier_str) => {
                        self.push_operand(Variable(*identifier_str));
                        was_operand = true;
                    }
                    Token::MemberAccessor => {
                        //next token should be an identifier
                        self.next();
                        let popped = self.operand_stack_mut().pop();
                        let cur_token = self.cur();
                        if let Token::Identifier(name) = cur_token {
                            let cur_expr = popped.unwrap();
                            let member_access_expr = MemberAccess(Box::new(cur_expr), *name);
                            self.push_operand(member_access_expr);
                            was_operand = true;
                        } else {
                            return Err(ParsingError::ExprError(
                                "Failed parsing member acessor".into(),
                            ));
                        }
                    }
                    Token::LiteralInteger(i) => {
                        self.push_operand(IntegerValue(*i));
                        was_operand = true;
                    }
                    Token::LiteralFloat(f) => {
                        self.push_operand(FloatValue(*f));
                        was_operand = true;
                    }
                    Token::LiteralString(f) => {
                        //@TODO cloneless: store the literals somewhere it can be fetched by ref?
                        self.push_operand(StringValue(*f));
                        was_operand = true;
                    }
                    Token::None => {
                        self.push_operand(NoneExpr);
                        was_operand = true;
                    }
                    Token::True => {
                        self.push_operand(BooleanValue(true));
                        was_operand = true;
                    }
                    Token::False => {
                        self.push_operand(BooleanValue(false));
                        was_operand = true;
                    }
                    Token::Operator(o) => self.push_operator(*o),
                    _ => {
                        //close paren, close bracket are not part of expr, as in "they're just syntax"
                        not_part_of_expr = true;
                    }
                }
            }
            if not_part_of_expr {
                break;
            }

            self.next();
            if self.can_go() && Token::MemberAccessor == *self.cur() {
                continue;
            }

            //if the token is an open paren... then wait a minute, could be a function call!
            //same thing for an open bracket! could be an array!
            //try parse it first!

            if self.can_go() {
                if let Token::OpenArrayBracket = self.cur() {
                    continue;
                }
                if let Token::OpenParen = self.cur() {
                    continue;
                }
            }

            if was_operand {
                //base case: there is only an operator and an operand, like "-1"
                if self.operand_stack().len() == 1 && self.operator_stack().len() == 1 {
                    let last_operand = self.operand_stack_mut().pop().unwrap();
                    let op = self.operator_stack_mut().pop().unwrap();
                    self.push_operand(UnaryExpression(op, Box::new(last_operand)));
                }
                //repeat case: 2 * -----2 or even 2 * -2, consume all the minus signals
                else if self.operator_stack().len() > 1 && self.operand_stack().len() == 2 {
                    while self.operator_stack().len() > 1 {
                        let last_operand = self.operand_stack_mut().pop().unwrap();
                        let op = self.operator_stack_mut().pop().unwrap();

                        self.push_operand(UnaryExpression(op, Box::new(last_operand)));
                    }
                }
                //if we enter the previous if, we will have an operand, operator, and an unary exp operand

                let has_sufficient_operands = self.operand_stack().len() >= 2;
                let has_pending_operators = !self.operator_stack().is_empty();

                if has_sufficient_operands && has_pending_operators {
                    let rhs_root = self.operand_stack_mut().pop().unwrap();
                    let lhs_root = self.operand_stack_mut().pop().unwrap();

                    //sanity check: only one of them is a binary operation, or none of them.
                    //if both are binary exprs, we will lose some data during this transformation.
                    //however, this shouldn't happen
                    assert!(
                        !(matches!(lhs_root, BinaryOperation(..))
                            && matches!(rhs_root, BinaryOperation(..)))
                    );

                    let op = self.operator_stack_mut().pop().unwrap();
                    let precedence_root = precedence(op);

                    //@TODO cloneless: In this case we clone because it's kinda hard to
                    //do only using regular moves

                    let mut bin_op = None;
                    if let BinaryOperation(lhs_down, op_down, rhs_down) = &lhs_root {
                        let precedence_down = precedence(*op_down);
                        if precedence_root > precedence_down {
                            bin_op = Some(BinaryOperation(
                                lhs_down.clone(),
                                *op_down,
                                Box::new(BinaryOperation(
                                    rhs_down.clone(),
                                    op,
                                    Box::new(rhs_root.clone()),
                                )),
                            ));
                        }
                    } else if let BinaryOperation(lhs_down, op_down, rhs_down) = &rhs_root {
                        let precedence_down = precedence(*op_down);
                        if precedence_root > precedence_down {
                            bin_op = Some(BinaryOperation(
                                lhs_down.clone(),
                                *op_down,
                                Box::new(BinaryOperation(
                                    rhs_down.clone(),
                                    op,
                                    Box::new(lhs_root.clone()),
                                )),
                            ));
                        }
                    }
                    //@TODO cloneless: at least the cases that don't need fixing are cloneless
                    if bin_op.is_none() {
                        bin_op = Some(BinaryOperation(Box::new(lhs_root), op, Box::new(rhs_root)));
                    }
                    self.push_operand(bin_op.unwrap());
                }
            }
        }

        //consume the remaining operators
        if self.operand_stack().len() == 1 {
            while !self.operator_stack().is_empty() {
                let expr = self.operand_stack_mut().pop().unwrap();
                let operator = self.operator_stack_mut().pop().unwrap();
                self.push_operand(UnaryExpression(operator, Box::new(expr)));
            }
        }

        if !self.operator_stack().is_empty() {
            return Err(ParsingError::ExprError(format!(
                "Unparsed operators: {:?}, operands = {:?}",
                self.operator_stack(),
                self.operand_stack()
            )));
        }

        if self.operand_stack().len() > 1 {
            return Err(ParsingError::ExprError(format!(
                "Unparsed operands: {:?}",
                self.operand_stack()
            )));
        }

        if self.operand_stack().is_empty() {
            return Err(ParsingError::ExprError(String::from(
                "Empty operand stack, didn't parse anything",
            )));
        }
        //let remaining_tokens = Vec::from(token_queue);
        let resulting_expr = clean_parens(self.operand_stack_mut().pop().unwrap());

        Ok(ParseExpressionResult { resulting_expr })
    }

    //expr, expr, ..., expr
    fn parse_comma_sep_list_expr(
        &mut self,
    ) -> Result<ParseListExpressionResult, ParsingError> {
        let mut expressions = vec![];
        loop {
            let parse_result = self.parse_expr();

            match parse_result {
                Ok(r) => {
                    expressions.push(r.resulting_expr);
                }
                Err(e) => {
                    eprintln!("Error on parse: {:?}", e);
                    break;
                }
            }

            if self.can_go() {
                if let Token::Comma = self.cur() {
                    self.next();
                    continue;
                }
                break;
            }
            break;
        }

        if expressions.is_empty() {
            return Err(ParsingError::ExprError(String::from("While parsing list of expressions: no expression was found. Deal with edge cases before calling this expr.")));
        }

        Ok(ParseListExpressionResult {
            resulting_expr_list: expressions,
        })
    }
}

struct ParseListExpressionResult {
    //remaining_tokens: Vec<&'a Token>,
    resulting_expr_list: Vec<Expr>,
}

pub struct ParseExpressionResult {
    //remaining_tokens: Vec<&'a Token>,
    resulting_expr: Expr,
}

pub fn parse_ast(tokens: Vec<Token>) -> Vec<AST> {
    let mut parser = Parser::new(tokens);
    parser.parse_ast()
}

#[cfg(test)]
mod tests {

    use crate::{ast::lexer::{StringInterner}, semantic::context::test_utils::tls_interner};

    thread_local! {
        static INTERNER: StringInterner = StringInterner::new();
    }

    tls_interner!(INTERNER);
    
    fn tokenize(str: &str) -> Result<Vec<Token>, String> {
        INTERNER.with(|interner| crate::ast::lexer::tokenize(str, interner))
    }

    impl TypeBoundName {
        //do not delete, used by tests!
        pub fn simple(name: InternedString, name_type: InternedString) -> Self {
            Self {
                name,
                name_type: ASTType::Simple(name_type),
            }
        }
        pub fn generic_1(
            name: InternedString,
            name_type: InternedString,
            generic: InternedString,
        ) -> Self {
            Self {
                name,
                name_type: ASTType::Generic(name_type, vec![ASTType::Simple(generic)]),
            }
        }
    }


    use super::*;
    use AST::*;

    //Parses a single expression
    fn parse(tokens: Vec<Token>) -> Expr {
        let mut parser = Parser::new(tokens);
        parser.parse_expr().unwrap().resulting_expr
    }
   
 

    #[test]
    fn multiline_code() {
        let tokens = tokenize(
            "
x = 'abc' + 'cde'
y = x + str(True)",
        )
        .unwrap();
        let result = parse_ast(tokens);
        let expected = vec![
            Assign {
                path: vec![istr!("x")],
                expression: BinaryOperation(
                    Box::new(StringValue(istr!("abc").into())),
                    Operator::Plus,
                    Box::new(StringValue(istr!("cde").into())),
                ),
            },
            Assign {
                path: vec![istr!("y")],
                expression: BinaryOperation(
                    Box::new(Variable(istr!("x"))),
                    Operator::Plus,
                    Box::new(FunctionCall(
                        Box::new(Variable(istr!("str"))),
                        vec![BooleanValue(true)],
                    )),
                ),
            },
        ];

        assert_eq!(expected, result);
    }

    #[test]
    fn while_statement() {
        let tokens = tokenize(
            "
while True:
    x = 1
    break
",
        )
        .unwrap();

        let result = parse_ast(tokens);
        let expected = vec![WhileStatement {
            expression: BooleanValue(true),
            body: vec![
                Assign {
                    path: vec![istr!("x")],
                    expression: IntegerValue(1),
                },
                Break,
            ],
        }];
        assert_eq!(expected, result);
    }

    #[test]
    fn while_statement_with_if_and_expr() {
        let tokens = tokenize(
            "while x < 1000000:
    if x / 5 == 0:
        break
        
",
        )
        .unwrap();

        let result = parse_ast(tokens);
        let expected = vec![WhileStatement {
            expression: BinaryOperation(
                Box::new(Variable(istr!("x"))),
                Operator::Less,
                Box::new(IntegerValue(1_000_000)),
            ),
            body: vec![IfStatement {
                true_branch: ASTIfStatement {
                    expression: BinaryOperation(
                        Box::new(BinaryOperation(
                            Box::new(Variable(istr!("x"))),
                            Operator::Divide,
                            Box::new(IntegerValue(5)),
                        )),
                        Operator::Equals,
                        Box::new(IntegerValue(0)),
                    ),
                    statements: vec![Break],
                },
                elifs: vec![],
                final_else: None,
            }],
        }];
        assert_eq!(expected, result);
    }

    #[test]
    fn if_statement_with_print_after_and_newlines_before_and_after() {
        let tokens = tokenize(
            "
if x == 0:
    x = x + 1
else:
    x = 999
    if x == 1:
        print(2)
print(x)
",
        )
        .unwrap();

        let result = parse_ast(tokens);
        let expected = vec![
            IfStatement {
                true_branch: ASTIfStatement {
                    expression: BinaryOperation(
                        Box::new(Variable(istr!("x"))),
                        Operator::Equals,
                        IntegerValue(0).into(),
                    ),
                    statements: vec![Assign {
                        path: vec![istr!("x")],
                        expression: BinaryOperation(
                            Variable(istr!("x")).into(),
                            Operator::Plus,
                            IntegerValue(1).into(),
                        ),
                    }],
                },
                elifs: vec![],
                final_else: Some(vec![
                    Assign {
                        path: vec![istr!("x")],
                        expression: IntegerValue(999),
                    },
                    IfStatement {
                        true_branch: ASTIfStatement {
                            expression: BinaryOperation(
                                Variable(istr!("x")).into(),
                                Operator::Equals,
                                IntegerValue(1).into(),
                            ),
                            statements: vec![StandaloneExpr(FunctionCall(
                                Variable(istr!("print")).into(),
                                vec![IntegerValue(2)],
                            ))],
                        },
                        elifs: vec![],
                        final_else: None,
                    },
                ]),
            },
            StandaloneExpr(FunctionCall(Variable(istr!("print")).into(), vec![Variable(istr!("x"))])),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn bunch_of_newlines() {
        let source_wacky = "

<tab><tab><tab><tab><tab><tab><tab>        

if x == 0:
<tab><tab><tab><tab><tab><tab><tab>
<tab>x = x + 1<tab><tab><tab><tab><tab><tab>
<tab>

<tab>if x == 1:
<tab><tab><tab><tab><tab>
<tab><tab>print(2)

<tab>
print(x)


";

        let source_replaced = source_wacky.replace("<tab>", "    ");

        let tokens = tokenize(source_replaced.as_str()).unwrap();

        let result = parse_ast(tokens);
        let expected = vec![
            IfStatement {
                true_branch: ASTIfStatement {
                    expression: BinaryOperation(
                        Variable(istr!("x")).into(),
                        Operator::Equals,
                        IntegerValue(0).into(),
                    ),
                    statements: vec![
                        Assign {
                            path: vec![istr!("x")],
                            expression: BinaryOperation(
                                Variable(istr!("x")).into(),
                                Operator::Plus,
                                IntegerValue(1).into(),
                            ),
                        },
                        IfStatement {
                            true_branch: ASTIfStatement {
                                expression: BinaryOperation(
                                    Variable(istr!("x")).into(),
                                    Operator::Equals,
                                    IntegerValue(1).into(),
                                ),
                                statements: vec![StandaloneExpr(FunctionCall(
                                    Variable(istr!("print")).into(),
                                    vec![IntegerValue(2)],
                                ))],
                            },
                            elifs: vec![],
                            final_else: None,
                        },
                    ],
                },
                elifs: vec![],
                final_else: None,
            },
            StandaloneExpr(FunctionCall(Variable(istr!("print")).into(), vec![Variable(istr!("x"))])),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn if_statement() {
        let tokens = tokenize(
            "
if x == 0:
    x = x + 1",
        )
        .unwrap();
        let result = parse_ast(tokens);
        let expected = vec![IfStatement {
            true_branch: ASTIfStatement {
                expression: BinaryOperation(
                    Variable(istr!("x")).into(),
                    Operator::Equals,
                    IntegerValue(0).into(),
                ),
                statements: vec![Assign {
                    path: vec![istr!("x")],
                    expression: BinaryOperation(
                        Variable(istr!("x")).into(),
                        Operator::Plus,
                        IntegerValue(1).into(),
                    ),
                }],
            },
            elifs: vec![],
            final_else: None,
        }];
        assert_eq!(expected, result);
    }

    #[test]
    fn if_statement_with_print_after() {
        let tokens = tokenize(
            "if x == 0:
    x = x + 1
print(x)",
        )
        .unwrap();
        let result = parse_ast(tokens);
        let expected = vec![
            IfStatement {
                true_branch: ASTIfStatement {
                    expression: BinaryOperation(
                        Variable(istr!("x")).into(),
                        Operator::Equals,
                        IntegerValue(0).into(),
                    ),
                    statements: vec![Assign {
                        path: vec![istr!("x")],
                        expression: BinaryOperation(
                            Variable(istr!("x")).into(),
                            Operator::Plus,
                            IntegerValue(1).into(),
                        ),
                    }],
                },
                elifs: vec![],
                final_else: None,
            },
            StandaloneExpr(FunctionCall(Variable(istr!("print")).into(), vec![Variable(istr!("x"))])),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn multiline_code2() {
        let tokens = tokenize(
            "x = 'abc' + 'cde'
y = x + str(True)
print(y)",
        )
        .unwrap();
        let result = parse_ast(tokens);
        let expected = vec![
            Assign {
                path: vec![istr!("x")],
                expression: BinaryOperation(
                    StringValue(istr!("abc").into()).into(),
                    Operator::Plus,
                    StringValue(istr!("cde").into()).into(),
                ),
            },
            Assign {
                path: vec![istr!("y")],
                expression: BinaryOperation(
                    Variable(istr!("x")).into(),
                    Operator::Plus,
                    Box::new(FunctionCall(
                        Variable(istr!("str")).into(),
                        vec![BooleanValue(true)],
                    )),
                ),
            },
            StandaloneExpr(FunctionCall(Variable(istr!("print")).into(), vec![Variable(istr!("y"))])),
        ];

        assert_eq!(expected, result);
    }

    #[test]
    fn parse_literal_alone() {
        //1 + 1
        let result = parse(vec![Token::LiteralInteger(1)]);

        let expected = IntegerValue(1);
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_variable() {
        //1 + 1
        let result = parse(vec![Token::Identifier(istr!("x"))]);

        let expected = Variable(istr!("x"));
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_1_plus_1() {
        //1 + 1
        let result = parse(vec![
            Token::LiteralInteger(1),
            Token::Operator(Operator::Plus),
            Token::LiteralInteger(1),
        ]);

        let expected = BinaryOperation(1.into(), Operator::Plus, 1.into());
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_float_10_plus_integer_1() {
        //10.0 + 1
        let result = parse(vec![
            Token::LiteralFloat(10.0.into()),
            Token::Operator(Operator::Plus),
            Token::LiteralInteger(1),
        ]);

        let expected = BinaryOperation(10.0.into(), Operator::Plus, 1.into());
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_float_3_values_2_ops() {
        //1 + 2 + 3
        let result = parse(vec![
            Token::LiteralInteger(1),
            Token::Operator(Operator::Plus),
            Token::LiteralInteger(2),
            Token::Operator(Operator::Plus),
            Token::LiteralInteger(3),
        ]);
        let expected = BinaryOperation(
            BinaryOperation(1.into(), Operator::Plus, 2.into()).into(),
            Operator::Plus,
            3.into(),
        );

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_multiplication() {
        //1 * 2
        let result = parse(vec![
            Token::LiteralInteger(1),
            Token::Operator(Operator::Multiply),
            Token::LiteralInteger(2),
        ]);

        let expected = BinaryOperation(1.into(), Operator::Multiply, 2.into());
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_multiplication_2() {
        //1 * 2 * 3
        let result = parse(vec![
            Token::LiteralInteger(1),
            Token::Operator(Operator::Multiply),
            Token::LiteralInteger(2),
            Token::Operator(Operator::Multiply),
            Token::LiteralInteger(3),
        ]);

        let expected = BinaryOperation(
            Box::new(BinaryOperation(1.into(), Operator::Multiply, 2.into())),
            Operator::Multiply,
            3.into(),
        );

        assert_eq!(result, expected);
    }
    #[test]
    fn parse_mul_rhs_precedence() {
        //1 + 2 * 3
        let result = parse(vec![
            Token::LiteralInteger(1),
            Token::Operator(Operator::Plus),
            Token::LiteralInteger(2),
            Token::Operator(Operator::Multiply),
            Token::LiteralInteger(3),
        ]);

        /*
          +
         / \
        1   *
           / \
          2   3
        */
        let expected = BinaryOperation(
            1.into(),
            Operator::Plus,
            Box::new(BinaryOperation(2.into(), Operator::Multiply, 3.into())),
        );

        assert_eq!(expected, result);
    }

    #[test]
    fn parse_div_rhs_precedence() {
        //1 + 2 / 3
        let result = parse(vec![
            Token::LiteralInteger(1),
            Token::Operator(Operator::Plus),
            Token::LiteralInteger(2),
            Token::Operator(Operator::Divide),
            Token::LiteralInteger(3),
        ]);

        /*
          +
         / \
        1  (div)
           / \
          2   3
        */
        let expected = BinaryOperation(
            1.into(),
            Operator::Plus,
            BinaryOperation(2.into(), Operator::Divide, 3.into()).into(),
        );

        assert_eq!(expected, result);
    }

    #[test]
    fn parse_mul_lhs_precedence() {
        //1 * 2 + 3
        let result = parse(vec![
            Token::LiteralInteger(1),
            Token::Operator(Operator::Multiply),
            Token::LiteralInteger(2),
            Token::Operator(Operator::Plus),
            Token::LiteralInteger(3),
        ]);

        let expected = BinaryOperation(
            Box::new(BinaryOperation(1.into(), Operator::Multiply, 2.into())),
            Operator::Plus,
            3.into(),
        );

        assert_eq!(expected, result);
    }
    #[test]
    fn parse_complex_precedence() {
        //1 + 2 * 3 * 4 + 5
        let result = parse(vec![
            Token::LiteralInteger(1),
            Token::Operator(Operator::Plus),
            Token::LiteralInteger(2),
            Token::Operator(Operator::Multiply),
            Token::LiteralInteger(3),
            Token::Operator(Operator::Multiply),
            Token::LiteralInteger(4),
            Token::Operator(Operator::Plus),
            Token::LiteralInteger(5),
        ]);

        let expected = BinaryOperation(
            Box::new(BinaryOperation(
                1.into(),
                Operator::Plus,
                Box::new(BinaryOperation(
                    Box::new(BinaryOperation(2.into(), Operator::Multiply, 3.into())),
                    Operator::Multiply,
                    4.into(),
                )),
            )),
            Operator::Plus,
            5.into(),
        );

        assert_eq!(expected, result);
    }

    #[test]
    fn parse_more_complex_precedence() {
        //1 + 2 * 3 + 4 * 5 / 6 + 7 * 8
        let result = parse(vec![
            Token::LiteralInteger(1),
            Token::Operator(Operator::Plus),
            Token::LiteralInteger(2),
            Token::Operator(Operator::Multiply),
            Token::LiteralInteger(3),
            Token::Operator(Operator::Plus),
            Token::LiteralInteger(4),
            Token::Operator(Operator::Multiply),
            Token::LiteralInteger(5),
            Token::Operator(Operator::Divide),
            Token::LiteralInteger(6),
            Token::Operator(Operator::Plus),
            Token::LiteralInteger(7),
            Token::Operator(Operator::Multiply),
            Token::LiteralInteger(8),
        ]);

        let expected = BinaryOperation(
            Box::new(BinaryOperation(
                Box::new(BinaryOperation(
                    1.into(),
                    Operator::Plus,
                    Box::new(BinaryOperation(2.into(), Operator::Multiply, 3.into())),
                )),
                Operator::Plus,
                Box::new(BinaryOperation(
                    Box::new(BinaryOperation(4.into(), Operator::Multiply, 5.into())),
                    Operator::Divide,
                    6.into(),
                )),
            )),
            Operator::Plus,
            Box::new(BinaryOperation(7.into(), Operator::Multiply, 8.into())),
        );

        assert_eq!(expected, result);
    }

    #[test]
    fn parse_literal_parens() {
        //(1)
        let result = parse(vec![
            Token::OpenParen,
            Token::LiteralInteger(1),
            Token::CloseParen,
        ]);

        let expected = IntegerValue(1);

        assert_eq!(expected, result);
    }
    #[test]
    fn parse_parens_expr() {
        //(1 + 2) * 3
        let result = parse(vec![
            Token::OpenParen,
            Token::LiteralInteger(1),
            Token::Operator(Operator::Plus),
            Token::LiteralInteger(2),
            Token::CloseParen,
            Token::Operator(Operator::Multiply),
            Token::LiteralInteger(3),
        ]);

        /*
          +
         / \
        1   *
           / \
          2   3
        */
        let expected = BinaryOperation(
            BinaryOperation(1.into(), Operator::Plus, 2.into()).into(),
            Operator::Multiply,
            3.into(),
        );

        assert_eq!(expected, result);
    }

    #[test]
    fn integration_with_lexer() {
        let tokens = tokenize("(1 + 2) * 3").unwrap();
        let result = parse(tokens);
        let expected = BinaryOperation(
            BinaryOperation(1.into(), Operator::Plus, 2.into()).into(),
            Operator::Multiply,
            3.into(),
        );

        assert_eq!(expected, result);
    }

    #[test]
    fn complex_parenthesized_expr() {
        let tokens = tokenize("(1 + 2) * (3 + 1 + (10 / 5))").unwrap();
        let result = parse(tokens);
        let expected = BinaryOperation(
            BinaryOperation(1.into(), Operator::Plus, 2.into()).into(),
            Operator::Multiply,
            BinaryOperation(
                BinaryOperation(3.into(), Operator::Plus, 1.into()).into(),
                Operator::Plus,
                BinaryOperation(10.into(), Operator::Divide, 5.into()).into(),
            )
            .into(),
        );

        assert_eq!(expected, result);
    }

    #[test]
    fn tons_of_useless_parenthesis() {
        let tokens = tokenize("(((((((((1)))))))))").unwrap();
        let result = parse(tokens);

        let expected = IntegerValue(1);

        assert_eq!(expected, result);
    }

    #[test]
    fn identifier_multiplied() {
        let tokens = tokenize("some_identifier * 5").unwrap();
        let result = parse(tokens);
        let expected = BinaryOperation(
            Variable(istr!("some_identifier")).into(),
            Operator::Multiply,
            5.into(),
        );

        assert_eq!(expected, result);
    }

    #[test]
    fn just_an_identifier() {
        let tokens = tokenize("some_identifier").unwrap();
        let result = parse(tokens);
        let expected = Variable(istr!("some_identifier"));

        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_without_args() {
        let tokens = tokenize("some_identifier()").unwrap();
        let result = parse(tokens);
        let expected = FunctionCall(Variable(istr!("some_identifier")).into(), vec![]);

        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_with_one_param() {
        let tokens = tokenize("some_identifier(1)").unwrap();
        let result = parse(tokens);
        let expected = FunctionCall(Variable(istr!("some_identifier")).into(), vec![IntegerValue(1)]);

        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_with_many_args() {
        let tokens = tokenize("some_identifier(1, 2, 3)").unwrap();
        let result = parse(tokens);
        let expected = FunctionCall(
            Variable(istr!("some_identifier")).into(),
            vec![IntegerValue(1), IntegerValue(2), IntegerValue(3)],
        );

        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_with_expression() {
        let tokens = tokenize("some_identifier(1 * 2)").unwrap();
        let result = parse(tokens);
        let expected = FunctionCall(
            Variable(istr!("some_identifier")).into(),
            vec![BinaryOperation(1.into(), Operator::Multiply, 2.into())],
        );

        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_with_list_of_expressions() {
        let tokens = tokenize("some_identifier(1 * 2, 3 + 5, 88)").unwrap();
        let result = parse(tokens);
        let expected = FunctionCall(
            Variable(istr!("some_identifier")).into(),
            vec![
                BinaryOperation(1.into(), Operator::Multiply, 2.into()),
                BinaryOperation(3.into(), Operator::Plus, 5.into()),
                IntegerValue(88),
            ],
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_with_nested_call_with_no_args() {
        let tokens = tokenize("some_identifier(nested())").unwrap();
        let result = parse(tokens);
        let expected = FunctionCall(
            Variable(istr!("some_identifier")).into(),
            vec![FunctionCall(Variable(istr!("nested")).into(), vec![])],
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_with_nested_call_with_single_arg() {
        let tokens = tokenize("some_identifier(nested(1))").unwrap();
        let result = parse(tokens);
        let expected = FunctionCall(
            Variable(istr!("some_identifier")).into(),
            vec![FunctionCall(
                Variable(istr!("nested")).into(),
                vec![IntegerValue(1)],
            )],
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_with_nested_call_with_multiple_args() {
        let tokens = tokenize("some_identifier(nested(1, 2))").unwrap();
        let result = parse(tokens);
        let expected = FunctionCall(
            Variable(istr!("some_identifier")).into(),
            vec![FunctionCall(
                Variable(istr!("nested")).into(),
                vec![IntegerValue(1), IntegerValue(2)],
            )],
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_with_nested_call_with_multiple_expr() {
        let tokens = tokenize("some_identifier(nested(1 * 2, 2 / 3.4))").unwrap();
        let result = parse(tokens);
        let expected = FunctionCall(
            Variable(istr!("some_identifier")).into(),
            vec![FunctionCall(
                Variable(istr!("nested")).into(),
                vec![
                    BinaryOperation(1.into(), Operator::Multiply, 2.into()),
                    BinaryOperation(2.into(), Operator::Divide, (3.4).into()),
                ],
            )],
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_with_nested_call_with_multiple_expr2() {
        let tokens = tokenize("some_identifier(nested(1), 1)").unwrap();
        let result = parse(tokens);
        let expected = FunctionCall(
            Variable(istr!("some_identifier")).into(),
            vec![
                FunctionCall(Variable(istr!("nested")).into(), vec![IntegerValue(1)]),
                IntegerValue(1),
            ],
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_with_nested_call_with_multiple_expr_also_unnested() {
        let tokens = tokenize("some_identifier(nested(1 * 2, 2 / 3.4), 3, nested2())").unwrap();
        let result = parse(tokens);
        let expected = FunctionCall(
            Variable(istr!("some_identifier")).into(),
            vec![
                FunctionCall(
                    Variable(istr!("nested")).into(),
                    vec![
                        BinaryOperation(1.into(), Operator::Multiply, 2.into()),
                        BinaryOperation(2.into(), Operator::Divide, (3.4).into()),
                    ],
                ),
                IntegerValue(3),
                FunctionCall(Variable(istr!("nested2")).into(), vec![]),
            ],
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn multiply_fcall() {
        let tokens = tokenize("some_identifier(1) * 5").unwrap();
        let result = parse(tokens);
        let call = FunctionCall(Variable(istr!("some_identifier")).into(), vec![IntegerValue(1)]);
        let expected = BinaryOperation(call.into(), Operator::Multiply, 5.into());
        assert_eq!(expected, result);
    }

    #[test]
    fn multiply_fcall_multiple_args() {
        let tokens = tokenize("some_identifier(1, 2) * 5").unwrap();
        let result = parse(tokens);
        let call = FunctionCall(
            Variable(istr!("some_identifier")).into(),
            vec![IntegerValue(1), IntegerValue(2)],
        );
        let expected = BinaryOperation(call.into(), Operator::Multiply, 5.into());
        assert_eq!(expected, result);
    }

    #[test]
    fn multiply_fcall_nested_last() {
        let tokens = tokenize("some_identifier(nested()) * 5").unwrap();
        let result = parse(tokens);
        let call = FunctionCall(
            Box::new(Variable(istr!("some_identifier"))),
            vec![FunctionCall(Box::new(Variable(istr!("nested"))), vec![])],
        );
        let expected = BinaryOperation(Box::new(call), Operator::Multiply, 5.into());
        assert_eq!(expected, result);
    }

    #[test]
    fn multiply_fcall_multiple_args_nested_last() {
        let tokens = tokenize("some_identifier(1, nested()) * 5").unwrap();
        let result = parse(tokens);
        let call = FunctionCall(
            Box::new(Variable(istr!("some_identifier"))),
            vec![
                IntegerValue(1),
                FunctionCall(Box::new(Variable(istr!("nested"))), vec![]),
            ],
        );
        let expected = BinaryOperation(Box::new(call), Operator::Multiply, 5.into());
        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_with_nested_call_with_multiple_expr_also_unnested_used_in_expression_right() {
        let tokens = tokenize("some_identifier(nested(1 * 2, 2 / 3.4), 3, nested2()) * 5").unwrap();
        let result = parse(tokens);
        let call = FunctionCall(
            Box::new(Variable(istr!("some_identifier"))),
            vec![
                FunctionCall(
                    Box::new(Variable(istr!("nested"))),
                    vec![
                        BinaryOperation(1.into(), Operator::Multiply, 2.into()),
                        BinaryOperation(2.into(), Operator::Divide, (3.4).into()),
                    ],
                ),
                IntegerValue(3),
                FunctionCall(Box::new(Variable(istr!("nested2"))), vec![]),
            ],
        );
        let expected = BinaryOperation(Box::new(call), Operator::Multiply, 5.into());
        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_with_nested_call_with_multiple_expr_also_unnested_used_in_expression_left() {
        let tokens = tokenize("5 * some_identifier(nested(1 * 2, 2 / 3.4), 3, nested2())").unwrap();
        let result = parse(tokens);
        let call = FunctionCall(
            Box::new(Variable(istr!("some_identifier"))),
            vec![
                FunctionCall(
                    Box::new(Variable(istr!("nested"))),
                    vec![
                        BinaryOperation(1.into(), Operator::Multiply, 2.into()),
                        BinaryOperation(2.into(), Operator::Divide, (3.4).into()),
                    ],
                ),
                IntegerValue(3),
                FunctionCall(Box::new(Variable(istr!("nested2"))), vec![]),
            ],
        );
        let expected = BinaryOperation(5.into(), Operator::Multiply, Box::new(call));
        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_with_a_bunch_of_useless_params() {
        let tokens = tokenize("func((((((1))))))").unwrap();
        let result = parse(tokens);
        let expected = FunctionCall(Box::new(Variable(istr!("func"))), vec![IntegerValue(1)]);
        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_in_right_binop() {
        let tokens = tokenize("2 * func(1)").unwrap();
        let result = parse(tokens);
        let expected = BinaryOperation(
            (2).into(),
            Operator::Multiply,
            Box::new(FunctionCall(
                Box::new(Variable(istr!("func"))),
                vec![IntegerValue(1)],
            )),
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn unary_function_call() {
        let tokens = tokenize("-func(1)").unwrap();
        let result = parse(tokens);
        let expected = UnaryExpression(
            Operator::Minus,
            Box::new(FunctionCall(
                Box::new(Variable(istr!("func"))),
                vec![IntegerValue(1)],
            )),
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_in_left_binop() {
        let tokens = tokenize("func(1) * 2").unwrap();
        let result = parse(tokens);
        let expected = BinaryOperation(
            Box::new(FunctionCall(
                Box::new(Variable(istr!("func"))),
                vec![IntegerValue(1)],
            )),
            Operator::Multiply,
            (2).into(),
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn function_call_in_both_sides_binop() {
        let tokens = tokenize("func(1) * func(2)").unwrap();
        let result = parse(tokens);
        let expected = BinaryOperation(
            Box::new(FunctionCall(
                Box::new(Variable(istr!("func"))),
                vec![IntegerValue(1)],
            )),
            Operator::Multiply,
            Box::new(FunctionCall(
                Box::new(Variable(istr!("func"))),
                vec![IntegerValue(2)],
            )),
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn minus_one() {
        let tokens = tokenize("-1").unwrap();
        let result = parse(tokens);
        let expected = UnaryExpression(Operator::Minus, Box::new(IntegerValue(1)));
        assert_eq!(expected, result);
    }

    #[test]
    fn minus_expr() {
        let tokens = tokenize("-(5.0 / 9.0)").unwrap();
        let result = parse(tokens);
        let expected = UnaryExpression(
            Operator::Minus,
            Box::new(BinaryOperation(
                (5.0).into(),
                Operator::Divide,
                (9.0).into(),
            )),
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn two_times_minus_one() {
        let tokens = tokenize("2 * -1").unwrap();
        let result = parse(tokens);
        let expected = BinaryOperation(
            (2).into(),
            Operator::Multiply,
            Box::new(UnaryExpression(Operator::Minus, 1.into())),
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn two_times_minus_repeated_one() {
        let tokens = tokenize("2 * --1").unwrap();
        let result = parse(tokens);
        let expected = BinaryOperation(
            (2).into(),
            Operator::Multiply,
            Box::new(UnaryExpression(
                Operator::Minus,
                Box::new(UnaryExpression(Operator::Minus, 1.into())),
            )),
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn two_times_minus_plus_minus_one() {
        let tokens = tokenize("2 * -+-1").unwrap();
        let result = parse(tokens);
        let expected = BinaryOperation(
            (2).into(),
            Operator::Multiply,
            Box::new(UnaryExpression(
                Operator::Minus,
                Box::new(UnaryExpression(
                    Operator::Plus,
                    Box::new(UnaryExpression(Operator::Minus, 1.into())),
                )),
            )),
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn two_times_minus_plus_minus_one_parenthesized() {
        let tokens = tokenize("2 * (-+-1)").unwrap();
        let result = parse(tokens);
        let expected = BinaryOperation(
            (2).into(),
            Operator::Multiply,
            Box::new(UnaryExpression(
                Operator::Minus,
                Box::new(UnaryExpression(
                    Operator::Plus,
                    Box::new(UnaryExpression(Operator::Minus, 1.into())),
                )),
            )),
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn two_times_minus_plus_minus_one_in_function_call() {
        let tokens = tokenize("2 * func(-+-1)").unwrap();
        let result = parse(tokens);
        let expected = BinaryOperation(
            (2).into(),
            Operator::Multiply,
            Box::new(FunctionCall(
                Box::new(Variable(istr!("func"))),
                vec![UnaryExpression(
                    Operator::Minus,
                    Box::new(UnaryExpression(
                        Operator::Plus,
                        Box::new(UnaryExpression(Operator::Minus, 1.into())),
                    )),
                )],
            )),
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn fahrenheit_1_expr() {
        let tokens = tokenize("-(5.0 / 9.0) * 32").unwrap();
        let result = parse(tokens);

        let dividend = BinaryOperation(
            Box::new(UnaryExpression(
                Operator::Minus,
                Box::new(BinaryOperation(
                    (5.0).into(),
                    Operator::Divide,
                    (9.0).into(),
                )),
            )),
            Operator::Multiply,
            (32).into(),
        );

        assert_eq!(dividend, result);
    }

    #[test]
    fn fahrenheit_expr() {
        let tokens = tokenize("(-(5.0 / 9.0) * 32) / (1 - (5.0 / 9.0))").unwrap();
        let result = parse(tokens);

        let dividend = BinaryOperation(
            Box::new(UnaryExpression(
                Operator::Minus,
                Box::new(BinaryOperation(
                    (5.0).into(),
                    Operator::Divide,
                    (9.0).into(),
                )),
            )),
            Operator::Multiply,
            (32).into(),
        );

        let divisor = BinaryOperation(
            1.into(),
            Operator::Minus,
            Box::new(BinaryOperation(
                (5.0).into(),
                Operator::Divide,
                (9.0).into(),
            )),
        );

        let fahrenheit = BinaryOperation(Box::new(dividend), Operator::Divide, Box::new(divisor));

        assert_eq!(fahrenheit, result);
    }

    #[test]
    fn test_assign() {
        let tokens = tokenize("x = 1").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![Assign {
            path: vec![istr!("x")],
            expression: IntegerValue(1),
        }];
        assert_eq!(expected, result);
    }

    #[test]
    fn test_parse_ast_first_token_is_identifier() {
        let tokens = tokenize("x * 1").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![StandaloneExpr(BinaryOperation(
            Box::new(Variable(istr!("x"))),
            Operator::Multiply,
            1.into(),
        ))];
        assert_eq!(expected, result);
    }

    #[test]
    fn test_parse_assign_expr() {
        let tokens = tokenize("x = x * 1").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![Assign {
            path: vec![istr!("x")],
            expression: BinaryOperation(Box::new(Variable(istr!("x"))), Operator::Multiply, 1.into()),
        }];
        assert_eq!(expected, result);
    }

    #[test]
    fn test_parse_just_id_ast() {
        let tokens = tokenize("x").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![StandaloneExpr(Variable(istr!("x")))];
        assert_eq!(expected, result);
    }

    #[test]
    fn not_operator() {
        let tokens = tokenize("not True").unwrap();
        let result = parse(tokens);
        let expected = UnaryExpression(Operator::Not, Box::new(BooleanValue(true)));

        assert_eq!(expected, result);
    }

    #[test]
    fn none() {
        let tokens = tokenize("None").unwrap();
        let result = parse(tokens);
        let expected = NoneExpr;

        assert_eq!(expected, result);
    }

    #[test]
    fn not_true_and_false() {
        let tokens = tokenize("not (True and False)").unwrap();
        let result = parse(tokens);
        let expected = UnaryExpression(
            Operator::Not,
            Box::new(BinaryOperation(
                Box::new(BooleanValue(true)),
                Operator::And,
                Box::new(BooleanValue(false)),
            )),
        );

        assert_eq!(expected, result);
    }

    #[test]
    fn two_expressions_binary_that_needs_inverting_operands_no_information_is_lost() {
        let tokens = tokenize("(1 + 2 * 3) + (4 + 5 * 6)").unwrap();
        let result = parse(tokens);

        let expected = "BinaryOperation(BinaryOperation(IntegerValue(1), Plus, BinaryOperation(IntegerValue(2), Multiply, IntegerValue(3))), Plus, BinaryOperation(IntegerValue(4), Plus, BinaryOperation(IntegerValue(5), Multiply, IntegerValue(6))))";
        println!("{:#?}", result);
        assert_eq!(expected, format!("{:?}", result));
    }

    #[test]
    fn assign_boolean_expr() {
        let tokens = tokenize("x = not (True and False) or (False)").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![Assign {
            path: vec![istr!("x")],
            expression: BinaryOperation(
                Box::new(UnaryExpression(
                    Operator::Not,
                    Box::new(BinaryOperation(
                        Box::new(BooleanValue(true)),
                        Operator::And,
                        Box::new(BooleanValue(false)),
                    )),
                )),
                Operator::Or,
                Box::new(BooleanValue(false)),
            ),
        }];

        assert_eq!(expected, result);
    }

    #[test]
    fn assign_string_expr() {
        let tokens = tokenize("x = 'abc'").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![Assign {
            path: vec![istr!("x")],
            expression: StringValue(istr!("abc").into()),
        }];

        assert_eq!(expected, result);
    }

    #[test]
    fn declare_typed() {
        let tokens = tokenize("x: str = 'abc'").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![Declare {
            var: TypeBoundName::simple(istr!("x"), istr!("str")),
            expression: StringValue(istr!("abc").into()),
        }];

        assert_eq!(expected, result);
    }

    #[test]
    fn assign_string_concat_expr() {
        let tokens = tokenize("x = 'abc' + 'cde'").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![Assign {
            path: vec![istr!("x")],
            expression: BinaryOperation(
                Box::new(StringValue(istr!("abc").into())),
                Operator::Plus,
                Box::new(StringValue(istr!("cde").into())),
            ),
        }];

        assert_eq!(expected, result);
    }

    #[test]
    fn array_of_ints() {
        let tokens = tokenize("[1,2,3]").unwrap();
        let result = parse(tokens);
        let expected = Array(vec![IntegerValue(1), IntegerValue(2), IntegerValue(3)]);

        assert_eq!(expected, result);
    }

    #[test]
    fn array_of_strings() {
        let tokens = tokenize("[\"one\",\"two\",\"3\"]").unwrap();
        let result = parse(tokens);
        let expected = Array(vec![
            StringValue(istr!("one")),
            StringValue(istr!("two")),
            StringValue(istr!("3")),
        ]);

        assert_eq!(expected, result);
    }

    #[test]
    fn array_of_stuff() {
        let tokens = tokenize("[1, 'two', True, 4.565]").unwrap();
        let result = parse(tokens);
        let expected = Array(vec![
            IntegerValue(1),
            StringValue(istr!("two")),
            BooleanValue(true),
            FloatValue(4.565.into()),
        ]);

        assert_eq!(expected, result);
    }

    #[test]
    fn assign_array() {
        let tokens = tokenize("x = [1, 2]").unwrap();
        let result = parse_ast(tokens);
        let expr = Array(vec![IntegerValue(1), IntegerValue(2)]);
        let expected = vec![Assign {
            path: vec![istr!("x")],
            expression: expr,
        }];
        assert_eq!(expected, result);
    }

    #[test]
    fn member_acessor() {
        let tokens = tokenize("obj.prop").unwrap();
        println!("{:?}", tokens);
        let result = parse_ast(tokens);
        let expected = vec![StandaloneExpr(MemberAccess(
            Box::new(Variable(istr!("obj").into())),
            istr!("prop").into(),
        ))];
        assert_eq!(expected, result);
    }

    #[test]
    fn assign_member() {
        let tokens = tokenize("obj.prop = 1").unwrap();
        println!("{:?}", tokens);
        let result = parse_ast(tokens);
        let expected = vec![Assign {
            path: vec![istr!("obj").into(), istr!("prop").into()],
            expression: IntegerValue(1),
        }];
        assert_eq!(expected, result);
    }

    #[test]
    fn member_compare() {
        let tokens = tokenize("self.current >= self.max").unwrap();
        println!("{:?}", tokens);
        let result = parse_ast(tokens);
        let expected = vec![StandaloneExpr(BinaryOperation(
            Box::new(MemberAccess(
                Box::new(Variable(istr!("self").into())),
                istr!("current").into(),
            )),
            Operator::GreaterEquals,
            Box::new(MemberAccess(
                Box::new(Variable(istr!("self").into())),
                istr!("max").into(),
            )),
        ))];
        assert_eq!(expected, result);
    }

    #[test]
    fn for_item_in_list_print() {
        let tokens = tokenize(
            "
for item in list:
    print(item)
",
        )
        .unwrap();
        let result = parse_ast(tokens);
        let expected = vec![ForStatement {
            item_name: istr!("item").into(),
            list_expression: Variable(istr!("list").into()),
            body: vec![StandaloneExpr(FunctionCall(
                Box::new(Variable(istr!("print").into())),
                vec![Variable(istr!("item").into())],
            ))],
        }];
        assert_eq!(expected, result);
    }

    #[test]
    fn function_decl() {
        let tokens = tokenize(
            "
def function(x: i32):
    print(x)
",
        )
        .unwrap();
        let result = parse_ast(tokens);
        let expected = vec![DeclareFunction {
            function_name: istr!("function").into(),
            parameters: vec![TypeBoundName::simple(istr!("x"), istr!("i32"))],
            body: vec![StandaloneExpr(FunctionCall(
                Box::new(Variable(istr!("print").into())),
                vec![Variable(istr!("x").into())],
            ))],
            return_type: None,
        }];
        assert_eq!(expected, result);
    }

    #[test]
    fn function_decl_noparams() {
        let tokens = tokenize(
            "
def function():
    print(x)
",
        )
        .unwrap();
        let result = parse_ast(tokens);
        let expected = vec![DeclareFunction {
            function_name: istr!("function").into(),
            parameters: vec![],
            body: vec![StandaloneExpr(FunctionCall(
                Box::new(Variable(istr!("print").into())),
                vec![Variable(istr!("x").into())],
            ))],
            return_type: None,
        }];
        assert_eq!(expected, result);
    }

    #[test]
    fn function_decl_manyparams() {
        let tokens = tokenize(
            "
def function(x: i32,y: u32,z: MyType):
    print(x)
",
        )
        .unwrap();
        let result = parse_ast(tokens);
        let expected = vec![DeclareFunction {
            function_name: istr!("function").into(),
            parameters: vec![
                TypeBoundName::simple(istr!("x"), istr!("i32")),
                TypeBoundName::simple(istr!("y"), istr!("u32")),
                TypeBoundName::simple(istr!("z"), istr!("MyType")),
            ],
            body: vec![StandaloneExpr(FunctionCall(
                Box::new(Variable(istr!("print").into())),
                vec![Variable(istr!("x").into())],
            ))],
            return_type: None,
        }];
        assert_eq!(expected, result);
    }

    #[test]
    fn return_nothing() {
        let tokens = tokenize(
            "
def function(x: i32):
    return
",
        )
        .unwrap();
        let result = parse_ast(tokens);
        let expected = vec![DeclareFunction {
            function_name: istr!("function").into(),
            parameters: vec![TypeBoundName::simple(istr!("x"), istr!("i32"))],
            body: vec![Return(None)],
            return_type: None,
        }];
        assert_eq!(expected, result);
    }

    #[test]
    fn return_expr() {
        let tokens = tokenize(
            "
def function(x: i32) -> i32:
    return x + 1
",
        )
        .unwrap();
        let result = parse_ast(tokens);
        let expected = vec![DeclareFunction {
            function_name: istr!("function").into(),
            parameters: vec![TypeBoundName::simple(istr!("x"), istr!("i32"))],
            body: vec![Return(Some(BinaryOperation(
                Box::new(Variable(istr!("x").into())),
                Operator::Plus,
                Box::new(IntegerValue(1)),
            )))],
            return_type: Some(ASTType::Simple(istr!("i32").into())),
        }];
        assert_eq!(expected, result);
    }

    #[test]
    fn generic_type() {
        let tokens = tokenize(
            "
some_var : List<i32> = [1, 2]
",
        )
        .unwrap();
        let result = parse_ast(tokens);

        assert_eq!(
            result,
            vec![Declare {
                var: TypeBoundName::generic_1(istr!("some_var"), istr!("List"), istr!("i32")),
                expression: Array(vec![IntegerValue(1), IntegerValue(2)])
            }]
        );
    }

    #[test]
    fn struct_definition_and_then_method() {
        let tokens = tokenize(
            "
struct Struct1:
    field1: i32
    field2: i64

def my_function(param1: i32, param2: i32) -> i32:
    return param1 * param2 / (param2 - param1)
",
        )
        .unwrap();
        let result = parse_ast(tokens);

        assert_eq!(
            result,
            vec![
                StructDeclaration {
                    struct_name: istr!("Struct1").into(),
                    type_parameters: vec![],
                    body: vec![
                        TypeBoundName::simple(istr!("field1"), istr!("i32")),
                        TypeBoundName::simple(istr!("field2"), istr!("i64"))
                    ]
                },
                DeclareFunction {
                    function_name: istr!("my_function").into(),
                    parameters: vec![
                        TypeBoundName::simple(istr!("param1"), istr!("i32")),
                        TypeBoundName::simple(istr!("param2"), istr!("i32"))
                    ],
                    body: vec![Return(Some(BinaryOperation(
                        Box::new(BinaryOperation(
                            Box::new(Variable(istr!("param1").into())),
                            Operator::Multiply,
                            Box::new(Variable(istr!("param2").into()))
                        )),
                        Operator::Divide,
                        Box::new(BinaryOperation(
                            Box::new(Variable(istr!("param2").into())),
                            Operator::Minus,
                            Box::new(Variable(istr!("param1").into()))
                        ))
                    )))],
                    return_type: Some(ASTType::Simple(istr!("i32").into()))
                }
            ]
        );
    }

    #[test]
    fn struct_definition() {
        let tokens = tokenize(
            "
struct SomeStruct:
    field: i32
    otherfield: str
",
        )
        .unwrap();
        let result = parse_ast(tokens);
        let expected = vec![StructDeclaration {
            struct_name: istr!("SomeStruct").into(),
            type_parameters: vec![],
            body: vec![
                TypeBoundName::simple(istr!("field"), istr!("i32")),
                TypeBoundName::simple(istr!("otherfield"), istr!("str")),
            ],
        }];
        assert_eq!(expected, result);
    }

    #[test]
    fn access_at_index() {
        let tokens = tokenize("list[1]").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![StandaloneExpr(IndexAccess(
            Box::new(Variable(istr!("list").into())),
            Box::new(IntegerValue(1)),
        ))];
        assert_eq!(expected, result);
    }

    #[test]
    fn access_at_string() {
        let tokens = tokenize("a_map[\"value\"]").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![StandaloneExpr(IndexAccess(
            Box::new(Variable(istr!("a_map").into())),
            Box::new(StringValue(istr!("value").into())),
        ))];
        assert_eq!(expected, result);
    }

    #[test]
    fn access_at_list() {
        //this is crazy
        let tokens = tokenize("a_map[[]]").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![StandaloneExpr(IndexAccess(
            Box::new(Variable(istr!("a_map").into())),
            Box::new(Array(vec![])),
        ))];
        assert_eq!(expected, result);
    }

    #[test]
    fn function_return_indexed() {
        let tokens = tokenize("some_call()[1]").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![StandaloneExpr(IndexAccess(
            Box::new(FunctionCall(Box::new(Variable(istr!("some_call").into())), vec![])),
            Box::new(IntegerValue(1)),
        ))];
        assert_eq!(expected, result);
    }

    #[test]
    fn function_argument_is_indexed() {
        let tokens = tokenize("some_call(var[1])").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![StandaloneExpr(FunctionCall(
            Box::new(Variable(istr!("some_call").into())),
            vec![IndexAccess(
                Box::new(Variable(istr!("var").into())),
                Box::new(IntegerValue(1)),
            )],
        ))];
        assert_eq!(expected, result);
    }

    #[test]
    fn index_expression_lhs_in_binary_op() {
        //1.0 * (1.0 + (2.3 * args.__index__(1)) / 87.1)
        let tokens = tokenize("args[1] + 1").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![StandaloneExpr(BinaryOperation(
            IndexAccess(Variable(istr!("args").into()).into(), IntegerValue(1).into()).into(),
            Operator::Plus,
            IntegerValue(1).into(),
        ))];
        assert_eq!(expected, result);
    }

    #[test]
    fn index_expression_rhs_in_binary_op() {
        //1.0 * (1.0 + (2.3 * args.__index__(1)) / 87.1)
        let tokens = tokenize("1 + args[1]").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![StandaloneExpr(BinaryOperation(
            IntegerValue(1).into(),
            Operator::Plus,
            IndexAccess(Variable(istr!("args").into()).into(), IntegerValue(1).into()).into(),
        ))];
        assert_eq!(expected, result);
    }

    #[test]
    fn method_call_empty() {
        let tokens = tokenize("method.call()").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![StandaloneExpr(FunctionCall(
            Box::new(MemberAccess(
                Box::new(Variable(istr!("method").into())),
                istr!("call").into(),
            )),
            vec![],
        ))];
        assert_eq!(expected, result);
    }
    #[test]
    fn method_call_oneparam() {
        let tokens = tokenize("method.call(1)").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![StandaloneExpr(FunctionCall(
            Box::new(MemberAccess(
                Box::new(Variable(istr!("method").into())),
                istr!("call").into(),
            )),
            vec![IntegerValue(1)],
        ))];
        assert_eq!(expected, result);
    }

    #[test]
    fn method_call_manyargs() {
        let tokens = tokenize("method.call(1, 2)").unwrap();
        let result = parse_ast(tokens);
        let expected = vec![StandaloneExpr(FunctionCall(
            Box::new(MemberAccess(
                Box::new(Variable(istr!("method").into())),
                istr!("call").into(),
            )),
            vec![IntegerValue(1), IntegerValue(2)],
        ))];
        assert_eq!(expected, result);
    }
}
