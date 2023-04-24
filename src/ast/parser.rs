#[cfg(not(test))]
use std::ops::Deref;


use crate::ast::lexer::{Operator, Token};
use crate::commons::float::FloatLiteral;
use crate::interner::{InternedString, StringInterner};
use crate::semantic::context::{FileTableEntry, FileTableIndex};


//This is a box wrapper type so that I can implement a "double deref"
//automatically to make testing easier
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprBox {
    pub expr: Box<SpanExpr>,
}

impl ExprBox {
    pub fn new(expr: SpanExpr) -> ExprBox {
        ExprBox {
            expr: Box::new(expr),
        }
    }

    pub fn span(&self) -> AstSpan {
        self.expr.span
    }
}

#[cfg(not(test))]
impl Deref for ExprBox {
    type Target = Box<SpanExpr>;

    fn deref(&self) -> &Self::Target {
        &self.expr
    }
}

pub trait Spanned {
    fn get_span(&self) -> AstSpan;
}

impl Spanned for TokenSpanIndex {
    fn get_span(&self) -> AstSpan {
        AstSpan {
            start: *self,
            end: *self,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringSpan(pub InternedString, pub AstSpan);

impl InternedString {
    pub fn spanned(self, span: AstSpan) -> StringSpan {
        StringSpan(self, span)
    }

    pub fn token_spanned(self, loc: TokenSpanIndex) -> StringSpan {
        self.spanned(AstSpan {
            start: loc,
            end: loc,
        })
    }
}

impl Spanned for StringSpan {
    fn get_span(&self) -> AstSpan {
        self.1
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SpannedOperator(pub Operator, pub AstSpan);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    IntegerValue(i128, AstSpan),
    FloatValue(FloatLiteral, AstSpan),
    StringValue(StringSpan),
    BooleanValue(bool, AstSpan),
    NoneValue(AstSpan),
    Variable(StringSpan),
    //the last parameter here contains the closing parenthesis.
    FunctionCall(ExprBox, Vec<SpanExpr>, AstSpan),
    //the last parameter here contains the array closing bracket
    IndexAccess(ExprBox, ExprBox, AstSpan),
    BinaryOperation(ExprBox, SpannedOperator, ExprBox),
    Parenthesized(ExprBox),
    UnaryExpression(SpannedOperator, ExprBox),
    MemberAccess(ExprBox, StringSpan),
    //the last parameter here contains the entire span, from start to beginning
    Array(Vec<SpanExpr>, AstSpan), //maybe there could be a syntax to specify the type of the array
                                   //ex: instead of just x = [1,2,3] it could be x = [1, 2, 3] array<i32>
                                   //or like sum = array<i32>[].sum() would return 0
                                   //x: array<i32> = [] should work too
}

impl Spanned for Expr {
    fn get_span(&self) -> AstSpan {
        match self {
            IntegerValue(.., span)
            | FloatValue(_, span)
            | NoneValue(span)
            | BooleanValue(_, span) => *span,
            StringValue(s) | Variable(s) => s.1,
            FunctionCall(f, _, end) => f.expr.span.range(end),
            IndexAccess(arr, _, end) => arr.expr.span.range(end),
            BinaryOperation(start, _, end) => start.expr.span.range(&end.expr.span),
            Parenthesized(e) => e.expr.span,
            UnaryExpression(op, expr) => op.1.range(&expr.expr.span),
            MemberAccess(expr, member) => expr.expr.span.range(&member.1),
            Array(.., span) => *span,
        }
    }
}

impl Expr {
    fn with_span(self, span: AstSpan) -> SpanExpr {
        SpanExpr { expr: self, span }
    }

    pub fn self_spanning(self) -> SpanExpr {
        SpanExpr {
            span: self.get_span(),
            expr: self,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AstSpan {
    pub start: TokenSpanIndex,
    pub end: TokenSpanIndex,
}

impl Spanned for AstSpan {
    fn get_span(&self) -> AstSpan {
        *self
    }
}

impl AstSpan {
    pub fn range(&self, other: &Self) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }
    pub fn prefix(&self, other: &Self) -> Self {
        Self {
            start: other.start,
            end: self.end,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpanExpr {
    pub expr: Expr,
    pub span: AstSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpanAST {
    pub ast: AST,
    pub span: AstSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTIfStatement {
    pub expression: SpanExpr,
    pub statements: Vec<SpanAST>,
}

impl Spanned for ASTIfStatement {
    fn get_span(&self) -> AstSpan {
        self.expression
            .span
            .range(&self.statements.last().unwrap().span)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ASTType {
    Simple(StringSpan),
    Generic(StringSpan, Vec<ASTType>),
}

impl ASTType {
    pub fn to_string(&self, interner: &StringInterner) -> String {
        match self {
            ASTType::Simple(s) => interner.borrow(s.0).to_string(),
            ASTType::Generic(s, params) => {
                let generic_params = params
                    .iter()
                    .map(|x| x.to_string(interner))
                    .collect::<Vec<_>>()
                    .join(",");
                format!("{}<{}>", interner.borrow(s.0), generic_params)
            }
        }
    }
}

impl Spanned for ASTType {
    fn get_span(&self) -> AstSpan {
        match self {
            ASTType::Simple(s) => s.1,
            ASTType::Generic(s, type_args) => s.1.range(&type_args.last().unwrap().get_span()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeBoundName {
    pub name: StringSpan,
    pub name_type: ASTType,
}

impl Spanned for TypeBoundName {
    fn get_span(&self) -> AstSpan {
        self.name.1.range(&self.name_type.get_span())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AST {
    StandaloneExpr(SpanExpr),
    Assign {
        path: SpanExpr,
        expression: SpanExpr,
    },
    Declare {
        var: TypeBoundName,
        expression: SpanExpr,
    },
    IfStatement {
        true_branch: ASTIfStatement,
        elifs: Vec<ASTIfStatement>,
        final_else: Option<Vec<SpanAST>>,
    },
    WhileStatement {
        expression: SpanExpr,
        body: Vec<SpanAST>,
    },
    ForStatement {
        item_name: StringSpan,
        list_expression: SpanExpr,
        body: Vec<SpanAST>,
    },
    StructDeclaration {
        struct_name: StringSpan,
        type_parameters: Vec<StringSpan>,
        body: Vec<TypeBoundName>,
    },
    DeclareFunction {
        function_name: StringSpan,
        parameters: Vec<TypeBoundName>,
        body: Vec<SpanAST>,
        return_type: Option<ASTType>,
    },
    Break(AstSpan),
    Intrinsic(AstSpan),
    Return(AstSpan, Option<SpanExpr>),
    Root(Vec<SpanAST>),
}

impl AST {
    pub fn self_spanning(self) -> SpanAST {
        SpanAST {
            span: self.get_span(),
            ast: self,
        }
    }

    pub fn span_prefixed<T>(self, prefix: T) -> SpanAST
    where
        T: Spanned,
    {
        SpanAST {
            span: self.get_span().prefix(&prefix.get_span()),
            ast: self,
        }
    }
}

impl Spanned for AST {
    fn get_span(&self) -> AstSpan {
        match self {
            AST::StandaloneExpr(s) => s.span,
            AST::Assign { path, expression } => {
                expression.expr.get_span().prefix(&path.expr.get_span())
            }
            AST::Declare { var, expression } => var.get_span().range(&expression.span),
            AST::IfStatement {
                true_branch,
                elifs,
                final_else,
            } => match final_else {
                Some(f) => true_branch.get_span().range(&f.last().unwrap().span),
                None => {
                    if elifs.is_empty() {
                        true_branch.get_span()
                    } else {
                        true_branch
                            .get_span()
                            .range(&elifs.last().unwrap().get_span())
                    }
                }
            },
            AST::WhileStatement { expression, body } => {
                expression.span.range(&body.last().unwrap().span)
            }
            AST::ForStatement {
                item_name,
                list_expression: _,
                body,
            } => item_name.get_span().range(&body.last().unwrap().span),
            AST::StructDeclaration {
                struct_name,
                type_parameters: _,
                body,
            } => struct_name
                .get_span()
                .range(&body.last().unwrap().get_span()),
            AST::DeclareFunction {
                function_name,
                parameters: _,
                body,
                return_type: _,
            } => function_name.get_span().range(&body.last().unwrap().span),
            AST::Break(span) => *span,
            AST::Intrinsic(span) => *span,
            AST::Return(span, expr) => match expr {
                Some(e) => span.range(&e.span),
                None => *span,
            },
            AST::Root(r) => {
                let first = r.first().unwrap().span;
                let last = r.last().unwrap().span;
                first.range(&last)
            }
        }
    }
}

fn precedence(o: Operator) -> u32 {
    match o {
        Operator::Multiply | Operator::Divide => 100,
        _ => 1,
    }
}

fn clean_parens(expr: SpanExpr) -> SpanExpr {
    match expr.expr {
        Parenthesized(e) => clean_parens(*e.expr).expr.with_span(expr.span),
        UnaryExpression(op, e) => {
            let span = e.expr.span;
            UnaryExpression(op, ExprBox::new(clean_parens(*e.expr))).with_span(span)
        }
        BinaryOperation(left, op, right) => {
            let left_clean = ExprBox::new(clean_parens(*left.expr));
            let right_clean = ExprBox::new(clean_parens(*right.expr));
            BinaryOperation(left_clean, op, right_clean).with_span(expr.span)
        }
        _ => expr,
    }
}

pub struct Parser<'tok> {
    parsing_state: Vec<ParsingState>,
    tokens: &'tok TokenTable,
    errors: Vec<(ParsingError, FileTableIndex, TokenSpanIndex)>,
    irrecoverable_error: bool,
}

struct ParsingState {
    operator_stack: Vec<SpannedOperator>,
    operand_stack: Vec<SpanExpr>,
    index: usize,
    current_indent: usize,
}

#[derive(Debug, Clone)]
pub enum ParsingError {
    ExprError(String),
    InvalidSyntax(String),
    ContextForError(String, Box<ParsingError>),
    Fatal(String),
    //TypeBoundMissingTypeSpecifier,
    TypeBoundExpectedColonAfterFieldName,
}

impl ToString for ParsingError {
    fn to_string(&self) -> String {
        match self {
            ParsingError::ExprError(e) => format!("Expression error: {e}"),
            ParsingError::InvalidSyntax(e) => format!("Invalid syntax: {e}"),
            ParsingError::ContextForError(context, e) => {
                format!("{context}, error: {err}", err = e.to_string())
            }
            ParsingError::Fatal(e) => {
                format!("Error: {e}")
            }
            ParsingError::TypeBoundExpectedColonAfterFieldName => {
                "Error: Expected column after field name".to_string()
            }
        }
    }
}

pub enum ParsingEvent {
    //When the compiler is trying to parse X but found Y, must ignore and retry using another grammar rule
    TryAnotherGrammarRule,
    //When the compiler found the correct grammar rule, but the input is not legal. It can continue parsing the next elements to report more errors.
    //However the parser will generate AST that is unusable by the next stages
    GrammarRuleFail(ParsingError, FileTableIndex, TokenSpanIndex),
    //When there is a syntax error so big that there is just no recovering from it, continuing parsing would only throw hundreds more errors
    NonRecoverable(ParsingError, FileTableIndex, TokenSpanIndex),
    //When parsing of a grammar rule succeeds
    Success(SpanAST),
}

macro_rules! parse_guard {
    ($parser:expr, $pattern:pat_param) => {
        parse_guard!($parser, $pattern, ParsingEvent::TryAnotherGrammarRule)
    };
    ($parser:expr, $pattern:pat_param, $result:expr) => {
        if let $pattern = $parser.cur().token {
            $parser.next();
            if (!$parser.can_advance_on_line()) {
                return $result;
            }
        } else {
            return $result;
        }
    };
}

macro_rules! expect_token {
    ($parser:expr, $token:tt::$pattern:tt) => {
        if let $token::$pattern = $parser.cur().token {
            $parser.next();
        } else {
            let token = $parser.cur().clone();
            let span = &$parser.tokens.spans[token.span_index.0];
            return ParsingEvent::NonRecoverable(
                ParsingError::InvalidSyntax(format!(
                    "Expected {}, got {}",
                    ($token::$pattern).name(),
                    $parser.cur().token.name()
                )),
                span.file,
                token.span_index,
            );
        }
    };
    ($parser:expr, $token:tt::$pattern:tt, $msg:expr) => {
        if let $token::$pattern = $parser.cur().token {
            $parser.next();
        } else {
            let token = $parser.cur().clone();
            let span = &$parser.tokens.spans[token.span_index.0];
            return ParsingEvent::NonRecoverable(
                ParsingError::InvalidSyntax(format!($msg, $parser.cur().token.name())),
                span.file,
                token.span_index,
            );
        }
    };
}

macro_rules! expect_identifier {
    ($parser:expr, $role:expr) => {{
        let token = $parser.cur().clone();
        let span = &$parser.tokens.spans[token.span_index.0];
        if let Token::Identifier(id) = token.token {
            $parser.next();
            id.token_spanned(token.span_index)
        } else {
            return ParsingEvent::GrammarRuleFail(
                ParsingError::InvalidSyntax(format!(
                    "Expected {}, got {}",
                    $role,
                    $parser.cur().token.name()
                )),
                span.file,
                token.span_index,
            );
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

use super::lexer::{TokenData, TokenSpanIndex, TokenTable};

impl<'tok> Parser<'tok> {
    pub fn new(tokens: &'tok TokenTable) -> Parser<'tok> {
        Parser {
            parsing_state: vec![ParsingState {
                index: 0,
                operator_stack: vec![],
                operand_stack: vec![],
                current_indent: 0,
            }],
            tokens,
            errors: vec![],
            irrecoverable_error: false,
        }
    }

    pub fn get_errors(&self) -> &[(ParsingError, FileTableIndex, TokenSpanIndex)] {
        &self.errors
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

    fn cur(&self) -> &TokenData {
        self.cur_offset(0)
    }

    fn loc(&self) -> TokenSpanIndex {
        self.cur_offset(0).span_index
    }

    fn prev_token(&self) -> Option<&TokenData> {
        self.cur_offset_opt(-1)
    }

    fn cur_offset(&self, offset: isize) -> &TokenData {
        return self.cur_offset_opt(offset).unwrap();
    }

    fn cur_offset_opt(&self, offset: isize) -> Option<&TokenData> {
        let index = self.parsing_state.last().unwrap().index as isize + offset;
        self.tokens.tokens.get(index as usize)
    }

    fn is_last(&self) -> bool {
        self.parsing_state.last().unwrap().index == self.tokens.tokens.len() - 1
    }

    fn is_not_end(&self) -> bool {
        self.parsing_state.last().unwrap().index < self.tokens.tokens.len()
    }

    fn can_advance_on_line(&self) -> bool {
        self.is_not_end() && !self.cur_is_newline()
    }

    fn cur_is_newline(&self) -> bool {
        matches!(self.cur().token, Token::NewLine)
    }

    fn push_operand(&mut self, token: SpanExpr) {
        self.parsing_state
            .last_mut()
            .unwrap()
            .operand_stack
            .push(token);
    }

    fn push_operator(&mut self, operator: SpannedOperator) {
        self.parsing_state
            .last_mut()
            .unwrap()
            .operator_stack
            .push(operator);
    }

    fn operand_stack(&self) -> &[SpanExpr] {
        return &self.parsing_state.last().unwrap().operand_stack;
    }

    fn operator_stack(&self) -> &[SpannedOperator] {
        return &self.parsing_state.last().unwrap().operator_stack;
    }

    fn operand_stack_mut(&mut self) -> &mut Vec<SpanExpr> {
        return &mut self.parsing_state.last_mut().unwrap().operand_stack;
    }

    fn operator_stack_mut(&mut self) -> &mut Vec<SpannedOperator> {
        return &mut self.parsing_state.last_mut().unwrap().operator_stack;
    }

    pub fn parse_assign(&mut self) -> ParsingEvent {
        //assignments are x = y,
        //and the lhs is also an expression, involving index acessors and method calls in the left hand side.

        //try to parse the lhs expression before the =
        //create a new stack here, so that we can pop if after the expr we don't find what we want
        self.new_stack();

        let lhs = self.parse_expr();

        let Ok(lvalue_expr) = lhs else {
            self.pop_stack();
            return ParsingEvent::TryAnotherGrammarRule;
        };

        if !self.can_advance_on_line() {
            self.pop_stack();
            return ParsingEvent::TryAnotherGrammarRule;
        }

        let Token::Assign = self.cur().token else {
            self.pop_stack();
            return ParsingEvent::TryAnotherGrammarRule;
        };

        self.next();

        let expr = self.parse_expr();

        match expr {
            Ok(e) => ParsingEvent::Success(
                AST::Assign {
                    path: lvalue_expr.resulting_expr,
                    expression: e.resulting_expr,
                }
                .self_spanning(),
            ),
            Err(e) => self.grammar_fail(ParsingError::ContextForError(
                "Expected expression after assign".into(),
                Box::new(e),
            )),
        }
    }

    pub fn parse_declaration(&mut self) -> ParsingEvent {
        let decl = self.parse_type_bound_name();

        let Ok(Some(typed_var_decl)) = decl else { return ParsingEvent::TryAnotherGrammarRule; };
        //no need to do .next here, parse_type_bound_name already does a .next()
        self.next();
        let cur = self.cur();
        if let Token::Assign = cur.token {
            self.next();
            let expr = self.parse_expr();
            match expr {
                Ok(expr) => ParsingEvent::Success(
                    AST::Declare {
                        var: typed_var_decl,
                        expression: expr.resulting_expr,
                    }
                    .self_spanning(),
                ),
                Err(e) => self.grammar_fail(ParsingError::ContextForError(
                    "Expected expression after assign".to_string(),
                    e.into(),
                )),
            }
        } else {
            ParsingEvent::TryAnotherGrammarRule
        }
    }

    pub fn parse_if_statement(&mut self) -> ParsingEvent {
        let begin = self.cur().span_index;
        parse_guard!(self, Token::IfKeyword);

        let expr = match self.parse_expr() {
            Ok(result) => result.resulting_expr,
            Err(e) => {
                return self.grammar_fail(ParsingError::ContextForError(
                    "Expected expression for if statement".into(),
                    e.into(),
                ))
            }
        };

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
            .span_prefixed(begin)
        });

        self.new_stack();

        //lets try getting the else statement:
        let cur_identation = self.get_expected_indent();
        let identation_else = self.skip_whitespace_newline();

        if !self.can_advance_on_line() || identation_else != cur_identation {
            self.pop_stack();
            return ParsingEvent::Success(if_statement);
        }

        if let Token::ElseKeyword = self.cur().token {
            self.next();
            expect_colon_newline!(self);

            if_statement = indented!(self, {
                let ast = self.parse_ast();
                match if_statement.ast {
                    AST::IfStatement {
                        true_branch, elifs, ..
                    } => AST::IfStatement {
                        true_branch,
                        elifs,
                        final_else: Some(ast),
                    }
                    .span_prefixed(begin),
                    _ => {
                        return self.non_recoverable(ParsingError::Fatal(
                            "Unexpected AST during if parsing".to_string(),
                        ));
                    }
                }
            });
        } else {
            self.pop_stack();
        }
        ParsingEvent::Success(if_statement)
    }

    pub fn parse_structdef(&mut self) -> ParsingEvent {
        let begin = self.cur().span_index;
        parse_guard!(self, Token::StructDef);

        let struct_name = expect_identifier!(self, "struct name");
        expect_colon_newline!(self);

        let struct_def = indented!(self, {
            let mut fields = vec![];

            loop {
                self.skip_whitespace_newline();
                if !self.can_advance_on_line() {
                    break;
                }
                let Token::Identifier(_) = self.cur().token else { break; };
                let parsed = self.parse_type_bound_name().unwrap().unwrap();
                fields.push(parsed);

                if !self.can_advance_on_line() {
                    break;
                }

                self.next();
                let Token::NewLine = self.cur().token else { break; };

                self.next();
                if !self.can_advance_on_line() {
                    break;
                }
                //if a second newline is found, then the struct declaration is found
                if let Token::NewLine = self.cur().token {
                    break;
                }
            }

            AST::StructDeclaration {
                struct_name,
                type_parameters: vec![],
                body: fields,
            }
            .span_prefixed(begin)
        });

        ParsingEvent::Success(struct_def)
    }

    pub fn parse_while_statement(&mut self) -> ParsingEvent {
        let begin = self.cur().span_index;
        parse_guard!(self, Token::WhileKeyword);

        let expr = match self.parse_expr() {
            Ok(result) => result.resulting_expr,
            Err(e) => {
                return self.grammar_fail(ParsingError::ContextForError(
                    "Expected expression for while statement".into(),
                    e.into(),
                ))
            }
        };

        expect_colon_newline!(self);

        ParsingEvent::Success(indented!(self, {
            let ast = self.parse_ast();
            AST::WhileStatement {
                expression: expr,
                body: ast,
            }
            .span_prefixed(begin)
        }))
    }

    pub fn parse_for_statement(&mut self) -> ParsingEvent {
        let begin = self.cur().span_index;
        parse_guard!(self, Token::ForKeyword);
        let variable_name = expect_identifier!(self, "variable name in for loop");
        expect_token!(self, Token::InKeyword);

        let expr = match self.parse_expr() {
            Ok(result) => result.resulting_expr,
            Err(e) => {
                return self.grammar_fail(ParsingError::ContextForError(
                    "Expected expression for iterable in for statement".into(),
                    e.into(),
                ))
            }
        };

        expect_colon_newline!(self);

        ParsingEvent::Success(indented!(self, {
            let ast = self.parse_ast();

            AST::ForStatement {
                item_name: variable_name,
                list_expression: expr,
                body: ast,
            }
            .span_prefixed(begin)
        }))
    }

    pub fn parse_type_name(&mut self) -> Option<ASTType> {
        let begin = self.cur().span_index;
        let Token::Identifier(type_name) = self.cur().token else {
            return None;
        };

        if !self.can_advance_on_line() {
            return Some(ASTType::Simple(type_name.token_spanned(begin)));
        }

        let peek_next = self.cur_offset(1);

        let Token::Operator(Operator::Less) = peek_next.token else {
            return Some(ASTType::Simple(type_name.token_spanned(begin)));
        };

        self.next(); //commits the peek_next
        self.next();

        let generic_begin = self.cur().span_index;
        let Token::Identifier(generic_name) = self.cur().token else {

            panic!("For now we dont have proper error handling for mistakes in generic types, cur = {:?}", self.cur())
         };
        self.next();

        if let Token::Operator(Operator::Greater) = self.cur().token {
            Some(ASTType::Generic(
                type_name.token_spanned(begin),
                vec![ASTType::Simple(generic_name.token_spanned(generic_begin))],
            ))
        } else {
            panic!("For now we don't suport more than 1 generic argument (i'm lazy).")
        }
    }

    //Tries to parse a bound name with its type, for instance var: i32
    //leaves cursor in the next token after the type
    pub fn parse_type_bound_name(&mut self) -> Result<Option<TypeBoundName>, ParsingError> {
        let begin = self.cur().span_index;
        let Token::Identifier(name) = self.cur().token else { return Ok(None); };
        self.next();

        if !self.can_advance_on_line() {
            return Ok(None);
        }

        let Token::Colon = self.cur().token else {
            return Err(ParsingError::TypeBoundExpectedColonAfterFieldName);
        };
        self.next();

        let typename = self.parse_type_name();

        match typename {
            Some(x) => Ok(Some(TypeBoundName {
                name: name.token_spanned(begin),
                name_type: x,
            })),
            None => Ok(None),
        }
    }

    pub fn parse_def_statement(&mut self) -> ParsingEvent {
        let begin = self.cur().span_index;
        parse_guard!(self, Token::DefKeyword);
        let function_name = expect_identifier!(self, "function name");

        expect_token!(self, Token::OpenParen);

        let mut params: Vec<TypeBoundName> = vec![];

        while let Token::Identifier(_) = self.cur().token {
            let param = self.parse_type_bound_name().unwrap().unwrap();

            params.push(param);
            self.next();
            if let Token::Comma = self.cur().token {
                self.next();
            } else {
                break;
            }
        }

        if let Token::CloseParen = self.cur().token {
            self.next();
        } else {
            return self.non_recoverable(ParsingError::Fatal(
                "Expected close paren after parameters in function declaration".to_string(),
            ));
        }

        let mut return_type: Option<ASTType> = None;

        if let Token::ArrowRight = self.cur().token {
            self.next();

            return_type = self.parse_type_name();

            if return_type.is_none() {
                return self.non_recoverable(ParsingError::Fatal(
                    "Expected type name after arrow right on function declaration".into(),
                ));
            }
            self.next();
        }

        expect_token!(
            self,
            Token::Colon,
            "Expected colon paren after parameters and return type in function declaration, got {}"
        );

        ParsingEvent::Success(indented!(self, {
            let ast = self.parse_ast();

            AST::DeclareFunction {
                function_name,
                parameters: params,
                body: ast,
                return_type,
            }
            .span_prefixed(begin)
        }))
    }

    pub fn parse_break(&mut self) -> ParsingEvent {
        let tok = *self.cur();
        if let Token::BreakKeyword = tok.token {
            self.next();

            return ParsingEvent::Success(AST::Break(tok.span_index.get_span()).self_spanning());
        }

        ParsingEvent::TryAnotherGrammarRule
    }

    pub fn parse_return(&mut self) -> ParsingEvent {
        let tok = *self.cur();
        if let Token::ReturnKeyword = tok.token {
            self.next();
            if self.can_advance_on_line() {
                let expr = match self.parse_expr() {
                    Ok(result) => result.resulting_expr,
                    Err(e) => {
                        return self.grammar_fail(ParsingError::ContextForError(
                            "Expected expression on return statement".into(),
                            e.into(),
                        ))
                    }
                };

                return ParsingEvent::Success(
                    AST::Return(tok.span_index.get_span(), Some(expr)).self_spanning(),
                );
            }
            return ParsingEvent::Success(
                AST::Return(tok.span_index.get_span(), None).span_prefixed(tok.span_index),
            );
        }
        ParsingEvent::TryAnotherGrammarRule
    }

    pub fn parse_intrinsic(&mut self) -> ParsingEvent {
        let tok = *self.cur();
        if let Token::IntrinsicKeyword = tok.token {
            self.next();
            if !self.can_advance_on_line() {
                return ParsingEvent::Success(
                    AST::Intrinsic(tok.span_index.get_span()).self_spanning(),
                );
            } else {
                return self.grammar_fail(ParsingError::InvalidSyntax(
                    "Intrinsic must be the only content of the method if it's present".to_string(),
                ));
            }
        }
        ParsingEvent::TryAnotherGrammarRule
    }

    pub fn parse_standalone_expr(&mut self) -> ParsingEvent {
        ParsingEvent::Success(
            self.parse_expr()
                .ok()
                .map(|expr| AST::StandaloneExpr(expr.resulting_expr).self_spanning())
                .unwrap(),
        )
    }

    //returns the identation level until the first non-whitespace token
    //final state of this function is right at newline, before the identations
    fn skip_whitespace_newline(&mut self) -> usize {
        let mut identation_level = 0;
        while self.is_not_end() {
            match self.cur().token {
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

    //this skips tokens until the next line
    fn skip_to_next_line(&mut self) {
        while self.is_not_end() {
            match self.cur().token {
                Token::NewLine => {
                    self.next();
                    return;
                }
                _ => self.next(),
            }
        }
    }

    //@TODO this is kinda hacky, I extracted this from the parse_ast method, but it's kinda ugly
    fn handle_parsing_result(
        &mut self,
        name: &str,
        parsing_result: ParsingEvent,
        results: &mut Vec<SpanAST>,
        parsed_successfully: &mut bool,
    ) {
        match parsing_result {
            ParsingEvent::Success(parsed_result) => {
                results.push(parsed_result);
                *parsed_successfully = true;
                let popped = self.pop_stack();
                //correct indentation found: commit
                self.set_cur(&popped);

                if !(!self.is_not_end() || self.cur_is_newline()) {
                    let cur = self.cur();
                    let span = &self.tokens.spans[cur.span_index.0];
                    let file = span.file;
                    let error_msg = format!(
                        "Newline or EOF expected after {}, got {:?}",
                        name, cur.token
                    );

                    self.errors.push((
                        ParsingError::InvalidSyntax(error_msg),
                        file,
                        cur.span_index,
                    ));
                }
            }
            ParsingEvent::TryAnotherGrammarRule => {
                self.pop_stack();
            }
            ParsingEvent::GrammarRuleFail(error, file, token) => {
                self.skip_to_next_line();
                self.errors.push((error, file, token))
            }
            ParsingEvent::NonRecoverable(error, file, token) => {
                self.errors.push((error, file, token))
            }
        }
    }

    pub fn parse_ast(&mut self) -> Vec<SpanAST> {
        let mut parsed_successfully: bool;
        let mut results: Vec<SpanAST> = vec![];

        macro_rules! try_parse {
            ($name:expr, $parse_method:tt) => {
                if !parsed_successfully {
                    //println!("Size of stack is {}", &self.parsing_state.len().to_string());
                    self.new_stack();

                    let parsing_result = self.$parse_method();
                    self.handle_parsing_result(
                        $name,
                        parsing_result,
                        &mut results,
                        &mut parsed_successfully,
                    );
                }
            };
        }

        loop {
            if self.irrecoverable_error {
                break;
            }
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
            try_parse!("assignment", parse_assign);
            try_parse!("declaration", parse_declaration);
            try_parse!("if block", parse_if_statement);
            try_parse!("while block", parse_while_statement);
            try_parse!("for block", parse_for_statement);
            try_parse!("function definition", parse_def_statement);
            try_parse!("break", parse_break);
            try_parse!("return", parse_return);
            try_parse!("intrinsic", parse_intrinsic);
            try_parse!("standalone expression", parse_standalone_expr);

            if !parsed_successfully {
                let error = self.non_recoverable(ParsingError::Fatal(
                    "Unrecognized expression/statement".to_string(),
                ));
                self.handle_parsing_result("", error, &mut results, &mut parsed_successfully);
            }

            let is_end = !self.is_not_end();
            if is_end {
                break;
            };

            if self.cur_is_newline() {
                continue;
            };
            let error = self.non_recoverable(ParsingError::Fatal(
                "Unrecognized expression/statement".to_string(),
            ));
            self.handle_parsing_result("", error, &mut results, &mut parsed_successfully);
        }

        results
    }

    fn index_access_helper(&mut self, indexable_expr: SpanExpr) -> Result<SpanExpr, ParsingError> {
        if let Token::CloseParen = self.cur().token {
            return Err(ParsingError::InvalidSyntax(
                "Invalid syntax: must inform index value".to_string(),
            ));
        }

        self.new_stack();
        let list_of_exprs = self.parse_comma_sep_list_expr();

        let ending = self.cur().span_index;

        match list_of_exprs {
            //try parse stuff
            Ok(expressions) => {
                //commit the result
                let popped = self.pop_stack();
                let mut resulting_exprs = expressions.resulting_expr_list;
                if resulting_exprs.len() != 1 {
                    return Err(ParsingError::InvalidSyntax(
                        "Invalid syntax: must inform index value".to_string(),
                    ));
                }

                let fcall = IndexAccess(
                    ExprBox::new(indexable_expr),
                    ExprBox::new(resulting_exprs.pop().unwrap()),
                    ending.get_span(),
                )
                .self_spanning();

                self.set_cur(&popped);

                Ok(fcall)
            }
            Err(e) => Err(e),
        }
    }

    fn function_call_helper(&mut self, expr_callable: SpanExpr) -> Result<SpanExpr, ParsingError> {
        if let Token::CloseParen = self.cur().token {
            return Ok(FunctionCall(
                ExprBox::new(expr_callable),
                vec![],
                self.cur().span_index.get_span(),
            )
            .self_spanning());
        }

        self.new_stack();
        let list_of_exprs = self.parse_comma_sep_list_expr();
        let ending = self.cur().span_index;

        match list_of_exprs {
            //try parse stuff
            Ok(expressions) => {
                //commit the result
                let popped = self.pop_stack();
                let resulting_exprs = expressions.resulting_expr_list;

                let fcall = FunctionCall(
                    ExprBox::new(expr_callable),
                    resulting_exprs,
                    ending.get_span(),
                )
                .self_spanning();

                self.set_cur(&popped);

                Ok(fcall)
            }
            Err(e) => Err(e),
        }
    }

    #[allow(clippy::too_many_lines)] //no patience to fix this, expr parsing is messy and I will not touch it
    pub fn parse_expr(&mut self) -> Result<ParseExpressionResult, ParsingError> {
        loop {
            if !self.can_advance_on_line() {
                break;
            }
            let mut was_operand = false;
            let mut not_part_of_expr = false;
            //if there is an open paren, we collect all the tokens for this open paren
            //and parse the sub-expression recursively
            {
                let tok = self.cur();

                let prev_token = self.prev_token().cloned().map(|x| x.token);
                match tok.token {
                    Token::OpenParen => {
                        //move to the first token, out of the OpenParen
                        //when parsing an open paren, it can either be a parenthesized expression, or a function call.
                        //A parenthesized expression can appear everywhere, and so does a function call.
                        //However, if the previous token was an operator, then the open paren can only be valid if we are
                        //parsing a parenthesized expression.
                        //A function call open paren can only appear in the following scenarios:
                        // - An expression that hasn't been fully parsed yet like `an_identifier` but next token is `(`
                        // - `"a literal token"` and then `(` is also a valid function call.
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
                                match current_expr.expr {
                                    BinaryOperation(left, op, right) => {
                                        let right_side_fcall =
                                            self.function_call_helper(*right.expr)?;
                                        self.push_operand(
                                            BinaryOperation(
                                                left,
                                                op,
                                                ExprBox::new(right_side_fcall),
                                            )
                                            .self_spanning(),
                                        );
                                        was_operand = true;
                                    }
                                    expr => {
                                        let fcall =
                                            self.function_call_helper(expr.self_spanning())?;
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
                                    let parenthesized = Parenthesized(ExprBox::new(resulting_expr));
                                    let popped = self.pop_stack();
                                    self.push_operand(parenthesized.self_spanning());
                                    self.set_cur(&popped);
                                    was_operand = true;
                                }
                                Err(e) => {
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

                                match current_expr.expr {
                                    BinaryOperation(left, op, right) => {
                                        let right_side_index_access =
                                            self.index_access_helper(*right.expr)?;
                                        self.push_operand(
                                            BinaryOperation(
                                                left,
                                                op,
                                                ExprBox::new(right_side_index_access),
                                            )
                                            .self_spanning(),
                                        );
                                        was_operand = true;
                                    }
                                    expr => {
                                        let index_access =
                                            self.index_access_helper(expr.self_spanning())?;
                                        self.push_operand(index_access);
                                        was_operand = true;
                                    }
                                }
                            }
                        } else {
                            //this is parsing an array literal!
                            self.new_stack(); //new parsing stack/state
                            self.next(); //move to the first token, out of the open array
                            if let Token::CloseArrayBracket = self.cur().token {
                                self.push_operand(
                                    Array(vec![], self.cur().span_index.get_span()).self_spanning(),
                                );
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
                                        self.push_operand(
                                            Array(
                                                resulting_exprs,
                                                self.cur().span_index.get_span(),
                                            )
                                            .self_spanning(),
                                        );
                                        self.set_cur(&popped);
                                    }
                                    Err(e) => {
                                        return Err(e);
                                    }
                                }
                            }
                        }
                    }
                    Token::Identifier(identifier_str) => {
                        self.push_operand(
                            Variable(identifier_str.token_spanned(tok.span_index)).self_spanning(),
                        );
                        was_operand = true;
                    }
                    Token::MemberAccessor => {
                        //next token should be an identifier
                        self.next();
                        let popped = self.operand_stack_mut().pop();
                        let cur_token = self.cur();
                        if let Token::Identifier(name) = cur_token.token {
                            let cur_expr = popped.unwrap();
                            let member_access_expr = MemberAccess(
                                ExprBox::new(cur_expr),
                                name.token_spanned(cur_token.span_index),
                            );
                            self.push_operand(member_access_expr.self_spanning());
                            was_operand = true;
                        } else {
                            return Err(ParsingError::ExprError(
                                "Failed parsing member acessor".into(),
                            ));
                        }
                    }
                    Token::LiteralInteger(i) => {
                        self.push_operand(
                            IntegerValue(i, tok.span_index.get_span()).self_spanning(),
                        );
                        was_operand = true;
                    }
                    Token::LiteralFloat(f) => {
                        self.push_operand(FloatValue(f, tok.span_index.get_span()).self_spanning());
                        was_operand = true;
                    }
                    Token::LiteralString(s) => {
                        //@TODO cloneless: store the literals somewhere it can be fetched by ref?
                        self.push_operand(
                            StringValue(s.token_spanned(tok.span_index)).self_spanning(),
                        );
                        was_operand = true;
                    }
                    Token::None => {
                        self.push_operand(NoneValue(tok.span_index.get_span()).self_spanning());
                        was_operand = true;
                    }
                    Token::True => {
                        self.push_operand(
                            BooleanValue(true, tok.span_index.get_span()).self_spanning(),
                        );
                        was_operand = true;
                    }
                    Token::False => {
                        self.push_operand(
                            BooleanValue(false, tok.span_index.get_span()).self_spanning(),
                        );
                        was_operand = true;
                    }
                    Token::Operator(o) => {
                        self.push_operator(SpannedOperator(o, tok.span_index.get_span()))
                    }
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
            if self.can_advance_on_line() && Token::MemberAccessor == self.cur().token {
                continue;
            }

            //if the token is an open paren... then wait a minute, could be a function call!
            //same thing for an open bracket! could be an array!
            //try parse it first!

            if self.can_advance_on_line() {
                if let Token::OpenArrayBracket = self.cur().token {
                    continue;
                }
                if let Token::OpenParen = self.cur().token {
                    continue;
                }
            }

            if was_operand {
                //base case: there is only an operator and an operand, like "-1"
                if self.operand_stack().len() == 1 && self.operator_stack().len() == 1 {
                    let last_operand = self.operand_stack_mut().pop().unwrap();
                    let op = self.operator_stack_mut().pop().unwrap();
                    self.push_operand(
                        UnaryExpression(op, ExprBox::new(last_operand)).self_spanning(),
                    );
                }
                //repeat case: 2 * -----2 or even 2 * -2, consume all the minus signals
                else if self.operator_stack().len() > 1 && self.operand_stack().len() == 2 {
                    while self.operator_stack().len() > 1 {
                        let last_operand = self.operand_stack_mut().pop().unwrap();
                        let op = self.operator_stack_mut().pop().unwrap();

                        self.push_operand(
                            UnaryExpression(op, ExprBox::new(last_operand)).self_spanning(),
                        );
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
                        !(matches!(lhs_root.expr, BinaryOperation(..))
                            && matches!(rhs_root.expr, BinaryOperation(..)))
                    );

                    let op = self.operator_stack_mut().pop().unwrap();
                    let precedence_root = precedence(op.0);

                    //@TODO cloneless: In this case we clone because it's kinda hard to
                    //do only using regular moves

                    let mut bin_op = None;
                    if let BinaryOperation(lhs_down, op_down, rhs_down) = &lhs_root.expr {
                        let precedence_down = precedence(op_down.0);
                        if precedence_root > precedence_down {
                            bin_op = Some(
                                BinaryOperation(
                                    lhs_down.clone(),
                                    *op_down,
                                    ExprBox::new(
                                        BinaryOperation(
                                            rhs_down.clone(),
                                            op,
                                            ExprBox::new(rhs_root.clone()),
                                        )
                                        .self_spanning(),
                                    ),
                                )
                                .self_spanning(),
                            );
                        }
                    } else if let BinaryOperation(lhs_down, op_down, rhs_down) = &rhs_root.expr {
                        let precedence_down = precedence(op_down.0);
                        if precedence_root > precedence_down {
                            bin_op = Some(
                                BinaryOperation(
                                    lhs_down.clone(),
                                    *op_down,
                                    ExprBox::new(
                                        BinaryOperation(
                                            rhs_down.clone(),
                                            op,
                                            ExprBox::new(lhs_root.clone()),
                                        )
                                        .self_spanning(),
                                    ),
                                )
                                .self_spanning(),
                            );
                        }
                    }
                    //@TODO cloneless: at least the cases that don't need fixing are cloneless
                    if bin_op.is_none() {
                        bin_op = Some(
                            BinaryOperation(ExprBox::new(lhs_root), op, ExprBox::new(rhs_root))
                                .self_spanning(),
                        );
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
                self.push_operand(UnaryExpression(operator, ExprBox::new(expr)).self_spanning());
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
    fn parse_comma_sep_list_expr(&mut self) -> Result<ParseListExpressionResult, ParsingError> {
        let mut expressions = vec![];
        loop {
            let parse_result = self.parse_expr();

            match parse_result {
                Ok(r) => {
                    expressions.push(r.resulting_expr);
                }
                Err(_e) => {
                    break;
                }
            }

            if self.can_advance_on_line() {
                if let Token::Comma = self.cur().token {
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

    fn non_recoverable(&mut self, parsing_error: ParsingError) -> ParsingEvent {
        self.irrecoverable_error = true;
        let cur = self.cur();
        let span = &self.tokens.spans[cur.span_index.0];

        ParsingEvent::NonRecoverable(parsing_error, span.file, cur.span_index)
    }

    fn grammar_fail(&mut self, parsing_error: ParsingError) -> ParsingEvent {
        let cur = self.cur();
        let span = &self.tokens.spans[cur.span_index.0];

        ParsingEvent::GrammarRuleFail(parsing_error, span.file, cur.span_index)
    }
}

struct ParseListExpressionResult {
    //remaining_tokens: Vec<&'a Token>,
    resulting_expr_list: Vec<SpanExpr>,
}

pub struct ParseExpressionResult {
    resulting_expr: SpanExpr,
}

pub fn parse_ast<'a>(
    tokens: &'a TokenTable,
    file_table: &[FileTableEntry],
) -> (Vec<SpanAST>, Parser<'a>) {
    let mut parser = Parser::new(tokens);
    let result = parser.parse_ast();

    print_errors(&parser, file_table, tokens);

    (result, parser)
}

fn print_errors(parser: &Parser, file_table: &[FileTableEntry], tokens: &TokenTable) {
    for (error, file, token) in parser.errors.iter() {
        let err = error.to_string();
        let file_name = &file_table[file.0].path;
        let tok = tokens.tokens[token.0];
        let span = tokens.spans[tok.span_index.0];
        let line = span.start.line;
        let column = span.start.column;
        println!("{err}\nat {file_name}:{line}:{column}\n");
    }
}

#[cfg(test)]
mod tests {

    thread_local! {
        static INTERNER: StringInterner = StringInterner::new();
    }

    #[cfg(test)]
    impl Deref for ExprBox {
        type Target = Expr;

        fn deref(&self) -> &Self::Target {
            &self.expr.expr
        }
    }

    #[cfg(test)]
    impl Deref for SpannedOperator {
        type Target = Operator;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    #[cfg(test)]
    impl Deref for SpanAST {
        type Target = AST;

        fn deref(&self) -> &Self::Target {
            &self.ast
        }
    }

    tls_interner!(INTERNER);

    fn tokenize(str: &str) -> Result<TokenTable, String> {
        INTERNER.with(|x| crate::ast::lexer::tokenize(FileTableIndex(0), str, x))
    }

    impl TypeBoundName {
        //do not delete, used by tests!
        pub fn simple(name: StringSpan, name_type: StringSpan) -> Self {
            Self {
                name,
                name_type: ASTType::Simple(name_type),
            }
        }
        pub fn generic_1(name: StringSpan, name_type: StringSpan, generic: StringSpan) -> Self {
            Self {
                name,
                name_type: ASTType::Generic(name_type, vec![ASTType::Simple(generic)]),
            }
        }
    }

    use std::{assert_matches::assert_matches, ops::Deref};

    use crate::{
        ast::ast_printer::{print_ast, print_fully_parenthesized_ast},
        interner::StringInterner,
        semantic::context::{test_utils::tls_interner, FileTableIndex},
    };

    use super::*;

    //Parses a single expression
    fn parse(tokens: TokenTable) -> SpanExpr {
        let table = &[FileTableEntry {
            ast: AST::Break(AstSpan {
                start: TokenSpanIndex(0),
                end: TokenSpanIndex(0),
            }),
            token_table: tokens,
            index: FileTableIndex(0),
            contents: "none",
            path: "test".to_string(),
        }];
        let mut parser = Parser::new(&table[0].token_table);
        print_errors(&parser, table, &table[0].token_table);
        parser.parse_expr().unwrap().resulting_expr
    }

    fn test_parse(tokens: TokenTable) -> Vec<SpanAST> {
        let table = &[FileTableEntry {
            ast: AST::Break(AstSpan {
                start: TokenSpanIndex(0),
                end: TokenSpanIndex(0),
            }),
            token_table: tokens,
            index: FileTableIndex(0),
            contents: "none",
            path: "test".to_string(),
        }];
        let mut parser = Parser::new(&table[0].token_table);
        let result = parser.parse_ast();
        print_errors(&parser, table, &table[0].token_table);
        if !parser.get_errors().is_empty() {
            panic!("Deu ruim");
        }
        result
    }

    #[test]
    fn just_an_identifier() {
        let _some_id = istr("some_identifier");
        let tokens = tokenize("some_identifier").unwrap();
        let result = parse(tokens);

        assert_matches!(result.expr, Variable(StringSpan(_some_id, _)));
    }

    use match_deref::match_deref;

    impl Deref for SpanExpr {
        type Target = Expr;

        fn deref(&self) -> &Self::Target {
            &self.expr
        }
    }

    #[cfg(test)]
    impl Deref for StringSpan {
        type Target = str;

        fn deref(&self) -> &Self::Target {
            //@SAFETY actually not very safe.
            //This is, indeed, a hack.
            //The tradeoff here is to make testing easier.
            //We will return a str with a lifetime tied to &self, which is deallocated
            //before INTERNER. INTERNER will be available for the duration of all tests,
            //and this deref is only called inside a test.
            //While it should be safe for this use, don't do this anywhere else.
            INTERNER.with(|x| {
                let borrowed_str = x.borrow(self.0);
                unsafe { std::mem::transmute(borrowed_str) }
            })
        }
    }

    #[test]
    fn function_call_without_args() {
        let tokens = tokenize("some_identifier()").unwrap();
        let result = parse(tokens);

        assert!(match_deref! {
            match &result.expr {
                FunctionCall(Deref @ Variable(Deref @ "some_identifier"), Deref @ [], _) => true,
                _ => panic!("Unexpected parsing output: {:#?}", result.expr)
            }
        });
    }

    #[test]
    fn function_call_with_one_param() {
        let tokens = tokenize("some_identifier(1)").unwrap();
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall(
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [Deref @ IntegerValue(1, _)],
                    _
                ) => true,
                _ => false
            }
        })
    }

    #[test]
    fn function_call_with_many_args() {
        let tokens = tokenize("some_identifier(1, 2, 3)").unwrap();
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall(
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [Deref @ IntegerValue(1, _), Deref @ IntegerValue(2, _), Deref @ IntegerValue(3, _)],
                    _
                ) => true,
                _ => false
            }
        })
    }

    #[test]
    fn function_call_with_expression() {
        let tokens = tokenize("some_identifier(1 * 2)").unwrap();
        let result = parse(tokens);

        assert!(match_deref! {
            match &result.expr {
                FunctionCall (
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [Deref @ BinaryOperation(
                        Deref @ IntegerValue(1, _),
                        SpannedOperator(Operator::Multiply, _),
                        Deref @ IntegerValue(2, _),
                    )], _
                )  => true,
                 _ => false
            }
        });
    }

    #[test]
    fn function_call_with_list_of_expressions() {
        let tokens = tokenize("some_identifier(1 * 2, 3 + 5, 88)").unwrap();
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall (
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [
                        Deref @ BinaryOperation(
                            Deref @ IntegerValue(1, _),
                            SpannedOperator(Operator::Multiply, _),
                            Deref @ IntegerValue(2, _),
                        ),
                        Deref @ BinaryOperation(
                            Deref @ IntegerValue(3, _),
                            SpannedOperator(Operator::Plus, _),
                            Deref @ IntegerValue(5, _),
                        ),
                        Deref @ IntegerValue(88, _)
                    ], _
                )  => true,
                 _ => false
            }
        });
    }

    #[test]
    fn function_call_with_nested_call_with_no_args() {
        let tokens = tokenize("some_identifier(nested())").unwrap();
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall (
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [
                        Deref @ FunctionCall(
                            Deref @ Variable(Deref @ "nested"),
                            Deref @ [],
                            _
                        ),
                    ], _
                )  => true,
                 _ => false
            }
        });
    }

    #[test]
    fn function_call_with_nested_call_with_single_arg() {
        let tokens = tokenize("some_identifier(nested(1))").unwrap();
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall (
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [
                        Deref @ FunctionCall(
                            Deref @ Variable(Deref @ "nested"),
                            Deref @ [
                                Deref @ IntegerValue(1, _)
                            ],
                            _
                        ),
                    ], _
                )  => true,
                 _ => false
            }
        });
    }

    #[test]
    fn function_call_with_nested_call_with_multiple_args() {
        let tokens = tokenize("some_identifier(nested(1, 2))").unwrap();
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall (
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [
                        Deref @ FunctionCall(
                            Deref @ Variable(Deref @ "nested"),
                            Deref @ [
                                Deref @ IntegerValue(1, _),
                                Deref @ IntegerValue(2, _)
                            ],
                            _
                        ),
                    ], _
                )  => true,
                 _ => false
            }
        });
    }

    #[test]
    fn function_call_with_nested_call_with_multiple_expr() {
        let tokens = tokenize("some_identifier(nested(1 * 2, 2 / 3.4))").unwrap();
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall (
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [
                        Deref @ FunctionCall(
                            Deref @ Variable(Deref @ "nested"),
                            Deref @ [
                                Deref @ BinaryOperation (
                                    Deref @ IntegerValue(1, _),
                                    SpannedOperator(Operator::Multiply, _),
                                    Deref @ IntegerValue(2, _)
                                ),
                                Deref @ BinaryOperation (
                                    Deref @ IntegerValue(2, _),
                                    SpannedOperator(Operator::Divide, _),
                                    Deref @ FloatValue(FloatLiteral(3.4), _)
                                )

                            ],
                            _
                        ),
                    ], _
                )  => true,
                 _ => false
            }
        });
    }

    #[test]
    fn function_call_with_nested_call_with_multiple_expr2() {
        let tokens = tokenize("some_identifier(nested(1), 1)").unwrap();
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall (
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [
                        Deref @ FunctionCall(
                            Deref @ Variable(Deref @ "nested"),
                            Deref @ [
                                Deref @ IntegerValue(1, _),
                            ],
                            _
                        ),
                        Deref @ IntegerValue(1, _),
                    ], _
                )  => true,
                 _ => false
            }
        });
    }

    #[test]
    fn while_statement_with_if_and_expr() {
        parse_and_print_back_to_original(
            "
            while x < 1000000:
                if (x / 5) == 0:
                    break
",
        );
    }

    #[test]
    fn function_call_with_nested_call_with_multiple_expr_also_unnested() {
        parse_and_print_back_to_original("some_identifier(nested(1 * 2, 2 / 3.4), 3, nested2())");
    }

    fn unindent(str: &str) -> String {
        let mut indent = 0;
        let lines = str.lines().collect::<Vec<_>>();
        if lines.len() == 1 {
            return str.to_string();
        }

        for c in str.lines().nth(1).unwrap().chars() {
            if c == ' ' {
                indent += 1;
            } else {
                break;
            }
        }

        //create a string with the same number of spaces as the indentation
        let indent_str = " ".repeat(indent);

        return str
            .lines()
            .map(|line| {
                if line.starts_with(&indent_str) {
                    line[indent..].to_string()
                } else {
                    line.to_string()
                }
            })
            .collect::<Vec<String>>()
            .join("\n");
    }

    fn parse_and_print_back_to_original(source: &str) {
        let unindented = unindent(source);
        let tokens = tokenize(&unindented).unwrap();
        let result = test_parse(tokens);
        let printed = INTERNER.with(|i| print_ast(&result, i));
        pretty_assertions::assert_eq!(printed.trim(), unindented.trim());
    }

    fn parse_and_compare(source: &str, expected: &str) {
        let unindented_src = unindent(source);
        let tokens = tokenize(&unindented_src).unwrap();

        let unindented_expected = unindent(expected);

        let result = test_parse(tokens);
        let printed = INTERNER.with(|i| print_ast(&result, i));
        pretty_assertions::assert_eq!(printed.trim(), unindented_expected.trim());
    }

    #[test]
    fn if_statement_with_print_after_and_newlines_before_and_after() {
        parse_and_print_back_to_original(
            "
            if x == 0:
                x = x + 1
            else:
                x = 999
                if x == 1:
                    print(2)
            print(x)
        ",
        );
    }

    #[test]
    fn if_statement() {
        parse_and_print_back_to_original(
            "
        if x == 0:
            x = x + 1
        ",
        );
    }

    #[test]
    fn if_statement_with_print_after() {
        parse_and_print_back_to_original(
            "
        if x == 0:
            x = x + 1
        print(x)
        ",
        );
    }

    #[test]
    fn multiline_code2() {
        parse_and_print_back_to_original(
            "
        x = \"abc\" + \"cde\"
        y = x + str(True)
        print(y)
        ",
        );
    }

    #[test]
    fn integration_with_lexer() {
        parse_and_print_back_to_original("(1 + 2) * 3");
    }

    #[test]
    fn complex_parenthesized_expr() {
        parse_and_compare(
            "(1 + 2) * (3 + 1 + (10 / 5))",
            "(1 + 2) * ((3 + 1) + (10 / 5))",
        );
    }

    #[test]
    fn tons_of_useless_parenthesis() {
        parse_and_compare("(((((((((1)))))))))", "1");
    }

    #[test]
    fn identifier_multiplied() {
        parse_and_print_back_to_original("some_identifier * 5");
    }

    #[test]
    fn multiply_fcall() {
        parse_and_print_back_to_original("some_identifier(1) * 5");
    }

    #[test]
    fn multiply_fcall_multiple_args() {
        parse_and_print_back_to_original("some_identifier(1, 2) * 5");
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

        let tokens = tokenize(&unindent(&source_replaced)).unwrap();
        let result = test_parse(tokens);
        let printed = INTERNER.with(|i| print_ast(&result, i));
        print!("{}", printed);
        pretty_assertions::assert_eq!(
            printed.trim(),
            "
if x == 0:
    x = x + 1
    if x == 1:
        print(2)
print(x)"
                .trim()
        );
    }

    #[test]
    fn multiply_fcall_nested_last() {
        parse_and_print_back_to_original("some_identifier(nested()) * 5");
    }

    #[test]
    fn multiply_fcall_multiple_args_nested_last() {
        parse_and_print_back_to_original("some_identifier(1, nested()) * 5");
    }

    #[test]
    fn function_call_with_nested_call_with_multiple_expr_also_unnested_used_in_expression_right() {
        parse_and_print_back_to_original(
            "some_identifier(nested(1 * 2, 2 / 3.4), 3, nested2()) * 5",
        );
    }

    #[test]
    fn function_call_with_nested_call_with_multiple_expr_also_unnested_used_in_expression_left() {
        parse_and_print_back_to_original(
            "5 * some_identifier(nested(1 * 2, 2 / 3.4), 3, nested2())",
        );
    }

    #[test]
    fn function_call_with_a_bunch_of_useless_params() {
        parse_and_compare("func((((((1))))))", "func(1)");
    }

    #[test]
    fn function_call_in_right_binop() {
        parse_and_print_back_to_original("2 * func(1)");
    }

    #[test]
    fn unary_function_call() {
        parse_and_compare("-func(1)", "- func(1)");
    }

    #[test]
    fn function_call_in_left_binop() {
        parse_and_print_back_to_original("func(1) * 2");
    }

    #[test]
    fn function_call_in_both_sides_binop() {
        parse_and_print_back_to_original("func(1) * func(2)");
    }

    #[test]
    fn minus_one() {
        parse_and_compare("-1", "- 1");
    }

    #[test]
    fn minus_expr() {
        parse_and_compare("-(5.0 / 9.0)", "- (5 / 9)");
    }

    #[test]
    fn two_times_minus_one() {
        parse_and_compare("2 * -1", "2 * - 1");
    }

    #[test]
    fn two_times_minus_repeated_one() {
        parse_and_compare("2 * --1", "2 * - - 1");
    }

    #[test]
    fn two_times_minus_plus_minus_one() {
        parse_and_compare("2 * -+-1", "2 * - + - 1");
    }

    #[test]
    fn two_times_minus_plus_minus_one_parenthesized() {
        parse_and_compare("2 * (-+-1)", "2 * - + - 1");
    }

    #[test]
    fn two_times_minus_plus_minus_one_in_function_call() {
        parse_and_compare("2 * func(-+-1)", "2 * func(- + - 1)");
    }

    #[test]
    fn fahrenheit_1_expr() {
        parse_and_compare("-(5.0 / 9.0) * 32", "- (5 / 9) * 32");
    }

    #[test]
    fn fahrenheit_expr() {
        parse_and_compare(
            "(-(5.0 / 9.0) * 32) / (1 - (5.0 / 9.0))",
            "(- (5 / 9) * 32) / (1 - (5 / 9))",
        );
    }

    #[test]
    fn test_assign() {
        parse_and_print_back_to_original("x = 1");
    }

    #[test]
    fn test_parse_ast_first_token_is_identifier() {
        parse_and_print_back_to_original("x * 1");
    }

    #[test]
    fn test_parse_assign_expr() {
        parse_and_print_back_to_original("x = x * 1");
    }

    #[test]
    fn test_parse_just_id_ast() {
        parse_and_print_back_to_original("x");
    }

    #[test]
    fn not_operator() {
        parse_and_print_back_to_original("not True");
    }

    #[test]
    fn none() {
        parse_and_print_back_to_original("None");
    }

    #[test]
    fn not_true_and_false() {
        parse_and_print_back_to_original("not (True and False)");
    }

    #[test]
    fn two_expressions_binary_that_needs_inverting_operands_no_information_is_lost() {
        parse_and_compare("(1 + 2 * 3) + (4 + 5 * 6)", "(1 + (2 * 3)) + (4 + (5 * 6))");
    }

    #[test]
    fn assign_boolean_expr() {
        parse_and_compare(
            "x = not (True and False) or (False)",
            "x = not (True and False) or False",
        );
    }

    #[test]
    fn assign_string_expr() {
        parse_and_compare("x = 'abc'", "x = \"abc\"");
    }

    #[test]
    fn declare_typed() {
        parse_and_print_back_to_original("x: str = \"abc\"");
    }

    #[test]
    fn assign_string_concat_expr() {
        parse_and_compare("x = 'abc' + 'cde'", "x = \"abc\" + \"cde\"");
    }

    #[test]
    fn array_of_ints() {
        parse_and_print_back_to_original("[1, 2, 3]");
    }

    #[test]
    fn array_of_strings() {
        parse_and_print_back_to_original("[\"one\", \"two\", \"3\"]");
    }

    #[test]
    fn array_of_stuff() {
        parse_and_compare("[1, 'two', True, 4.565]", "[1, \"two\", True, 4.565]");
    }

    #[test]
    fn assign_array() {
        parse_and_print_back_to_original("x = [1, 2]");
    }

    #[test]
    fn member_acessor() {
        parse_and_print_back_to_original("obj.prop");
    }

    #[test]
    fn assign_member() {
        parse_and_print_back_to_original("obj.prop = 1");
    }

    #[test]
    fn member_compare() {
        parse_and_print_back_to_original("self.current >= self.max");
    }

    #[test]
    fn for_item_in_list_print() {
        parse_and_print_back_to_original(
            "
    for item in list:
        print(item)
    ",
        );
    }

    #[test]
    fn function_decl() {
        parse_and_print_back_to_original(
            "
    def function(x: i32):
        print(x)
    ",
        );
    }

    #[test]
    fn function_decl_noparams() {
        parse_and_print_back_to_original(
            "
    def function():
        print(x)
    ",
        );
    }

    #[test]
    fn function_decl_manyparams() {
        parse_and_print_back_to_original(
            "
    def function(x: i32, y: u32, z: MyType):
        print(x)
    ",
        );
    }

    #[test]
    fn return_nothing() {
        parse_and_print_back_to_original(
            "
    def function(x: i32):
        return
    ",
        );
    }

    #[test]
    fn return_expr() {
        parse_and_print_back_to_original(
            "
    def function(x: i32) -> i32:
        return x + 1
    ",
        );
    }

    #[test]
    fn generic_type() {
        parse_and_print_back_to_original(
            "
    some_var: List<i32> = [1, 2]
    ",
        );
    }

    #[test]
    fn struct_definition_and_then_method() {
        parse_and_compare(
            "
    struct Struct1:
        field1: i32
        field2: i64

    def my_function(param1: i32, param2: i32) -> i32:
        return param1 * param2 / (param2 - param1)
    ",
            "
    struct Struct1:
        field1: i32
        field2: i64

    def my_function(param1: i32, param2: i32) -> i32:
        return (param1 * param2) / (param2 - param1)",
        );
    }

    #[test]
    fn struct_definition() {
        parse_and_print_back_to_original(
            "
    struct SomeStruct:
        field: i32
        otherfield: str
    ",
        );
    }

    #[test]
    fn access_at_index() {
        parse_and_print_back_to_original("list[1]");
    }

    #[test]
    fn access_at_string() {
        parse_and_print_back_to_original("a_map[\"value\"]");
    }

    #[test]
    fn access_at_list() {
        //this is crazy
        parse_and_print_back_to_original("a_map[[]]");
    }

    #[test]
    fn function_return_indexed() {
        parse_and_print_back_to_original("some_call()[1]");
    }

    #[test]
    fn function_argument_is_indexed() {
        parse_and_print_back_to_original("some_call(var[1])");
    }

    #[test]
    fn index_expression_lhs_in_binary_op() {
        //1.0 * (1.0 + (2.3 * args.__index__(1)) / 87.1)
        parse_and_print_back_to_original("args[1] + 1");
    }

    #[test]
    fn index_expression_rhs_in_binary_op() {
        //1.0 * (1.0 + (2.3 * args.__index__(1)) / 87.1)
        parse_and_print_back_to_original("1 + args[1]");
    }

    #[test]
    fn method_call_empty() {
        parse_and_print_back_to_original("method.call()");
    }
    #[test]
    fn method_call_oneparam() {
        parse_and_print_back_to_original("method.call(1)");
    }

    #[test]
    fn method_call_manyargs() {
        parse_and_print_back_to_original("method.call(1, 2)");
    }

    #[test]
    fn index_assign() {
        parse_and_print_back_to_original("array[0] = 1");
    }

    #[test]
    fn obj_property_assign() {
        parse_and_print_back_to_original("obj.array = [1]");
    }

    #[test]
    fn obj_property_index_assign() {
        parse_and_print_back_to_original("obj.array[0] = 1");
    }

    #[test]
    fn obj_method_array_assign() {
        parse_and_print_back_to_original("obj.getter()[0] = 1");
    }

    #[test]
    fn obj_method_prop_array_assign() {
        parse_and_print_back_to_original("obj.getter().prop[0] = 1");
    }

    #[test]
    fn array_obj_method_prop_array_assign() {
        parse_and_print_back_to_original("array[obj.getter().prop[0]] = 1");
    }

    #[test]
    fn array_obj_method_prop_array_binexp_assign() {
        parse_and_print_back_to_original("array[obj.getter().prop[0] + obj.getter().prop[1]] = 1");
    }

    #[test]
    fn simple_index_binary_expression() {
        parse_and_print_back_to_original("array[i + 1] = 1");
    }

    #[test]
    fn deref_value() {
        let tokens = tokenize("*val").unwrap();
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                UnaryExpression(
                    Deref @ Operator::Multiply,
                    Deref @ Variable(Deref @ "val")
                ) => true,
                _ => false
            }
        })
    }

    #[test]
    fn ref_value() {
        parse_and_print_back_to_original("& val");
    }

    #[test]
    fn deref_value_and_set() {
        parse_and_print_back_to_original("* val = 1");
    }

    #[test]
    fn set_ptr_complex_expr() {
        let unindented =
            unindent("* array[obj.getter().prop[0].some_ptr + obj.getter().prop[1]] = 1");
        let tokens = tokenize(&unindented).unwrap();
        let result = test_parse(tokens);
        let printed = INTERNER.with(|i| print_fully_parenthesized_ast(&result, i));

        assert_eq!(
            printed,
            "* (array[(obj.getter().prop[0].some_ptr + obj.getter().prop[1])]) = 1"
        );
    }
}
