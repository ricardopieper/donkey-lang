use crate::ast::lexer::{Operator, Token};
use crate::commons::float::FloatLiteral;
use crate::interner::InternedString;
use crate::semantic::context::FileTableEntry;
use std::fmt::Display;
use std::ops::{ControlFlow, Deref, FromResidual, Try};

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
}

impl AsRef<Expr> for ExprBox {
    fn as_ref(&self) -> &Expr {
        self.expr.deref()
    }
}

impl AsRef<AstSpan> for ExprBox {
    fn as_ref(&self) -> &AstSpan {
        &self.expr.span
    }
}

impl Deref for ExprBox {
    type Target = Expr;

    fn deref(&self) -> &Self::Target {
        self.expr.deref()
    }
}

impl Deref for SpanExpr {
    type Target = Expr;

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

impl Spanned for SpannedOperator {
    fn get_span(&self) -> AstSpan {
        self.1
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    IntegerValue(i128, AstSpan),
    FloatValue(FloatLiteral, AstSpan),
    StringValue(StringSpan),
    CharValue(char, AstSpan),
    BooleanValue(bool, AstSpan),
    NoneValue(AstSpan),
    Variable(StringSpan),
    //Had to name SelfValue because Self is a keyword in Rust
    SelfValue(AstSpan),
    //the last parameter here contains the closing parenthesis location.
    FunctionCall(ExprBox, Vec<ASTType>, Vec<SpanExpr>, AstSpan),
    //the last parameter here contains the array closing bracket
    IndexAccess(ExprBox, ExprBox, AstSpan),
    BinaryOperation(ExprBox, SpannedOperator, ExprBox),
    Parenthesized(ExprBox),
    UnaryExpression(SpannedOperator, ExprBox),
    MemberAccess(ExprBox, StringSpan),
    Cast(ExprBox, ASTType, AstSpan),
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
            | CharValue(_, span)
            | FloatValue(_, span)
            | NoneValue(span)
            | BooleanValue(_, span) => *span,
            StringValue(s) | Variable(s) => s.get_span(),
            FunctionCall(f, _, _, end) => f.get_span().range(end),
            IndexAccess(arr, _, end) => arr.get_span().range(end),
            BinaryOperation(start, _, end) => start.get_span().range(&end.expr.span),
            Parenthesized(e) => e.get_span(),
            UnaryExpression(op, expr) => op.get_span().range(expr.as_ref()),
            MemberAccess(expr, member) => expr.get_span().range(&member.1),
            Array(.., span) => *span,
            Cast(.., span) => *span,
            SelfValue(span) => *span,
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

impl Spanned for AstSpan {
    fn get_span(&self) -> AstSpan {
        *self
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

impl Display for ASTType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTType::Simple(s) => s.0.write_str(f),
            ASTType::Generic(s, params) => {
                s.0.write_str(f)?;
                f.write_str("<")?;
                let mut first = true;
                for p in params {
                    if !first {
                        f.write_str(", ")?;
                    }
                    first = false;
                    p.fmt(f)?;
                }
                f.write_str(">")
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
pub struct FunctionDeclaration {
    pub function_name: StringSpan,
    pub parameters: Vec<TypeBoundName>,
    pub type_parameters: Vec<StringSpan>,
    pub body: Vec<SpanAST>,
    pub return_type: Option<ASTType>,
    pub is_bound_to_self: bool,
    pub is_varargs: bool,
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
    ImplDeclaration {
        struct_name: StringSpan,
        type_parameters: Vec<StringSpan>,
        body: Vec<FunctionDeclaration>,
    },
    DeclareFunction(FunctionDeclaration),
    Break(AstSpan),
    Intrinsic(AstSpan),
    External(AstSpan),
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
            AST::DeclareFunction(FunctionDeclaration {
                function_name,
                body,
                ..
            }) => {
                let function_name_span = function_name.get_span();
                match body.last() {
                    Some(last) => function_name_span.range(&last.span),
                    None => function_name_span.range(&function_name_span),
                }
            }
            AST::Break(span) => *span,
            AST::Intrinsic(span) => *span,
            AST::External(span) => *span,
            AST::Return(span, expr) => match expr {
                Some(e) => span.range(&e.span),
                None => *span,
            },
            AST::Root(r) => {
                let first = r.first().unwrap().span;
                let last = r.last().unwrap().span;
                first.range(&last)
            }
            AST::ImplDeclaration { struct_name, .. } => struct_name.get_span(),
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
        Array(exprs, span) => {
            let mut new_exprs = vec![];
            for e in exprs {
                new_exprs.push(clean_parens(e));
            }
            Array(new_exprs, span).with_span(span)
        }
        FunctionCall(func, type_args, args, span) => {
            let func_clean = clean_parens(*func.expr);
            let mut new_args = vec![];
            for a in args {
                new_args.push(clean_parens(a));
            }
            FunctionCall(ExprBox::new(func_clean), type_args, new_args, span).with_span(span)
        }
        IndexAccess(base_obj, index_expr, span) => {
            let base_obj_clean = clean_parens(*base_obj.expr);
            let index_expr_clean = clean_parens(*index_expr.expr);
            IndexAccess(
                ExprBox::new(base_obj_clean),
                ExprBox::new(index_expr_clean),
                span,
            )
            .with_span(span)
        }
        MemberAccess(base, member) => {
            let base_clean = clean_parens(*base.expr);
            MemberAccess(ExprBox::new(base_clean), member).with_span(expr.span)
        }
        _ => expr,
    }
}

pub struct Parser<'tok> {
    parsing_state: Vec<ParsingState>,
    tokens: &'tok TokenTable,
    pub errors: Vec<(ParsingErrorDetails, TokenSpanIndex)>,
    irrecoverable_error: bool,
}

struct ParsingState {
    operator_stack: Vec<SpannedOperator>,
    operand_stack: Vec<SpanExpr>,
    index: usize,
    current_indent: usize,
}

#[derive(Debug, Clone)]
pub enum ParsingErrorDetails {
    ExprError(String),
    InvalidSyntax(String),
    ContextForError(String, Box<ParsingErrorDetails>),
    Fatal(String),
    //TypeBoundMissingTypeSpecifier,
    TypeBoundExpectedColonAfterFieldName,
}

impl ToString for ParsingErrorDetails {
    fn to_string(&self) -> String {
        match self {
            ParsingErrorDetails::ExprError(e) => format!("Expression error: {e}"),
            ParsingErrorDetails::InvalidSyntax(e) => format!("Invalid syntax: {e}"),
            ParsingErrorDetails::ContextForError(context, e) => {
                format!("{context}, error: {err}", err = e.to_string())
            }
            ParsingErrorDetails::Fatal(e) => {
                format!("Error: {e}")
            }
            ParsingErrorDetails::TypeBoundExpectedColonAfterFieldName => {
                "Error: Expected column after field name".to_string()
            }
        }
    }
}

#[derive(Debug)]
pub enum ParsingEvent<T> {
    //When the compiler is trying to parse X but found Y, must ignore and retry using another grammar rule,
    //for instance, it is starting to parse a while statement, but found a def keyword
    TryAnotherGrammarRule,
    //When the compiler found the correct grammar rule, but the input is not legal. It can continue parsing the next elements to report more errors.
    //However the parser will generate AST that is unusable by the next stages
    GrammarRuleFail(ParsingErrorDetails, TokenSpanIndex),
    //When there is a syntax error so big that there is just no recovering from it, continuing parsing would only throw hundreds more errors.
    //Those are most errors @TODO maybe should be the only one?
    NonRecoverable(ParsingErrorDetails, TokenSpanIndex),
    //When parsing of a grammar rule succeeds
    Success(T),
}

pub type ASTParsingEvent = ParsingEvent<SpanAST>;

impl<T> FromResidual<ParsingEvent<!>> for ParsingEvent<T> {
    fn from_residual(residual: ParsingEvent<!>) -> Self {
        match residual {
            ParsingEvent::TryAnotherGrammarRule => ParsingEvent::TryAnotherGrammarRule,
            ParsingEvent::GrammarRuleFail(details, token) => {
                ParsingEvent::GrammarRuleFail(details, token)
            }
            ParsingEvent::NonRecoverable(details, token) => {
                ParsingEvent::NonRecoverable(details, token)
            }
            ParsingEvent::Success(s) => s, //this returns Never, thus it typechecks, but it will never be called
        }
    }
}

impl<T> Try for ParsingEvent<T> {
    type Output = T;

    type Residual = ParsingEvent<!>;

    fn from_output(output: Self::Output) -> Self {
        ParsingEvent::Success(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            ParsingEvent::Success(res) => ControlFlow::Continue(res),
            ParsingEvent::TryAnotherGrammarRule => {
                ControlFlow::Break(ParsingEvent::TryAnotherGrammarRule)
            }
            ParsingEvent::GrammarRuleFail(details, token) => {
                ControlFlow::Break(ParsingEvent::GrammarRuleFail(details, token))
            }
            ParsingEvent::NonRecoverable(details, token) => {
                ControlFlow::Break(ParsingEvent::NonRecoverable(details, token))
            }
        }
    }
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

    #[cfg(test)]
    pub fn get_errors(&self) -> &[(ParsingErrorDetails, TokenSpanIndex)] {
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
        self.parsing_state.last_mut().unwrap().current_indent
    }

    fn set_cur(&mut self, parsing_state: &ParsingState) {
        self.parsing_state.last_mut().unwrap().index = parsing_state.index;
        self.parsing_state.last_mut().unwrap().current_indent = parsing_state.current_indent;
    }

    fn set_cur_for_expr(&mut self, parsing_state: &ParsingState) {
        self.parsing_state.last_mut().unwrap().index = parsing_state.index;
        self.parsing_state.last_mut().unwrap().current_indent = parsing_state.current_indent;
        self.parsing_state
            .last_mut()
            .unwrap()
            .operand_stack
            .extend(parsing_state.operand_stack.iter().cloned());
        self.parsing_state
            .last_mut()
            .unwrap()
            .operator_stack
            .extend(parsing_state.operator_stack.iter().cloned());
    }

    fn peek(&self) -> &TokenData {
        self.cur_offset(0)
    }

    fn prev_token(&self) -> Option<&TokenData> {
        self.cur_offset_opt(-1)
    }

    fn cur_offset(&self, offset: isize) -> &TokenData {
        self.cur_offset_opt(offset).unwrap()
    }

    fn cur_offset_opt(&self, offset: isize) -> Option<&TokenData> {
        let index = self.parsing_state.last().unwrap().index as isize + offset;
        self.tokens.tokens.get(index as usize)
    }

    fn is_not_end(&self) -> bool {
        self.parsing_state.last().unwrap().index < self.tokens.tokens.len()
    }

    fn can_peek_on_line(&self) -> bool {
        self.is_not_end() && !self.cur_is_newline()
    }

    fn cur_is_newline(&self) -> bool {
        matches!(self.peek().token, Token::NewLine)
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
        &self.parsing_state.last().unwrap().operand_stack
    }

    fn operator_stack(&self) -> &[SpannedOperator] {
        &self.parsing_state.last().unwrap().operator_stack
    }

    fn operand_stack_mut(&mut self) -> &mut Vec<SpanExpr> {
        &mut self.parsing_state.last_mut().unwrap().operand_stack
    }

    fn operator_stack_mut(&mut self) -> &mut Vec<SpannedOperator> {
        &mut self.parsing_state.last_mut().unwrap().operator_stack
    }

    pub fn parse_variable_assign(&mut self) -> ASTParsingEvent {
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

        if !self.can_peek_on_line() {
            self.pop_stack();
            return ParsingEvent::TryAnotherGrammarRule;
        }

        let Token::Assign = self.peek().token else {
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
            Err(e) => self.non_recoverable(ParsingErrorDetails::ContextForError(
                "Expected expression after assign".into(),
                Box::new(e),
            )),
        }
    }

    pub fn parse_variable_declaration(&mut self) -> ASTParsingEvent {
        let decl = self.parse_type_bound_name();

        let Ok(Some(typed_var_decl)) = decl else {
            return ParsingEvent::TryAnotherGrammarRule;
        };
        //no need to do .next here, parse_type_bound_name already does a .next()

        let cur = self.peek();
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
                Err(e) => self.grammar_fail(ParsingErrorDetails::ContextForError(
                    "Expected expression after assign".to_string(),
                    e.into(),
                )),
            }
        } else {
            ParsingEvent::TryAnotherGrammarRule
        }
    }

    pub fn guard(&mut self, tok: Token) -> ParsingEvent<()> {
        if self.peek().token == tok {
            self.next();
            if !self.can_peek_on_line() {
                ParsingEvent::TryAnotherGrammarRule
            } else {
                ParsingEvent::Success(())
            }
        } else {
            ParsingEvent::TryAnotherGrammarRule
        }
    }

    pub fn expect(&mut self, tok: Token, message: &'static str) -> ParsingEvent<()> {
        if self.peek().token == tok {
            self.next();
            ParsingEvent::Success(())
        } else {
            let token = *self.peek();
            self.irrecoverable_error = true;
            ParsingEvent::NonRecoverable(
                ParsingErrorDetails::InvalidSyntax(format!(
                    "{message}, expected {} got {}",
                    tok.name(),
                    self.peek().token.name()
                )),
                token.span_index,
            )
        }
    }

    pub fn expect_colon_newline(&mut self, message: &'static str) -> ParsingEvent<()> {
        self.expect(Token::Colon, message)?;
        self.expect(Token::NewLine, message)
    }

    pub fn expect_id(&mut self, role: &'static str) -> ParsingEvent<StringSpan> {
        let token = *self.peek();

        if let Token::Identifier(id) = token.token {
            self.next();
            ParsingEvent::Success(id.token_spanned(token.span_index))
        } else {
            ParsingEvent::GrammarRuleFail(
                ParsingErrorDetails::InvalidSyntax(format!(
                    "Expected {}, got {}",
                    role,
                    self.peek().token.name()
                )),
                token.span_index,
            )
        }
    }

    pub fn parse_if_statement(&mut self) -> ASTParsingEvent {
        let begin = self.peek().span_index;
        self.guard(Token::IfKeyword)?;

        let expr = match self.parse_expr() {
            Ok(result) => result.resulting_expr,
            Err(e) => {
                return self.grammar_fail(ParsingErrorDetails::ContextForError(
                    "Expected expression for if statement".into(),
                    e.into(),
                ));
            }
        };

        self.expect_colon_newline("after if statement expression");

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

        if !self.can_peek_on_line() || identation_else != cur_identation {
            self.pop_stack();
            return ParsingEvent::Success(if_statement);
        }

        if let Token::ElseKeyword = self.peek().token {
            self.next();
            self.expect_colon_newline("after else keyword");

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
                        return self.non_recoverable(ParsingErrorDetails::Fatal(
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

    pub fn parse_structdef(&mut self) -> ASTParsingEvent {
        let begin = self.peek().span_index;

        self.guard(Token::StructDef)?;

        let struct_name = self.expect_id("struct name")?;

        let type_parameters = match self.parse_type_parameters() {
            Ok(value) => value,
            Err(err) => return self.non_recoverable(err),
        };
        self.expect_colon_newline("after struct name in struct definition");

        let struct_def = indented!(self, {
            let mut fields = vec![];

            loop {
                self.skip_whitespace_newline();
                if !self.can_peek_on_line() {
                    break;
                }
                let Token::Identifier(_) = self.peek().token else {
                    break;
                };
                //@TODO improve error reporting here, remove unwraps
                let parsed = self.parse_type_bound_name().unwrap().unwrap();
                fields.push(parsed);

                if !self.is_not_end() {
                    break;
                }

                let Token::NewLine = self.peek().token else {
                    break;
                };

                self.next();
                if !self.can_peek_on_line() {
                    break;
                }
                //if a second newline is found, then the struct declaration is found and finished
                if let Token::NewLine = self.peek().token {
                    break;
                }
            }

            AST::StructDeclaration {
                struct_name,
                type_parameters,
                body: fields,
            }
            .span_prefixed(begin)
        });

        ParsingEvent::Success(struct_def)
    }

    pub fn parse_impl(&mut self) -> ASTParsingEvent {
        let begin = self.peek().span_index;

        self.guard(Token::ImplKeyword)?;

        let struct_name = self.expect_id("struct impl name")?;

        let type_parameters = match self.parse_type_parameters() {
            Ok(value) => value,
            Err(err) => return self.non_recoverable(err),
        };
        self.expect_colon_newline("after struct name in struct definition");

        let methods = indented!(self, {
            let mut methods = vec![];
            let cur_identation = self.get_expected_indent();

            loop {
                self.new_stack();
                let indentation = self.skip_whitespace_newline();
                if !self.can_peek_on_line() || indentation != cur_identation {
                    self.pop_stack();
                    break;
                }

                let method = self.parse_function_or_method_declaration();
                let s = self.pop_stack();
                match method {
                    ParsingEvent::Success(method) => {
                        methods.push(method);
                        self.set_cur(&s);
                    }
                    ParsingEvent::TryAnotherGrammarRule => {
                        break;
                    }
                    ParsingEvent::GrammarRuleFail(details, token) => {
                        return ParsingEvent::GrammarRuleFail(details, token);
                    }
                    ParsingEvent::NonRecoverable(details, token) => {
                        return ParsingEvent::NonRecoverable(details, token);
                    }
                }
            }

            methods
        });

        ParsingEvent::Success(
            AST::ImplDeclaration {
                struct_name,
                type_parameters,
                body: methods,
            }
            .span_prefixed(begin),
        )
    }

    fn parse_type_parameters(&mut self) -> Result<Vec<StringSpan>, ParsingErrorDetails> {
        let mut type_parameters = vec![];
        if let Token::Operator(Operator::Less) = self.peek().token {
            self.next();
            loop {
                let cur = self.peek();
                if let Token::Identifier(param) = cur.token {
                    type_parameters.push(param.token_spanned(cur.span_index));
                } else {
                    return Err(ParsingErrorDetails::Fatal(
                        "Expected identifier for generic parameter".into(),
                    ));
                }
                self.next();
                if let Token::Comma = self.peek().token {
                    self.next();
                } else {
                    break;
                }
            }
            if let Token::Operator(Operator::Greater) = self.peek().token {
                self.next();
            } else {
                return Err(ParsingErrorDetails::Fatal(
                    "Expected > after generic parameters".into(),
                ));
            }

            if type_parameters.is_empty() {
                return Err(ParsingErrorDetails::Fatal(
                    "Expected at least one generic parameter".into(),
                ));
            }
        }
        Ok(type_parameters)
    }

    //this is in a call, like do_something<Some<Complicated>, Type<H<e<R>>, E>>()
    fn parse_type_arguments_in_generic_position(
        &mut self,
    ) -> Result<Vec<ASTType>, ParsingErrorDetails> {
        let mut type_parameters = vec![];
        if let Token::Operator(Operator::Less) = self.peek().token {
            self.next();

            loop {
                let type_parsed = self.parse_type_name();
                if let Some(ty) = type_parsed {
                    type_parameters.push(ty);
                } else {
                    return Err(ParsingErrorDetails::Fatal(
                        "Expected type name for generic parameter".into(),
                    ));
                }
                //the parse_type_name leaves the state in the next token after the type name, for instance:
                //<List<T>, i32> imagine we just parsed List<T>, the cur token is the comma
                //<List<T>> imagine we just parsed List<T>, the cur token is the >
                if let Token::Comma = self.peek().token {
                    self.next();
                } else {
                    break;
                }
            }
            if let Token::Operator(Operator::Greater) = self.peek().token {
                self.next();
            } else {
                return Err(ParsingErrorDetails::Fatal(
                    "Expected > after generic parameters".into(),
                ));
            }

            if type_parameters.is_empty() {
                return Err(ParsingErrorDetails::Fatal(
                    "Expected at least one generic parameter".into(),
                ));
            }
        }
        Ok(type_parameters)
    }

    pub fn parse_while_statement(&mut self) -> ASTParsingEvent {
        let begin = self.peek().span_index;
        self.guard(Token::WhileKeyword)?;

        let expr = match self.parse_expr() {
            Ok(result) => result.resulting_expr,
            Err(e) => {
                return self.grammar_fail(ParsingErrorDetails::ContextForError(
                    "Expected expression for while statement".into(),
                    e.into(),
                ));
            }
        };

        self.expect_colon_newline("after expression in while statement");

        ParsingEvent::Success(indented!(self, {
            let ast = self.parse_ast();
            AST::WhileStatement {
                expression: expr,
                body: ast,
            }
            .span_prefixed(begin)
        }))
    }

    pub fn parse_for_statement(&mut self) -> ASTParsingEvent {
        let begin = self.peek().span_index;
        self.guard(Token::ForKeyword)?;
        let variable_name = self.expect_id("variable name in for loop")?;
        self.expect(Token::InKeyword, "parsing for loop statement")?;

        let expr = match self.parse_expr() {
            Ok(result) => result.resulting_expr,
            Err(e) => {
                return self.grammar_fail(ParsingErrorDetails::ContextForError(
                    "Expected expression for iterable in for statement".into(),
                    e.into(),
                ));
            }
        };

        self.expect_colon_newline("after iterable expression in for statement");

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

    //This method leaves the cursor in the next token after the type
    pub fn parse_type_name(&mut self) -> Option<ASTType> {
        //first we expect a type name
        let begin = self.peek().span_index;
        let Token::Identifier(type_name) = self.peek().token else {
            return None;
        };

        let base_type = type_name.token_spanned(begin);
        //type name confirmed, check next
        self.next();

        //we're finished, just return
        if !self.can_peek_on_line() {
            return Some(ASTType::Simple(base_type));
        }

        let next = self.peek();

        //println!("next token = {:?}", next.token);

        match next.token {
            //if the next token is <, means we have next generics
            Token::Operator(Operator::Less) => {
                self.next(); //go to what is likely the nested type name
                let parsed = self.parse_type_name().unwrap();
                let generic = Some(ASTType::Generic(
                    type_name.token_spanned(begin),
                    vec![parsed],
                ));
                //the parse_type_name leaves the state in the next token after the type name, which will have to be the >
                if self.peek().token != Token::Operator(Operator::Greater) {
                    panic!(
                        "After parsing nested generics, expected token to be < but it is = {:?}",
                        self.peek()
                    )
                } //read<List<T>[>] next will be in []
                self.next();
                generic
            }
            _ => {
                //if it's not < then we assume another syntax would start to be parsed,
                Some(ASTType::Simple(base_type))
            }
        }
    }

    //Tries to parse a bound name with its type, for instance var: i32
    //leaves cursor in the next token after the type
    pub fn parse_type_bound_name(&mut self) -> Result<Option<TypeBoundName>, ParsingErrorDetails> {
        let begin = self.peek().span_index;
        let Token::Identifier(name) = self.peek().token else {
            return Ok(None);
        };
        self.next();

        if !self.can_peek_on_line() {
            return Ok(None);
        }

        let Token::Colon = self.peek().token else {
            return Err(ParsingErrorDetails::TypeBoundExpectedColonAfterFieldName);
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

    pub fn parse_def_function_statement(&mut self) -> ParsingEvent<SpanAST> {
        let begin = self.peek().span_index;
        let function = self.parse_function_or_method_declaration()?;
        ParsingEvent::Success(AST::DeclareFunction(function).span_prefixed(begin))
    }

    pub fn parse_function_or_method_declaration(&mut self) -> ParsingEvent<FunctionDeclaration> {
        self.guard(Token::DefKeyword)?;
        let function_name = self.expect_id("function name")?;

        let type_parameters = match self.parse_type_parameters() {
            Ok(value) => value,
            Err(err) => return self.non_recoverable(err),
        };

        self.expect(Token::OpenParen, "parsing function declaration")?;

        let mut params: Vec<TypeBoundName> = vec![];

        let mut has_self = false;
        if let Token::SelfKeyword = self.peek().token {
            has_self = true;
            self.next();
        }
        if has_self && let Token::Comma = self.peek().token {
            self.next();
        }
        while let Token::Identifier(_) = self.peek().token {
            let param = self.parse_type_bound_name().unwrap().unwrap();
            params.push(param);
            if let Token::Comma = self.peek().token {
                self.next();
            } else {
                break;
            }
        }
        let mut is_varargs = false;
        if let Token::Ellipsis = self.peek().token {
            is_varargs = true;
            self.next();
        }

        if let Token::CloseParen = self.peek().token {
            self.next();
        } else {
            return self.non_recoverable(ParsingErrorDetails::InvalidSyntax(format!(
                "Expected close paren after parameters in function declaration, got {:?}",
                self.peek().token
            )));
        }

        let mut return_type: Option<ASTType> = None;

        if let Token::ArrowRight = self.peek().token {
            self.next();

            return_type = self.parse_type_name();

            if return_type.is_none() {
                return self.non_recoverable(ParsingErrorDetails::Fatal(
                    "Expected type name after arrow right on function declaration".into(),
                ));
            }
        }

        self.expect(
            Token::Colon,
            "after parsing function signature in a function declaration",
        )?;

        ParsingEvent::Success(indented!(self, {
            let ast = self.parse_ast();

            FunctionDeclaration {
                function_name,
                parameters: params,
                type_parameters,
                body: ast,
                return_type,
                is_varargs,
                is_bound_to_self: has_self,
            }
        }))
    }

    pub fn parse_break(&mut self) -> ASTParsingEvent {
        let tok = *self.peek();
        if let Token::BreakKeyword = tok.token {
            self.next();

            return ParsingEvent::Success(AST::Break(tok.span_index.get_span()).self_spanning());
        }

        ParsingEvent::TryAnotherGrammarRule
    }

    pub fn parse_return(&mut self) -> ASTParsingEvent {
        let tok = *self.peek();
        if let Token::ReturnKeyword = tok.token {
            self.next();
            if self.can_peek_on_line() {
                let expr = match self.parse_expr() {
                    Ok(result) => result.resulting_expr,
                    Err(e) => {
                        return self.grammar_fail(ParsingErrorDetails::ContextForError(
                            "Expected expression on return statement".into(),
                            e.into(),
                        ));
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

    pub fn parse_intrinsic(&mut self) -> ASTParsingEvent {
        let tok = *self.peek();
        if let Token::IntrinsicKeyword = tok.token {
            self.next();
            if !self.can_peek_on_line() {
                return ParsingEvent::Success(
                    AST::Intrinsic(tok.span_index.get_span()).self_spanning(),
                );
            } else {
                return self.grammar_fail(ParsingErrorDetails::InvalidSyntax(
                    "Intrinsic must be the only content of the method if it's present".to_string(),
                ));
            }
        }
        ParsingEvent::TryAnotherGrammarRule
    }

    pub fn parse_external(&mut self) -> ASTParsingEvent {
        let tok = *self.peek();
        if let Token::ExternalKeyword = tok.token {
            self.next();
            if !self.can_peek_on_line() {
                return ParsingEvent::Success(
                    AST::External(tok.span_index.get_span()).self_spanning(),
                );
            } else {
                return self.grammar_fail(ParsingErrorDetails::InvalidSyntax(
                    "External must be the only content of the method if it's present".to_string(),
                ));
            }
        }
        ParsingEvent::TryAnotherGrammarRule
    }

    pub fn parse_standalone_expr(&mut self) -> ASTParsingEvent {
        let parsed_expr = self.parse_expr();
        match parsed_expr {
            Ok(result) => {
                ParsingEvent::Success(
                    AST::StandaloneExpr(result.resulting_expr).self_spanning(),
                )
            }
            Err(e) => {
                self.grammar_fail(ParsingErrorDetails::ContextForError(
                    "Expected expression".into(),
                    e.into(),
                ))
            }
        }
    }

    //returns the identation level until the first non-whitespace token
    //final state of this function is right at newline, before the identations
    fn skip_whitespace_newline(&mut self) -> usize {
        let mut identation_level = 0;
        while self.is_not_end() {
            match self.peek().token {
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
            match self.peek().token {
                Token::NewLine => {
                    self.next();
                    return;
                }
                _ => self.next(),
            }
        }
    }

    //Returns whether parsing can continue
    fn handle_parsing_result(
        &mut self,
        name: &str,
        parsing_result: ASTParsingEvent,
        results: &mut Vec<SpanAST>,
        parsed_successfully: &mut bool,
    ) -> bool {
        match parsing_result {
            ParsingEvent::Success(parsed_result) => {
                results.push(parsed_result);
                *parsed_successfully = true;
                let popped = self.pop_stack();
                //correct indentation found: commit
                self.set_cur(&popped);

                if self.is_not_end() && !self.cur_is_newline() {
                    let cur = self.peek();
                    let error_msg = format!(
                        "Newline or EOF expected after {}, got {:?}",
                        name, cur.token
                    );

                    self.errors.push((
                        ParsingErrorDetails::InvalidSyntax(error_msg),
                        cur.span_index,
                    ));
                    return false;
                }
                true
            }
            ParsingEvent::TryAnotherGrammarRule => {
                self.pop_stack();
                true
            }
            ParsingEvent::GrammarRuleFail(error, token) => {
                self.skip_to_next_line();
                self.errors.push((error, token));
                true
            }
            ParsingEvent::NonRecoverable(error, token) => {
                self.errors.push((error, token));
                false
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
                    if !self.handle_parsing_result(
                        $name,
                        parsing_result,
                        &mut results,
                        &mut parsed_successfully,
                    ) {
                        break;
                    }
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

            try_parse!("variable assignment", parse_variable_assign);
            try_parse!("variable declaration", parse_variable_declaration);
            try_parse!("if block", parse_if_statement);
            try_parse!("while block", parse_while_statement);
            try_parse!("for block", parse_for_statement);
            try_parse!("function definition", parse_def_function_statement);
            try_parse!("break", parse_break);
            try_parse!("return", parse_return);
            try_parse!("struct definition", parse_structdef);
            try_parse!("impl definition", parse_impl);
            try_parse!("intrinsic", parse_intrinsic);
            try_parse!("external", parse_external);
            try_parse!("standalone expression", parse_standalone_expr);

            if !parsed_successfully {
                let error = self.non_recoverable(ParsingErrorDetails::Fatal(
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
            let error = self.non_recoverable(ParsingErrorDetails::Fatal(
                "Unrecognized expression/statement".to_string(),
            ));
            self.handle_parsing_result("", error, &mut results, &mut parsed_successfully);
        }

        results
    }

    fn index_access_helper(
        &mut self,
        indexable_expr: SpanExpr,
    ) -> Result<SpanExpr, ParsingErrorDetails> {
        if let Token::CloseParen = self.peek().token {
            return Err(ParsingErrorDetails::InvalidSyntax(
                "Invalid syntax: must inform index value".to_string(),
            ));
        }

        self.new_stack();
        let list_of_exprs = self.parse_comma_sep_list_expr();

        let ending = self.peek().span_index;

        match list_of_exprs {
            //try parse stuff
            Ok(expressions) => {
                //commit the result
                let popped = self.pop_stack();
                let mut resulting_exprs = expressions.resulting_expr_list;
                if resulting_exprs.len() != 1 {
                    return Err(ParsingErrorDetails::InvalidSyntax(
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

    fn function_call_helper(
        &mut self,
        expr_callable: SpanExpr,
    ) -> Result<SpanExpr, ParsingErrorDetails> {
        if let Token::CloseParen = self.peek().token {
            return Ok(FunctionCall(
                ExprBox::new(expr_callable),
                vec![],
                vec![],
                self.peek().span_index.get_span(),
            )
            .self_spanning());
        }

        self.new_stack();
        let list_of_exprs = self.parse_comma_sep_list_expr();
        let ending = self.peek().span_index;

        match list_of_exprs {
            //try parse stuff
            Ok(expressions) => {
                //commit the result
                let popped = self.pop_stack();
                let resulting_exprs = expressions.resulting_expr_list;

                let fcall = FunctionCall(
                    ExprBox::new(expr_callable),
                    vec![],
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
    pub fn parse_expr(&mut self) -> Result<ParseExpressionResult, ParsingErrorDetails> {
        loop {
            if !self.can_peek_on_line() {
                break;
            }
            let mut was_operand = false;
            let mut not_part_of_expr = false;
            //if there is an open paren, we collect all the tokens for this open paren
            //and parse the sub-expression recursively
            {
                let tok = *self.peek();

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
                        // - `function_call<T>()` also valid when function has generics
                        let mut could_be_fcall = true;

                        if prev_token.is_none() {
                            could_be_fcall = false;
                        }
                        if self.operand_stack().is_empty() {
                            could_be_fcall = false;
                        }
                        if let Some(Token::Operator(op)) = prev_token {
                            if let Operator::Greater = op {
                                could_be_fcall = true;
                            } else {
                                could_be_fcall = false;
                            }
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
                            if let Token::CloseArrayBracket = self.peek().token {
                                self.push_operand(
                                    Array(vec![], self.peek().span_index.get_span())
                                        .self_spanning(),
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
                                                self.peek().span_index.get_span(),
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
                        //println!("Parsing identifier {}", identifier_str);
                        self.parse_identifier(identifier_str, &tok);
                        was_operand = true;
                    }
                    Token::MemberAccessor => {
                        //next token should be an identifier
                        self.next();
                        let popped = self.operand_stack_mut().pop();
                        let cur_token = self.peek();
                        if let Token::Identifier(name) = cur_token.token {
                            let cur_expr = popped.unwrap();
                            let member_access_expr = MemberAccess(
                                ExprBox::new(cur_expr),
                                name.token_spanned(cur_token.span_index),
                            );
                            self.push_operand(member_access_expr.self_spanning());
                            was_operand = true;
                        } else {
                            return Err(ParsingErrorDetails::ExprError(
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
                    Token::LiteralChar(c) => {
                        self.push_operand(CharValue(c, tok.span_index.get_span()).self_spanning());
                        was_operand = true;
                    }
                    Token::SelfKeyword => {
                        self.push_operand(SelfValue(tok.span_index.get_span()).self_spanning());
                        was_operand = true;
                    }
                    Token::LiteralString(s) => {
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
                    Token::Operator(Operator::Greater) => {
                        //This is some logic to check if the > is a right shift or not, because >> could be closing nested generics, like List<List<T>>.
                        //In this context we're parsing an expression, so we can assume >> is a right shift.

                        //Current is >, peek the next one to see if it's also > but don't move the cursor
                        if let Token::Operator(Operator::Greater) = self.cur_offset(1).token {
                            //it's a right shift
                            self.push_operator(SpannedOperator(
                                Operator::BitShiftRight,
                                tok.span_index.get_span(),
                            ));
                            self.next(); //commits the right shift
                        } else {
                            self.push_operator(SpannedOperator(
                                Operator::Greater,
                                tok.span_index.get_span(),
                            ));
                        }
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

            if self.can_peek_on_line() {
                match self.peek().token {
                    //if it's a member accessor, then we skip it and continue parsing. This is because
                    //we could be on a binary expression: obj.field1 + obj.field2. If you remove this continue, it will parse as (obj.field1 + obj).field2.

                    //Remember also: *c.x = 1 in C means a deref on x, not c!
                    Token::MemberAccessor => continue,
                    //if the token is an open paren... then wait a minute, could be a function call!
                    //same thing for an open bracket! could be an array!
                    //try parse it first!
                    Token::OpenArrayBracket => continue,
                    Token::OpenParen => continue,
                    _ => {}
                }
            }

            if was_operand {
                //if it was an operand && the next token is `as`, then we cast
                if self.can_peek_on_line() {
                    let cur = self.peek().token;
                    if let Token::AsKeyword = cur {
                        self.next();
                        let expr = self.operand_stack_mut().pop().unwrap();
                        let target_type = self.parse_type_name();
                        let start = expr.get_span().start;
                        match target_type {
                            Some(target_type) => {
                                let end = target_type.get_span().end;
                                let resulting_expr = Expr::Cast(
                                    ExprBox { expr: expr.into() },
                                    target_type,
                                    AstSpan { start, end },
                                );
                                self.push_operand(resulting_expr.self_spanning());
                            }
                            None => {
                                return Err(ParsingErrorDetails::ExprError(
                                    "Cast expression failed: expected type name after `as` keyword"
                                        .into(),
                                ));
                            }
                        }
                    }
                }
                //after the cast we still go ahead and parse the rest of the expression

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
            return Err(ParsingErrorDetails::ExprError(format!(
                "Unparsed operators: {:?}, operands = {:?}",
                self.operator_stack(),
                self.operand_stack()
            )));
        }

        if self.operand_stack().len() > 1 {
            return Err(ParsingErrorDetails::ExprError(format!(
                "Unparsed operands: {:?}",
                self.operand_stack()
            )));
        }

        if self.operand_stack().is_empty() {
            return Err(ParsingErrorDetails::ExprError(String::from(
                "Unexpected empty operand stack",
            )));
        }
        //let remaining_tokens = Vec::from(token_queue);
        let resulting_expr = clean_parens(self.operand_stack_mut().pop().unwrap());

        Ok(ParseExpressionResult { resulting_expr })
    }

    fn parse_identifier(&mut self, identifier_str: InternedString, tok: &TokenData) {
        //We are parsing an identifier. If we find <, we are parsing a generic function call, which *can* be followed by either () or (expr, expr, ..., expr).
        self.new_stack();

        self.next();
        //simplest case: it's an identifier in the end of the line/file
        if !self.can_peek_on_line() {
            self.pop_stack();
            self.push_operand(
                Variable(identifier_str.token_spanned(tok.span_index)).self_spanning(),
            );
            return;
        }
        let mut type_parameters: Vec<ASTType> = vec![];
        let cur = self.peek();
        if let Token::Operator(Operator::Less) = cur.token {
            //@TODO this parse_type_parameters only works for simple cases, like <T, U, V>, but not on more complex ones like List<List<T>> for instance
            type_parameters = match self.parse_type_arguments_in_generic_position() {
                Ok(value) => value,
                Err(_) => {
                    self.pop_stack();
                    self.push_operand(
                        Variable(identifier_str.token_spanned(tok.span_index)).self_spanning(),
                    );
                    return;
                }
            };
        }

        if !self.can_peek_on_line()
            && !type_parameters.is_empty()
            && self.peek().token != Token::OpenParen
        {
            todo!(
                "Make error reporting work here: expected ( for parameter list after generic types, type params: {type_parameters:?}"
            );
        }

        if self.peek().token != Token::OpenParen && type_parameters.is_empty() {
            self.pop_stack();
            self.push_operand(
                Variable(identifier_str.token_spanned(tok.span_index)).self_spanning(),
            );
            return;
        }

        //if we found type parameters, then the type name parser leaves the cursor at the next token after all the type parameters and < > characters.
        //In this case, if we are about to parse a function call, we are already in the open paren. Otherwise we advance to the open paren.
        //if type_parameters.is_empty() {
        self.next();
        //}

        let params = if self.peek().token == Token::CloseParen {
            vec![]
        } else {
            self.parse_comma_sep_list_expr()
                .expect("Could not parse parameters for some reason")
                .resulting_expr_list
        };

        if self.peek().token != Token::CloseParen {
            todo!("Make error reporting work here: expected ) after parameter list");
        }
        let cur = self.peek();
        self.push_operand(
            Expr::FunctionCall(
                ExprBox::new(
                    Expr::Variable(identifier_str.token_spanned(tok.span_index)).self_spanning(),
                ),
                type_parameters,
                params,
                cur.span_index.get_span(),
            )
            .self_spanning(),
        );
        let popped = self.pop_stack();
        self.set_cur_for_expr(&popped);
    }

    //expr, expr, ..., expr
    fn parse_comma_sep_list_expr(
        &mut self,
    ) -> Result<ParseListExpressionResult, ParsingErrorDetails> {
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

            if self.can_peek_on_line() {
                if let Token::Comma = self.peek().token {
                    self.next();
                    continue;
                }
                break;
            }
            break;
        }

        if expressions.is_empty() {
            return Err(ParsingErrorDetails::Fatal(
                "While parsing list of expressions, no expression was found. This is an internal error of the compiler. The parser should check for closing parens or brackets before calling this function.".to_string()));
        }

        Ok(ParseListExpressionResult {
            resulting_expr_list: expressions,
        })
    }

    fn non_recoverable<T>(&mut self, parsing_error: ParsingErrorDetails) -> ParsingEvent<T> {
        self.irrecoverable_error = true;
        let cur = self.peek();
        ParsingEvent::NonRecoverable(parsing_error, cur.span_index)
    }

    fn grammar_fail<T>(&mut self, parsing_error: ParsingErrorDetails) -> ParsingEvent<T> {
        let cur = self.peek();
        ParsingEvent::GrammarRuleFail(parsing_error, cur.span_index)
    }
}

struct ParseListExpressionResult {
    //remaining_tokens: Vec<&'a Token>,
    resulting_expr_list: Vec<SpanExpr>,
}

pub struct ParseExpressionResult {
    resulting_expr: SpanExpr,
}

#[allow(dead_code)]
pub fn parse_ast<'a>(
    tokens: &'a TokenTable,
    file_table: &[FileTableEntry],
) -> (Vec<SpanAST>, Parser<'a>) {
    let mut parser = Parser::new(tokens);
    let result = parser.parse_ast();

    print_errors(&parser, file_table, tokens);

    (result, parser)
}

pub fn print_errors(parser: &Parser, file_table: &[FileTableEntry], tokens: &TokenTable) {
    for (error, token) in parser.errors.iter() {
        let err = error.to_string();
        let file_name = &file_table[token.file.0].path;
        let tok = tokens.tokens[token.index];
        let span = tokens.spans[tok.span_index.index];
        let line = span.start.line;
        let column = span.start.column;
        println!("{err}\nat {file_name}:{line}:{column}\n");
    }
}

#[cfg(test)]
mod tests {

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

    fn tokenize(str: &str) -> TokenTable {
        match crate::ast::lexer::tokenize(FileTableIndex(0), str) {
            crate::ast::lexer::LexerResult::Ok(e) => e,
            _ => panic!("Failed tokenizing"),
        }
    }

    use std::{assert_matches::assert_matches, ops::Deref};

    use crate::{
        ast::ast_printer::{print_ast, print_fully_parenthesized_ast},
        interner::StringInterner,
        semantic::context::{FileTableIndex, test_utils::istr},
    };

    use super::*;

    //Parses a single expression
    fn parse(tokens: TokenTable) -> SpanExpr {
        let table = &[FileTableEntry {
            ast: AST::Break(AstSpan {
                start: TokenSpanIndex {
                    index: 0,
                    file: FileTableIndex(0),
                },
                end: TokenSpanIndex {
                    index: 0,
                    file: FileTableIndex(0),
                },
            }),
            token_table: tokens,
            contents: "none",
            path: "test".to_string(),
            index: FileTableIndex(0),
        }];
        let mut parser = Parser::new(&table[0].token_table);
        print_errors(&parser, table, &table[0].token_table);
        parser.parse_expr().unwrap().resulting_expr
    }

    fn test_parse(tokens: TokenTable) -> Vec<SpanAST> {
        let table = &[FileTableEntry {
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
            token_table: tokens,
            index: FileTableIndex(0),
            contents: "none",
            path: "test".to_string(),
        }];
        let mut parser = Parser::new(&table[0].token_table);

        let result = parser.parse_ast();
        print_errors(&parser, table, &table[0].token_table);
        if !parser.get_errors().is_empty() {
            panic!("Parsing failed");
        }
        result
    }

    #[test]
    fn impl_struct() {
        parse_and_print_back_to_original(
            "
impl SomeStruct:
    def method(self):
        print(self)
    ",
        );
    }

    #[test]
    fn impl_struct_multiple_methods() {
        //You can have empty lines between methods ofc but then it's annoying to test here :)
        parse_and_print_back_to_original(
            "
impl SomeStruct:
    def method(self):
        print(self)
    def method2(self):
        print(self)
    def not_method():
        print(self)
    ",
        );
    }

    #[test]
    fn impl_struct_function_and_then_regular_function_with_newline_in_between() {
        parse_and_print_back_to_original(
            "
impl SomeStruct:
    def method(self):
        print(self)

def not_method():
    print(1)
    ",
        );
    }

    #[test]
    fn impl_struct_function_and_then_regular_function_without_newline_in_between() {
        parse_and_compare(
            "
impl SomeStruct:
    def method(self):
        print(self)
def not_method():
    print(1)
    ",
            "
impl SomeStruct:
    def method(self):
        print(self)

def not_method():
    print(1)
    ",
        );
    }

    #[test]
    fn method_with_params() {
        parse_and_compare(
            "
impl SomeStruct:
    def method(self, param: i32):
        print(self)
    ",
            "
impl SomeStruct:
    def method(self, param: i32):
        print(self)
    ",
        );
    }

    #[test]
    fn just_an_identifier() {
        let _some_id = istr("some_identifier");
        let tokens = tokenize("some_identifier");
        let result = parse(tokens);

        assert_matches!(result.expr, Variable(StringSpan(_some_id, _)));
    }

    use match_deref::match_deref;

    #[cfg(test)]
    impl Deref for StringSpan {
        type Target = str;

        fn deref(&self) -> &Self::Target {
            StringInterner::get().borrow(self.0)
        }
    }

    #[test]
    fn bitshift_right() {
        let tokens = tokenize("x >> y");
        let result = parse(tokens);

        assert!(match_deref! {
            match &result.expr {
                BinaryOperation(
                    Deref @ Variable(Deref @ "x"),
                    SpannedOperator(Operator::BitShiftRight, _),
                    Deref @ Variable(Deref @ "y")
                ) => true,
                _ => panic!("Unexpected parsing output: {:#?}", result.expr)
            }
        });
    }

    #[test]
    fn function_call_without_args() {
        let tokens = tokenize("some_identifier()");
        let result = parse(tokens);

        assert!(match_deref! {
            match &result.expr {
                FunctionCall(Deref @ Variable(Deref @ "some_identifier"),  Deref @ [],  Deref @ [], _) => true,
                _ => panic!("Unexpected parsing output: {:#?}", result.expr)
            }
        });
    }

    #[test]
    fn function_call_with_one_param() {
        let tokens = tokenize("some_identifier(1)");
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall(
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [],
                    Deref @ [Deref @ IntegerValue(1, _)],
                    _
                ) => true,
                _ => false
            }
        })
    }

    #[test]
    fn function_call_with_many_args() {
        let tokens = tokenize("some_identifier(1, 2, 3)");
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall(
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [],
                    Deref @ [Deref @ IntegerValue(1, _), Deref @ IntegerValue(2, _), Deref @ IntegerValue(3, _)],
                    _
                ) => true,
                _ => false
            }
        })
    }

    #[test]
    fn function_call_with_expression() {
        let tokens = tokenize("some_identifier(1 * 2)");
        let result = parse(tokens);

        assert!(match_deref! {
            match &result.expr {
                FunctionCall (
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [],
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
        let tokens = tokenize("some_identifier(1 * 2, 3 + 5, 88)");
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall (
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [],
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
        let tokens = tokenize("some_identifier(nested())");
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall (
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [],
                    Deref @ [
                        Deref @ FunctionCall(
                            Deref @ Variable(Deref @ "nested"),
                            Deref @ [],
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
        let tokens = tokenize("some_identifier(nested(1))");
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall (
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [],
                    Deref @ [
                        Deref @ FunctionCall(
                            Deref @ Variable(Deref @ "nested"),
                            Deref @ [],
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
        let tokens = tokenize("some_identifier(nested(1, 2))");
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall (
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [],
                    Deref @ [
                        Deref @ FunctionCall(
                            Deref @ Variable(Deref @ "nested"),
                            Deref @ [],
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
        let tokens = tokenize("some_identifier(nested(1 * 2, 2 / 3.4))");
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall (
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [],
                    Deref @ [
                        Deref @ FunctionCall(
                            Deref @ Variable(Deref @ "nested"),
                            Deref @ [],
                            Deref @ [
                                Deref @ BinaryOperation (
                                    Deref @ IntegerValue(1, _),
                                    SpannedOperator(Operator::Multiply, _),
                                    Deref @ IntegerValue(2, _)
                                ),
                                Deref @ BinaryOperation (
                                    Deref @ IntegerValue(2, _),
                                    SpannedOperator(Operator::Divide, _),
                                    Deref @ FloatValue(FloatLiteral(_), _)
                                    //not matching the exact 3.4 value here because rust doesn't like float matching
                                    //maybe migrate to parse_and_print_back_to_original
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
        let tokens = tokenize("some_identifier(nested(1), 1)");
        let result = parse(tokens);
        assert!(match_deref! {
            match &result.expr {
                FunctionCall (
                    Deref @ Variable (Deref @ "some_identifier"),
                    Deref @ [],
                    Deref @ [
                        Deref @ FunctionCall(
                            Deref @ Variable(Deref @ "nested"),
                            Deref @ [],
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
    fn simple_less_than() {
        parse_and_print_back_to_original("x < 1000000");
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
        let tokens = tokenize(&unindented);
        let result = test_parse(tokens);
        let printed = print_ast(&result);
        pretty_assertions::assert_eq!(printed.trim(), unindented.trim());
    }

    fn parse_and_compare(source: &str, expected: &str) {
        let unindented_src = unindent(source);
        let tokens = tokenize(&unindented_src);

        let unindented_expected = unindent(expected);

        let result = test_parse(tokens);
        let printed = print_ast(&result);
        pretty_assertions::assert_eq!(printed.trim(), unindented_expected.trim());
    }

    fn parse_and_compare_parenthesized(source: &str, expected: &str) {
        let unindented_src = unindent(source);
        let tokens = tokenize(&unindented_src);

        let unindented_expected = unindent(expected);

        let result = test_parse(tokens);
        let printed = print_fully_parenthesized_ast(&result);
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
    fn parsing_bunch_of_gt_lt() {
        parse_and_compare("<><> 2", "< > < > 2");
    }

    #[test]
    fn bitshift_right_text() {
        parse_and_compare("x >> y", "x >> y");
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

        let tokens = tokenize(&unindent(&source_replaced));
        let result = test_parse(tokens);
        let printed = print_ast(&result);
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
        parse_and_compare("x = \"abc\"", "x = \"abc\"");
    }

    #[test]
    fn assign_char_expr() {
        parse_and_compare("x = 'a'", "x = 'a'");
    }

    #[test]
    fn declare_typed() {
        parse_and_print_back_to_original("x: str = \"abc\"");
    }

    #[test]
    fn assign_string_concat_expr() {
        parse_and_compare("x = \"abc\" + \"cde\"", "x = \"abc\" + \"cde\"");
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
        parse_and_compare(
            "[1, \"two\", '\\n', True, 4.565]",
            "[1, \"two\", '\\n', True, 4.565]",
        );
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
    fn parse_intrinsic_varargs() {
        parse_and_print_back_to_original(
            "
    def varargs(x: i32, ...) -> i32:
        intrinsic
    ",
        );
    }

    #[test]
    fn parse_intrinsic_varargs_only() {
        parse_and_print_back_to_original(
            "
    def varargs(...) -> i32:
        intrinsic
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
    fn struct_definition_set_field() {
        parse_and_print_back_to_original(
            "
struct SomeStruct:
    field: i32
    otherfield: str

def main():
    x = SomeStruct()
    x.field = 1
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
        let tokens = tokenize("*val");
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
    fn set_ptr_simple_index_expr() {
        let unindented = unindent("*array[idx] = 1");
        let tokens = tokenize(&unindented);
        let result = test_parse(tokens);
        let printed = print_fully_parenthesized_ast(&result);

        assert_eq!(printed, "(* (array[idx])) = 1");
    }

    #[test]
    fn set_ptr_get_field_from_ptr() {
        let unindented = unindent("(*c).x = 1");
        let tokens = tokenize(&unindented);
        let result = test_parse(tokens);
        let printed = print_fully_parenthesized_ast(&result);

        assert_eq!(printed, "(* (c)).x = 1");
    }

    #[test]
    fn set_ptr_get_field_from_ptr_unparenthesized() {
        //This is the same behavior as C
        let unindented = unindent("*c.x = 1");
        let tokens = tokenize(&unindented);
        let result = test_parse(tokens);
        let printed = print_fully_parenthesized_ast(&result);

        assert_eq!(printed, "(* (c.x)) = 1");
    }

    #[test]
    fn set_ptr_complex_expr() {
        let unindented =
            unindent("* array[obj.getter().prop[0].some_ptr + obj.getter().prop[1]] = 1");
        let tokens = tokenize(&unindented);
        let result = test_parse(tokens);
        let printed = print_fully_parenthesized_ast(&result);

        assert_eq!(
            printed,
            "(* (array[(obj.getter().prop[0].some_ptr + obj.getter().prop[1])])) = 1"
        );
    }

    #[test]
    fn parse_struct_generic() {
        parse_and_print_back_to_original(
            "
struct SomeStruct<T>:
    field: T
    otherfield: u64
",
        );
    }

    #[test]
    fn parse_generic_function_call() {
        parse_and_print_back_to_original("List<T>()");
    }

    #[test]
    fn parse_generic_function_call_with_multiple_generic_args() {
        parse_and_print_back_to_original("List<T, U, V, X>()");
    }

    #[test]
    fn parse_generic_function_call_with_param() {
        parse_and_print_back_to_original("List<T>(x)");
    }

    #[test]
    fn parse_generic_function_call_with_multiple_params() {
        parse_and_print_back_to_original("List<T>(x, y, z)");
    }

    #[test]
    fn parse_generic_function_call_with_multiple_params_and_generics() {
        parse_and_print_back_to_original("List<T, U, V>(x, y, z)");
    }

    #[test]
    fn parse_generic_function_call_with_multiple_params_and_generics_for_function_returning_call() {
        parse_and_print_back_to_original("make_fn<T, U, V>(x, y, z)()");
    }

    #[test]
    fn parse_generic_function_call_with_multiple_params_and_generics_for_function_returning_call_with_params()
     {
        parse_and_print_back_to_original("make_fn<T, U, V>(x, y, z)(1, \"abc\")");
    }

    #[test]
    fn parse_generic_function_call_with_multiple_params_and_generics_for_function_returning_call_with_params_with_indexing()
     {
        parse_and_print_back_to_original("make_fn<T, U, V>(x, y, z)(1, \"abc\")[0]");
    }

    #[test]
    fn parse_generic_function_call_with_multiple_params_and_generics_for_function_returning_call_with_params_with_indexing_and_unary()
     {
        parse_and_print_back_to_original("& make_fn<T, U, V>(x, y, z)(1, \"abc\")[0]");
    }

    #[test]
    fn parse_generic_function_call_with_multiple_params_and_generics_for_function_returning_call_with_params_with_indexing_and_unary_ref_minus()
     {
        parse_and_print_back_to_original("- & make_fn<T, U, V>(x, y, z)(1, \"abc\")[0]");
    }

    #[test]
    fn binop_crazy_function_call() {
        parse_and_print_back_to_original(
            "make_fn<T, U, V>(x, y, z)(1, \"abc\")[0] * make_fn<X, Y, Z>(g, h, i)(2, \"def\")[1]",
        );
    }

    #[test]
    fn parse_function_generic() {
        parse_and_print_back_to_original(
            "
        def list_new<T>() -> List<T>:
            list = List<T>()
            return list
",
        );
    }

    #[test]
    fn parse_function_nested_generic() {
        parse_and_print_back_to_original(
            "
    def count<T>(list: ptr<List<T>>) -> i32:
        return 1
",
        );
    }

    #[test]
    fn parse_call_with_nested_generic_args() {
        parse_and_print_back_to_original(
            "
read<List<T>>(list_ptr)
        ",
        );
    }

    #[test]
    fn very_simple_cast() {
        parse_and_print_back_to_original("1 as f32");
    }

    #[test]
    fn cast_binary_expr() {
        parse_and_compare_parenthesized("1 + 1 as f32", "(1 + (1 as f32))");
    }

    #[test]
    fn cast_binary_expr_on_both_sides() {
        parse_and_compare_parenthesized("1 as f32 + 1 as f32", "((1 as f32) + (1 as f32))");
    }
}
