use crate::{
    commons::float::FloatLiteral,
    interner::{InternedString, StringInterner},
    semantic::context::FileTableIndex,
};

#[derive(Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub struct TokenSpanIndex(pub usize);

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub struct TokenData {
    pub token: Token,
    pub span_index: TokenSpanIndex,
}

#[derive(Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub struct SourceLocation {
    pub line: u32,
    pub column: u32,
}

#[derive(Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub struct TokenSpan {
    pub file: FileTableIndex,
    pub start: SourceLocation,
    pub end: SourceLocation,
}

pub struct TokenTable {
    pub tokens: Vec<TokenData>,
    pub spans: Vec<TokenSpan>,
}

impl TokenTable {
    fn new() -> Self {
        TokenTable {
            tokens: vec![],
            spans: vec![],
        }
    }

    pub fn add(
        &mut self,
        token: Token,
        file: FileTableIndex,
        start: SourceLocation,
        end: SourceLocation,
    ) {
        let cur_len = self.spans.len();
        self.spans.push(TokenSpan { file, start, end });
        self.tokens.push(TokenData {
            span_index: TokenSpanIndex(cur_len),
            token,
        });
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Ampersand,
    Divide,
    Mod,
    BitShiftLeft,
    BitShiftRight,
    Not,
    Equals,
    NotEquals,
    Or,
    And,
    Xor,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
}
impl ToString for Operator {
    fn to_string(&self) -> String {
        match self {
            Operator::Plus => "+".into(),
            Operator::Minus => "-".into(),
            Operator::Multiply => "*".into(),
            Operator::Divide => "/".into(),
            Operator::Equals => "==".into(),
            Operator::NotEquals => "!=".into(),
            Operator::Greater => ">".into(),
            Operator::GreaterEquals => ">=".into(),
            Operator::LessEquals => "<=".into(),
            Operator::Less => "<".into(),
            Operator::Mod => "%".into(),
            Operator::Ampersand => "&".into(),
            Operator::And => "and".into(),
            Operator::Or => "or".into(),
            Operator::Not => "not".into(),
            _ => "operator_str doesn't implement this operator".into(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Token {
    LiteralFloat(FloatLiteral),
    LiteralInteger(i128),
    LiteralString(InternedString),
    Operator(Operator),
    Identifier(InternedString),
    NewLine,
    Assign,
    True,
    False,
    None,
    Comma,
    Colon,
    StructDef,
    IfKeyword,
    ForKeyword,
    ReturnKeyword,
    InKeyword,
    WhileKeyword,
    IntrinsicKeyword,
    BreakKeyword,
    ElifKeyword,
    ElseKeyword,
    DefKeyword,
    OpenParen,
    CloseParen,
    OpenArrayBracket,
    CloseArrayBracket,
    MemberAccessor,
    ArrowRight,
    Indentation,
}

impl Token {
    pub const fn name(&self) -> &'static str {
        match self {
            Token::LiteralFloat(_) => "float literal",
            Token::LiteralInteger(_) => "integer literal",
            Token::LiteralString(_) => "string literal",
            Token::Operator(_) => "operator",
            Token::Identifier(_) => "identifier",
            Token::NewLine => "new line",
            Token::Assign => "assign = ",
            Token::True | Token::False => "boolean literal",
            Token::None => "None",
            Token::Comma => "comma ,",
            Token::Colon => "colon :",
            Token::StructDef => "struct keyword",
            Token::IfKeyword => "if keyword",
            Token::ForKeyword => "for keyword",
            Token::IntrinsicKeyword => "intrinsic keyword",
            Token::ReturnKeyword => "return keyword",
            Token::InKeyword => "in keyword",
            Token::WhileKeyword => "while keyword",
            Token::BreakKeyword => "break keyword",
            Token::ElifKeyword => "elif keyword",
            Token::ElseKeyword => "else keyword",
            Token::DefKeyword => "def keyword",
            Token::OpenParen => "open parenthesis (",
            Token::CloseParen => "close parenthesis )",
            Token::OpenArrayBracket => "open bracket [",
            Token::CloseArrayBracket => "close bracket ]",
            Token::MemberAccessor => "member access (dot)",
            Token::ArrowRight => "arrow right (->)",
            Token::Indentation => "indentation",
        }
    }
}

#[derive(Debug)]
pub enum PartialToken {
    UndefinedOrWhitespace,
    Operator(String),
    Identifier(String),
}

impl PartialToken {
    fn to_token(&self, interner: &StringInterner) -> Token {
        match self {
            Self::UndefinedOrWhitespace => {
                panic!("Unexpected undefined token. This is a tokenizer bug.")
            }
            Self::Identifier(s) => match s.as_ref() {
                "None" => Token::None,
                "not" => Token::Operator(Operator::Not),
                "True" => Token::True,
                "False" => Token::False,
                "and" => Token::Operator(Operator::And),
                "or" => Token::Operator(Operator::Or),
                "if" => Token::IfKeyword,
                "elif" => Token::ElifKeyword,
                "else" => Token::ElseKeyword,
                "for" => Token::ForKeyword,
                "def" => Token::DefKeyword,
                "intrinsic" => Token::IntrinsicKeyword,
                "return" => Token::ReturnKeyword,
                "in" => Token::InKeyword,
                "while" => Token::WhileKeyword,
                "break" => Token::BreakKeyword,
                "struct" => Token::StructDef,
                _ => Token::Identifier(interner.get(s)),
            },
            Self::Operator(s) => match s.as_ref() {
                "+" => Token::Operator(Operator::Plus),
                "-" => Token::Operator(Operator::Minus),
                "*" => Token::Operator(Operator::Multiply),
                "%" => Token::Operator(Operator::Mod),
                "/" => Token::Operator(Operator::Divide),
                "^" => Token::Operator(Operator::Xor),
                "<<" => Token::Operator(Operator::BitShiftLeft),
                ">>" => Token::Operator(Operator::BitShiftRight),
                "==" => Token::Operator(Operator::Equals),
                "->" => Token::ArrowRight,
                "=" => Token::Assign,
                "&" => Token::Operator(Operator::Ampersand),
                "!=" => Token::Operator(Operator::NotEquals),
                "(" => Token::OpenParen,
                ")" => Token::CloseParen,
                ">" => Token::Operator(Operator::Greater),
                "<" => Token::Operator(Operator::Less),
                ">=" => Token::Operator(Operator::GreaterEquals),
                "<=" => Token::Operator(Operator::LessEquals),
                _ => panic!("Unimplemented operator {s}"),
            },
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub struct LineIndex {
    //This is a char index
    start: usize,
    //this is a char index too, includes newlines
    end: usize,
    //The number of the line
    line: u32,
}

pub struct Tokenizer<'interner> {
    index: usize,
    file: FileTableIndex,
    source: String,
    chars: Vec<char>,
    cur_partial_token: PartialToken,
    eater_buf: String,
    interner: &'interner StringInterner,
    line_indices: Vec<LineIndex>,
    start_token: SourceLocation,
    end_token: SourceLocation,
}

impl<'interner> Tokenizer<'interner> {
    pub fn new(
        file: FileTableIndex,
        source: String,
        interner: &'interner StringInterner,
    ) -> Tokenizer<'interner> {
        let chars = source.chars().collect::<Vec<_>>();
        //add a new line always so that the loop finds a newline in the end

        let mut line_indices: Vec<LineIndex> = vec![];

        let mut line = LineIndex {
            start: 1,
            end: 1,
            line: 1, //lines start at 1
        };
        let mut last_line_needs_push = true;
        for c in chars.iter() {
            line.end += 1;
            last_line_needs_push = true;

            if *c == '\n' {
                last_line_needs_push = false;
                let next = LineIndex {
                    start: line.end,
                    end: line.end,
                    line: line.line + 1,
                };
                let this_line = std::mem::replace(&mut line, next);
                line_indices.push(this_line);
            }
        }

        if last_line_needs_push {
            line_indices.push(line);
        }

        Tokenizer {
            index: 0,
            file,
            source,
            chars,
            cur_partial_token: PartialToken::UndefinedOrWhitespace,
            eater_buf: String::new(),
            interner,
            line_indices,
            start_token: SourceLocation { line: 0, column: 0 },
            end_token: SourceLocation { line: 0, column: 0 },
        }
    }

    fn get_location(&self, index: usize) -> SourceLocation {
        //line_indices is ordered, just find the one that has end >= index
        for line in self.line_indices.iter() {
            if line.end >= index {
                return SourceLocation {
                    line: line.line,
                    column: (index - (line.start - 1)) as u32,
                };
            }
        }
        //if no location has found, then we must be in the last (or only) line.
        let last = self.line_indices.last().unwrap();
        SourceLocation {
            line: last.line,
            column: (index - last.start) as u32,
        }
    }

    fn start_token(&mut self) {
        self.start_token = self.get_location(self.index);
    }

    fn end_token(&mut self) {
        self.end_token = self.get_location(self.index);
    }

    fn reset_eater_buffer(&mut self) {
        self.eater_buf = String::new();
    }

    fn next(&mut self) {
        self.advance(1);
    }

    fn advance(&mut self, offset: usize) {
        self.index += offset;
    }

    fn cur(&self) -> char {
        self.cur_offset(0)
    }

    fn cur_offset(&self, offset: isize) -> char {
        self.chars[(self.index as isize + offset) as usize]
    }

    fn can_go(&self) -> bool {
        self.index < self.chars.len()
    }

    fn eat_numbers(&mut self) -> bool {
        let mut ate = false;
        while self.can_go() && self.cur().is_numeric() {
            self.eater_buf.push(self.cur());
            self.next();
            ate = true;
        }
        ate
    }

    fn eat_identifier(&mut self) -> Option<&str> {
        let start = self.index;
        self.start_token();
        let first_char_is_valid_identifier =
            self.can_go() && self.cur().is_ascii_alphabetic() || self.cur() == '_';

        if first_char_is_valid_identifier {
            self.eater_buf.push(self.cur());
            self.next();
        } else {
            return None;
        }

        while self.can_go() && (self.cur().is_ascii_alphanumeric() || self.cur() == '_') {
            self.eater_buf.push(self.cur());
            self.next();
        }
        let end = self.index;
        self.end_token();
        Some(&self.source[start..end])
    }

    fn eat_char(&mut self, char_to_eat: char) -> bool {
        if self.can_go() && self.cur() == char_to_eat {
            self.eater_buf.push(self.cur());
            self.next();
            true
        } else {
            false
        }
    }

    fn eat_string_literal(&mut self) -> bool {
        let stop = self.cur();
        if stop != '\'' && stop != '"' {
            return false;
        }
        self.next();
        let mut is_escaping = false;
        let mut finished = false;
        while self.can_go() {
            let cur = self.cur();
            if cur == '\\' && !is_escaping {
                is_escaping = true;
                self.next();
                continue;
            }
            if is_escaping {
                if stop == '\'' && cur == '\'' {
                    self.eater_buf.push('\'');
                } else if stop == '"' && cur == '"' {
                    self.eater_buf.push('"');
                } else if cur == '\\' {
                    self.eater_buf.push('\\');
                } else if cur == '0' {
                    self.eater_buf.push('\0');
                } else {
                    panic!("cannot escape char {cur}");
                }
                is_escaping = false;
                self.next();
                continue;
            }
            if stop == '\'' && cur == '\'' {
                finished = true;
                break;
            }
            if stop == '"' && cur == '"' {
                finished = true;
                break;
            }
            self.eater_buf.push(cur);
            self.next();
        }
        finished
    }

    fn commit_current_token(&mut self, table: &mut TokenTable) {
        if let PartialToken::UndefinedOrWhitespace = self.cur_partial_token {
            return;
        }

        let cur_token = std::mem::replace(
            &mut self.cur_partial_token,
            PartialToken::UndefinedOrWhitespace,
        );

        let as_token = cur_token.to_token(self.interner);
        table.add(as_token, self.file, self.start_token, self.end_token);
    }

    fn match_partial(&mut self, query: &str) -> (bool, usize) {
        let mut matched_chars = 0;
        for (i, item) in query.chars().enumerate() {
            if self.cur_offset(i as isize) != item {
                return (false, 0);
            }
            matched_chars += 1;
        }
        (true, matched_chars)
    }

    fn match_first_and_advance<'a>(&mut self, query: &'a [&'a str]) -> Option<&str> {
        self.start_token();
        for q in query {
            let cur_idx = self.index;
            let (success, len) = self.match_partial(q);
            if success {
                self.advance(len);
                let new_idx = self.index;
                return Some(&self.source[cur_idx..new_idx]);
            }
        }
        self.end_token();
        None
    }

    pub fn tokenize(mut self) -> Result<TokenTable, String> {
        let mut token_table = TokenTable::new();

        let operators = &[
            "+", "->", "-", "*", "%", "/", "<<", ">>", "<=", ">=", ">", "<", "!=", "==", "=", "^",
            "(", ")", "&",
        ];
        while self.can_go() {
            self.commit_current_token(&mut token_table);
            if self.cur().is_numeric() {
                self.reset_eater_buffer();

                self.start_token();
                self.eat_numbers();
                self.eat_char('.');
                self.eat_numbers();
                self.eat_char('e');
                self.eat_char('-');
                self.eat_numbers();

                self.end_token();

                let token = if self.eater_buf.contains('.') || self.eater_buf.contains('e') {
                    self.eater_buf
                        .parse::<f64>()
                        .map_err(|_| "Error parsing float value")
                        .map(|f| Token::LiteralFloat(f.into()))
                } else {
                    self.eater_buf
                        .parse::<i128>()
                        .map_err(|_| "Error parsing integer value")
                        .map(Token::LiteralInteger)
                }?;

                token_table.add(token, self.file, self.start_token, self.end_token);
            } else if self.index > 0 && self.cur_offset(-1) == '\n' && self.cur() == ' ' {
                let mut current = self.get_location(self.index);

                let mut current_spaces = 0;
                while self.can_go() && self.cur() == ' ' {
                    current_spaces += 1;
                    self.next();
                }

                assert!(
                    current_spaces % 4 == 0,
                    "Indentation must be a multiple of 4"
                );
                let indents = current_spaces / 4;

                for i in 0..indents {
                    let location = self.get_location(self.index + (4 * i));
                    token_table.add(Token::Indentation, self.file, current, location);
                    current = location;
                }
            } else if self.cur() == '\n' {
                self.start_token();
                self.end_token();
                token_table.add(Token::NewLine, self.file, self.start_token, self.end_token);
                self.next();
            } else if self.cur().is_whitespace() {
                //if it's whitespace and there's a pending token, add it
                self.next();
            } else if let Some(s) = self.match_first_and_advance(operators) {
                self.cur_partial_token = PartialToken::Operator(s.to_string());
            } else if self.cur().is_ascii_alphabetic() || self.cur() == '_' {
                self.reset_eater_buffer();
                let identifier = self.eat_identifier().unwrap();
                self.cur_partial_token = PartialToken::Identifier(identifier.to_string());
            } else if self.cur() == '\'' || self.cur() == '"' {
                self.reset_eater_buffer();

                self.start_token();
                self.eat_string_literal();
                self.end_token();

                let interned = self.interner.get(&self.eater_buf);

                token_table.add(
                    Token::LiteralString(interned),
                    self.file,
                    self.start_token,
                    self.end_token,
                );

                self.next();
            } else {
                self.start_token();
                let token = match self.cur() {
                    ',' => Some(Token::Comma),
                    ':' => Some(Token::Colon),
                    '[' => Some(Token::OpenArrayBracket),
                    ']' => Some(Token::CloseArrayBracket),
                    '.' => Some(Token::MemberAccessor),
                    _ => None,
                };

                if let Some(t) = token {
                    self.next();
                    self.end_token();
                    token_table.add(t, self.file, self.start_token, self.end_token)
                } else {
                    return Err(format!("Unrecognized token {}", self.cur()));
                }
            }
        }
        self.commit_current_token(&mut token_table);

        Ok(token_table)
    }
}

pub fn tokenize(
    file: FileTableIndex,
    source: &str,
    interner: &StringInterner,
) -> Result<TokenTable, String> {
    Tokenizer::new(file, source.to_string(), interner).tokenize()
}

#[cfg(test)]
mod tests {
    use crate::semantic::context::test_utils::tls_interner;

    use super::*;
    thread_local! {
        static INTERNER: StringInterner = StringInterner::new();
    }

    tls_interner!(INTERNER);

    fn tokenize(str: &str) -> Result<Vec<Token>, String> {
        INTERNER.with(|interner| {
            crate::ast::lexer::tokenize(FileTableIndex(0), str, interner).map(|table| {
                table
                    .tokens
                    .into_iter()
                    .map(|x| x.token)
                    .collect::<Vec<_>>()
            })
        })
    }

    #[test]
    fn tokenizer_simple_number() -> Result<(), String> {
        let result = tokenize("2")?;
        assert_eq!(result, [Token::LiteralInteger(2)]);
        Ok(())
    }
    #[test]
    fn tokenizer_bigger_number() -> Result<(), String> {
        let result = tokenize("22")?;
        assert_eq!(result, [Token::LiteralInteger(22)]);
        Ok(())
    }
    #[test]
    fn tokenizer_decimal_number() -> Result<(), String> {
        let result = tokenize("22.321")?;
        assert_eq!(result, [Token::LiteralFloat(22.321.into())]);
        Ok(())
    }

    #[test]
    fn tokenizer_decimal_exponent_number() -> Result<(), String> {
        let result = tokenize("22.22e2")?;
        assert_eq!(result, [Token::LiteralFloat(22.22e2.into())]);
        Ok(())
    }
    #[test]
    fn tokenizer_operator() -> Result<(), String> {
        let result = tokenize("+")?;
        assert_eq!(result, [Token::Operator(Operator::Plus)]);
        Ok(())
    }

    #[test]
    fn tokenizer_number_space_operator() -> Result<(), String> {
        let result = tokenize("6 +")?;
        assert_eq!(
            result,
            [Token::LiteralInteger(6), Token::Operator(Operator::Plus)]
        );
        Ok(())
    }

    #[test]
    fn tokenizer_number_space_operator_space_operator() -> Result<(), String> {
        let result = tokenize("6 + +")?;
        assert_eq!(
            result,
            [
                Token::LiteralInteger(6),
                Token::Operator(Operator::Plus),
                Token::Operator(Operator::Plus)
            ]
        );
        Ok(())
    }

    #[test]
    fn tokenizer_not_equals() -> Result<(), String> {
        let result = tokenize("10 != 12")?;
        assert_eq!(
            result,
            [
                Token::LiteralInteger(10),
                Token::Operator(Operator::NotEquals),
                Token::LiteralInteger(12),
            ]
        );
        Ok(())
    }

    #[test]
    fn tokenizer_unrecognized_token() -> Result<(), &'static str> {
        let result = tokenize("10 # 12");
        match result {
            Ok(_) => Err("Operator # doesnt exist and shouldn't be tokenized"),
            Err(_) => Ok(()),
        }
    }

    #[test]
    fn tokenizer_many_operators() -> Result<(), String> {
        let result = tokenize("10 + - / * << >> != == -12")?;
        assert_eq!(
            result,
            [
                Token::LiteralInteger(10),
                Token::Operator(Operator::Plus),
                Token::Operator(Operator::Minus),
                Token::Operator(Operator::Divide),
                Token::Operator(Operator::Multiply),
                Token::Operator(Operator::BitShiftLeft),
                Token::Operator(Operator::BitShiftRight),
                Token::Operator(Operator::NotEquals),
                Token::Operator(Operator::Equals),
                Token::Operator(Operator::Minus),
                Token::LiteralInteger(12),
            ]
        );
        Ok(())
    }

    #[test]
    fn tokenizer_number_space_operator_space_number() -> Result<(), String> {
        let result = tokenize("6 + 6")?;
        assert_eq!(
            result,
            [
                Token::LiteralInteger(6),
                Token::Operator(Operator::Plus),
                Token::LiteralInteger(6),
            ]
        );
        Ok(())
    }

    #[test]
    fn tokenizer_number_space_operator_lots_of_space_number() -> Result<(), String> {
        let result = tokenize("6         +                                6.2312e99")?;
        assert_eq!(
            result,
            [
                Token::LiteralInteger(6),
                Token::Operator(Operator::Plus),
                Token::LiteralFloat(6.2312e99.into()),
            ]
        );
        Ok(())
    }

    #[test]
    fn tokenizer_number_operator_number() -> Result<(), String> {
        let result = tokenize("6+6")?;
        assert_eq!(
            result,
            [
                Token::LiteralInteger(6),
                Token::Operator(Operator::Plus),
                Token::LiteralInteger(6),
            ]
        );
        Ok(())
    }

    #[test]
    fn tokenizer_space_corner_cases() -> Result<(), String> {
        let result = tokenize("   6         +             6.2312e99   ")?;
        assert_eq!(
            result,
            [
                Token::LiteralInteger(6),
                Token::Operator(Operator::Plus),
                Token::LiteralFloat(6.2312e99.into()),
            ]
        );
        Ok(())
    }

    #[test]
    fn tokenier_openparen() -> Result<(), String> {
        let result = tokenize("(")?;
        assert_eq!(result, [Token::OpenParen]);
        Ok(())
    }

    #[test]
    fn tokenier_closeparen() -> Result<(), String> {
        let result = tokenize(")")?;
        assert_eq!(result, [Token::CloseParen]);
        Ok(())
    }

    #[test]
    fn tokenier_opencloseparen() -> Result<(), String> {
        let result = tokenize("()")?;
        assert_eq!(result, [Token::OpenParen, Token::CloseParen]);
        Ok(())
    }

    #[test]
    fn tokenier_opencloseparen_with_expr() -> Result<(), String> {
        let result = tokenize("(1 + 2) * 3")?;
        assert_eq!(
            result,
            [
                Token::OpenParen,
                Token::LiteralInteger(1),
                Token::Operator(Operator::Plus),
                Token::LiteralInteger(2),
                Token::CloseParen,
                Token::Operator(Operator::Multiply),
                Token::LiteralInteger(3)
            ]
        );
        Ok(())
    }

    #[test]
    fn tokenizer_identifier() -> Result<(), String> {
        let result = tokenize("some_identifier")?;
        assert_eq!(result, [Token::Identifier(istr("some_identifier"))]);
        Ok(())
    }

    #[test]
    fn tokenizer_function_call() -> Result<(), String> {
        let result = tokenize("some_identifier(1)")?;
        assert_eq!(
            result,
            [
                Token::Identifier(istr("some_identifier")),
                Token::OpenParen,
                Token::LiteralInteger(1),
                Token::CloseParen
            ]
        );
        Ok(())
    }

    #[test]
    fn assign_operator() -> Result<(), String> {
        let result = tokenize("x = 1")?;
        assert_eq!(
            result,
            [
                Token::Identifier(istr("x")),
                Token::Assign,
                Token::LiteralInteger(1)
            ]
        );
        Ok(())
    }

    #[test]
    fn none() -> Result<(), String> {
        let result = tokenize("None")?;
        assert_eq!(result, [Token::None]);
        Ok(())
    }

    #[test]
    fn boolean_tokens() -> Result<(), String> {
        let result = tokenize("not True and False or ^")?;
        assert_eq!(
            result,
            [
                Token::Operator(Operator::Not),
                Token::True,
                Token::Operator(Operator::And),
                Token::False,
                Token::Operator(Operator::Or),
                Token::Operator(Operator::Xor)
            ]
        );
        Ok(())
    }

    #[test]
    fn string_literal() -> Result<(), String> {
        let result = tokenize("'abc'")?;
        assert_eq!(result, [Token::LiteralString(istr("abc"))]);
        Ok(())
    }

    #[test]
    fn string_literal_doublequotes() -> Result<(), String> {
        let result = tokenize("\"abc\"")?;
        assert_eq!(result, [Token::LiteralString(istr("abc"))]);
        Ok(())
    }

    #[test]
    fn string_literal_escapedouble() -> Result<(), String> {
        let result = tokenize("\"a\\\"b\\\"c\"")?;
        assert_eq!(result, [Token::LiteralString(istr("a\"b\"c"))]);
        Ok(())
    }

    #[test]
    fn string_literal_escapesingle() -> Result<(), String> {
        let result = tokenize("\'a\\'b\\'c\'")?;
        assert_eq!(result, [Token::LiteralString(istr("a'b'c"))]);
        Ok(())
    }

    #[test]
    fn tokenize_if() -> Result<(), String> {
        let result = tokenize(
            "if x == 0:
    x = x + 1",
        )?;
        assert_eq!(
            result,
            [
                Token::IfKeyword,
                Token::Identifier(istr("x")),
                Token::Operator(Operator::Equals),
                Token::LiteralInteger(0),
                Token::Colon,
                Token::NewLine,
                Token::Indentation,
                Token::Identifier(istr("x")),
                Token::Assign,
                Token::Identifier(istr("x")),
                Token::Operator(Operator::Plus),
                Token::LiteralInteger(1)
            ]
        );
        Ok(())
    }

    #[test]
    fn method_call() -> Result<(), String> {
        let result = tokenize("obj.method")?;
        assert_eq!(
            result,
            [
                Token::Identifier(istr("obj")),
                Token::MemberAccessor,
                Token::Identifier(istr("method")),
            ]
        );
        Ok(())
    }

    #[test]
    fn method_call2() -> Result<(), String> {
        let result = tokenize("obj . method")?;
        assert_eq!(
            result,
            [
                Token::Identifier(istr("obj")),
                Token::MemberAccessor,
                Token::Identifier(istr("method")),
            ]
        );
        Ok(())
    }

    #[test]
    fn for_list() -> Result<(), String> {
        let result = tokenize("for item in ls:")?;
        assert_eq!(
            result,
            [
                Token::ForKeyword,
                Token::Identifier(istr("item")),
                Token::InKeyword,
                Token::Identifier(istr("ls")),
                Token::Colon,
            ]
        );
        Ok(())
    }

    #[test]
    fn def_function() -> Result<(), String> {
        let result = tokenize("def function(x: i32):")?;
        assert_eq!(
            result,
            [
                Token::DefKeyword,
                Token::Identifier(istr("function")),
                Token::OpenParen,
                Token::Identifier(istr("x")),
                Token::Colon,
                Token::Identifier(istr("i32")),
                Token::CloseParen,
                Token::Colon,
            ]
        );
        Ok(())
    }

    #[test]
    fn def_function_return() -> Result<(), String> {
        let result = tokenize("def function(x: i32) -> i32:")?;
        assert_eq!(
            result,
            [
                Token::DefKeyword,
                Token::Identifier(istr("function")),
                Token::OpenParen,
                Token::Identifier(istr("x")),
                Token::Colon,
                Token::Identifier(istr("i32")),
                Token::CloseParen,
                Token::ArrowRight,
                Token::Identifier(istr("i32")),
                Token::Colon,
            ]
        );
        Ok(())
    }

    #[test]
    fn return_keyword() -> Result<(), String> {
        let result = tokenize("return")?;
        assert_eq!(result, [Token::ReturnKeyword]);
        Ok(())
    }

    #[test]
    fn array_access() -> Result<(), String> {
        let result = tokenize("array[0]")?;
        assert_eq!(
            result,
            [
                Token::Identifier(istr("array")),
                Token::OpenArrayBracket,
                Token::LiteralInteger(0),
                Token::CloseArrayBracket
            ]
        );
        Ok(())
    }

    #[test]
    fn class_def() -> Result<(), String> {
        let result = tokenize("struct Test:")?;
        assert_eq!(
            result,
            [
                Token::StructDef,
                Token::Identifier(istr("Test")),
                Token::Colon
            ]
        );
        Ok(())
    }
}
