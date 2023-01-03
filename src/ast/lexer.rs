use crate::commons::float::FloatLiteral;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
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

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    LiteralFloat(FloatLiteral),
    LiteralInteger(i128),
    LiteralString(String),
    Operator(Operator),
    Identifier(String),
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
    RaiseKeyword,
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
            Token::Assign => "assign",
            Token::True | Token::False => "boolean literal",
            Token::None => "none",
            Token::Comma => "comma",
            Token::Colon => "colon",
            Token::StructDef => "struct keyword",
            Token::IfKeyword => "if keyword",
            Token::ForKeyword => "for keyword",
            Token::IntrinsicKeyword => "intrinsic keyword",
            Token::RaiseKeyword => "raise keyword",
            Token::ReturnKeyword => "return keyword",
            Token::InKeyword => "in keyword",
            Token::WhileKeyword => "while keyword",
            Token::BreakKeyword => "break keyword",
            Token::ElifKeyword => "elif keyword",
            Token::ElseKeyword => "else keyword",
            Token::DefKeyword => "def keyword",
            Token::OpenParen => "open parenthesis",
            Token::CloseParen => "close parenthesis",
            Token::OpenArrayBracket => "open bracket",
            Token::CloseArrayBracket => "close bracket",
            Token::MemberAccessor => "member access (dot)",
            Token::ArrowRight => "arrow right (->)",
            Token::Indentation => "indentation",
        }
    }
}

#[derive(Debug)]
enum PartialToken {
    UndefinedOrWhitespace,
    LiteralFloat(String),
    Operator(String),
    Identifier(String),
    String(String),
    NewLine,
    Comma,
    OpenArrayBracket,
    CloseArrayBracket,
    MemberAccessor,
    Colon,
}

impl PartialToken {
    fn to_token(&self) -> Token {
        match self {
            Self::UndefinedOrWhitespace => {
                panic!("Unexpected undefined token. This is a tokenizer bug.")
            }
            Self::Identifier(s) => match s.as_str() {
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
                "raise" => Token::RaiseKeyword,
                "return" => Token::ReturnKeyword,
                "in" => Token::InKeyword,
                "while" => Token::WhileKeyword,
                "break" => Token::BreakKeyword,
                "struct" => Token::StructDef,
                _ => Token::Identifier(s.clone()),
            },
            Self::Comma => Token::Comma,
            Self::Colon => Token::Colon,
            Self::NewLine => Token::NewLine,
            Self::MemberAccessor => Token::MemberAccessor,
            Self::OpenArrayBracket => Token::OpenArrayBracket,
            Self::CloseArrayBracket => Token::CloseArrayBracket,
            Self::LiteralFloat(s) => {
                if s.contains('.') || s.contains('e') {
                    match s.parse::<f64>() {
                        Ok(f) => Token::LiteralFloat(f.into()),
                        _ => panic!("Error parsing float value {}. Should have generated a tokenizer error. This is a bug.", s)
                    }
                } else {
                    match s.parse::<i128>() {
                        Ok(f) => Token::LiteralInteger(f),
                        _ => panic!("Error parsing integer value {}. Should have generated a tokenizer error. This is a bug.", s)
                    }
                }
            }
            Self::String(s) => Token::LiteralString(s.clone()),
            Self::Operator(s) => match s.as_str() {
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
                "!=" => Token::Operator(Operator::NotEquals),
                "(" => Token::OpenParen,
                ")" => Token::CloseParen,
                ">" => Token::Operator(Operator::Greater),
                "<" => Token::Operator(Operator::Less),
                ">=" => Token::Operator(Operator::GreaterEquals),
                "<=" => Token::Operator(Operator::LessEquals),
                _ => panic!("Unimplemented operator {}", s),
            },
        }
    }
}

pub struct Tokenizer {
    index: usize,
    chars: Vec<char>,
    cur_partial_token: PartialToken,
    final_result: Vec<Token>,
    eater_buf: String,
}

impl Tokenizer {
    pub fn new(source: &str) -> Tokenizer {
        Tokenizer {
            index: 0,
            chars: source.chars().collect(),
            cur_partial_token: PartialToken::UndefinedOrWhitespace,
            final_result: vec![],
            eater_buf: String::new(),
        }
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

    fn eat_identifier(&mut self) -> bool {
        let first_char_is_valid_identifier =
            self.can_go() && self.cur().is_ascii_alphabetic() || self.cur() == '_';

        if first_char_is_valid_identifier {
            self.eater_buf.push(self.cur());
            self.next();
        } else {
            return false;
        }

        while self.can_go() && (self.cur().is_ascii_alphanumeric() || self.cur() == '_') {
            self.eater_buf.push(self.cur());
            self.next();
        }

        true
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
                } else {
                    panic!("cannot escape char {}", cur);
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

    fn commit_current_token(&mut self) {
        if let PartialToken::UndefinedOrWhitespace = self.cur_partial_token {
            return;
        }
        let cur_token = std::mem::replace(
            &mut self.cur_partial_token,
            PartialToken::UndefinedOrWhitespace,
        );
        self.final_result.push(cur_token.to_token());
    }

    fn clone_buf(&self) -> String {
        self.eater_buf.clone()
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

    fn match_first_and_advance<'a>(&mut self, query: &'a [&'a str]) -> Option<&'a str> {
        for q in query {
            let (success, len) = self.match_partial(q);
            if success {
                self.advance(len);
                return Some(q);
            }
        }
        None
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, String> {
        let operators = &[
            "+", "->", "-", "*", "%", "/", "<<", ">>", "<=", ">=", ">", "<", "!=", "==", "=", "^",
            "(", ")",
        ];
        while self.can_go() {
            self.commit_current_token();
            if self.cur().is_numeric() {
                self.reset_eater_buffer();
                self.eat_numbers();
                self.eat_char('.');
                self.eat_numbers();
                self.eat_char('e');
                self.eat_char('-');
                self.eat_numbers();
                self.cur_partial_token = PartialToken::LiteralFloat(self.clone_buf());
                self.reset_eater_buffer();
            } else if self.cur() == ',' {
                self.cur_partial_token = PartialToken::Comma;
                self.commit_current_token();
                self.next();
            } else if self.cur() == ':' {
                self.cur_partial_token = PartialToken::Colon;
                self.commit_current_token();
                self.next();
            } else if self.cur() == '[' {
                self.cur_partial_token = PartialToken::OpenArrayBracket;
                self.commit_current_token();
                self.next();
            } else if self.cur() == ']' {
                self.cur_partial_token = PartialToken::CloseArrayBracket;
                self.commit_current_token();
                self.next();
            } else if self.cur() == '.' {
                self.cur_partial_token = PartialToken::MemberAccessor;
                self.commit_current_token();
                self.next();
            } else if self.cur() == '\n' {
                self.cur_partial_token = PartialToken::NewLine;
                self.commit_current_token();
                self.next();
            } else if self.index > 0 && self.cur_offset(-1) == '\n' && self.cur() == ' ' {
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
                for _i in 0..indents {
                    self.final_result.push(Token::Indentation);
                }
            } else if self.cur().is_whitespace() {
                //if it's whitespace and there's a pending token, add it
                self.next();
            } else if let Some(s) = self.match_first_and_advance(operators) {
                self.cur_partial_token = PartialToken::Operator(String::from(s));
                self.commit_current_token();
            } else if self.cur().is_ascii_alphabetic() || self.cur() == '_' {
                self.eat_identifier();
                self.cur_partial_token = PartialToken::Identifier(self.clone_buf());
                self.reset_eater_buffer();
            } else if self.cur() == '\'' || self.cur() == '"' {
                self.eat_string_literal();
                self.cur_partial_token = PartialToken::String(self.clone_buf());
                self.commit_current_token();
                self.reset_eater_buffer();
                self.next();
            } else {
                return Err(format!("Unrecognized token {}", self.cur()));
            }
        }
        self.commit_current_token();
        Ok(self.final_result)
    }
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, String> {
    Tokenizer::new(source).tokenize()
}

#[cfg(test)]
mod tests {
    use super::*;
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
        assert_eq!(result, [Token::Identifier(String::from("some_identifier"))]);
        Ok(())
    }

    #[test]
    fn tokenizer_function_call() -> Result<(), String> {
        let result = tokenize("some_identifier(1)")?;
        assert_eq!(
            result,
            [
                Token::Identifier(String::from("some_identifier")),
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
                Token::Identifier(String::from("x")),
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
        assert_eq!(result, [Token::LiteralString(String::from("abc"))]);
        Ok(())
    }

    #[test]
    fn string_literal_doublequotes() -> Result<(), String> {
        let result = tokenize("\"abc\"")?;
        assert_eq!(result, [Token::LiteralString(String::from("abc"))]);
        Ok(())
    }

    #[test]
    fn string_literal_escapedouble() -> Result<(), String> {
        let result = tokenize("\"a\\\"b\\\"c\"")?;
        assert_eq!(result, [Token::LiteralString(String::from("a\"b\"c"))]);
        Ok(())
    }

    #[test]
    fn string_literal_escapesingle() -> Result<(), String> {
        let result = tokenize("\'a\\'b\\'c\'")?;
        assert_eq!(result, [Token::LiteralString(String::from("a'b'c"))]);
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
                Token::Identifier(String::from("x")),
                Token::Operator(Operator::Equals),
                Token::LiteralInteger(0),
                Token::Colon,
                Token::NewLine,
                Token::Indentation,
                Token::Identifier(String::from("x")),
                Token::Assign,
                Token::Identifier(String::from("x")),
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
                Token::Identifier(String::from("obj")),
                Token::MemberAccessor,
                Token::Identifier(String::from("method")),
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
                Token::Identifier(String::from("obj")),
                Token::MemberAccessor,
                Token::Identifier(String::from("method")),
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
                Token::Identifier("item".into()),
                Token::InKeyword,
                Token::Identifier("ls".into()),
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
                Token::Identifier("function".into()),
                Token::OpenParen,
                Token::Identifier("x".into()),
                Token::Colon,
                Token::Identifier("i32".into()),
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
                Token::Identifier("function".into()),
                Token::OpenParen,
                Token::Identifier("x".into()),
                Token::Colon,
                Token::Identifier("i32".into()),
                Token::CloseParen,
                Token::ArrowRight,
                Token::Identifier("i32".into()),
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
    fn raise_exception_expr() -> Result<(), String> {
        let result = tokenize("raise SomeError")?;
        assert_eq!(
            result,
            [Token::RaiseKeyword, Token::Identifier("SomeError".into())]
        );
        Ok(())
    }

    #[test]
    fn array_access() -> Result<(), String> {
        let result = tokenize("array[0]")?;
        assert_eq!(
            result,
            [
                Token::Identifier("array".into()),
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
                Token::Identifier("Test".into()),
                Token::Colon
            ]
        );
        Ok(())
    }

    #[test]
    fn cannot_declare_intermediate() {
        let result = tokenize("$0 = 1");

        assert_eq!(result.unwrap_err(), "Unrecognized token $");
    }
}
