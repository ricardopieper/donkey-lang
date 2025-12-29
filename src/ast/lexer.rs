use std::{
    error::Error,
    ops::{ControlFlow, FromResidual, Try},
};

use crate::{
    commons::float::FloatLiteral, interner::InternedString, semantic::context::FileTableIndex,
};

#[derive(Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub struct TokenSpanIndex {
    pub index: usize,
    pub file: FileTableIndex,
}

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
    pub start: SourceLocation,
    pub end: SourceLocation,
}

#[derive(Debug)]
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
        self.spans.push(TokenSpan { start, end });
        self.tokens.push(TokenData {
            span_index: TokenSpanIndex {
                index: cur_len,
                file,
            },
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
            Operator::BitShiftLeft => "<<".into(),
            Operator::BitShiftRight => ">>".into(),
            Operator::Xor => "^".into(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Token {
    LiteralFloat(FloatLiteral),
    LiteralInteger(i128),
    LiteralChar(char),
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
    ExternalKeyword,
    AsKeyword,
    BreakKeyword,
    ElifKeyword,
    ElseKeyword,
    DefKeyword,
    ImplKeyword,
    SelfKeyword,
    DoubleColon, // ::
    OpenParen,
    CloseParen,
    Ellipsis,
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
            Token::LiteralChar(_) => "char literal",
            Token::LiteralString(_) => "string literal",
            Token::Operator(_) => "operator",
            Token::Identifier(_) => "identifier",
            Token::NewLine => "new line",
            Token::Assign => "assign = ",
            Token::True => "boolean literal true",
            Token::False => "boolean literal false",
            Token::None => "None",
            Token::Comma => "comma ,",
            Token::Colon => "colon :",
            Token::Ellipsis => "ellipsis (...)",
            Token::StructDef => "struct keyword",
            Token::IfKeyword => "if keyword",
            Token::ForKeyword => "for keyword",
            Token::IntrinsicKeyword => "intrinsic keyword",
            Token::ExternalKeyword => "external keyword",
            Token::ReturnKeyword => "return keyword",
            Token::InKeyword => "in keyword",
            Token::WhileKeyword => "while keyword",
            Token::BreakKeyword => "break keyword",
            Token::ImplKeyword => "impl keyword",
            Token::ElifKeyword => "elif keyword",
            Token::ElseKeyword => "else keyword",
            Token::DefKeyword => "def keyword",
            Token::DoubleColon => "double colon ::",
            Token::OpenParen => "open parenthesis (",
            Token::CloseParen => "close parenthesis )",
            Token::OpenArrayBracket => "open bracket [",
            Token::CloseArrayBracket => "close bracket ]",
            Token::MemberAccessor => "member access (dot)",
            Token::ArrowRight => "arrow right (->)",
            Token::Indentation => "indentation",
            Token::AsKeyword => "as keyword (cast)",
            Token::SelfKeyword => "self keyword",
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

pub struct Lexer {
    index: usize,
    file: FileTableIndex,
    source: String,
    chars: Vec<char>,
    line_indices: Vec<LineIndex>,
}

pub enum LexerResult<T> {
    Ok(T),
    NoMatch,
    EndOfInput,
    Error(String),
}

impl<T> FromResidual<LexerResult<!>> for LexerResult<T> {
    fn from_residual(residual: LexerResult<!>) -> Self {
        match residual {
            LexerResult::Ok(val) => val, //val is never, so it typechecks
            LexerResult::EndOfInput => LexerResult::EndOfInput,
            LexerResult::NoMatch => LexerResult::NoMatch,
            LexerResult::Error(e) => LexerResult::Error(e),
        }
    }
}

impl<T> Try for LexerResult<T> {
    type Output = T;

    type Residual = LexerResult<!>;

    fn from_output(output: Self::Output) -> Self {
        LexerResult::Ok(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            LexerResult::Ok(val) => ControlFlow::Continue(val),
            LexerResult::EndOfInput => ControlFlow::Break(LexerResult::EndOfInput),
            LexerResult::NoMatch => ControlFlow::Break(LexerResult::NoMatch),
            LexerResult::Error(e) => ControlFlow::Break(LexerResult::Error(e)),
        }
    }
}

impl<T, E: Error> From<Result<T, E>> for LexerResult<T> {
    fn from(val: Result<T, E>) -> Self {
        match val {
            Ok(v) => LexerResult::Ok(v),
            Err(e) => LexerResult::Error(e.to_string()),
        }
    }
}

impl<T> LexerResult<T> {
    pub fn unwrap(self) -> T {
        match self {
            LexerResult::Ok(v) => v,
            LexerResult::NoMatch => panic!("Unwrap on lexer result error: No match"),
            LexerResult::EndOfInput => panic!("Unwrap on lexer result error: End of input"),
            LexerResult::Error(e) => panic!("Unwrap on lexer result error: {e}"),
        }
    }
}

impl Iterator for Lexer {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.chars.len() {
            return None;
        }
        let ch = self.chars[self.index];
        self.index += 1;
        Some(ch)
    }
}

impl Lexer {
    pub fn new(file: FileTableIndex, source: String) -> Lexer {
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

        Lexer {
            index: 0,
            file,
            source,
            chars,
            //eater_buf: String::new(),
            line_indices,
        }
    }

    fn get_cur_location(&self) -> SourceLocation {
        self.get_location(self.index)
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

    //fn reset_eater_buffer(&mut self) {
    //    self.eater_buf.clear();
    //}

    fn next(&mut self) {
        self.advance(1);
    }

    fn advance(&mut self, offset: usize) {
        self.index += offset;
    }

    fn peek(&self) -> LexerResult<char> {
        self.cur_offset(0)
    }

    fn cur_offset(&self, offset: isize) -> LexerResult<char> {
        if (self.index as isize + offset) as usize >= self.chars.len() {
            LexerResult::EndOfInput
        } else {
            LexerResult::Ok(self.chars[(self.index as isize + offset) as usize])
        }
    }

    fn next_while<F>(&mut self, pred: F) -> usize
    where
        F: Fn(char) -> bool,
    {
        let mut count = 0;
        while let LexerResult::Ok(ch) = self.peek()
            && pred(ch)
        {
            self.next();
            count += 1;
        }
        count
    }

    fn eat_identifier(&mut self) -> LexerResult<(&str, SourceLocation, SourceLocation)> {
        let start = self.index;
        let start_loc = self.get_cur_location();

        //notice the ALPHABETIC here, not ALPHANUMERIC
        if let LexerResult::Ok(ch) = self.peek()
            && !ch.is_ascii_alphabetic()
            && ch != '_'
        {
            return LexerResult::NoMatch;
        }

        self.next();

        self.next_while(|ch| ch.is_ascii_alphanumeric() || ch == '_');

        let end = self.index;
        let end_loc = self.get_cur_location();
        LexerResult::Ok((&self.source[start..end], start_loc, end_loc))
    }

    fn eat_char(&mut self, char_to_eat: char) -> bool {
        if let LexerResult::Ok(c) = self.peek()
            && c == char_to_eat
        {
            self.next();
            true
        } else {
            false
        }
    }

    fn eat_char_literal(&mut self) -> LexerResult<char> {
        let stop = self.peek()?;
        if stop != '\'' {
            return LexerResult::NoMatch;
        }
        self.next();
        //@TODO maybe there's a char ascii math trick to escape this without a match and produce the escaped char from it's representation
        //@TODO support for unicode chars?
        let cur = self.peek()?;
        let is_escaping = cur == '\\';
        let char = if is_escaping {
            self.next();
            let cur = self.peek()?;

            

            match cur {
                '\\' => '\\',
                '\'' => '\'',
                '0' => '\0',
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                _ => return LexerResult::Error(format!("Cannot escape char {cur}")),
            }
        } else {
            cur
        };

        self.next();
        let ch = self.peek()?;
        if ch != '\'' {
            return LexerResult::Error(format!("expected \\' after char, instead found {}. Char literals can only be 1 char long, or must be escaped", ch));
        }
        self.next();

        LexerResult::Ok(char)
    }

    fn eat_string_literal(&mut self) -> LexerResult<String> {
        let double_quote = self.peek()?;
        if double_quote != '"' {
            return LexerResult::NoMatch;
        }
        //This needs a separate buffer because we need to unescape the string
        let mut buf = String::new();
        self.next();
        let mut is_escaping = false;
        while let LexerResult::Ok(cur) = self.peek() {
            //start a escape sequence
            if cur == '\\' && !is_escaping {
                is_escaping = true;
                self.next();
                continue;
            }

            if is_escaping {
                let escapable_chars = ['\\', '"', '0', 'n', 't', 'r'];
                let resulting_chars = ['\\', '"', '\0', '\n', '\t', '\r'];
                let pos = escapable_chars.iter().position(|x| *x == cur);
                if let Some(pos) = pos {
                    buf.push(resulting_chars[pos]);
                } else {
                    return LexerResult::Error(format!("Cannot escape char {cur}"));
                }
                is_escaping = false;

                self.next();
                continue;
            }
            if cur == '"' {
                return LexerResult::Ok(buf);
            }
            buf.push(cur);
            self.next();
        }
        LexerResult::NoMatch
    }

    fn match_partial(&mut self, query: &str) -> LexerResult<usize> {
        let mut matched_chars = 0;
        for (i, item) in query.chars().enumerate() {
            match self.cur_offset(i as isize) {
                LexerResult::Ok(c) if c == item => {
                    matched_chars += 1;
                }
                _ => {
                    return LexerResult::NoMatch;
                }
            }
        }
        LexerResult::Ok(matched_chars)
    }

    fn match_first_and_advance<'a>(
        &mut self,
        query: &'a [&'a str],
    ) -> LexerResult<(&str, SourceLocation, SourceLocation)> {
        let start = self.get_cur_location();
        for q in query {
            let cur_idx = self.index;

            if let LexerResult::Ok(matched_chars) = self.match_partial(q) {
                self.advance(matched_chars);
                let new_idx = self.index;
                let end = self.get_cur_location();
                return LexerResult::Ok((&self.source[cur_idx..new_idx], start, end));
            }
        }
        LexerResult::NoMatch
    }

    fn tokenize_number_literal(&mut self) -> LexerResult<(Token, SourceLocation, SourceLocation)> {
        let start = self.get_cur_location();
        let start_index = self.index;

        self.next_while(|ch| ch.is_numeric());

        let mut is_float = self.eat_char('.');

        self.next_while(|ch| ch.is_numeric());

        if self.eat_char('e') {
            is_float = true;
            self.eat_char('-');

            if self.next_while(|ch| ch.is_numeric()) == 0 {
                return LexerResult::Error("Expected number after e".to_string());
            }
        }
        let end = self.get_cur_location();
        let end_index = self.index;

        let span = &self.source[start_index..end_index];
        let token: LexerResult<Token> = if is_float {
            span.parse::<f64>()
                .map(|f| Token::LiteralFloat(f.into()))
                .into()
        } else {
            span.parse::<i128>().map(Token::LiteralInteger).into()
        };

        LexerResult::Ok((token?, start, end))
    }

    pub fn tokenize(mut self) -> LexerResult<TokenTable> {
        let mut token_table = TokenTable::new();

        let symbols = &[
            "+", "->", "-", "*", "%", "/", "<<", "<=", ">=", ">", "<", "!=", "==", "=", "^", "(",
            ")", "&", "...", ",", "[", "]", ".", "::", ":",
        ];

        while let LexerResult::Ok(ch) = self.peek() {
            if ch.is_numeric() {
                let (token, start, end) = self.tokenize_number_literal()?;
                token_table.add(token, self.file, start, end);
            } else if ch == ' ' && self.index > 0 && self.cur_offset(-1)? == '\n' {
                //parse indentation, I feel like this should be moved to parser instead of lexer
                let mut current_location = self.get_cur_location();
                let amount_of_spaces = self.skip_spaces();
                let indents = amount_of_spaces / 4;
                for i in 0..indents {
                    let location = self.get_location(self.index + (4 * i));
                    token_table.add(Token::Indentation, self.file, current_location, location);
                    current_location = location;
                }
            } else if ch == '#' {
                //skip until new line
                while let LexerResult::Ok(ch) = self.peek()
                    && ch != '\n'
                {
                    self.next();
                }
                self.next();
            } else if ch == '\n' {
                let (start, end) = (self.get_cur_location(), self.get_cur_location());
                token_table.add(Token::NewLine, self.file, start, end);
                self.next();
            } else if ch.is_whitespace() {
                //if it's whitespace and there's a pending token, add it
                self.next();
            } else if let LexerResult::Ok((s, start, end)) = self.match_first_and_advance(symbols) {
                let token = Lexer::symbol_to_token(s)?;
                token_table.add(token, self.file, start, end);
            } else if ch.is_ascii_alphabetic() || ch == '_' {
                let (identifier, start, end) = self.eat_identifier()?;
                let token = Lexer::identifier_to_token(identifier);
                token_table.add(token, self.file, start, end);
            } else if ch == '"' {
                let start = self.get_cur_location();
                let id = self.eat_string_literal()?;
                let end = self.get_cur_location();

                let interned = InternedString::new(&id);

                token_table.add(Token::LiteralString(interned), self.file, start, end);

                self.next();
            } else if ch == '\'' {
                let start = self.get_cur_location();
                let char = self.eat_char_literal()?;
                let end = self.get_cur_location();

                token_table.add(Token::LiteralChar(char), self.file, start, end);
            } else {
                return LexerResult::Error(format!("Unrecognized token {ch}"));
            }
        }

        LexerResult::Ok(token_table)
    }

    fn skip_spaces(&mut self) -> usize {
        let mut current_spaces = 0;
        while let LexerResult::Ok(' ') = self.peek() {
            current_spaces += 1;
            self.next();
        }

        let location = self.get_cur_location();
        assert!(
            current_spaces % 4 == 0,
            "Indentation must be a multiple of 4, found {current_spaces} at {location:?}"
        );
        current_spaces
    }

    fn symbol_to_token(identifier: &str) -> LexerResult<Token> {
        LexerResult::Ok(match identifier {
            "+" => Token::Operator(Operator::Plus),
            "," => Token::Comma,
            "[" => Token::OpenArrayBracket,
            "]" => Token::CloseArrayBracket,
            "." => Token::MemberAccessor,
            "-" => Token::Operator(Operator::Minus),
            "*" => Token::Operator(Operator::Multiply),
            "%" => Token::Operator(Operator::Mod),
            "/" => Token::Operator(Operator::Divide),
            "^" => Token::Operator(Operator::Xor),
            "<<" => Token::Operator(Operator::BitShiftLeft),
            //">>" => Token::Operator(Operator::BitShiftRight), //this is a problem because of generics, solved in parser instead
            "==" => Token::Operator(Operator::Equals),
            "->" => Token::ArrowRight,
            "=" => Token::Assign,
            "&" => Token::Operator(Operator::Ampersand),
            "!=" => Token::Operator(Operator::NotEquals),
            "(" => Token::OpenParen,
            ")" => Token::CloseParen,
            ":" => Token::Colon,
            "::" => Token::DoubleColon,
            ">" => Token::Operator(Operator::Greater),
            "<" => Token::Operator(Operator::Less),
            ">=" => Token::Operator(Operator::GreaterEquals),
            "<=" => Token::Operator(Operator::LessEquals),
            "..." => Token::Ellipsis,
            _ => return LexerResult::Error(format!("Unimplemented operator {identifier}")),
        })
    }

    fn identifier_to_token(identifier: &str) -> Token {
        match identifier {
            "None" => Token::None,
            "as" => Token::AsKeyword,
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
            "impl" => Token::ImplKeyword,
            "intrinsic" => Token::IntrinsicKeyword,
            "external" => Token::ExternalKeyword,
            "return" => Token::ReturnKeyword,
            "in" => Token::InKeyword,
            "while" => Token::WhileKeyword,
            "break" => Token::BreakKeyword,
            "struct" => Token::StructDef,
            "self" => Token::SelfKeyword,
            _ => Token::Identifier(InternedString::new(identifier)),
        }
    }
}

pub fn tokenize(file: FileTableIndex, source: &str) -> LexerResult<TokenTable> {
    Lexer::new(file, source.to_string()).tokenize()
}

#[cfg(test)]
mod tests {
    use crate::semantic::context::test_utils::istr;

    use super::*;

    fn tokenize(str: &str) -> Result<Vec<Token>, String> {
        let tokenized = crate::ast::lexer::tokenize(FileTableIndex(0), str);
        match tokenized {
            LexerResult::Ok(table) => Ok(table.tokens.into_iter().map(|x| x.token).collect()),
            LexerResult::NoMatch => Err("No match".to_string()),
            LexerResult::EndOfInput => Err("End of input".to_string()),
            LexerResult::Error(e) => Err(e),
        }
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
    fn fail_tokenization_nothing_after_e_number() -> Result<(), String> {
        let result = tokenize("22.1e");
        match result {
            Ok(_) => Err("Should not be able to parse 22.1e".to_string()),
            Err(_) => Ok(()),
        }
    }

    #[test]
    fn tokenizer_decimal_exponent_number() -> Result<(), String> {
        let result = tokenize("22.22e2")?;
        assert_eq!(result, [Token::LiteralFloat(22.22e2.into())]);
        Ok(())
    }

    #[test]
    fn tokenizer_decimal_exponent_number_without_dot() -> Result<(), String> {
        let result = tokenize("22e2")?;
        assert_eq!(result, [Token::LiteralFloat(22e2.into())]);
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
        let result = tokenize("10 ?? 12");
        println!("{:?}", result);
        match result {
            Ok(_) => Err("Operator ?? doesnt exist and shouldn't be tokenized"),
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
                //>> is not a bitshift right because when we are parsing nested generics,
                //the operator >> is used to close the generic, but we need to see 2 > tokens individually.
                //For bitshift operators in actual expressions, we can peek the next one, see if is > too, and push a single BitShiftRight to the stack
                Token::Operator(Operator::Greater),
                Token::Operator(Operator::Greater),
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
        let result = tokenize("\"abc\"")?;
        assert_eq!(result, [Token::LiteralString(istr("abc"))]);
        Ok(())
    }

    #[test]
    fn char_literal() -> Result<(), String> {
        let result = tokenize("'a'")?;
        assert_eq!(result, [Token::LiteralChar('a')]);
        Ok(())
    }

    #[test]
    fn string_literal_escape_double_quotes() -> Result<(), String> {
        let result = tokenize("\"a\\\"b\\\"c\"")?;
        assert_eq!(result, [Token::LiteralString(istr("a\"b\"c"))]);
        Ok(())
    }

    #[test]
    fn string_literal_newline_tabs_and_null_terminator() -> Result<(), String> {
        let result = tokenize("\"a \t \n \0 \"")?;
        assert_eq!(result, [Token::LiteralString(istr("a \t \n \0 "))]);
        Ok(())
    }

    #[test]
    fn escape_char_literal_newline() -> Result<(), String> {
        let result = tokenize("'\\n'")?;
        assert_eq!(result, [Token::LiteralChar('\n')]);
        Ok(())
    }

    #[test]
    fn escape_char_literal_tab() -> Result<(), String> {
        let result = tokenize("'\\t'")?;
        assert_eq!(result, [Token::LiteralChar('\t')]);
        Ok(())
    }

    #[test]
    fn escape_char_literal_single_quote() -> Result<(), String> {
        let result = tokenize("'\\''")?;
        assert_eq!(result, [Token::LiteralChar('\'')]);
        Ok(())
    }

    #[test]
    fn escape_char_literal_single_doublequote() -> Result<(), String> {
        let result = tokenize("'\"'")?;
        assert_eq!(result, [Token::LiteralChar('\"')]);
        Ok(())
    }

    #[test]
    fn escape_char_literal_null_terminator() -> Result<(), String> {
        let result = tokenize("'\\0'")?;
        assert_eq!(result, [Token::LiteralChar('\0')]);
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
    fn struct_def() -> Result<(), String> {
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

    #[test]
    fn ellipsis() -> Result<(), String> {
        let result = tokenize("...")?;
        assert_eq!(result, [Token::Ellipsis,]);
        Ok(())
    }

    #[test]
    fn as_cast() -> Result<(), String> {
        let result = tokenize("as")?;
        assert_eq!(result, [Token::AsKeyword]);
        Ok(())
    }

    #[test]
    fn impl_keywprd() -> Result<(), String> {
        let result = tokenize("impl")?;
        assert_eq!(result, [Token::ImplKeyword]);
        Ok(())
    }

    #[test]
    fn self_keyword_in_method() -> Result<(), String> {
        let result = tokenize("def method(self)")?;
        assert_eq!(
            result,
            [
                Token::DefKeyword,
                Token::Identifier(istr("method")),
                Token::OpenParen,
                Token::SelfKeyword,
                Token::CloseParen
            ]
        );
        Ok(())
    }

    //
    #[test]
    fn member_compare() -> Result<(), String> {
        let result = tokenize("self.current >= self.max")?;
        assert_eq!(
            result,
            [
                Token::SelfKeyword,
                Token::MemberAccessor,
                Token::Identifier(istr("current")),
                Token::Operator(Operator::GreaterEquals),
                Token::SelfKeyword,
                Token::MemberAccessor,
                Token::Identifier(istr("max")),
            ]
        );
        Ok(())
    }

    #[test]
    fn double_colon() -> Result<(), String> {
        let result = tokenize("::")?;
        assert_eq!(result, [Token::DoubleColon]);
        Ok(())
    }

    #[test]
    fn double_colon_in_context() -> Result<(), String> {
        let result = tokenize("SomeType::some_function")?;
        assert_eq!(
            result,
            [
                Token::Identifier(istr("SomeType")),
                Token::DoubleColon,
                Token::Identifier(istr("some_function"))
            ]
        );
        Ok(())
    }

    #[test]
    fn colon() -> Result<(), String> {
        let result = tokenize(":")?;
        assert_eq!(result, [Token::Colon]);
        Ok(())
    }

    #[test]
    fn comment() -> Result<(), String> {
        let result = tokenize("#some comment")?;
        assert_eq!(result, []);
        Ok(())
    }
}
