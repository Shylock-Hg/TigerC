use std::fmt::Display;
use unicode_xid;

use crate::{cursor, ident_pool, ident_pool::Symbol};

#[derive(Debug, Clone)]
pub struct Pos {
    pub line: usize,
    pub column: usize,
}

impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("[")?;
        f.write_str(&format!("{}", self.line))?;
        f.write_str(", ")?;
        f.write_str(&format!("{}", self.column))?;
        f.write_str("]")
    }
}

#[derive(Debug, Clone)]
pub struct Posed<T> {
    node: T,
    pos: Pos,
}

impl<T> Posed<T> {
    pub fn new(node: T, pos: Pos) -> Posed<T> {
        Posed { node, pos }
    }

    pub fn node(&self) -> &T {
        &self.node
    }

    pub fn move_node(self) -> T {
        self.node
    }

    pub fn pos(&self) -> &Pos {
        &self.pos
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Error {
    UnknownChar,
    CommentUnterminated,
    StringLiteralUnterminated,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Comment,
    Ident(Symbol),
    Number(i64),
    Str(String),
    Slash,        // /
    OpenBrace,    // {
    CloseBrace,   // }
    OpenParen,    // (
    CloseParen,   // )
    OpenBracket,  // [
    CloseBracket, // ]
    Assign,       // :=
    Comma,        // ,
    SemiColon,    // ;
    And,          // &
    Or,           // |
    Colon,        // :
    Plus,         // +
    Minus,        // -
    Star,         // *
    Eq,           // =
    Ne,           // <>
    Gt,           // >
    Ge,           // >=
    Lt,           // <
    Le,           // <=
    Dot,          // .
    Dummy,        // just placeholder
    Error(Error),
    Eof,
}

pub fn tokenize<'a>(content: &'a str) -> impl Iterator<Item = Posed<Token>> + 'a {
    let mut cursor = cursor::Cursor::new(content);
    std::iter::from_fn(move || {
        let token = cursor.advance_token();
        if let Token::Eof = token.node {
            None
        } else {
            Some(token)
        }
    })
}

pub fn debug_token_stream(it: impl Iterator<Item = Posed<Token>>) -> String {
    let mut buf = String::new();
    for i in it {
        buf.push_str(&format!("{:?}, ", i));
    }
    buf
}

impl cursor::Cursor<'_> {
    pub fn advance_token(&mut self) -> Posed<Token> {
        let pos = Pos {
            line: self.line(),
            column: self.column(),
        };
        let token = self.advance_token_();
        match token {
            Token::Dummy | Token::Comment => self.advance_token(),
            _ => Posed::new(token, pos),
        }
    }

    fn advance_token_(&mut self) -> Token {
        let first_char = self.bump();
        if let Some(first_char) = first_char {
            match first_char {
                '/' => match self.first() {
                    '*' => self.block_comment(),
                    _ => Token::Slash,
                },
                '\n' => Token::Dummy,
                c if is_whitespace(c) => self.whitespace(),
                '&' => Token::And,
                '|' => Token::Or,
                '{' => Token::OpenBrace,
                '}' => Token::CloseBrace,
                '(' => Token::OpenParen,
                ')' => Token::CloseParen,
                '[' => Token::OpenBracket,
                ']' => Token::CloseBracket,
                ',' => Token::Comma,
                ';' => Token::SemiColon,
                ':' => match self.first() {
                    '=' => {
                        self.bump();
                        Token::Assign
                    }
                    _ => Token::Colon,
                },
                '+' => Token::Plus,
                '*' => Token::Star,
                '-' => Token::Minus,
                '=' => Token::Eq,
                '>' => match self.first() {
                    '=' => {
                        self.bump();
                        Token::Ge
                    }
                    _ => Token::Gt,
                },
                '<' => match self.first() {
                    '=' => {
                        self.bump();
                        Token::Le
                    }
                    '>' => {
                        self.bump();
                        Token::Ne
                    }
                    _ => Token::Lt,
                },
                '.' => Token::Dot,
                '"' => self.str(),
                c if is_digit(c) => self.number(),
                c if is_id_start(c) => {
                    let mut ident = String::new();
                    ident.push(c);
                    self.ident_continue(&mut ident);
                    Token::Ident(ident_pool::symbol(&ident))
                }
                _ => Token::Error(Error::UnknownChar),
            }
        } else {
            Token::Eof
        }
    }

    // process `/*..*/`
    fn block_comment(&mut self) -> Token {
        debug_assert!(self.prev() == '/');
        // bump `*`
        self.bump();
        // nested depth of block comments
        let mut depth = 1_usize;
        while let Some(c) = self.bump() {
            match c {
                '/' if self.first() == '*' => {
                    self.bump();
                    depth += 1;
                }
                '*' if self.first() == '/' => {
                    self.bump();
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                _ => (),
            }
        }
        if depth == 0 {
            Token::Comment
        } else {
            Token::Error(Error::CommentUnterminated)
        }
    }

    fn whitespace(&mut self) -> Token {
        debug_assert!(is_whitespace(self.prev()));
        self.eat_while(is_whitespace);
        Token::Dummy
    }

    fn ident_continue(&mut self, ident: &mut String) {
        debug_assert!(is_id_start(self.prev()));
        // Start is already eaten, eat the rest of identifier.
        self.eat_while_back(ident, is_id_continue);
    }

    fn str(&mut self) -> Token {
        debug_assert!(self.prev() == '"');
        let mut buf = String::new();
        // TODO support more escape
        let mut escape = false;
        loop {
            match self.first() {
                '"' => {
                    if escape {
                        buf.push(self.bump().unwrap());
                        escape = false;
                    } else {
                        break;
                    }
                }
                't' => {
                    if escape {
                        buf.push('\t');
                        self.bump();
                    } else {
                        buf.push(self.bump().unwrap());
                    }
                }
                'n' => {
                    if escape {
                        buf.push('\n');
                        self.bump();
                    } else {
                        buf.push(self.bump().unwrap());
                    }
                }
                '\\' => {
                    if escape {
                        escape = false;
                        buf.push(self.bump().unwrap());
                    } else {
                        escape = true;
                    }
                }
                _ => {
                    buf.push(self.bump().unwrap());
                }
            }
        }
        if self.first() != '"' {
            Token::Error(Error::StringLiteralUnterminated)
        } else {
            self.bump();
            Token::Str(buf)
        }
    }

    fn number(&mut self) -> Token {
        debug_assert!(is_digit(self.prev()));
        let mut buf = String::new();
        buf.push(self.prev());
        self.eat_while_back(&mut buf, is_digit);
        Token::Number(buf.parse::<i64>().unwrap())
    }
}

pub fn is_whitespace(c: char) -> bool {
    // This is Pattern_White_Space.
    //
    // Note that this set is stable (ie, it doesn't change with different
    // Unicode versions), so it's ok to just hard-code the values.

    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000A}' // \n
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

pub fn is_id_start(c: char) -> bool {
    // This is XID_Start OR '_' (which formally is not a XID_Start).
    c == '_' || unicode_xid::UnicodeXID::is_xid_start(c)
}

pub fn is_id_continue(c: char) -> bool {
    unicode_xid::UnicodeXID::is_xid_continue(c)
}

pub fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}
