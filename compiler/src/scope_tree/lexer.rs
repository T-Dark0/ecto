use super::Span;
use itertools::Itertools;
use logos::Logos;
use std::{
    fmt::{self, Debug},
    ops::Range,
};

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
#[logos(skip "[ \t]+")]
pub enum TokenKind {
    #[token("fn")]
    Fn,
    #[token("op")]
    Op,
    #[token("=")]
    Equals,
    #[token(":")]
    Colon,
    #[regex(r"\p{XID_Start}\p{XID_Continue}*")]
    Identifier,
    #[token("_")]
    Underscore,
    #[regex(r#""[^"]+""#)]
    NamePart,
    #[token(r"\")]
    Backslash,
    #[token("*")]
    Star,
    #[token(";")]
    Semicolon,
    #[token("<-")]
    LeftArrow,
    #[token("->")]
    RightArrow,
    #[token(",")]
    Comma,
    #[token("use")]
    Use,
    #[token(".")]
    Dot,
    #[token("\n")]
    NewLine,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,

    Eof,
    Error,
}
#[derive(Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}
impl Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{:?}", self.kind, self.span)
    }
}

pub struct Lexed {
    pub kinds: Vec<TokenKind>,
    pub spans: Vec<Span>,
}
#[derive(Clone, Copy)]
pub struct LexedSlice<'a> {
    pub kinds: &'a [TokenKind],
    pub spans: &'a [Span],
}
impl Lexed {
    pub fn as_slice(&self) -> LexedSlice<'_> {
        LexedSlice {
            kinds: &self.kinds,
            spans: &self.spans,
        }
    }
}

pub fn lex(source: &str) -> Lexed {
    let (kinds, spans) = TokenKind::lexer(source)
        .spanned()
        .map(|(k, s)| (k.unwrap_or(TokenKind::Error), to_span(s)))
        .coalesce(|prev, curr| match (prev.0, curr.0) {
            (TokenKind::Error, TokenKind::Error) => Ok((TokenKind::Error, prev.1.until(curr.1))),
            _ => Err((prev, curr)),
        })
        .collect();
    Lexed { kinds, spans }
}
fn to_span(range: Range<usize>) -> Span {
    Span {
        start: range.start as u32,
        len: (range.end - range.start) as u16,
    }
}
