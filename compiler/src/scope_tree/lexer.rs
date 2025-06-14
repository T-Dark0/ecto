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
    kinds: Vec<TokenKind>,
    spans: Vec<Span>,
}
#[derive(Clone, Copy)]
pub struct LexedSlice<'a> {
    kinds: &'a [TokenKind],
    spans: &'a [Span],
}
#[derive(Clone, Copy)]
pub struct LexedRef<'a> {
    kind: &'a TokenKind,
    span: &'a Span,
}
impl Lexed {
    pub fn as_slice(&self) -> LexedSlice<'_> {
        LexedSlice {
            kinds: &self.kinds,
            spans: &self.spans,
        }
    }
}
impl<'a> LexedSlice<'a> {
    pub fn kinds(self) -> &'a [TokenKind] {
        self.kinds
    }
    pub fn strip_prefix(self, prefix: &[TokenKind]) -> Option<Self> {
        let kinds = self.kinds.strip_prefix(prefix)?;
        let spans = self.spans.get((self.spans.len() - kinds.len())..)?;
        Some(Self { kinds, spans })
    }
    pub fn take_first(&mut self) -> Option<LexedRef<'a>> {
        match *self {
            Self {
                kinds: [kind, kinds @ ..],
                spans: [span, spans @ ..],
            } => {
                *self = Self { kinds, spans };
                Some(LexedRef { kind, span })
            }
            _ => None,
        }
    }
    pub fn len(self) -> usize {
        self.kinds.len()
    }
}
impl<'a> LexedRef<'a> {
    pub fn kind(self) -> &'a TokenKind {
        self.kind
    }
    pub fn span(self) -> &'a Span {
        self.span
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
