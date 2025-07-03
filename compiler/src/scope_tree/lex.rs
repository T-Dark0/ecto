use super::span::Span;
use logos::Logos;
use std::{
    fmt::{self, Debug},
    ops::Range,
};
use strum::EnumCount;

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, EnumCount)]
#[logos(skip "[ \r\t]+")]
pub enum TokenKind {
    #[token("fn")]
    Fn,
    #[token("\n")]
    Newline,
    #[token("op")]
    Op,
    #[token(":")]
    Colon,
    #[token("=")]
    Equals,
    #[token("=>")]
    FatArrow,
    #[regex(r"\p{XID_Start}\p{XID_Continue}*")]
    Ident,
    #[token("_")]
    Underscore,
    #[regex(r#""[^"]+""#)]
    Literal,
    #[token(r"\_")]
    BackslashUnderscore,
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

pub struct Lexer<'source> {
    raw: logos::Lexer<'source, TokenKind>,
    state: LexerState,
}
enum LexerState {
    Normal,
    Error(Span),
    PostError(Token),
}
impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            raw: TokenKind::lexer(source),
            state: LexerState::Normal,
        }
    }
    fn raw_next(&mut self) -> Option<Token> {
        let (kind, span) = match self.raw.next() {
            Some(Ok(kind)) => (kind, to_span(self.raw.span())),
            Some(Err(())) => (TokenKind::Error, to_span(self.raw.span())),
            None => return None,
        };
        Some(Token { kind, span })
    }
}
impl<'source> Iterator for Lexer<'source> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.state {
                LexerState::Normal => {
                    let tok = self.raw_next()?;
                    match tok.kind {
                        TokenKind::Error => {
                            self.state = LexerState::Error(tok.span);
                            continue;
                        }
                        _ => break Some(tok),
                    }
                }
                LexerState::Error(span) => {
                    let tok = self.raw_next()?;
                    match tok.kind {
                        TokenKind::Error => self.state = LexerState::Error(span.around(tok.span)),
                        _ => {
                            self.state = LexerState::PostError(tok);
                            break Some(Token {
                                kind: TokenKind::Error,
                                span,
                            });
                        }
                    }
                }
                LexerState::PostError(peeked) => {
                    self.state = LexerState::Normal;
                    break Some(peeked);
                }
            }
        }
    }
}
fn to_span(range: Range<usize>) -> Span {
    Span {
        start: range.start as u32,
        len: (range.end - range.start) as u16,
    }
}
