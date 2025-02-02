use super::Span;
use logos::Logos;

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
#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub fn lex(source: &str) -> Vec<Token> {
    let lexer = TokenKind::lexer(source);
    let mut out = Vec::<Token>::new();
    for (kind, span) in lexer.spanned() {
        let kind = kind.unwrap_or(TokenKind::Error);
        let span = Span {
            start: span.start as u32,
            len: (span.end - span.start) as u16,
        };
        match kind {
            TokenKind::Error => {
                if let Some(last) = out.last_mut() {
                    if last.kind == TokenKind::Error {
                        last.span = last.span.until(span);
                    }
                }
            }
            kind => out.push(Token { kind, span }),
        }
    }
    out
}
