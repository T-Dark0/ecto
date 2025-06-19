use crate::scope_tree::{
    ast::{Parsed, Scope},
    lexer::{Token, TokenKind},
    Span,
};

struct Parser<'source> {
    lexed: &'source [Token],
    last_span: Span,
}
impl<'source> Parser<'source> {
    fn scope(&mut self) -> Parsed<Scope> {
        let contents = self.scope_contents();
        match self.lexed {
            [token!(CloseParen), rest @ ..] => todo!(),
            [tok, rest @ ..] => todo!(),
            [] => todo!(),
        }
    }
    fn scope_contents(&mut self) -> Parsed<Scope> {
        todo!()
    }

    fn eof(&self) -> Token {
        Token {
            kind: TokenKind::Eof,
            span: self.last_span.empty_after(),
        }
    }
}

macro_rules! token {
    ($kind:ident) => {
        Token {
            kind: TokenKind::$kind,
            span: _,
        }
    };
    ($kind:ident, $span:pat) => {
        Token {
            kind: TokenKind::$kind,
            span: $span,
        }
    };
}
use token;
