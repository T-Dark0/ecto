use std::{backtrace::Backtrace, ops::Range, panic::Location, rc::Rc, slice};

use super::common::{AnyNode, Validity};
use crate::scope_tree::{
    ast::{
        Ident, OpArrow, OpBinding, OpBindings, OpDef, OpPart, OpParts, Parsed, Scope,
        ScopeContents, UseStmt,
    },
    Span,
};
use logos::Logos;

pub fn parse(pretty: &str) -> Result<Parsed<AnyNode>, Error> {
    parse_any_node(&mut Lexer {
        lexer: Token::lexer(pretty),
        peeked: None,
    })
}

#[derive(Debug, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
    trace: Rc<Backtrace>,
}
#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    LexError,
    UnexpectedEof,
    UnexpectedToken {
        expected: &'static [Token],
        got: Token,
    },
    InvalidSpanStart,
    InvalidSpanLen,
    MissingOpParts,
    MissingOpBindings,
}
impl Error {
    fn lex_error(span: Span) -> Self {
        Self {
            kind: ErrorKind::LexError,
            span,
            trace: Rc::new(Backtrace::force_capture()),
        }
    }
    fn unexpected_token(span: Span, expected: &'static [Token], got: Token) -> Self {
        Self {
            kind: ErrorKind::UnexpectedToken { expected, got },
            span,
            trace: Rc::new(Backtrace::force_capture()),
        }
    }
    fn unexpected_eof(span: Span) -> Self {
        Self {
            kind: ErrorKind::UnexpectedEof,
            span,
            trace: Rc::new(Backtrace::force_capture()),
        }
    }
    fn invalid_span_start(span: Span) -> Self {
        Self {
            kind: ErrorKind::InvalidSpanStart,
            span,
            trace: Rc::new(Backtrace::force_capture()),
        }
    }
    fn invalid_span_len(span: Span) -> Self {
        Self {
            kind: ErrorKind::InvalidSpanLen,
            span,
            trace: Rc::new(Backtrace::force_capture()),
        }
    }
    fn missing_op_parts(span: Span) -> Self {
        Self {
            kind: ErrorKind::MissingOpParts,
            span,
            trace: Rc::new(Backtrace::force_capture()),
        }
    }
    fn missing_op_bindings(span: Span) -> Self {
        Self {
            kind: ErrorKind::MissingOpBindings,
            span,
            trace: Rc::new(Backtrace::force_capture()),
        }
    }
}

fn parse_any_node(lexer: &mut Lexer<'_>) -> Result<Parsed<AnyNode>, Error> {
    Ok(match lexer.next()? {
        Token::ScopeContents => parse_scope_contents(lexer)?.map(AnyNode::ScopeContents),
        Token::UseStmt => parse_use_stmt(lexer)?.map(AnyNode::UseStmt),
        Token::Ident => parse_ident(lexer)?.map(AnyNode::Ident),
        Token::OpDef => parse_op_def(lexer)?.map(AnyNode::OpDef),
        Token::OpParts => parse_op_parts(lexer)?.map(AnyNode::OpParts),
        Token::OpPart => parse_op_part(lexer)?.map(AnyNode::OpPart),
        Token::OpBindings => parse_op_bindings(lexer)?.map(AnyNode::OpBindings),
        Token::OpBinding => parse_op_binding(lexer)?.map(AnyNode::OpBinding),
        Token::OpArrowLeft => parse_op_arrow_left(lexer)?.map(AnyNode::OpArrow),
        Token::OpArrowRight => parse_op_arrow_right(lexer)?.map(AnyNode::OpArrow),
        Token::Scope => parse_scope(lexer)?.map(AnyNode::Scope),
        Token::Error => Parsed::error(parse_span(lexer)?),
        tok => {
            return Err(Error::unexpected_token(
                lexer.span(),
                &[
                    Token::ScopeContents,
                    Token::UseStmt,
                    Token::Ident,
                    Token::OpDef,
                    Token::OpParts,
                    Token::OpPart,
                    Token::OpBindings,
                    Token::OpBinding,
                    Token::OpArrowLeft,
                    Token::OpArrowRight,
                    Token::Scope,
                    Token::Error,
                ],
                tok,
            ))
        }
    })
}
fn parse_validity(lexer: &mut Lexer<'_>) -> Result<Validity, Error> {
    Ok(match lexer.peek()? {
        Token::Star => {
            lexer.next()?;
            Validity::Recovered
        }
        _ => Validity::Valid,
    })
}
fn parse_span(lexer: &mut Lexer<'_>) -> Result<Span, Error> {
    lexer.expect(&Token::OpenSquareParen)?;
    let start = lexer
        .expect_slice(&Token::Number)?
        .parse()
        .map_err(|_| Error::invalid_span_start(lexer.span()))?;
    lexer.expect(&Token::Comma)?;
    let len = lexer
        .expect_slice(&Token::Number)?
        .parse()
        .map_err(|_| Error::invalid_span_len(lexer.span()))?;
    lexer.expect(&Token::CloseSquareParen)?;
    Ok(Span { start, len })
}
fn parse_scope_contents(lexer: &mut Lexer<'_>) -> Result<Parsed<ScopeContents>, Error> {
    let validity = parse_validity(lexer)?;
    let span = parse_span(lexer)?;
    lexer.expect(&Token::OpenRoundParen)?;
    let mut uses = Vec::new();
    let mut op_defs = Vec::new();
    let mut children = Vec::new();
    loop {
        match lexer.next()? {
            Token::UseStmt => uses.push(parse_use_stmt(lexer)?),
            Token::OpDef => op_defs.push(parse_op_def(lexer)?),
            Token::Scope => children.push(parse_scope(lexer)?),
            Token::CloseRoundParen => break,
            tok => {
                return Err(Error::unexpected_token(
                    lexer.span(),
                    &[
                        Token::UseStmt,
                        Token::OpDef,
                        Token::Scope,
                        Token::CloseRoundParen,
                    ],
                    tok,
                ))
            }
        }
    }
    Ok(parsed(
        validity,
        span,
        ScopeContents {
            uses,
            op_defs,
            children,
        },
    ))
}
fn parse_use_stmt(lexer: &mut Lexer<'_>) -> Result<Parsed<UseStmt>, Error> {
    let validity = parse_validity(lexer)?;
    let span = parse_span(lexer)?;
    lexer.expect(&Token::OpenRoundParen)?;
    let mut path = Vec::new();
    while *lexer.peek()? != Token::CloseRoundParen {
        path.push(parse_ident(lexer)?)
    }
    lexer.next()?;
    Ok(parsed(validity, span, UseStmt { path }))
}
fn parse_ident(lexer: &mut Lexer<'_>) -> Result<Parsed<Ident>, Error> {
    lexer.expect(&Token::Ident)?;
    let validity = parse_validity(lexer)?;
    let span = parse_span(lexer)?;
    Ok(parsed(validity, span, Ident))
}
fn parse_op_def(lexer: &mut Lexer<'_>) -> Result<Parsed<OpDef>, Error> {
    let validity = parse_validity(lexer)?;
    let span = parse_span(lexer)?;
    lexer.expect(&Token::OpenRoundParen)?;
    let mut parts = None;
    let mut bindings = None;
    loop {
        match lexer.next()? {
            Token::OpParts => parts = Some(parse_op_parts(lexer)?),
            Token::OpBindings => bindings = Some(parse_op_bindings(lexer)?),
            Token::CloseRoundParen => break,
            tok => {
                return Err(Error::unexpected_token(
                    lexer.span(),
                    &[Token::OpParts, Token::OpBindings, Token::CloseSquareParen],
                    tok,
                ))
            }
        }
    }
    let parts = parts.ok_or(Error::missing_op_parts(lexer.span()))?;
    let bindings = bindings.ok_or(Error::missing_op_bindings(lexer.span()))?;
    Ok(parsed(validity, span, OpDef { parts, bindings }))
}
fn parse_op_parts(lexer: &mut Lexer<'_>) -> Result<Parsed<OpParts>, Error> {
    let validity = parse_validity(lexer)?;
    let span = parse_span(lexer)?;
    lexer.expect(&Token::OpenRoundParen)?;
    let mut parts = Vec::new();
    while *lexer.peek()? != Token::CloseRoundParen {
        parts.push(parse_op_part(lexer)?);
    }
    lexer.next()?;
    Ok(parsed(validity, span, OpParts(parts)))
}
fn parse_op_part(lexer: &mut Lexer<'_>) -> Result<Parsed<OpPart>, Error> {
    let part = match lexer.next()? {
        Token::Argument => OpPart::Argument,
        Token::LazyArgument => OpPart::LazyArgument,
        Token::Literal => OpPart::Literal,
        Token::Variadic => OpPart::Variadic(parse_op_parts(lexer)?),
        tok => {
            return Err(Error::unexpected_token(
                lexer.span(),
                &[
                    Token::Argument,
                    Token::LazyArgument,
                    Token::Literal,
                    Token::Variadic,
                ],
                tok,
            ))
        }
    };
    let validity = parse_validity(lexer)?;
    let span = parse_span(lexer)?;
    Ok(parsed(validity, span, part))
}
fn parse_op_bindings(lexer: &mut Lexer<'_>) -> Result<Parsed<OpBindings>, Error> {
    let validity = parse_validity(lexer)?;
    let span = parse_span(lexer)?;
    lexer.expect(&Token::OpenRoundParen)?;
    let mut bindings = Vec::new();
    loop {
        match lexer.next()? {
            Token::OpBinding => bindings.push(parse_op_binding(lexer)?),
            Token::CloseRoundParen => break,
            tok => {
                return Err(Error::unexpected_token(
                    lexer.span(),
                    &[Token::OpBinding, Token::CloseRoundParen],
                    tok,
                ))
            }
        }
    }
    Ok(parsed(validity, span, OpBindings(bindings)))
}
fn parse_op_binding(lexer: &mut Lexer<'_>) -> Result<Parsed<OpBinding>, Error> {
    let validity = parse_validity(lexer)?;
    let span = parse_span(lexer)?;
    lexer.expect(&Token::OpenRoundParen)?;
    let lhs = parse_ident(lexer)?;
    let arrow = match lexer.next()? {
        Token::OpArrowLeft => parse_op_arrow_left(lexer)?,
        Token::OpArrowRight => parse_op_arrow_right(lexer)?,
        tok => {
            return Err(Error::unexpected_token(
                lexer.span(),
                &[Token::OpArrowLeft, Token::OpArrowRight],
                tok,
            ))
        }
    };
    let rhs = parse_ident(lexer)?;
    lexer.expect(&Token::CloseRoundParen)?;
    Ok(parsed(validity, span, OpBinding { lhs, arrow, rhs }))
}
fn parse_op_arrow_left(lexer: &mut Lexer<'_>) -> Result<Parsed<OpArrow>, Error> {
    let validity = parse_validity(lexer)?;
    let span = parse_span(lexer)?;
    Ok(parsed(validity, span, OpArrow::Left))
}
fn parse_op_arrow_right(lexer: &mut Lexer<'_>) -> Result<Parsed<OpArrow>, Error> {
    let validity = parse_validity(lexer)?;
    let span = parse_span(lexer)?;
    Ok(parsed(validity, span, OpArrow::Right))
}
fn parse_scope(lexer: &mut Lexer<'_>) -> Result<Parsed<Scope>, Error> {
    let validity = parse_validity(lexer)?;
    let span = parse_span(lexer)?;
    lexer.expect(&Token::OpenRoundParen)?;
    let contents = match lexer.next()? {
        Token::ScopeContents => parse_scope_contents(lexer)?,
        tok => {
            return Err(Error::unexpected_token(
                lexer.span(),
                &[Token::ScopeContents],
                tok,
            ))
        }
    };
    lexer.expect(&Token::CloseRoundParen)?;
    Ok(parsed(validity, span, Scope(contents)))
}
fn parsed<T>(validity: Validity, span: Span, node: T) -> Parsed<T> {
    match validity {
        Validity::Valid => Parsed::valid(span, node),
        Validity::Recovered => Parsed::recovered(span, node),
    }
}

struct Lexer<'source> {
    lexer: logos::Lexer<'source, Token>,
    peeked: Option<Result<Token, Error>>,
}
impl<'source> Lexer<'source> {
    fn next(&mut self) -> Result<Token, Error> {
        self.peeked.take().unwrap_or_else(|| Self::raw_next(&mut self.lexer))
    }
    fn peek(&mut self) -> Result<&Token, Error> {
        self.peeked
            .get_or_insert_with(|| Self::raw_next(&mut self.lexer))
            .as_ref()
            .map_err(Clone::clone)
    }
    fn raw_next(lexer: &mut logos::Lexer<'_, Token>) -> Result<Token, Error> {
        match lexer.next() {
            Some(Ok(tok)) => Ok(tok),
            Some(Err(())) => Err(Error::lex_error(Self::raw_span(lexer.span()))),
            None => Err(Error::unexpected_eof(Self::raw_span(lexer.span()))),
        }
    }
    fn expect(&mut self, tok: &'static Token) -> Result<(), Error> {
        match self.next()? {
            next if next == *tok => Ok(()),
            next => Err(Error::unexpected_token(
                self.span(),
                slice::from_ref(tok),
                next,
            )),
        }
    }
    fn slice(&self) -> &'source str {
        self.lexer.slice()
    }
    fn span(&self) -> Span {
        Self::raw_span(self.lexer.span())
    }
    fn raw_span(span: Range<usize>) -> Span {
        let Range { start, end } = span;
        Span {
            start: start as u32,
            len: (end - start) as u16,
        }
    }
    fn expect_slice(&mut self, tok: &'static Token) -> Result<&'source str, Error> {
        self.expect(tok)?;
        Ok(self.slice())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Logos)]
#[logos(skip r"[ \s\t\r\n]+")]
enum Token {
    #[token("ScopeContents")]
    ScopeContents,
    #[token("UseStmt")]
    UseStmt,
    #[token("Ident")]
    Ident,
    #[token("OpDef")]
    OpDef,
    #[token("OpParts")]
    OpParts,
    #[token("OpPart")]
    OpPart,
    #[token("Argument")]
    Argument,
    #[token("LazyArgument")]
    LazyArgument,
    #[token("Literal")]
    Literal,
    #[token("Variadic")]
    Variadic,
    #[token("OpBindings")]
    OpBindings,
    #[token("OpBinding")]
    OpBinding,
    #[token("OpArrowLeft")]
    OpArrowLeft,
    #[token("OpArrowRight")]
    OpArrowRight,
    #[token("Scope")]
    Scope,
    #[token("Error")]
    Error,
    #[token("*")]
    Star,
    #[token("[")]
    OpenSquareParen,
    #[token(",")]
    Comma,
    #[token("]")]
    CloseSquareParen,
    #[regex(r"\d+")]
    Number,
    #[token("(")]
    OpenRoundParen,
    #[token(")")]
    CloseRoundParen,
}
