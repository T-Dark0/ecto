use super::common::{AnyNode, Validity};
use crate::scope_tree::{
    ast::{
        Ident, NodeKind, OpArrow, OpBinding, OpBindings, OpDef, OpPart, OpParts, Parsed, Scope,
        UseStmt,
    },
    span::Span,
};
use logos::Logos;
use std::{
    fmt::Display,
    ops::Range,
    slice,
    sync::atomic::{AtomicU64, Ordering},
};

pub fn parse(pretty: &str) -> Result<Parsed<AnyNode>, Error> {
    parse_any_node(&mut Parser {
        lexer: Token::lexer(pretty),
        peeked: None,
    })
}

#[derive(Debug, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
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
        }
    }
    fn unexpected_token(span: Span, expected: &'static [Token], got: Token) -> Self {
        Self {
            kind: ErrorKind::UnexpectedToken { expected, got },
            span,
        }
    }
    fn unexpected_eof(span: Span) -> Self {
        Self {
            kind: ErrorKind::UnexpectedEof,
            span,
        }
    }
    fn invalid_span_start(span: Span) -> Self {
        Self {
            kind: ErrorKind::InvalidSpanStart,
            span,
        }
    }
    fn invalid_span_len(span: Span) -> Self {
        Self {
            kind: ErrorKind::InvalidSpanLen,
            span,
        }
    }
    fn missing_op_parts(span: Span) -> Self {
        Self {
            kind: ErrorKind::MissingOpParts,
            span,
        }
    }
    fn missing_op_bindings(span: Span) -> Self {
        Self {
            kind: ErrorKind::MissingOpBindings,
            span,
        }
    }
}

fn parse_any_node(parser: &mut Parser<'_>) -> Result<Parsed<AnyNode>, Error> {
    enter("parse_any_node", parser.lexer.remainder());
    let out = Ok(select! { parser, |tok, expected|
        next Token::Scope => parse_scope(parser)?.map(AnyNode::Scope),
        next Token::UseStmt => parse_use_stmt(parser)?.map(AnyNode::UseStmt),
        next Token::Ident => parse_ident(parser)?.map(AnyNode::Ident),
        next Token::OpDef => parse_op_def(parser)?.map(AnyNode::OpDef),
        next Token::OpParts => parse_op_parts(parser)?.map(AnyNode::OpParts),
        next Token::OpPart => parse_op_part(parser)?.map(AnyNode::OpPart),
        next Token::Argument => parse_argument(parser)?.map(AnyNode::OpPart),
        next Token::LazyArgument => parse_lazy_argument(parser)?.map(AnyNode::OpPart),
        next Token::Literal => parse_literal(parser)?.map(AnyNode::OpPart),
        next Token::Variadic => parse_variadic(parser)?.map(AnyNode::OpPart),
        next Token::OpBindings => parse_op_bindings(parser)?.map(AnyNode::OpBindings),
        next Token::OpBinding => parse_op_binding(parser)?.map(AnyNode::OpBinding),
        next Token::OpArrowLeft => parse_op_arrow_left(parser)?.map(AnyNode::OpArrow),
        next Token::OpArrowRight => parse_op_arrow_right(parser)?.map(AnyNode::OpArrow),
        next else => return Err(Error::unexpected_token(parser.span(), expected, tok)),
    });
    exit("parse_any_node", parser.lexer.remainder());
    out
}
fn parse_scope(parser: &mut Parser<'_>) -> Result<Parsed<Scope>, Error> {
    enter("parse_scope", parser.lexer.remainder());
    let out = span_and_validity(parser, NodeKind::Scope, |parser| {
        parser.expect(&Token::OpenRoundParen)?;
        let mut uses = Vec::new();
        let mut op_defs = Vec::new();
        let mut children = Vec::new();
        loop {
            select! { parser, |tok, expected|
                next Token::UseStmt => uses.push(parse_use_stmt(parser)?),
                next Token::OpDef => op_defs.push(parse_op_def(parser)?),
                next Token::Scope => children.push(parse_scope(parser)?),
                // FIXME: If I find an error here, what is it an error _in_?
                next Token::CloseRoundParen => break,
                next else => return Err(Error::unexpected_token(parser.span(), expected, tok)),
            }
        }
        Ok(Scope {
            uses,
            op_defs,
            children,
        })
    });
    exit("parse_scope", parser.lexer.remainder());
    out
}
fn parse_use_stmt(lexer: &mut Parser<'_>) -> Result<Parsed<UseStmt>, Error> {
    span_and_validity(lexer, NodeKind::UseStmt, |lexer| {
        lexer.expect(&Token::OpenRoundParen)?;
        let mut path = Vec::new();
        loop {
            select! { lexer, |tok, _|
                next Token::CloseRoundParen => break,
                peek else => path.push(parse_ident(lexer)?),
            }
        }
        Ok(UseStmt { path })
    })
}
fn parse_ident(lexer: &mut Parser<'_>) -> Result<Parsed<Ident>, Error> {
    let ident = select! { lexer, |tok, expected|
        next Token::Ident => Ident,
        next else => return Err(Error::unexpected_token(lexer.span(), expected, tok)),
    };
    span_and_validity(lexer, NodeKind::Ident, |_| Ok(ident))
}
fn parse_op_def(parser: &mut Parser<'_>) -> Result<Parsed<OpDef>, Error> {
    enter("parse_op_def", parser.lexer.remainder());
    let out = span_and_validity(parser, NodeKind::OpDef, |lexer| {
        lexer.expect(&Token::OpenRoundParen)?;
        let mut parts = None;
        let mut bindings = None;
        loop {
            select! { lexer, |tok, expected|
                next Token::OpParts => parts = Some(parse_op_parts(lexer)?),
                next Token::OpBindings => bindings = Some(parse_op_bindings(lexer)?),
                next Token::CloseRoundParen => break,
                next else => return Err(Error::unexpected_token(lexer.span(), expected, tok))
            }
        }
        let parts = parts.ok_or(Error::missing_op_parts(lexer.span()))?;
        let bindings = bindings.ok_or(Error::missing_op_bindings(lexer.span()))?;
        Ok(OpDef { parts, bindings })
    });
    exit("parse_op_def", parser.lexer.remainder());
    out
}
fn parse_op_parts(parser: &mut Parser<'_>) -> Result<Parsed<OpParts>, Error> {
    enter("parse_op_parts", parser.lexer.remainder());
    let out = span_and_validity(parser, NodeKind::OpParts, |lexer| {
        //FIXME: When being called for `OpPart::Variadic`, the "OpParts` token has yet to be eaten
        lexer.expect(&Token::OpenRoundParen)?;
        let mut parts = Vec::new();
        loop {
            select! { lexer, |tok, _|
                next Token::CloseRoundParen => break,
                peek else => parts.push(parse_op_part(lexer)?),
            }
        }
        Ok(OpParts(parts))
    });
    exit("parse_op_parts", parser.lexer.remainder());
    out
}
fn parse_op_part(parser: &mut Parser<'_>) -> Result<Parsed<OpPart>, Error> {
    select! { parser, |tok, expected|
        next Token::Argument => parse_argument(parser),
        next Token::LazyArgument => parse_lazy_argument(parser),
        next Token::Literal => parse_literal(parser),
        next Token::Variadic => parse_variadic(parser),
        next else => Err(Error::unexpected_token(parser.span(), expected, tok)),
    }
}
fn parse_argument(parser: &mut Parser<'_>) -> Result<Parsed<OpPart>, Error> {
    enter("parse_argument", parser.lexer.remainder());
    let out = span_and_validity(parser, NodeKind::OpPart, |_| Ok(OpPart::Argument));
    exit("parse_argument", parser.lexer.remainder());
    out
}
fn parse_lazy_argument(parser: &mut Parser<'_>) -> Result<Parsed<OpPart>, Error> {
    span_and_validity(parser, NodeKind::OpPart, |_| Ok(OpPart::LazyArgument))
}
fn parse_literal(parser: &mut Parser<'_>) -> Result<Parsed<OpPart>, Error> {
    span_and_validity(parser, NodeKind::OpPart, |_| Ok(OpPart::Literal))
}
fn parse_variadic(parser: &mut Parser<'_>) -> Result<Parsed<OpPart>, Error> {
    enter("parse_variadic", parser.lexer.remainder());
    let out = span_and_validity(parser, NodeKind::OpPart, |parser| {
        parser.expect(&Token::OpenRoundParen)?;
        parser.expect(&Token::OpParts)?;
        let parts = parse_op_parts(parser)?;
        parser.expect(&Token::CloseRoundParen)?;
        Ok(OpPart::Variadic(parts))
    });
    exit("parse_variadic", parser.lexer.remainder());
    out
}
fn parse_op_bindings(parser: &mut Parser<'_>) -> Result<Parsed<OpBindings>, Error> {
    enter("parse_op_bindings", parser.lexer.remainder());
    let out = span_and_validity(parser, NodeKind::OpBindings, |parser| {
        parser.expect(&Token::OpenRoundParen)?;
        let mut bindings = Vec::new();
        loop {
            select! { parser, |tok, expected|
                next Token::OpBinding => bindings.push(parse_op_binding(parser)?),
                next Token::CloseRoundParen => break,
                next else => return Err(Error::unexpected_token(parser.span(), expected, tok))
            }
        }
        Ok(OpBindings(bindings))
    });
    exit("parse_op_bindings", parser.lexer.remainder());
    out
}
fn parse_op_binding(parser: &mut Parser<'_>) -> Result<Parsed<OpBinding>, Error> {
    enter("parse_op_binding", parser.lexer.remainder());
    let out = span_and_validity(parser, NodeKind::OpBinding, |parser| {
        parser.expect(&Token::OpenRoundParen)?;
        let lhs = parse_ident(parser)?;
        let arrow = select! { parser, |tok, expected|
            next Token::OpArrowLeft => parse_op_arrow_left(parser)?,
            next Token::OpArrowRight => parse_op_arrow_right(parser)?,
            next else => return Err(Error::unexpected_token(parser.span(), expected, tok))
        };
        let rhs = parse_ident(parser)?;
        parser.expect(&Token::CloseRoundParen)?;
        Ok(OpBinding { lhs, arrow, rhs })
    });
    exit("parse_op_binding", parser.lexer.remainder());
    out
}
fn parse_op_arrow_left(lexer: &mut Parser<'_>) -> Result<Parsed<OpArrow>, Error> {
    span_and_validity(lexer, NodeKind::OpArrow, |_| Ok(OpArrow::Left))
}
fn parse_op_arrow_right(lexer: &mut Parser<'_>) -> Result<Parsed<OpArrow>, Error> {
    span_and_validity(lexer, NodeKind::OpArrow, |_| Ok(OpArrow::Right))
}

fn span_and_validity<'source, F, R>(
    lexer: &mut Parser<'source>,
    kind: NodeKind,
    f: F,
) -> Result<Parsed<R>, Error>
where
    F: FnOnce(&mut Parser<'source>) -> Result<R, Error>,
{
    let validity = parse_validity(lexer)?;
    let span = parse_span(lexer)?;
    Ok(match validity {
        Validity::Valid => Parsed::valid(span, f(lexer)?),
        Validity::Recovered => Parsed::recovered(span, f(lexer)?),
        Validity::Error => Parsed::error(span, kind),
    })
}
fn parse_validity(parser: &mut Parser<'_>) -> Result<Validity, Error> {
    Ok(select! { parser, |tok, _|
        next Token::Star => Validity::Recovered,
        next Token::ExclMark => Validity::Error,
        peek else => Validity::Valid
    })
}
fn parse_span(parser: &mut Parser<'_>) -> Result<Span, Error> {
    parser.expect(&Token::OpenSquareParen)?;
    let start = select! { parser, |tok, expected|
        next Token::Number => parser.slice().parse().map_err(|_|Error::invalid_span_start(parser.span()))?,
        next else => return Err(Error::unexpected_token(parser.span(),expected,tok)),
    };
    parser.expect(&Token::Comma)?;
    let len = select! { parser, |got, expected|
        next Token::Number => parser.slice().parse().map_err(|_| Error::invalid_span_len(parser.span()))?,
        next else => return Err(Error::unexpected_token(parser.span(), expected, got)),
    };
    parser.expect(&Token::CloseSquareParen)?;
    Ok(Span { start, len })
}

struct Parser<'source> {
    lexer: logos::Lexer<'source, Token>,
    peeked: Option<Result<Token, Error>>,
}
impl<'source> Parser<'source> {
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
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Logos)]
#[logos(skip r"[ \s\t\r\n]+")]
enum Token {
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
    #[token("*")]
    Star,
    #[token("!")]
    ExclMark,
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

macro_rules! select {
    ($lexer:ident, |$tok:ident, $expected:pat_param| $( $mode:ident $($($kind:ident)::*)|* => $body:expr ),* $(,)?) => {
        select! { @expected [$( $mode $($($kind)::*)|* => $body ),*] [] [$lexer, $tok, $expected] $( # $($($kind)::*)|* ),* }
    };

    (
        @expected [$($original:tt)*] [$($acc:tt)*] [$lexer:ident, $tok:ident, $expected:pat_param]
        # else
        $(, # $($($rkind:ident)::*)|* )*
    ) => {
        select! { @expected [$($original)*] [$($acc)*] [$lexer, $tok, $expected] $( # $($($rkind)::*)|* ),* }
    };
    (
        @expected [$($original:tt)*] [$($acc:tt)*] [$lexer:ident, $tok:ident, $expected:pat_param]
        # $($($fkind:ident)::*)|*
        $(, # $($($rkind:ident)::*)|* )*
    ) => {
        select! { @expected [$($original)*] [$($acc)* $($($fkind)::*,)*] [$lexer, $tok, $expected] $( # $($($rkind)::*)|* ),* }
    };
    (
        @expected [$( $mode:ident $($($kind:ident)::*)|* => $body:expr ),*] [$($acc:tt)*] [$lexer:ident, $tok:ident, $expected:pat_param]
    ) => {{
        let $tok = *$lexer.peek()?;
        let $expected = &[$($acc)*];
        match $tok {
            $(
                select! { @adapt-else $($($kind)::*)|* } => {
                    select! ( @mode $mode {_ = $lexer.next()} {});
                    $body
                }
            ),*
        }
    }};

    (@mode next {$($next:tt)*} {$($peek:tt)*} ) => {
        $($next)*
    };
    (@mode peek {$($next:tt)*} {$($peek:tt)*} ) => {
        $($peek)*
    };

    (@adapt-else else) => {
        _
    };
    (@adapt-else $($($kind:ident)::*)|*) => {
        $($($kind)::*)|*
    };
}
use select;

static DEPTH: AtomicU64 = AtomicU64::new(0);
fn enter(name: &str, remainder: &str) {
    debug(format_args!("enter {name}: {remainder:?}"));
    DEPTH.fetch_add(1, Ordering::Relaxed);
}
fn exit(name: &str, remainder: &str) {
    DEPTH.fetch_sub(1, Ordering::Relaxed);
    debug(format_args!("exit {name}: {remainder:?}"));
}
fn debug<T: Display>(val: T) {
    for _ in 0..DEPTH.load(Ordering::Relaxed) {
        print!("    ");
    }
    println!("{val}")
}
