use super::common::AnyNode;
use crate::{
    scope_tree::ast::{
        FnBody, FnDef, Ident, NodeKind, OpArrow, OpBinding, OpBindings, OpDef, OpPart, OpParts, Parsed, Scope, UseStmt,
        Validity,
    },
    span::Span,
};
use logos::Logos;
use macro_rules_attribute::macro_rules_attribute;
use std::{
    cell::Cell,
    fmt::{self, Debug, Display},
    str::FromStr,
};

pub fn parse(pretty: &str) -> Result<Parsed<AnyNode>, Error> {
    NodeStream::new(pretty).parse_any_node()
}

struct NodeStream<'source> {
    lexer: Lexer<'source>,
}
struct Node<'stream, 'source> {
    meta: NodeMetadata,
    contents: Option<&'stream mut NodeStream<'source>>,
}
#[derive(Clone, Copy)]
struct NodeMetadata {
    span: Span,
    kind: NodeKind,
    validity: Validity,
    location: SourceLocation,
}
#[derive(Clone, Copy)]
struct SourceLocation(Span);
impl<'source> NodeStream<'source> {
    fn new(source: &'source str) -> Self {
        Self {
            lexer: Lexer::new(source),
        }
    }
    #[macro_rules_attribute(trace)]
    fn parse_any_node(&mut self) -> Result<Parsed<AnyNode>, Error> {
        let node = self.next_or_err()?;
        match node.kind() {
            NodeKind::Scope => node.parse(|c| c.parse_scope().map(AnyNode::Scope)),
            NodeKind::UseStmt => node.parse(|c| c.parse_use_stmt().map(AnyNode::UseStmt)),
            NodeKind::FnDef => node.parse(|c| c.parse_fn_def().map(AnyNode::FnDef)),
            NodeKind::Ident => node.parse(|c| c.parse_ident().map(AnyNode::Ident)),
            NodeKind::OpDef => node.parse(|c| c.parse_op_def().map(AnyNode::OpDef)),
            NodeKind::OpParts => node.parse(|c| c.parse_op_parts().map(AnyNode::OpParts)),
            NodeKind::Argument => node.parse(|c| c.parse_argument().map(AnyNode::OpPart)),
            NodeKind::LazyArgument => node.parse(|c| c.parse_lazy_argument().map(AnyNode::OpPart)),
            NodeKind::OpPart => node.parse_error(NodeKind::OpPart).map(|p| p.map(AnyNode::OpPart)),
            NodeKind::Literal => node.parse(|c| c.parse_literal().map(AnyNode::OpPart)),
            NodeKind::Variadic => node.parse(|c| c.parse_variadic().map(AnyNode::OpPart)),
            NodeKind::OpBindings => node.parse(|c| c.parse_op_bindings().map(AnyNode::OpBindings)),
            NodeKind::OpBinding => node.parse(|c| c.parse_op_binding().map(AnyNode::OpBinding)),
            NodeKind::OpArrow => node.parse_error(NodeKind::OpArrow).map(|p| p.map(AnyNode::OpArrow)),
            NodeKind::OpArrowLeft => node.parse(|c| c.parse_op_arrow_left().map(AnyNode::OpArrow)),
            NodeKind::OpArrowRight => node.parse(|c| c.parse_op_arrow_right().map(AnyNode::OpArrow)),
            NodeKind::FnBody => node.parse(|c| c.parse_fn_body().map(AnyNode::FnBody)),
        }
    }
    #[macro_rules_attribute(trace)]
    fn parse_scope(&mut self) -> Result<Scope, Error> {
        let mut uses = Vec::new();
        let mut fn_defs = Vec::new();
        let mut children = Vec::new();
        while let Some(node) = self.next()? {
            match node.kind() {
                NodeKind::UseStmt => uses.push(node.parse(Self::parse_use_stmt)?),
                NodeKind::FnDef => fn_defs.push(node.parse(Self::parse_fn_def)?),
                NodeKind::Scope => children.push(node.parse(Self::parse_scope)?),
                _ => {
                    return Err(Error::new(node.span(), ErrorKind::UnexpectedNode(node.meta())));
                }
            }
        }
        Ok(Scope {
            uses,
            fn_defs,
            children,
        })
    }
    #[macro_rules_attribute(trace)]
    fn parse_use_stmt(&mut self) -> Result<UseStmt, Error> {
        let mut path = Vec::new();
        while let Some(node) = self.next()? {
            path.push(node.parse_full(NodeKind::Ident, Self::parse_ident)?);
        }
        Ok(UseStmt { path })
    }
    #[macro_rules_attribute(trace)]
    fn parse_fn_def(&mut self) -> Result<FnDef, Error> {
        let name = self.next_or_err()?.parse_full(NodeKind::Ident, Self::parse_ident)?;
        let mut op_def = None;
        let mut bodies = Vec::new();
        while let Some(node) = self.next()? {
            match node.kind() {
                NodeKind::OpDef if op_def.is_none() => op_def = Some(node.parse(Self::parse_op_def)?),
                NodeKind::OpDef => return Err(Error::new(node.span(), ErrorKind::DuplicateOpDef)),
                NodeKind::FnBody => bodies.push(node.parse(Self::parse_fn_body)?),
                _ => {
                    return Err(Error::new(node.span(), ErrorKind::UnexpectedNode(node.meta())));
                }
            }
        }
        Ok(FnDef { name, op_def, bodies })
    }
    #[macro_rules_attribute(trace)]
    fn parse_ident(&mut self) -> Result<Ident, Error> {
        Ok(Ident)
    }
    #[macro_rules_attribute(trace)]
    fn parse_op_def(&mut self) -> Result<OpDef, Error> {
        let parts = self.next_or_err()?.parse_full(NodeKind::OpParts, Self::parse_op_parts)?;
        let bindings = self.next_or_err()?.parse_full(NodeKind::OpBindings, Self::parse_op_bindings)?;
        Ok(OpDef { parts, bindings })
    }
    #[macro_rules_attribute(trace)]
    fn parse_op_parts(&mut self) -> Result<OpParts, Error> {
        let mut parts = Vec::new();
        while let Some(node) = self.next()? {
            match node.kind() {
                NodeKind::Argument => parts.push(node.parse(Self::parse_argument)?),
                NodeKind::LazyArgument => parts.push(node.parse(Self::parse_lazy_argument)?),
                NodeKind::Literal => parts.push(node.parse(Self::parse_literal)?),
                NodeKind::Variadic => parts.push(node.parse(Self::parse_variadic)?),
                _ => {
                    return Err(Error::new(node.span(), ErrorKind::UnexpectedNode(node.meta())));
                }
            }
        }
        Ok(OpParts(parts))
    }
    #[macro_rules_attribute(trace)]
    fn parse_argument(&mut self) -> Result<OpPart, Error> {
        Ok(OpPart::Argument)
    }
    #[macro_rules_attribute(trace)]
    fn parse_lazy_argument(&mut self) -> Result<OpPart, Error> {
        Ok(OpPart::LazyArgument)
    }
    #[macro_rules_attribute(trace)]
    fn parse_literal(&mut self) -> Result<OpPart, Error> {
        Ok(OpPart::Literal)
    }
    #[macro_rules_attribute(trace)]
    fn parse_variadic(&mut self) -> Result<OpPart, Error> {
        self.next_or_err()?.parse_full(NodeKind::OpParts, Self::parse_op_parts).map(OpPart::Variadic)
    }
    #[macro_rules_attribute(trace)]
    fn parse_op_bindings(&mut self) -> Result<OpBindings, Error> {
        let mut bindings = Vec::new();
        while let Some(node) = self.next()? {
            bindings.push(node.parse_full(NodeKind::OpBinding, Self::parse_op_binding)?);
        }
        Ok(OpBindings(bindings))
    }
    #[macro_rules_attribute(trace)]
    fn parse_op_binding(&mut self) -> Result<OpBinding, Error> {
        let lhs = self.next_or_err()?.parse_full(NodeKind::Ident, Self::parse_ident)?;
        let arrow = {
            let node = self.next_or_err()?;
            match node.kind() {
                NodeKind::OpArrowLeft => node.parse(Self::parse_op_arrow_left)?,
                NodeKind::OpArrowRight => node.parse(Self::parse_op_arrow_right)?,
                _ => {
                    return Err(Error::new(node.span(), ErrorKind::UnexpectedNode(node.meta())))?;
                }
            }
        };
        let rhs = self.next_or_err()?.parse_full(NodeKind::Ident, Self::parse_ident)?;
        Ok(OpBinding { lhs, arrow, rhs })
    }
    #[macro_rules_attribute(trace)]
    fn parse_op_arrow_left(&mut self) -> Result<OpArrow, Error> {
        Ok(OpArrow::Left)
    }
    #[macro_rules_attribute(trace)]
    fn parse_op_arrow_right(&mut self) -> Result<OpArrow, Error> {
        Ok(OpArrow::Right)
    }
    #[macro_rules_attribute(trace)]
    fn parse_fn_body(&mut self) -> Result<FnBody, Error> {
        let mut args = Vec::new();
        let body = loop {
            let Some(node) = self.next()? else { break None };
            match node.kind() {
                NodeKind::Ident => args.push(node.parse(Self::parse_ident)?),
                _ => break Some(node),
            }
        };
        let Some(body) = body else {
            return Err(Error::new(
                self.lexer.last_span.empty_after(),
                ErrorKind::DisembodiedFnBody,
            ));
        };
        let body = body.parse_full(NodeKind::Scope, Self::parse_scope)?;
        Ok(FnBody { args, body })
    }

    fn next(&mut self) -> Result<Option<Node<'_, 'source>>, Error> {
        let peeked = self.lexer.peek()?;
        if matches!(peeked.kind, TokenKind::Eof | TokenKind::CloseRoundParen) {
            return Ok(None);
        }
        let kind = self.parse_kind()?;
        let validity = self.parse_validity()?;
        let location = self.parse_location()?;
        let end_span = self.lexer.last_span;
        let contents = self.parse_contents()?;
        Ok(Some(Node {
            meta: NodeMetadata {
                span: peeked.span.around(end_span),
                kind,
                validity,
                location,
            },
            contents,
        }))
    }
    fn next_or_err(&mut self) -> Result<Node<'_, 'source>, Error> {
        let last_span = self.lexer.last_span;
        self.next()?.ok_or_else(|| Error::new(last_span.empty_after(), ErrorKind::UnexpectedEof))
    }
    fn parse_kind(&mut self) -> Result<NodeKind, Error> {
        let tok = self.lexer.next()?;
        Ok(match tok.kind {
            TokenKind::UseStmt => NodeKind::UseStmt,
            TokenKind::Ident => NodeKind::Ident,
            TokenKind::FnDef => NodeKind::FnDef,
            TokenKind::FnBody => NodeKind::FnBody,
            TokenKind::OpDef => NodeKind::OpDef,
            TokenKind::OpPart => NodeKind::OpPart,
            TokenKind::OpParts => NodeKind::OpParts,
            TokenKind::Argument => NodeKind::Argument,
            TokenKind::LazyArgument => NodeKind::LazyArgument,
            TokenKind::Literal => NodeKind::Literal,
            TokenKind::Variadic => NodeKind::Variadic,
            TokenKind::OpBindings => NodeKind::OpBindings,
            TokenKind::OpBinding => NodeKind::OpBinding,
            TokenKind::OpArrow => NodeKind::OpArrow,
            TokenKind::OpArrowLeft => NodeKind::OpArrowLeft,
            TokenKind::OpArrowRight => NodeKind::OpArrowRight,
            TokenKind::Scope => NodeKind::Scope,
            _ => return Err(Error::new(tok.span, ErrorKind::ExpectedNodeKind)),
        })
    }
    fn parse_validity(&mut self) -> Result<Validity, Error> {
        let tok = self.lexer.peek()?;
        Ok(match tok.kind {
            TokenKind::Star => {
                self.lexer.next()?;
                Validity::Recovered
            }
            TokenKind::ExclMark => {
                self.lexer.next()?;
                Validity::Error
            }
            _ => Validity::Valid,
        })
    }
    fn parse_location(&mut self) -> Result<SourceLocation, Error> {
        use {ErrorKind as E, TokenKind as K};
        self.expect(K::OpenSquareParen, E::ExpectedOpenSquareParen)?;
        let start = self.expect_num()?;
        self.expect(K::Comma, E::ExpectedComma)?;
        let len = self.expect_num()?;
        self.expect(K::CloseSquareParen, E::ExpectedCloseSquareParen)?;
        Ok(SourceLocation(Span::new(start, len)))
    }
    fn parse_contents(&mut self) -> Result<Option<&mut NodeStream<'source>>, Error> {
        let tok = self.lexer.peek()?;
        match tok.kind {
            TokenKind::OpenRoundParen => {
                self.lexer.next()?;
                Ok(Some(self))
            }
            _ => Ok(None),
        }
    }
    fn expect(&mut self, expected: TokenKind, err: ErrorKind) -> Result<(), Error> {
        self.expect_any([expected], err)
    }
    fn expect_any<const N: usize>(&mut self, expected: [TokenKind; N], err: ErrorKind) -> Result<(), Error> {
        let tok = self.lexer.next()?;
        if !expected.contains(&tok.kind) {
            return Err(Error::new(tok.span, err));
        };
        Ok(())
    }
    fn expect_num<T: FromStr>(&mut self) -> Result<T, Error> {
        self.expect(TokenKind::Number, ErrorKind::ExpectedNumber)?;
        self.lexer.lexer.slice().parse().map_err(|_| {
            Error::new(
                Span::from_usize_range(self.lexer.lexer.span()),
                ErrorKind::ExpectedNumber,
            )
        })
    }
}
impl<'stream, 'source> Node<'stream, 'source> {
    fn meta(&self) -> NodeMetadata {
        self.meta
    }
    fn kind(&self) -> NodeKind {
        self.meta.kind
    }
    fn span(&self) -> Span {
        self.meta.span
    }
    fn validity(&self) -> Validity {
        self.meta.validity
    }
    fn location(&self) -> SourceLocation {
        self.meta.location
    }
    fn parse<F, R>(self, parser: F) -> Result<Parsed<R>, Error>
    where
        F: FnOnce(&mut NodeStream<'source>) -> Result<R, Error>,
    {
        let contents = match self.contents {
            Some(c) => c,
            None => &mut NodeStream::new(""),
        };
        let parsed = match self.meta.validity {
            Validity::Valid => Parsed::valid(self.meta.location.0, parser(contents)?),
            Validity::Recovered => Parsed::recovered(self.meta.location.0, parser(contents)?),
            Validity::Error => Parsed::error(self.meta.location.0, self.meta.kind),
        };
        use TokenKind as K;
        contents.expect_any([K::CloseRoundParen, K::Eof], ErrorKind::UnclosedNode)?;
        Ok(parsed)
    }
    fn parse_full<F, R>(self, kind: NodeKind, parser: F) -> Result<Parsed<R>, Error>
    where
        F: FnOnce(&mut NodeStream<'source>) -> Result<R, Error>,
    {
        if self.kind() != kind {
            return Err(Error::new(self.span(), ErrorKind::UnexpectedNode(self.meta())));
        }
        self.parse(parser)
    }
    fn parse_error<K>(self, kind: NodeKind) -> Result<Parsed<K>, Error> {
        if self.validity() != Validity::Error {
            return Err(Error::new(self.span(), ErrorKind::ExpectedError));
        }
        Ok(Parsed::error(self.location().0, kind))
    }
}

struct Lexer<'source> {
    lexer: logos::Lexer<'source, TokenKind>,
    peeked: Option<Result<Token, Error>>,
    last_span: Span,
}
impl<'source> Lexer<'source> {
    fn new(source: &'source str) -> Self {
        Self {
            lexer: TokenKind::lexer(source),
            peeked: None,
            last_span: Span::new(0, 0),
        }
    }
    fn next(&mut self) -> Result<Token, Error> {
        let tok = self.peeked.take().unwrap_or_else(|| Self::raw_next(&mut self.lexer))?;
        self.last_span = tok.span;
        Ok(tok)
    }
    fn peek(&mut self) -> Result<Token, Error> {
        *self.peeked.get_or_insert_with(|| Self::raw_next(&mut self.lexer))
    }
    fn raw_next(lexer: &mut logos::Lexer<'source, TokenKind>) -> Result<Token, Error> {
        let kind = lexer.next();
        let span = Span::from_usize_range(lexer.span());
        match kind {
            Some(Ok(kind)) => Ok(Token { kind, span }),
            Some(Err(())) => Err(Error::new(span, ErrorKind::LexError)),
            None => Ok(Token {
                kind: TokenKind::Eof,
                span,
            }),
        }
    }
}
impl Debug for NodeMetadata {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let validity = match self.validity {
            Validity::Valid => "",
            Validity::Recovered => "*",
            Validity::Error => "!",
        };
        write!(
            f,
            "{:?}{}{:?} (@ {:?})",
            self.kind, validity, self.location.0, self.span
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Token {
    kind: TokenKind,
    span: Span,
}
#[derive(Debug, PartialEq, Eq, Clone, Copy, Logos)]
#[logos(skip r"[ \s\t\r\n]+")]
enum TokenKind {
    #[token("UseStmt")]
    UseStmt,
    #[token("Ident")]
    Ident,
    #[token("FnDef")]
    FnDef,
    #[token("FnBody")]
    FnBody,
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
    #[token("OpArrow")]
    OpArrow,
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

    Eof,
}
#[derive(Debug, Clone, Copy)]
#[expect(dead_code, reason = "Only used in the debug impl or in tests")]
pub struct Error {
    kind: ErrorKind,
    pub span: Span,
}
#[derive(Debug, Clone, Copy)]
enum ErrorKind {
    LexError,
    UnexpectedEof,
    ExpectedNodeKind,
    ExpectedOpenSquareParen,
    ExpectedNumber,
    ExpectedComma,
    ExpectedCloseSquareParen,
    #[expect(dead_code, reason = "Only used in the debug impl")]
    UnexpectedNode(NodeMetadata),
    UnclosedNode,
    DuplicateOpDef,
    DisembodiedFnBody,
    ExpectedError,
}
impl Error {
    fn new(span: Span, kind: ErrorKind) -> Self {
        Self { kind, span }
    }
}

macro_rules! trace {
    (fn $name:ident($($args:tt)*) -> $ret:ty {$($body:tt)*}) => {
        fn $name($($args)*) -> $ret {
            enter(concat!("enter ", stringify!($name)));
            let out = { $($body)* }?;
            exit(concat!("exit ", stringify!($name)));
            Ok(out)
        }
    }
}
use trace;

thread_local! {
    static DEPTH: Cell<u32> = const { Cell::new(0) };
}
fn enter(name: &str) {
    debug(name);
    DEPTH.set(DEPTH.get() + 1);
}
fn exit(name: &str) {
    DEPTH.set(DEPTH.get() - 1);
    debug(name);
}
fn debug<T: Display>(msg: T) {
    for _ in 0..DEPTH.get() {
        print!("    ")
    }
    println!("{msg}")
}
