use crate::scope_tree::{
    ast::{
        Ident, OpArrow, OpBinding, OpBindings, OpDef, OpPart, OpParts, Outcome, Parsed, Scope,
        UseStmt,
    },
    lex::{Lexer, Token, TokenKind},
    Span,
};
use bytemuck::TransparentWrapper;
use std::{
    marker::PhantomData,
    sync::atomic::{AtomicU32, Ordering},
};

pub fn parse(source: &str) -> (Parsed<Scope>, Vec<Error>) {
    let tokens = Lexer::new(source).collect::<Vec<_>>();
    let mut parser = Parser::<SkipNewlines>::new(ParserCore {
        lexed: &tokens,
        last_span: Span::new(0, 0),
        errors: Vec::new(),
    });
    let out = parser.parse_scope_contents();
    (out, parser.0.errors)
}

struct ParserCore<'source> {
    lexed: &'source [Token],
    last_span: Span,
    errors: Vec<Error>,
}

#[derive(TransparentWrapper)]
#[transparent(Core)]
#[repr(transparent)]
struct Parser_<Core, M>(Core, PhantomData<M>);
type Parser<'source, M> = Parser_<ParserCore<'source>, M>;

impl<'source, M> Parser<'source, M> {
    fn new(core: ParserCore<'source>) -> Self {
        Parser_(core, PhantomData)
    }
    fn cast<M2>(&mut self) -> &mut Parser<'source, M2> {
        Parser::wrap_mut(Parser::peel_mut(self))
    }
}
impl<'source, M: NewlineHandler> Parser<'source, M> {
    fn parse_top_level(&mut self) -> Parsed<Scope> {
        todo!()
    }
    fn parse_scope_contents(&mut self) -> Parsed<Scope> {
        self.spanning(|parser| {
            let mut uses = Vec::new();
            let mut op_defs = Vec::new();
            let mut children = Vec::new();
            loop {
                select! {
                    parser, |tok|
                    peek TokenKind::Use => uses.push(parser.parse_use()),
                    next TokenKind::Fn => if let Some(op_def) = parser.parse_fn_for_op() {
                        op_defs.push(op_def)
                    },
                    peek TokenKind::OpenParen => children.push(parser.parse_scope()),
                    peek TokenKind::CloseParen | TokenKind::Eof => break,
                    // RECOVERY: Check for misspellings of `fn`, like `function` or `func` or `def` or `defun`
                    // RECOVERY: Check for other kinds of parentheses, like `{}`
                    next else => (),
                }
            }
            Outcome::Valid(Scope {
                uses,
                op_defs,
                children,
            })
        })
    }
    fn parse_use(&mut self) -> Parsed<UseStmt> {
        enter("parse_use");
        let out = self.spanning(|parser| {
            parser.next();
            let mut path = Vec::new();
            path.push(parser.parse_ident());
            loop {
                select! {
                    parser, |tok|
                    next TokenKind::Dot => (),
                    next TokenKind::Comma => break,
                    next else => (),
                }
                path.push(parser.parse_ident())
            }
            // RECOVERY: Check if there's more idents-and-commas here. If so, someone tried to use a comma as a separator
            Outcome::Valid(UseStmt { path })
        });
        exit("parse_use");
        out
    }
    fn parse_ident(&mut self) -> Parsed<Ident> {
        enter("parse_ident");
        let out = select! { self, |tok|
            next TokenKind::Ident => Parsed::valid(tok.span, Ident),
            next else => Parsed::error(tok.span),
        };
        exit("parse_ident");
        out
    }
    fn parse_fn_for_op(&mut self) -> Option<Parsed<OpDef>> {
        self.parse_ident();
        select! { self, |tok|
            next TokenKind::OpenParen => (),
            next else => (),
        };
        let mut op_def = None;
        loop {
            select! { self, |tok|
                peek TokenKind::Op => {
                    let od = self.parse_op_def();
                    match op_def {
                        Some(_) => self.error(Error::new(od.span, ErrorKind::DuplicateOpDef)),
                        None => op_def = Some(od),
                    }
                },
                next TokenKind::Colon => self.skip_fn_arm(),
                next TokenKind::Equals => self.skip_fn_arm(),
                next TokenKind::CloseParen => break,
                next else => (),
            }
        }
        op_def
    }
    fn parse_op_def(&mut self) -> Parsed<OpDef> {
        self.spanning(|parser| {
            parser.next();
            let parts = parser.parse_op_parts();
            let bindings = select! {
                parser, try |tok|
                next TokenKind::Semicolon => parser.parse_op_bindings(),
                peek else => Parsed::valid(parser.last_span().empty_after(), OpBindings(Vec::new()))
            };
            Outcome::Valid(OpDef { parts, bindings })
        })
    }
    fn parse_op_parts(&mut self) -> Parsed<OpParts> {
        self.spanning(|parser| {
            let mut parts = Vec::new();
            loop {
                select! { parser, |tok|
                    next TokenKind::Underscore => parts.push(Parsed::valid(tok.span, OpPart::Argument)),
                    next TokenKind::BackslashUnderscore => parts.push(Parsed::valid(tok.span, OpPart::LazyArgument)),
                    next TokenKind::Literal => parts.push(Parsed::valid(tok.span, OpPart::Literal)),
                    next TokenKind::OpenParen => parts.push(parser.spanning(|parser| {
                        let contents = parser.parse_op_parts();
                        select! { parser, |tok|
                            next TokenKind::Star => Outcome::Valid(OpPart::Variadic(contents)),
                            next else => Outcome::Recovered(OpPart::Variadic(contents)),
                        }
                    })),
                    next TokenKind::Star => match parts.pop() {
                        Some(last) => parts.push(Parsed::valid(last.span, OpPart::Variadic(Parsed::valid(last.span, OpParts(vec![last]))))),
                        None => parser.error(Error::new(tok.span, ErrorKind::RepetitionOfNothing)),
                    },
                    peek TokenKind::Semicolon | TokenKind::Op | TokenKind::Colon | TokenKind::Equals | TokenKind::CloseParen => break,
                    next else => (),
                }
            }
            Outcome::Valid(OpParts(parts))
        })
    }
    fn parse_op_bindings(&mut self) -> Parsed<OpBindings> {
        self.spanning(|parser| {
            let mut bindings = Vec::new();
            loop {
                let tok = parser.peek();
                match tok.kind {
                    TokenKind::Op
                    | TokenKind::Colon
                    | TokenKind::Equals
                    | TokenKind::CloseParen => break,
                    TokenKind::Comma => {
                        parser.next();
                        parser.error(Error::new(tok.span, ErrorKind::MissingBindingDecl));
                        bindings.push(Parsed::error(tok.span))
                    }
                    _ => bindings.push(parser.parse_op_binding()),
                }
            }
            Outcome::Valid(OpBindings(bindings))
        })
    }
    fn parse_op_binding(&mut self) -> Parsed<OpBinding> {
        self.spanning(|parser| {
            let lhs = parser.parse_ident();
            let arrow = select! {parser, |tok|
                next TokenKind::LeftArrow => Parsed::valid(tok.span, OpArrow::Left),
                next TokenKind::RightArrow => Parsed::valid(tok.span, OpArrow::Right),
                peek TokenKind::Ident => {
                    parser.error(Error::new(tok.span, ErrorKind::MissingArrow));
                    Parsed::error(tok.span)
                },
                // RECOVERY allow the less-than and greater-than operators without arrow shafts
                // ERROR_QUALITY recognise the equals operator. Careful about it not being the equals of a different fn arm.
                next else => Parsed::error(tok.span)
            };
            let rhs = parser.parse_ident();
            Outcome::Valid(OpBinding { lhs, arrow, rhs })
        })
    }
    fn skip_fn_arm(&mut self) {
        use TokenKind as K;
        let this = self.cast::<KeepNewlines>();
        loop {
            let tok = this.next();
            match tok.kind {
                K::Newline => {
                    this.next();
                    match this.peek().kind {
                        K::Op | K::Colon | K::Equals => break,
                        _ => (),
                    }
                }
                K::CloseParen => break,
                _ => (),
            }
        }
    }
    fn parse_scope(&mut self) -> Parsed<Scope> {
        self.spanning(|parser| {
            parser.next();
            let scope = parser.parse_scope_contents();
            select! { parser, |tok|
                next TokenKind::CloseParen => scope.outcome,
                // RECOVERY: Check for other kinds of close paren
                // RECOVERY: Reparse in an indentation-sensitive manner, to try to spot the matching paren
                next else => scope.outcome.valid_into_recovered(),
            }
        })
    }

    fn spanning<F, R>(&mut self, f: F) -> Parsed<R>
    where
        F: FnOnce(&mut Self) -> Outcome<R>,
    {
        let first_span = self.peek().span;
        let outcome = f(self);
        let last_span = self.0.last_span;
        Parsed {
            outcome,
            span: first_span.around(last_span),
        }
    }
    fn last_span(&self) -> Span {
        self.0.last_span
    }
    fn error(&mut self, error: Error) {
        self.0.errors.push(error)
    }
    fn peek(&self) -> Token {
        let mut iter = self.0.lexed.iter().copied();
        match M::SKIP {
            true => iter.find(|t| t.kind != TokenKind::Newline),
            false => iter.next(),
        }
        .unwrap_or_else(|| self.eof())
    }
    fn next(&mut self) -> Token {
        let mut tokens = self.0.lexed.iter();
        let tok = loop {
            let tok = tokens.next().copied().unwrap_or_else(|| self.eof());
            if M::SKIP && tok.kind == TokenKind::Newline {
                continue;
            }
            break tok;
        };
        self.0.lexed = tokens.as_slice();
        self.0.last_span = tok.span;
        tok
    }
    fn eof(&self) -> Token {
        Token {
            kind: TokenKind::Eof,
            span: self.0.last_span.empty_after(),
        }
    }
}

macro_rules! select {
    ($parser:ident, $($(@$try:tt)? try)? |$tok:ident| $( $mode:ident $(|)? $($($kind:ident)::*)|* => $body:expr),* $(,)*) => {{
        let $tok = $parser.peek();
        select! { @split-last |$parser, $tok, $($($try)? try)?| [] $( { $mode $($($kind)::*)|* => $body } )* }
    }};
    (@split-last |$parser:ident, $tok:ident, $($(@$try:tt)? try)?| [$( {$($arm:tt)*} )*] {$($first:tt)*} $( {$($rest:tt)*})+) => {
        select! { @split-last |$parser, $tok, $($($try)? try)?| [$( {$($arm)*} )* {$($first)*}] $( {$($rest)*} )+ }
    };
    (@split-last |$parser:ident, $tok:ident, $($(@$try:tt)? try)?| [$( { $mode:ident $($($kind:ident)::*)|* => $body:expr } )*] { $last_mode:ident else => $last_body:expr }) => {
        match $tok.kind {
            $(
                $($($kind)::*)|* => {
                    select! { @mode $mode {$parser.next()} {} };
                    $body
                }
            )*
            _ => {
                select! { @mode $last_mode {$parser.next()} {} };
                select! { @try $($($try)? try)?
                     { $parser.error(Error::new($tok.span, ErrorKind::UnexpectedToken { expected: &[$( $($($kind)::*,)* )*], got: $tok.kind })); }
                };
                $last_body
            }

        }
    };
    (@mode next {$($next:tt)*} {$($peek:tt)*}) => {
        $($next)*
    };
    (@mode peek {$($next:tt)*} {$($peek:tt)*}) => {
        $($peek)*
    };
    (@try {$($fail:tt)*}) => {
        $($fail)*
    };
    (@try try {$($fail:tt)*}) => {};
}
use select;

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    kind: ErrorKind,
    span: Span,
}
#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    UnexpectedToken {
        expected: &'static [TokenKind],
        got: TokenKind,
    },
    DuplicateOpDef,
    RepetitionOfNothing,
    MissingBindingDecl,
    MissingArrow,
}
impl Error {
    fn new(span: Span, kind: ErrorKind) -> Self {
        Self { kind, span }
    }
}

trait NewlineHandler {
    const SKIP: bool;
}
struct SkipNewlines;
impl NewlineHandler for SkipNewlines {
    const SKIP: bool = true;
}
struct KeepNewlines;
impl NewlineHandler for KeepNewlines {
    const SKIP: bool = false;
}

static DEPTH: AtomicU32 = AtomicU32::new(0);
fn enter(msg: &str) {
    debug(format_args!("enter {msg}"));
    DEPTH.fetch_add(1, Ordering::Relaxed);
}
fn exit(msg: &str) {
    DEPTH.fetch_sub(1, Ordering::Relaxed);
    debug(format_args!("exit {msg}"));
}
fn debug<D: std::fmt::Display>(msg: D) {
    for _ in 0..DEPTH.load(Ordering::Relaxed) {
        print!("    ");
    }
    println!("{msg}")
}
