use crate::scope_tree::{
    ast::{
        Ident, NodeKind, OpArrow, OpBinding, OpBindings, OpDef, OpPart, OpParts, Outcome, Parsed,
        Scope, UseStmt,
    },
    lex::{Lexer, Token, TokenKind},
    span::Span,
};
use bytemuck::TransparentWrapper;
use ecto_macros::select;
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
    let out = parser.parse_top_level();
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

impl<'source, M: NewlineHandler> Parser<'source, M> {
    fn parse_top_level(&mut self) -> Parsed<Scope> {
        enter("parse_top_level");
        let scope = self.parse_scope_contents();
        select! { self, |tok, expected|
            next: TokenKind::Eof => (),
            next: _ => self.error(Error::new(tok.span, ErrorKind::UnexpectedToken { expected, got: tok.kind })),
        };
        exit("parse_top_level");
        scope
    }
    fn parse_scope_contents(&mut self) -> Parsed<Scope> {
        enter("parse_scope_contents");
        let out = self.spanning(|parser| {
            let mut uses = Vec::new();
            let mut op_defs = Vec::new();
            let mut children = Vec::new();
            loop {
                select! { parser, |tok, expected|
                    peek: TokenKind::Use => uses.push(parser.parse_use()),
                    next: TokenKind::Fn => if let Some(op_def) = parser.parse_fn_for_op() {
                        op_defs.push(op_def)
                    },
                    peek: TokenKind::OpenParen => children.push(parser.parse_scope()),
                    peek: TokenKind::CloseParen | TokenKind::Eof => break,
                    // RECOVERY: Check for misspellings of `fn`, like `function` or `func` or `def` or `defun`
                    // RECOVERY: Check for other kinds of parentheses, like `{}`
                    next: _ => parser.error(Error::new(tok.span, ErrorKind::UnexpectedToken { expected, got: tok.kind })),
                }
            }
            Outcome::Valid(Scope {
                uses,
                op_defs,
                children,
            })
        });
        exit("parse_scope_contents");
        out
    }
    fn parse_use(&mut self) -> Parsed<UseStmt> {
        enter("parse_use");
        let out = self.spanning(|parser| {
            parser.next();
            let mut path = Vec::new();
            path.push(parser.parse_ident());
            loop {
                select! { parser, |tok, expected|
                    next: TokenKind::Dot => (),
                    next: TokenKind::Comma => break,
                    next: _ => parser.error(Error::new(tok.span, ErrorKind::UnexpectedToken { expected, got: tok.kind })),
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
        let out = select! { self, |tok, expected|
            next: TokenKind::Ident => Parsed::valid(tok.span, Ident),
            // RECOVERY: also accept paths made of literals
            next: _ => {
                self.error(Error::new(tok.span, ErrorKind::UnexpectedToken { expected, got: tok.kind }));
                Parsed::error(tok.span, NodeKind::Ident)
            },
        };
        exit("parse_ident");
        out
    }
    fn parse_fn_for_op(&mut self) -> Option<Parsed<OpDef>> {
        enter("parse_fn_for_op");
        self.parse_ident();
        select! { self, |tok, expected|
            next: TokenKind::OpenParen => (),
            next: _ => self.error(Error::new(tok.span, ErrorKind::UnexpectedToken { expected, got: tok.kind })),
        };
        let mut op_def = None;
        loop {
            select! { self, |tok, expected|
                peek: TokenKind::Op => {
                    let od = self.parse_op_def();
                    match op_def {
                        Some(_) => self.error(Error::new(od.span, ErrorKind::DuplicateOpDef)),
                        None => op_def = Some(od),
                    }
                },
                next: TokenKind::Colon => self.skip_fn_arm(),
                next: TokenKind::Equals => self.skip_fn_arm(),
                next: TokenKind::CloseParen => break,
                next: _ => self.error(Error::new(tok.span, ErrorKind::UnexpectedToken { expected, got: tok.kind })),
            }
        }
        exit("parse_fn_for_op");
        op_def
    }
    fn parse_op_def(&mut self) -> Parsed<OpDef> {
        enter("parse_op_def");
        let out = self.spanning(|parser| {
            parser.next();
            let parts = parser.parse_op_parts();
            let bindings = select! {
                parser, |tok, _|
                next: TokenKind::Semicolon => parser.parse_op_bindings(),
                peek: _ => Parsed::valid(parser.last_span().empty_after(), OpBindings(Vec::new()))
            };
            Outcome::Valid(OpDef { parts, bindings })
        });
        exit("parse_op_def");
        out
    }
    fn parse_op_parts(&mut self) -> Parsed<OpParts> {
        enter("parse_op_parts");
        let out = self.spanning(|parser| {
            let mut parts = Vec::new();
            loop {
                select! { parser, |tok, expected|
                    next: TokenKind::Underscore => parts.push(Parsed::valid(tok.span, OpPart::Argument)),
                    next: TokenKind::BackslashUnderscore => parts.push(Parsed::valid(tok.span, OpPart::LazyArgument)),
                    next: TokenKind::Literal => parts.push(Parsed::valid(tok.span, OpPart::Literal)),
                    next: TokenKind::OpenParen => parts.push(parser.parse_bracketed_variadics()),
                    next: TokenKind::Star => {
                        let var = parser.parse_unbracketed_variadics(&mut parts, tok.span);
                        parts.push(var)
                    },
                    peek: TokenKind::Semicolon | TokenKind::Op | TokenKind::Colon | TokenKind::Equals | TokenKind::CloseParen => break,
                    next: _ => parser.error(Error::new(tok.span, ErrorKind::UnexpectedToken { expected, got: tok.kind })),
                }
            }
            Outcome::Valid(OpParts(parts))
        });
        exit("parse_op_parts");
        out
    }
    fn parse_bracketed_variadics(&mut self) -> Parsed<OpPart> {
        self.spanning(|parser| {
            let contents = parser.parse_op_parts();
            select! { parser, |tok, _|
                next: TokenKind::Star => Outcome::Valid(OpPart::Variadic(contents)),
                next: _ => {
                    parser.error(Error::new(tok.span, ErrorKind::MissingStarInVariadics));
                    Outcome::Recovered(OpPart::Variadic(contents))
                },
            }
        })
    }
    fn parse_unbracketed_variadics(
        &mut self,
        parts: &mut Vec<Parsed<OpPart>>,
        star_span: Span,
    ) -> Parsed<OpPart> {
        match parts.pop() {
            Some(last) => Parsed::valid(
                last.span.around(star_span),
                OpPart::Variadic(Parsed::valid(last.span, OpParts(vec![last]))),
            ),
            None => {
                let prev_span = star_span.empty_before();
                self.error(Error::new(star_span, ErrorKind::RepetitionOfNothing));
                Parsed::valid(
                    star_span,
                    OpPart::Variadic(Parsed::error(prev_span, NodeKind::OpParts)),
                )
            }
        }
    }
    fn parse_op_bindings(&mut self) -> Parsed<OpBindings> {
        enter("parse_op_bindings");
        let out = self.spanning(|parser| {
            let mut bindings = Vec::new();
            loop {
                select! { parser, |tok, _|
                    peek: TokenKind::Op | TokenKind::Colon | TokenKind::Equals | TokenKind::CloseParen => break,
                    next: TokenKind::Comma => continue,
                    peek: _ => bindings.push(parser.parse_op_binding()),
                }
            }
            Outcome::Valid(OpBindings(bindings))
        });
        exit("parse_op_bindings");
        out
    }
    fn parse_op_binding(&mut self) -> Parsed<OpBinding> {
        self.spanning(|parser| {
            let lhs = parser.parse_ident();
            let arrow = select! { parser, |tok, expected|
                next: TokenKind::LeftArrow => Parsed::valid(tok.span, OpArrow::Left),
                next: TokenKind::RightArrow => Parsed::valid(tok.span, OpArrow::Right),
                peek: TokenKind::Ident => {
                    parser.error(Error::new(tok.span, ErrorKind::MissingArrow));
                    Parsed::error(tok.span, NodeKind::OpArrow)
                },
                // RECOVERY allow the less-than and greater-than operators without arrow shafts
                // ERROR_QUALITY recognise the equals operator. Careful about it not being the equals of a different fn arm.
                next: _ => {
                    parser.error(Error::new(tok.span, ErrorKind::UnexpectedToken { expected, got: tok.kind }));
                    Parsed::error(tok.span, NodeKind::OpArrow)
                }
            };
            let rhs = parser.parse_ident();
            Outcome::Valid(OpBinding { lhs, arrow, rhs })
        })
    }
    fn skip_fn_arm(&mut self) {
        enter("skip_fn_arm");
        use TokenKind as K;
        let this = self.cast::<KeepNewlines>();
        let mut depth = 1;
        loop {
            debug(format_args!(
                "skip_fn_arms: depth: {depth}, remainder: {:?}",
                this.0.lexed
            ));
            select! {
                this, |tok, _|
                next: K::Newline => select!{ this, |tok, _|
                    peek: K::Op | K::Colon | K::Equals => (),
                    peek: _ if depth == 1 => break,
                    peek: _ => (),
                },
                next: K::OpenParen => depth += 1,
                peek: K::CloseParen if depth == 0 => break,
                next: K::CloseParen => depth -= 1,
                next: _ => ()
            }
        }
        exit("skip_fn_arm");
    }
    fn parse_scope(&mut self) -> Parsed<Scope> {
        enter("parse_scope");
        let out = self.spanning(|parser| {
            parser.next();
            let scope = parser.parse_scope_contents();
            select! { parser, |tok, expected|
                next: TokenKind::CloseParen => scope.outcome,
                // RECOVERY: Check for other kinds of close paren
                // RECOVERY: Reparse in an indentation-sensitive manner, to try to spot the matching paren
                next: _ => {
                    parser.error(Error::new(tok.span, ErrorKind::UnexpectedToken { expected, got: tok.kind }));
                    scope.outcome.valid_into_recovered()
                },
            }
        });
        exit("parse_scope");
        out
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
    fn peek(&self) -> Token {
        let mut iter = self.0.lexed.iter().copied();
        let tok = match M::SKIP {
            true => iter.find(|t| t.kind != TokenKind::Newline),
            false => iter.next(),
        }
        .unwrap_or_else(|| self.eof());
        debug(format_args!("peek: {tok:?}"));
        tok
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
        debug(format_args!("next: {tok:?}"));
        tok
    }
}
impl<'source, M> Parser<'source, M> {
    fn new(core: ParserCore<'source>) -> Self {
        Parser_(core, PhantomData)
    }
    fn cast<M2>(&mut self) -> &mut Parser<'source, M2> {
        Parser::wrap_mut(Parser::peel_mut(self))
    }
    fn error(&mut self, error: Error) {
        debug(format_args!("error: {error:?}"));
        self.0.errors.push(error)
    }
    fn eof(&self) -> Token {
        Token {
            kind: TokenKind::Eof,
            span: self.0.last_span.empty_after(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}
#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    UnexpectedToken {
        expected: &'static [TokenKind],
        got: TokenKind,
    },
    DuplicateOpDef,
    MissingStarInVariadics,
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

fn kind(t: Token) -> TokenKind {
    t.kind
}
fn unexpected_token<'source, M>(
    parser: &mut Parser<'source, M>,
    expected: &'static [TokenKind],
    got: Token,
) {
    parser.error(Error::new(
        got.span,
        ErrorKind::UnexpectedToken {
            expected,
            got: got.kind,
        },
    ))
}
