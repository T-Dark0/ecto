use crate::{
    parsed::Span,
    scope_tree::{
        ast::{
            FnBody, FnDef, Ident, Item, NodeKind, OpArrow, OpBinding, OpBindings, OpDef, OpPart, OpParts, Outcome,
            Parsed, Scope, ScopeElement, UseStmt,
        },
        lex::{Lexer, Token, TokenKind},
    },
};
use bytemuck::TransparentWrapper;
use ecto_macros::select;
use macro_rules_attribute::macro_rules_attribute;
use std::{any::type_name, cell::Cell, marker::PhantomData};

pub fn parse(source: &str) -> (Parsed<Scope>, Vec<Error>) {
    let tokens = Lexer::new(source).collect::<Vec<_>>();
    let mut parser = Parser::<SkipNewlines>::new(ParserCore {
        lexed: &tokens,
        errors: Vec::new(),
        last_span: Span::new(0, 0),
    });
    let out = parser.parse_open_scope();
    (out, parser.0.errors)
}

type Parser<'source, H> = Parser_<ParserCore<'source>, H>;
#[derive(TransparentWrapper)]
#[transparent(Core)]
#[repr(transparent)]
struct Parser_<Core, H>(Core, PhantomData<H>);
struct ParserCore<'source> {
    lexed: &'source [Token],
    errors: Vec<Error>,
    last_span: Span,
}
impl<'source, H: TokenHandler> Parser<'source, H> {
    #[macro_rules_attribute(trace)]
    fn parse_delimited_scope(&mut self) -> Parsed<Scope> {
        self.spanning(|parser| {
            parser.next();
            let scope = parser.cast::<Parenthesised>().raw_parse_scope();
            select! { parser.cast::<SkipNewlines>(), |tok, expected|
                next: TokenKind::CloseParen => scope,
                next: _ => {
                    parser.unexpected(expected, tok);
                    scope.valid_into_recovered()
                },
            }
        })
    }
    #[macro_rules_attribute(trace)]
    fn parse_open_scope(&mut self) -> Parsed<Scope> {
        self.spanning(|parser| parser.raw_parse_scope())
    }
    fn raw_parse_scope(&mut self) -> Outcome<Scope> {
        let mut contents = Vec::new();
        loop {
            select! { self, |tok, _|
                peek: TokenKind::Use => contents.push(ScopeElement::Item(self.parse_use().map(Item::UseStmt))),
                peek: TokenKind::Fn => contents.push(ScopeElement::Item(self.parse_fn_def().map(Item::FnDef))),
                peek: TokenKind::OpenParen => contents.push(ScopeElement::Child(self.parse_delimited_scope())),
                next: TokenKind::Eof => break,
                // RECOVERY: Check for misspellings of `fn`, like `function` or `func` or `def` or `defun`
                // RECOVERY: Check for other kinds of parentheses, like `{}`
                next: _ => contents.push(ScopeElement::Atom(tok)),
            }
        }
        Outcome::Valid(Scope(contents))
    }
    #[macro_rules_attribute(trace)]
    fn parse_use(&mut self) -> Parsed<UseStmt> {
        self.spanning(|parser| {
            parser.next();
            let mut path = Vec::new();
            path.push(parser.parse_ident());
            loop {
                select! { parser, |tok, expected|
                    next: TokenKind::Dot => (),
                    next: TokenKind::Comma => break Outcome::Valid(UseStmt { path }),
                    next try: TokenKind::Eof => {
                        parser.unexpected(expected, tok);
                        break Outcome::Recovered(UseStmt { path })
                    },
                    next: _ => parser.unexpected(expected, tok),
                }
                path.push(parser.parse_ident());
            }
            // RECOVERY: Check if there's more idents-and-commas here. If so, someone tried to use a comma as a separator
        })
    }
    #[macro_rules_attribute(trace)]
    fn parse_ident(&mut self) -> Parsed<Ident> {
        let ident = self.next();
        match ident.kind {
            TokenKind::Ident => Parsed::valid(ident.span, Ident),
            _ if resembles_ident(ident.kind) => {
                self.unexpected(&[TokenKind::Ident], ident);
                Parsed::recovered(ident.span, Ident)
            }
            _ => {
                self.unexpected(&[TokenKind::Ident], ident);
                Parsed::error(ident.span, NodeKind::Ident)
            }
        }
    }
    #[macro_rules_attribute(trace)]
    fn parse_fn_def(&mut self) -> Parsed<FnDef> {
        self.cast::<SkipNewlines>().spanning(|parser| {
            parser.next();
            let name = parser.parse_ident();
            parser.expect(TokenKind::OpenParen);
            let mut op_def = None;
            let mut bodies = Vec::new();
            loop {
                select! { parser, |tok, expected|
                    peek: TokenKind::Op => {
                        let od = parser.parse_op_def();
                        match op_def {
                            None => op_def = Some(od),
                            Some(_) => parser.error(Error::new(od.span, ErrorKind::DuplicateOpDef)),
                        }
                    },
                    next: TokenKind::Colon => parser.skip_fn_arm(),
                    peek: TokenKind::Equals => bodies.push(parser.parse_fn_body()),
                    next: TokenKind::CloseParen => break,
                    next try: TokenKind::Eof => {
                        parser.unexpected(expected, tok);
                        break
                    },
                    next: _ => parser.unexpected(expected, tok),
                }
            }
            Outcome::Valid(FnDef { name, op_def, bodies })
        })
    }
    #[macro_rules_attribute(trace)]
    fn parse_op_def(&mut self) -> Parsed<OpDef> {
        self.spanning(|parser| {
            parser.next();
            let parser = parser.cast::<FnArmTopLevel>();
            let parts = parser.parse_op_parts();
            let bindings = select! {
                parser, |tok, _|
                next: TokenKind::Semicolon => parser.parse_op_bindings(),
                peek: _ => Parsed::valid(parser.last_span().empty_after(), OpBindings(Vec::new()))
            };
            Outcome::Valid(OpDef { parts, bindings })
        })
    }
    #[macro_rules_attribute(trace)]
    fn parse_op_parts(&mut self) -> Parsed<OpParts> {
        self.cast::<FnArmTopLevel>().spanning(|parser| {
            let mut parts = Vec::new();
            loop {
                select! { parser, |tok, expected|
                    next: TokenKind::Underscore => parts.push(Parsed::valid(tok.span, OpPart::Argument)),
                    next: TokenKind::BackslashUnderscore => parts.push(Parsed::valid(tok.span, OpPart::LazyArgument)),
                    next: TokenKind::Literal => parts.push(Parsed::valid(tok.span, OpPart::Literal)),
                    peek: TokenKind::OpenParen => parts.push(parser.parse_bracketed_variadics()),
                    next: TokenKind::Star => {
                        let var = parser.parse_unbracketed_variadics(&mut parts, tok.span);
                        parts.push(var)
                    },
                    peek: TokenKind::Semicolon | TokenKind::Eof => break,
                    next: _ => parser.unexpected(expected, tok),
                }
            }
            Outcome::Valid(OpParts(parts))
        })
    }
    #[macro_rules_attribute(trace)]
    fn parse_bracketed_variadics(&mut self) -> Parsed<OpPart> {
        self.spanning(|parser| {
            parser.next();
            let parser = parser.cast::<Parenthesised>();
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
    #[macro_rules_attribute(trace)]
    fn parse_unbracketed_variadics(&mut self, parts: &mut Vec<Parsed<OpPart>>, star_span: Span) -> Parsed<OpPart> {
        match parts.pop() {
            Some(last) => Parsed::valid(
                last.span.around(star_span),
                OpPart::Variadic(Parsed::valid(last.span, OpParts(vec![last]))),
            ),
            None => {
                let prev_span = star_span.empty_before();
                self.error(Error::new(star_span, ErrorKind::RepetitionOfNothing));
                Parsed::valid(star_span, OpPart::Variadic(Parsed::error(prev_span, NodeKind::OpParts)))
            }
        }
    }
    #[macro_rules_attribute(trace)]
    fn parse_op_bindings(&mut self) -> Parsed<OpBindings> {
        self.cast::<FnArmTopLevel>().spanning(|parser| {
            let mut bindings = Vec::new();
            loop {
                select! { parser, |tok, _|
                    next: TokenKind::Comma => (),
                    next: TokenKind::Eof => break,
                    peek: _ => bindings.push(parser.parse_op_binding()),
                }
            }
            Outcome::Valid(OpBindings(bindings))
        })
    }
    #[macro_rules_attribute(trace)]
    fn parse_op_binding(&mut self) -> Parsed<OpBinding> {
        self.cast::<FnArmTopLevel>().spanning(|parser| {
            let lhs = parser.parse_ident();
            let arrow = select! { parser, |tok, expected|
                next: TokenKind::LeftArrow => Parsed::valid(tok.span, OpArrow::Left),
                next: TokenKind::RightArrow => Parsed::valid(tok.span, OpArrow::Right),
                peek try: TokenKind::Ident => {
                    parser.error(Error::new(tok.span, ErrorKind::MissingArrow));
                    Parsed::error(tok.span, NodeKind::OpArrow)
                },
                // RECOVERY allow the less-than and greater-than operators without arrow shafts
                // ERROR_QUALITY recognise the equals operator. Careful about it not being the equals of a different fn arm.
                next: _ => {
                    parser.unexpected(expected, tok);
                    Parsed::error(tok.span, NodeKind::OpArrow)
                }
            };
            let rhs = parser.parse_ident();
            Outcome::Valid(OpBinding { lhs, arrow, rhs })
        })
    }
    #[macro_rules_attribute(trace)]
    fn skip_fn_arm(&mut self) {
        let parser = self.cast::<FnArmTopLevel>();
        loop {
            select! { parser, |tok, _|
                next: TokenKind::OpenParen => parser.skip_fn_arm_nested(),
                next: TokenKind::Eof => break,
                next: _ => (),
            }
        }
    }
    #[macro_rules_attribute(trace)]
    fn skip_fn_arm_nested(&mut self) {
        let parser = self.cast::<SkipNewlines>();
        let mut depth = 1;
        loop {
            select! { parser, |tok, _|
                next: TokenKind::OpenParen => depth += 1,
                next: TokenKind::CloseParen => depth -= 1,
                next: TokenKind::Eof => {
                    parser.error(Error::new(tok.span, ErrorKind::UnclosedScope));
                    break
                },
                next: _ => (),
            }
            if depth == 0 {
                break;
            }
        }
    }
    #[macro_rules_attribute(trace)]
    fn parse_fn_body(&mut self) -> Parsed<FnBody> {
        self.spanning(|parser| {
            parser.next();
            let parser = parser.cast::<FnArmTopLevel>();
            let mut args = Vec::new();
            loop {
                select! { parser, |tok, expected|
                    next: TokenKind::Ident => args.push(Parsed::valid(tok.span, Ident)),
                    next: TokenKind::FatArrow => break,
                    // RECOVERY: Notice the start of another fn arm or the end of the current function
                    next try: TokenKind::Eof => {
                        parser.unexpected(expected, tok);
                        break
                    },
                    next: _ => parser.unexpected(expected, tok),
                }
            }
            let body = parser.parse_open_scope();
            Outcome::Valid(FnBody { args, body })
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
    #[macro_rules_attribute(trace)]
    fn expect(&mut self, kind: TokenKind) {
        let node = self.next();
        if node.kind != kind {
            self.unexpected(&[kind], node);
        }
    }
    fn last_span(&self) -> Span {
        self.0.last_span
    }
    fn next(&mut self) -> Token {
        let tok = Self::raw_next(&mut self.0.lexed);
        let tok = match tok {
            Some(tok) => {
                self.0.last_span = tok.span;
                tok
            }
            None => self.eof(),
        };
        self.0.last_span = tok.span;
        debug(format_args!("next: {tok:?}"));
        tok
    }
    fn peek(&mut self) -> Token {
        let tok = Self::raw_next(&mut { self.0.lexed });
        let tok = tok.unwrap_or_else(|| self.eof());
        debug(format_args!("peek: {tok:?}"));
        tok
    }
    fn raw_next(lexed: &mut &[Token]) -> Option<Token> {
        loop {
            let handling = H::handle(lexed);
            debug(format_args!(
                "raw_next: handling: {handling:?}, lexed: {:?}, ..",
                lexed.get(..10).unwrap_or(lexed)
            ));
            match handling {
                TokenHandling::Keep => break lexed.split_off_first().copied(),
                TokenHandling::Skip => _ = lexed.split_off_first(),
                TokenHandling::Stop => break None,
            }
        }
    }
    fn unexpected(&mut self, expected: &[TokenKind], got: Token) {
        self.error(Error::new(
            got.span,
            ErrorKind::UnexpectedToken {
                expected: [H::EXPECTED, expected].concat(),
                got: got.kind,
            },
        ))
    }
}
impl<'source, M> Parser<'source, M> {
    fn new(core: ParserCore<'source>) -> Self {
        Parser_(core, PhantomData)
    }
    fn cast<M2>(&mut self) -> &mut Parser<'source, M2> {
        debug(format_args!("cast: {}", type_name::<M2>().rsplit("::").next().unwrap()));
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

fn resembles_ident(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Fn | TokenKind::Op | TokenKind::Ident | TokenKind::Underscore | TokenKind::Use
    )
}

trait TokenHandler {
    const EXPECTED: &'static [TokenKind];
    fn handle(tokens: &[Token]) -> TokenHandling;
}
#[derive(Debug, Clone, Copy)]
enum TokenHandling {
    Keep,
    Skip,
    Stop,
}
struct SkipNewlines;
impl TokenHandler for SkipNewlines {
    const EXPECTED: &'static [TokenKind] = &[];
    fn handle(tokens: &[Token]) -> TokenHandling {
        match tokens {
            [token!(TokenKind::Newline), ..] => TokenHandling::Skip,
            _ => TokenHandling::Keep,
        }
    }
}
struct Parenthesised;
impl TokenHandler for Parenthesised {
    const EXPECTED: &'static [TokenKind] = &[TokenKind::CloseParen];
    fn handle(tokens: &[Token]) -> TokenHandling {
        match tokens {
            [token!(TokenKind::CloseParen), ..] => TokenHandling::Stop,
            [token!(TokenKind::Newline), ..] => TokenHandling::Skip,
            _ => TokenHandling::Keep,
        }
    }
}
struct FnArmTopLevel;
impl TokenHandler for FnArmTopLevel {
    const EXPECTED: &'static [TokenKind] = &[TokenKind::CloseParen, TokenKind::Newline];
    fn handle(tokens: &[Token]) -> TokenHandling {
        use TokenKind as K;
        match tokens {
            | [token!(K::CloseParen), ..] | [token!(K::Newline), token!(K::Op | K::Colon | K::Equals), ..] => {
                TokenHandling::Stop
            }
            [token!(K::Newline), ..] => TokenHandling::Skip,
            _ => TokenHandling::Keep,
        }
    }
}
macro_rules! token {
    ($kind:pat) => {
        Token { kind: $kind, span: _ }
    };
}
use token;

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}
#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    UnexpectedToken { expected: Vec<TokenKind>, got: TokenKind },
    DuplicateOpDef,
    MissingStarInVariadics,
    RepetitionOfNothing,
    MissingArrow,
    UnclosedScope,
}
impl Error {
    fn new(span: Span, kind: ErrorKind) -> Self {
        Self { kind, span }
    }
}

macro_rules! trace {
    (
        $(#[$meta:meta])*
        fn $name:ident $(<$($gen:ident : $bound:ident),*>)? ($($args:tt)*) $(-> $ret:ty)? {$($body:tt)*}
    ) => {
        $(#[$meta])*
        fn $name $(<$($gen : $bound)*>)? ($($args)*) $(-> $ret)? {
            enter(stringify!($name));
            let out = { $($body)* };
            exit(stringify!($name));
            out
        }
    }
}
use trace;

thread_local! {
    static DEPTH: Cell<u32> = const { Cell::new(0) };
}
fn enter(msg: &str) {
    debug(format_args!("enter {msg}"));
    DEPTH.set(DEPTH.get() + 1);
}
fn exit(msg: &str) {
    DEPTH.set(DEPTH.get() - 1);
    debug(format_args!("exit {msg}"));
}
fn debug<D: std::fmt::Display>(msg: D) {
    for _ in 0..DEPTH.get() {
        print!("    ");
    }
    println!("{msg}")
}
