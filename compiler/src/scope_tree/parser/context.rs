use super::{debug, Error, ErrorKind};
use crate::scope_tree::{
    lexer::{LexedSlice, Token, TokenKind},
    Span,
};

pub struct Context<'state, 'lexed, Hooks> {
    state: &'state mut ContextState<'lexed>,
    hooks: Hooks,
}
pub struct ContextState<'lexed> {
    pub lexed: LexedSlice<'lexed>,
    pub errors: Vec<Error>,
    pub last_span: Span,
}
impl<'state, 'lexed, H> Context<'state, 'lexed, H>
where
    H: Hooks,
{
    pub fn next(&mut self) -> Token {
        let out = Self::raw_next(self.hooks, &mut self.state.lexed).unwrap_or_else(|| self.eof());
        self.state.last_span = out.span;
        debug(format_args!("next {out:?}"));
        out
    }
    pub fn peek(&self) -> Token {
        let out =
            Self::raw_next(self.hooks, &mut { self.state.lexed }).unwrap_or_else(|| self.eof());
        debug(format_args!("peek {out:?}"));
        out
    }
    pub fn skip(&mut self) {
        let out = Self::raw_next(self.hooks, &mut self.state.lexed).unwrap_or_else(|| self.eof());
        self.state.last_span = out.span;
        debug(format_args!("skip {out:?}"));
    }
    pub fn error(&mut self, span: Span, kind: ErrorKind) {
        self.state.errors.push(Error { span, kind })
    }
    pub fn last_span(&self) -> Span {
        self.state.last_span
    }
    pub fn reborrow<'new>(&'new mut self) -> Context<'new, 'lexed, H> {
        Context {
            state: self.state,
            hooks: self.hooks,
        }
    }
    pub fn set_stop<S>(self, stop: S) -> Context<'state, 'lexed, impl Hooks>
    where
        S: Stop,
    {
        Context {
            state: self.state,
            hooks: FnHooks {
                stop,
                skip: self.hooks.skip(),
            },
        }
    }
    pub fn add_stop<S>(self, stop: S) -> Context<'state, 'lexed, impl Hooks>
    where
        S: Stop,
    {
        fn make_or<F1, F2>(f1: F1, f2: F2) -> impl Stop
        where
            F1: Stop,
            F2: Stop,
        {
            move |t| f1(t) || f2(t)
        }

        let stop_hook = self.hooks.stop();
        self.set_stop(make_or(stop_hook, stop))
    }
    pub fn set_skip<S>(self, skip: S) -> Context<'state, 'lexed, impl Hooks>
    where
        S: Fn(TokenKind) -> bool + Copy,
    {
        Context {
            state: self.state,
            hooks: FnHooks {
                stop: self.hooks.stop(),
                skip,
            },
        }
    }

    fn raw_next(hooks: H, lexed: &mut LexedSlice<'lexed>) -> Option<Token> {
        loop {
            if hooks.stop()(lexed.kinds()) {
                debug("raw_next stop");
                break None;
            }
            let first = lexed.take_first()?;
            if hooks.skip()(*first.kind()) {
                debug(format_args!("raw_next skip {:?}", first.kind()));
                continue;
            }
            break Some(Token {
                kind: *first.kind(),
                span: *first.span(),
            });
        }
    }
    fn eof(&self) -> Token {
        Token {
            kind: TokenKind::Eof,
            span: self.state.last_span.empty_after(),
        }
    }
}
impl<'state, 'lexed> Context<'state, 'lexed, ()> {
    pub fn new(state: &'state mut ContextState<'lexed>) -> Context<'state, 'lexed, impl Hooks> {
        fn stop(_: &[TokenKind]) -> bool {
            false
        }
        fn skip(_: TokenKind) -> bool {
            false
        }
        Context {
            state,
            hooks: FnHooks { stop, skip },
        }
    }
}
impl<'lexed> ContextState<'lexed> {
    pub fn new(lexed: LexedSlice<'lexed>) -> Self {
        Self {
            lexed,
            errors: Vec::new(),
            last_span: Span::new_empty(0),
        }
    }
}

pub trait Stop: Fn(&[TokenKind]) -> bool + Copy {}
impl<F: Fn(&[TokenKind]) -> bool + Copy> Stop for F {}

pub trait Skip: Fn(TokenKind) -> bool + Copy {}
impl<F: Fn(TokenKind) -> bool + Copy> Skip for F {}

pub trait Hooks: Copy {
    type Stop: Stop;
    type Skip: Skip;

    fn stop(&self) -> Self::Stop;
    fn skip(&self) -> Self::Skip;
}
#[derive(Clone, Copy)]
struct FnHooks<Stop, Skip> {
    stop: Stop,
    skip: Skip,
}
impl<S1, S2> Hooks for FnHooks<S1, S2>
where
    S1: Stop,
    S2: Skip,
{
    type Stop = S1;
    type Skip = S2;
    fn stop(&self) -> Self::Stop {
        self.stop
    }
    fn skip(&self) -> Self::Skip {
        self.skip
    }
}
