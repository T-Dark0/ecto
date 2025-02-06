use std::marker::PhantomData;

use super::{debug, Error, ErrorKind};
use crate::scope_tree::{
    lexer::{LexedSlice, Token, TokenKind},
    Span,
};

pub struct Context<'state, 'lexed, Hooks> {
    pub state: &'state mut ContextState<'lexed>,
    pub hooks: Hooks,
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
    pub fn reborrow<'new>(&'new mut self) -> Context<'new, 'lexed, H> {
        Context {
            state: self.state,
            hooks: self.hooks,
        }
    }
    pub fn set_stop<S>(self, stop: S) -> Context<'state, 'lexed, impl Hooks>
    where
        S: Fn(&[TokenKind]) -> bool + Copy,
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
        S: Fn(&[TokenKind]) -> bool + Copy,
    {
        fn make_or<F1, F2>(f1: F1, f2: F2) -> impl Fn(&[TokenKind]) -> bool + Copy
        where
            F1: Fn(&[TokenKind]) -> bool + Copy,
            F2: Fn(&[TokenKind]) -> bool + Copy,
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
        let LexedSlice { kinds, spans } = lexed;
        loop {
            if hooks.stop()(*kinds) {
                debug(format_args!(
                    "raw_next stop {:?}",
                    kinds.get(..2).unwrap_or(kinds)
                ));
                break None;
            }
            break match (*kinds, *spans) {
                (&[kind, ref k @ ..], &[span, ref s @ ..]) => {
                    (*kinds, *spans) = (k, s);
                    if hooks.skip()(kind) {
                        debug(format_args!("raw_next skip {kind:?}"));
                        continue;
                    }
                    Some(Token { kind, span })
                }
                _ => None,
            };
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
        fn stop<'a>(_: &'a [TokenKind]) -> bool {
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
pub trait Hooks: Copy {
    type Stop: Fn(&[TokenKind]) -> bool + Copy;
    type Skip: Fn(TokenKind) -> bool + Copy;

    fn stop(&self) -> Self::Stop;
    fn skip(&self) -> Self::Skip;
}
#[derive(Clone, Copy)]
struct FnHooks<Stop, Skip> {
    stop: Stop,
    skip: Skip,
}
impl<Stop, Skip> Hooks for FnHooks<Stop, Skip>
where
    Stop: Fn(&[TokenKind]) -> bool + Copy,
    Skip: Fn(TokenKind) -> bool + Copy,
{
    type Stop = Stop;
    type Skip = Skip;

    fn stop(&self) -> Self::Stop {
        self.stop
    }
    fn skip(&self) -> Self::Skip {
        self.skip
    }
}

pub struct Stop<F>(F);
pub struct Skip<F>(F);

pub struct Cons<H, T>(H, T);
pub struct Nil;

pub struct Zero;
pub struct Next<T>(T);

struct Select<T>(PhantomData<T>);
impl<T> Select<T> {
    fn new() -> Self {
        Self(PhantomData)
    }
}

trait Selector {
    type Selected;
}
impl<T> Selector for Select<T> {
    type Selected = T;
}

trait Remove<Target, Index>
where
    Target: Selector,
{
    type Remainder;
    fn remove(self) -> (Target::Selected, Self::Remainder);
}
impl<Target, Head, Tail> Remove<Target, Zero> for Cons<Head, Tail>
where
    Target: Selector<Selected = Head>,
{
    type Remainder = Tail;
    fn remove(self) -> (Target::Selected, Self::Remainder) {
        (self.0, self.1)
    }
}
impl<Target, Index, Head, Tail> Remove<Target, Next<Index>> for Cons<Head, Tail>
where
    Target: Selector,
    Tail: Remove<Target, Index>,
{
    type Remainder = Cons<Head, Tail::Remainder>;
    fn remove(self) -> (Target::Selected, Self::Remainder) {
        let (selected, remainder) = self.1.remove();
        (selected, Cons(self.0, remainder))
    }
}

trait Replace<Fillers, Indices> {
    type Output;
    fn replace(self, fillers: Fillers) -> Self::Output;
}
impl<Fillers, Index, Indices, Head, Tail> Replace<Fillers, Cons<Index, Indices>>
    for Cons<Head, Tail>
where
    Head: Selector,
    Fillers: Remove<Head, Index>,
    Tail: Replace<Fillers::Remainder, Indices>,
{
    type Output = Cons<Head::Selected, Tail::Output>;
    fn replace(self, fillers: Fillers) -> Self::Output {
        let (selected, remainder) = fillers.remove();
        Cons(selected, self.1.replace(remainder))
    }
}
impl<Unused> Replace<Unused, Zero> for Nil {
    type Output = Nil;
    fn replace(self, _: Unused) -> Self::Output {
        self
    }
}

fn build<T, I1, I2, S1, S2>(fillers: T) -> (Stop<S1>, Skip<S2>)
where
    T: Remove<Select<Stop<S1>>, I1, Remainder: Remove<Select<Skip<S2>>, I2>>,
{
    let placeholders = Cons(
        Select::<Stop<S1>>::new(),
        Cons(Select::<Skip<S2>>::new(), Nil),
    );
    let Cons(stop, Cons(skip, Nil)) = placeholders.replace(fillers);
    (stop, skip)
}
