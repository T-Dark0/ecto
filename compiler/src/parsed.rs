use std::{
    fmt::{self, Debug},
    ops::Range,
};

#[derive(Debug, Eq, Clone, Copy)]
pub struct Parsed<T, E> {
    pub span: Span,
    pub outcome: Outcome<T, E>,
}
#[derive(Debug, Eq, Clone, Copy)]
pub enum Outcome<T, E> {
    Valid(T),
    Recovered(T),
    Error(E),
}
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Validity {
    Valid,
    Recovered,
    Error,
}
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub len: u16,
}
impl<T, E> Parsed<T, E> {
    pub fn valid(span: Span, node: T) -> Self {
        Self {
            span,
            outcome: Outcome::Valid(node),
        }
    }
    pub fn recovered(span: Span, node: T) -> Self {
        Self {
            span,
            outcome: Outcome::Recovered(node),
        }
    }
    pub fn error(span: Span, kind: E) -> Self {
        Self {
            span,
            outcome: Outcome::Error(kind),
        }
    }
    pub fn map<F, R>(self, f: F) -> Parsed<R, E>
    where
        F: FnOnce(T) -> R,
    {
        match self.outcome {
            Outcome::Valid(v) => Parsed::valid(self.span, f(v)),
            Outcome::Recovered(r) => Parsed::recovered(self.span, f(r)),
            Outcome::Error(k) => Parsed::error(self.span, k),
        }
    }
    pub fn as_ref_node(&self) -> Parsed<&T, E>
    where
        E: Clone,
    {
        match &self.outcome {
            Outcome::Valid(v) => Parsed::valid(self.span, v),
            Outcome::Recovered(e) => Parsed::recovered(self.span, e),
            Outcome::Error(k) => Parsed::error(self.span, k.clone()),
        }
    }
    #[cfg_attr(not(test), expect(dead_code, reason = "only used in tests"))]
    pub fn render(&self) -> Render<'_, T, E> {
        Render(self)
    }
}
impl<T1, T2, E1, E2> PartialEq<Parsed<T2, E2>> for Parsed<T1, E1>
where
    T1: PartialEq<T2>,
    E1: PartialEq<E2>,
{
    fn eq(&self, other: &Parsed<T2, E2>) -> bool {
        self.outcome == other.outcome && self.span == other.span
    }
}
impl<T, E> Outcome<T, E> {
    pub fn valid_into_recovered(self) -> Self {
        match self {
            Outcome::Valid(x) | Outcome::Recovered(x) => Outcome::Recovered(x),
            Outcome::Error(k) => Outcome::Error(k),
        }
    }
}
impl<T1, T2, E1, E2> PartialEq<Outcome<T2, E2>> for Outcome<T1, E1>
where
    T1: PartialEq<T2>,
    E1: PartialEq<E2>,
{
    fn eq(&self, other: &Outcome<T2, E2>) -> bool {
        match (self, other) {
            (Outcome::Valid(v1), Outcome::Valid(v2)) => v1 == v2,
            (Outcome::Recovered(r1), Outcome::Recovered(r2)) => r1 == r2,
            (Outcome::Error(k1), Outcome::Error(k2)) => k1 == k2,
            _ => false,
        }
    }
}
impl Span {
    pub fn new(start: u32, len: u16) -> Self {
        Self { start, len }
    }
    pub fn from_usize_range(range: Range<usize>) -> Self {
        Self {
            start: range.start as u32,
            len: (range.end - range.start) as u16,
        }
    }
    pub fn around(self, rhs: Self) -> Self {
        let end = rhs.start + u32::from(rhs.len);
        Self {
            start: self.start,
            len: (end - self.start) as u16,
        }
    }
    pub fn empty_after(self) -> Self {
        Self {
            start: self.start + u32::from(self.len),
            len: 0,
        }
    }
    pub fn empty_before(self) -> Self {
        Self {
            start: self.start,
            len: 0,
        }
    }
    #[cfg_attr(not(test), expect(dead_code, reason = "only used in tests"))]
    pub fn to_usize_range(self) -> Range<usize> {
        self.start as usize..(self.start + u32::from(self.len)) as usize
    }
}
impl Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}, {}]", self.start, self.len)
    }
}

pub trait RenderParsed<E>: Sized {
    fn fmt(this: &Parsed<Self, E>, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}
pub struct Render<'a, T, E>(&'a Parsed<T, E>);
impl<'a, T, E> Debug for Render<'a, T, E>
where
    T: RenderParsed<E>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        RenderParsed::fmt(self.0, f)
    }
}
