use std::{
    fmt::{self, Debug},
    ops::Range,
};

mod ast;
mod lex;
mod parse;

/// Represents a span in cursor units. For example, `Span { start: 3, len: 5 }` on the string `abcdefghij` would refer to the portion
/// `defgh`: the `3` refers to the third space between letters, the third position a typing cursor may be placed at.
///
/// Incidentally, this implies that the first byte of a span with `start = n` is the `n`th byte of the input.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub len: u16,
}
impl Span {
    pub fn new(start: u32, len: u16) -> Self {
        Self { start, len }
    }
    pub fn new_empty(start: u32) -> Self {
        Self { start, len: 0 }
    }
    pub fn around(self, rhs: Self) -> Self {
        let end = rhs.start + u32::from(rhs.len);
        Self {
            start: self.start,
            len: (end - self.start) as u16,
        }
    }
    pub fn between(self, rhs: Self) -> Self {
        let after_lhs = self.start + u32::from(self.len);
        let before_rhs = rhs.start;
        Self {
            start: after_lhs,
            len: (before_rhs - after_lhs) as u16,
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
    pub fn to_usize_range(self) -> Range<usize> {
        self.start as usize..(self.start + u32::from(self.len)) as usize
    }
}
impl Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}, {}]", self.start, self.len)
    }
}
