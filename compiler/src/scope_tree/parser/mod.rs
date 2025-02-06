use super::Span;
use std::{
    fmt::Display,
    sync::atomic::{AtomicU32, Ordering},
};

mod context;
mod parse;
mod test;

#[derive(Debug, PartialEq, Eq)]
struct Error {
    span: Span,
    kind: ErrorKind,
}
#[derive(Debug, PartialEq, Eq)]
enum ErrorKind {
    EmptyUse,
    MissingSemiAfterUse,
    UnclosedFnDecl,
    DuplicateOperatorDefinition,
    LazyBeforeNonArg,
    RepetitionOfNothing,
    DoubleRepetition,
    UnterminatedOpGroup,
    UnstarredOpGroup,
    InvalidArrow,
    ExpectedArrow,
    UnclosedScope,
}
impl Error {
    fn new(span: Span, kind: ErrorKind) -> Self {
        Self { span, kind }
    }
}

static DEPTH: AtomicU32 = AtomicU32::new(0);
fn start(msg: impl Display) {
    debug(format_args!("{msg} start"));
    DEPTH.fetch_add(1, Ordering::Relaxed);
}
fn end(msg: impl Display) {
    DEPTH.fetch_sub(1, Ordering::Relaxed);
    debug(format_args!("{msg} end"));
}
fn debug(msg: impl Display) {
    for _ in 0..DEPTH.load(Ordering::Relaxed) {
        print!("    ");
    }
    println!("{msg}")
}
