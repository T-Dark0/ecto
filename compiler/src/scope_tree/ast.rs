use super::prettyprint::ToFormattingNode;
use crate::scope_tree::{prettyprint, span::Span};
use std::fmt::{self, Debug};

#[derive(Debug, PartialEq, Eq)]
pub struct Scope {
    pub uses: Vec<Parsed<UseStmt>>,
    pub fn_defs: Vec<Parsed<FnDef>>,
    pub children: Vec<Parsed<Scope>>,
}
#[derive(Debug, PartialEq, Eq)]
pub struct UseStmt {
    pub path: Vec<Parsed<Ident>>,
}
#[derive(Debug, PartialEq, Eq)]
pub struct Ident;
#[derive(Debug, PartialEq, Eq)]
pub struct FnDef {
    pub name: Parsed<Ident>,
    pub op_def: Option<Parsed<OpDef>>,
    pub bodies: Vec<Parsed<FnBody>>,
}
#[derive(Debug, PartialEq, Eq)]
pub struct OpDef {
    pub parts: Parsed<OpParts>,
    pub bindings: Parsed<OpBindings>,
}
#[derive(Debug, PartialEq, Eq)]
pub struct OpParts(pub Vec<Parsed<OpPart>>);
#[derive(Debug, PartialEq, Eq)]
pub enum OpPart {
    Argument,
    LazyArgument,
    Literal,
    Variadic(Parsed<OpParts>),
}
#[derive(Debug, PartialEq, Eq)]
pub struct OpBindings(pub Vec<Parsed<OpBinding>>);
#[derive(Debug, PartialEq, Eq)]
pub struct OpBinding {
    pub lhs: Parsed<Ident>,
    pub arrow: Parsed<OpArrow>,
    pub rhs: Parsed<Ident>,
}
#[derive(Debug, PartialEq, Eq)]
pub enum OpArrow {
    Left,
    Right,
}
#[derive(Debug, PartialEq, Eq)]
pub struct FnBody {
    pub args: Vec<Parsed<Ident>>,
    pub body: Parsed<Scope>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum NodeKind {
    Scope,
    UseStmt,
    FnDef,
    Ident,
    OpDef,
    OpParts,
    OpPart,
    Argument,
    LazyArgument,
    Literal,
    Variadic,
    OpBindings,
    OpBinding,
    OpArrow,
    OpArrowLeft,
    OpArrowRight,
    FnBody,
}

#[derive(Eq, Clone, Copy)]
pub struct Parsed<T> {
    pub span: Span,
    pub outcome: Outcome<T>,
}
#[derive(Eq, Clone, Copy)]
pub enum Outcome<T> {
    Valid(T),
    Recovered(T),
    Error(NodeKind),
}
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Validity {
    Valid,
    Recovered,
    Error,
}
impl<T> Parsed<T> {
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
    pub fn error(span: Span, kind: NodeKind) -> Self {
        Self {
            span,
            outcome: Outcome::Error(kind),
        }
    }
    pub fn map<F, R>(self, f: F) -> Parsed<R>
    where
        F: FnOnce(T) -> R,
    {
        match self.outcome {
            Outcome::Valid(v) => Parsed::valid(self.span, f(v)),
            Outcome::Recovered(r) => Parsed::recovered(self.span, f(r)),
            Outcome::Error(k) => Parsed::error(self.span, k),
        }
    }
    pub fn map_outcome<F, R>(self, f: F) -> Parsed<R>
    where
        F: FnOnce(Outcome<T>) -> Outcome<R>,
    {
        Parsed {
            span: self.span,
            outcome: f(self.outcome),
        }
    }
    pub fn as_ref(&self) -> Parsed<&T> {
        match &self.outcome {
            Outcome::Valid(v) => Parsed::valid(self.span, v),
            Outcome::Recovered(e) => Parsed::recovered(self.span, e),
            Outcome::Error(k) => Parsed::error(self.span, *k),
        }
    }
}
impl<T: ToFormattingNode> Debug for Parsed<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&prettyprint::render(self.as_ref()))
    }
}
impl<T, U> PartialEq<Parsed<U>> for Parsed<T>
where
    T: PartialEq<U>,
{
    fn eq(&self, other: &Parsed<U>) -> bool {
        self.outcome == other.outcome && self.span == other.span
    }
}
impl<T> Outcome<T> {
    pub fn valid_into_recovered(self) -> Self {
        match self {
            Outcome::Valid(x) | Outcome::Recovered(x) => Outcome::Recovered(x),
            Outcome::Error(k) => Outcome::Error(k),
        }
    }
}
impl<T, U> PartialEq<Outcome<U>> for Outcome<T>
where
    T: PartialEq<U>,
{
    fn eq(&self, other: &Outcome<U>) -> bool {
        match (self, other) {
            (Outcome::Valid(v1), Outcome::Valid(v2)) => v1 == v2,
            (Outcome::Recovered(r1), Outcome::Recovered(r2)) => r1 == r2,
            (Outcome::Error(k1), Outcome::Error(k2)) => k1 == k2,
            _ => false,
        }
    }
}
