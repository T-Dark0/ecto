use crate::scope_tree::Span;

#[derive(Debug, PartialEq, Eq)]
pub struct ScopeContents {
    pub uses: Vec<Parsed<UseStmt>>,
    pub op_defs: Vec<Parsed<OpDef>>,
    pub children: Vec<Parsed<Scope>>,
}
#[derive(Debug, PartialEq, Eq)]
pub struct Scope(pub Parsed<ScopeContents>);
#[derive(Debug, PartialEq, Eq)]
pub struct UseStmt {
    pub path: Vec<Parsed<Ident>>,
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
pub struct Ident;

#[derive(Debug, Eq)]
pub struct Parsed<T> {
    pub span: Span,
    pub outcome: Outcome<T>,
}
#[derive(Debug, Eq)]
pub enum Outcome<T> {
    Valid(T),
    Recovered(T),
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
    pub fn error(span: Span) -> Self {
        Self {
            span,
            outcome: Outcome::Error,
        }
    }
    pub fn map<F, R>(self, f: F) -> Parsed<R>
    where
        F: FnOnce(T) -> R,
    {
        match self.outcome {
            Outcome::Valid(v) => Parsed::valid(self.span, f(v)),
            Outcome::Recovered(r) => Parsed::recovered(self.span, f(r)),
            Outcome::Error => Parsed::error(self.span),
        }
    }
    pub fn as_ref(&self) -> Parsed<&T> {
        match &self.outcome {
            Outcome::Valid(v) => Parsed::valid(self.span, v),
            Outcome::Recovered(e) => Parsed::recovered(self.span, e),
            Outcome::Error => Parsed::error(self.span),
        }
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
impl<T, U> PartialEq<Outcome<U>> for Outcome<T>
where
    T: PartialEq<U>,
{
    fn eq(&self, other: &Outcome<U>) -> bool {
        match (self, other) {
            (Outcome::Valid(v1), Outcome::Valid(v2)) => v1 == v2,
            (Outcome::Recovered(r1), Outcome::Recovered(r2)) => r1 == r2,
            (Outcome::Error, Outcome::Error) => true,
            _ => false,
        }
    }
}
