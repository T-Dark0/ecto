use crate::scope_tree::Span;

#[derive(Debug, PartialEq, Eq)]
pub struct Scope {
    pub uses: Vec<Parsed<UseStmt>>,
    pub op_defs: Vec<Parsed<OpDef>>,
    pub children: Vec<Parsed<Scope>>,
}
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

#[derive(Debug, PartialEq, Eq)]
pub struct Parsed<T> {
    pub span: Span,
    pub outcome: Outcome<T>,
}
#[derive(Debug, PartialEq, Eq)]
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
    pub fn as_ref(&self) -> Parsed<&T> {
        let Self { span, outcome } = self;
        match outcome {
            Outcome::Valid(v) => Parsed::valid(*span, v),
            Outcome::Recovered(e) => Parsed::recovered(*span, e),
            Outcome::Error => Parsed::error(*span),
        }
    }
}
