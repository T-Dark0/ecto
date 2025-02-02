use super::Span;

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
    Error,
}
impl<T> Parsed<T> {
    pub fn valid(data: T, span: Span) -> Self {
        Self {
            outcome: Outcome::Valid(data),
            span,
        }
    }
    pub fn error(span: Span) -> Self {
        Self {
            outcome: Outcome::Error,
            span,
        }
    }

    pub fn map<F, U>(self, f: F) -> Parsed<U>
    where
        F: FnOnce(T) -> U,
    {
        let Self { outcome, span } = self;
        let outcome = match outcome {
            Outcome::Valid(t) => Outcome::Valid(f(t)),
            Outcome::Error => Outcome::Error,
        };
        Parsed { outcome, span }
    }
    pub fn is_error(&self) -> bool {
        matches!(self.outcome, Outcome::Error)
    }
}
