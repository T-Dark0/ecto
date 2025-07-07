use crate::{parsed, scope_tree::prettyprint};
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

pub type Parsed<T> = parsed::Parsed<T, NodeKind>;
pub type Outcome<T> = parsed::Outcome<T, NodeKind>;

macro_rules! impl_debug {
    ($($ty:ty)*) => {
        $(
            impl Debug for Parsed<$ty> {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.write_str(&prettyprint::render(self.as_ref_node()))
                }
            }
        )*
    };
}
impl_debug! { Scope UseStmt FnDef Ident OpDef OpParts OpPart OpBindings OpBinding OpArrow FnBody }
