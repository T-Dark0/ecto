use super::formatting_node::{FormattingArena, FormattingNode, NodeContext, ToFormattingNode};
use crate::scope_tree::ast::{
    FnBody, FnDef, Ident, OpArrow, OpBinding, OpBindings, OpDef, OpPart, OpParts, Scope, UseStmt,
};
use std::fmt::{self, Debug};

#[derive(PartialEq, Eq)]
pub enum AnyNode {
    Scope(Scope),
    UseStmt(UseStmt),
    FnDef(FnDef),
    FnBody(FnBody),
    Ident(Ident),
    OpDef(OpDef),
    OpParts(OpParts),
    OpPart(OpPart),
    OpBindings(OpBindings),
    OpBinding(OpBinding),
    OpArrow(OpArrow),
}
macro_rules! impl_traits {
    ($($node:ident)*) => {
        impl Debug for AnyNode {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $( Self::$node(node) => Debug::fmt(node, f), )*
                }
            }
        }
        $(
            impl PartialEq<$node> for AnyNode {
                fn eq(&self, other: &$node) -> bool {
                    let Self::$node(this) = self else { return false };
                    this == other
                }
            }
        )*
        impl ToFormattingNode for AnyNode {
            fn to_formatting_node<'arena>(
                &self,
                ctx: NodeContext,
                arena: &mut FormattingArena<'arena>,
            ) -> FormattingNode<'arena> {
                match self {
                    $( Self::$node(node) => node.to_formatting_node(ctx, arena) ),*
                }
            }
        }

    };
}
impl_traits! {Scope UseStmt FnDef FnBody Ident OpDef OpParts OpPart OpBindings OpBinding OpArrow }
