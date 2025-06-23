use crate::scope_tree::ast::{
    Ident, OpArrow, OpBinding, OpBindings, OpDef, OpPart, OpParts, Scope, ScopeContents, UseStmt,
};

use super::formatting_node::{FormattingArena, FormattingNode, NodeContext, ToFormattingNode};

#[derive(PartialEq, Eq)]
pub enum Validity {
    Valid,
    Recovered,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AnyNode {
    ScopeContents(ScopeContents),
    UseStmt(UseStmt),
    Ident(Ident),
    OpDef(OpDef),
    OpParts(OpParts),
    OpPart(OpPart),
    OpBindings(OpBindings),
    OpBinding(OpBinding),
    OpArrow(OpArrow),
    Scope(Scope),
}
macro_rules! impl_partial_eq {
    ($($node:ident)*) => {
        $(
            impl PartialEq<$node> for AnyNode {
                fn eq(&self, other: &$node) -> bool {
                    let Self::$node(this) = self else { return false };
                    this == other
                }
            }
        )*

    };
}
impl_partial_eq! { ScopeContents UseStmt Ident OpDef OpParts OpPart OpBindings OpBinding OpArrow Scope }

impl ToFormattingNode for AnyNode {
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        match self {
            AnyNode::ScopeContents(s) => s.to_formatting_node(ctx, arena),
            AnyNode::UseStmt(u) => u.to_formatting_node(ctx, arena),
            AnyNode::Ident(i) => i.to_formatting_node(ctx, arena),
            AnyNode::OpDef(o) => o.to_formatting_node(ctx, arena),
            AnyNode::OpParts(o) => o.to_formatting_node(ctx, arena),
            AnyNode::OpPart(o) => o.to_formatting_node(ctx, arena),
            AnyNode::OpBindings(o) => o.to_formatting_node(ctx, arena),
            AnyNode::OpBinding(o) => o.to_formatting_node(ctx, arena),
            AnyNode::OpArrow(o) => o.to_formatting_node(ctx, arena),
            AnyNode::Scope(s) => s.to_formatting_node(ctx, arena),
        }
    }
}
