use crate::scope_tree::{
    ast::{
        Ident, OpArrow, OpBinding, OpBindings, OpDef, OpPart, OpParts, Outcome, Parsed, Scope,
        UseStmt,
    },
    Span,
};
use std::fmt::Write;
use typed_arena::Arena;

pub fn to_formatting_node<'arena, N>(
    parsed: Parsed<N>,
    arena: &mut FormattingArena<'arena>,
) -> FormattingNode<'arena>
where
    N: ToFormattingNode,
{
    match &parsed.outcome {
        Outcome::Valid(node) => node.to_formatting_node(NodeContext::valid(parsed.span), arena),
        Outcome::Recovered(node) => {
            node.to_formatting_node(NodeContext::recovered(parsed.span), arena)
        }
        Outcome::Error => FormattingNode {
            text: arena.name_and_span("Missing", parsed.span),
            children: &[],
        },
    }
}
pub struct NodeContext {
    pub span: Span,
    pub validity: Validity,
}
#[derive(PartialEq, Eq)]
pub enum Validity {
    Valid,
    Recovered,
}
impl NodeContext {
    pub fn valid(span: Span) -> Self {
        Self {
            span,
            validity: Validity::Valid,
        }
    }
    pub fn recovered(span: Span) -> Self {
        Self {
            span,
            validity: Validity::Recovered,
        }
    }
}

#[derive(Clone, Copy)]
pub struct FormattingNode<'arena> {
    pub text: &'arena str,
    pub children: &'arena [Self],
}
pub struct FormattingArena<'arena> {
    text: &'arena Arena<u8>,
    children: &'arena Arena<FormattingNode<'arena>>,
    text_scratch_buffer: String,
    children_scratch_buffer: Vec<FormattingNode<'arena>>,
}
impl<'arena> FormattingArena<'arena> {
    pub fn new(text: &'arena Arena<u8>, children: &'arena Arena<FormattingNode<'arena>>) -> Self {
        Self {
            text,
            children,
            text_scratch_buffer: String::new(),
            children_scratch_buffer: Vec::new(),
        }
    }
    pub fn name_and_context(&mut self, name: &str, ctx: NodeContext) -> &'arena str {
        self.text_scratch_buffer.push_str(name);
        if ctx.validity == Validity::Recovered {
            self.text_scratch_buffer.push('*');
        }
        _ = write!(&mut self.text_scratch_buffer, "{:?}", ctx.span);
        let out = self.text.alloc_str(&self.text_scratch_buffer);
        self.text_scratch_buffer.clear();
        out
    }
    pub fn name_and_span(&mut self, name: &str, span: Span) -> &'arena str {
        self.text_scratch_buffer.push_str(name);
        _ = write!(&mut self.text_scratch_buffer, "{span:?}");
        let out = self.text.alloc_str(&self.text_scratch_buffer);
        self.text_scratch_buffer.clear();
        out
    }
    pub fn unit(&mut self, name: &str, ctx: NodeContext) -> FormattingNode<'arena> {
        FormattingNode {
            text: self.name_and_context(name, ctx),
            children: &[],
        }
    }
    pub fn add_child<C>(&mut self, child: &Parsed<C>) -> &mut Self
    where
        C: ToFormattingNode,
    {
        let child = to_formatting_node(child.as_ref(), self);
        self.children_scratch_buffer.push(child);
        self
    }
    pub fn add_children<'a, C, N>(&mut self, children: C) -> &mut Self
    where
        C: IntoIterator<Item = &'a Parsed<N>>,
        N: ToFormattingNode + 'a,
    {
        for child in children {
            self.add_child(child);
        }
        self
    }
    pub fn finish_children(&mut self) -> &'arena [FormattingNode<'arena>] {
        self.children.alloc_extend(self.children_scratch_buffer.drain(..))
    }
}

pub trait ToFormattingNode {
    fn to_formatting_node<'arena>(
        &self,
        context: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena>;
}
impl ToFormattingNode for Scope {
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        FormattingNode {
            text: arena.name_and_context("Scope", ctx),
            children: arena
                .add_children(&self.uses)
                .add_children(&self.op_defs)
                .add_children(&self.children)
                .finish_children(),
        }
    }
}
impl ToFormattingNode for UseStmt {
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        FormattingNode {
            text: arena.name_and_context("UseStmt", ctx),
            children: arena.add_children(&self.path).finish_children(),
        }
    }
}
impl ToFormattingNode for OpDef {
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        FormattingNode {
            text: arena.name_and_context("OpDef", ctx),
            children: arena.add_child(&self.bindings).add_child(&self.parts).finish_children(),
        }
    }
}
impl ToFormattingNode for OpBindings {
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        FormattingNode {
            text: arena.name_and_context("OpBindings", ctx),
            children: arena.add_children(&self.0).finish_children(),
        }
    }
}
impl ToFormattingNode for OpBinding {
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        FormattingNode {
            text: arena.name_and_context("OpBinding", ctx),
            children: arena
                .add_child(&self.lhs)
                .add_child(&self.arrow)
                .add_child(&self.rhs)
                .finish_children(),
        }
    }
}
impl ToFormattingNode for OpArrow {
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        FormattingNode {
            text: arena.name_and_context("OpArrow", ctx),
            children: &[],
        }
    }
}
impl ToFormattingNode for OpParts {
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        arena.unit("OpParts", ctx)
    }
}
impl ToFormattingNode for OpPart {
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        match self {
            OpPart::Argument => arena.unit("Argument", ctx),
            OpPart::LazyArgument => arena.unit("LazyArgument", ctx),
            OpPart::Literal => arena.unit("Literal", ctx),
            OpPart::Variadic(parsed) => FormattingNode {
                text: arena.name_and_context("Variadic", ctx),
                children: arena.add_child(parsed).finish_children(),
            },
        }
    }
}
impl ToFormattingNode for Ident {
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        arena.unit("Ident", ctx)
    }
}
impl<T> ToFormattingNode for &T
where
    T: ToFormattingNode,
{
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        T::to_formatting_node(self, ctx, arena)
    }
}
