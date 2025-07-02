use super::common::Validity;
use crate::scope_tree::{
    ast::{
        FnBody, FnDef, Ident, NodeKind, OpArrow, OpBinding, OpBindings, OpDef, OpPart, OpParts, Outcome, Parsed, Scope,
        UseStmt,
    },
    span::Span,
};
use std::fmt::Write;
use typed_arena::Arena;

pub fn to_formatting_node<'arena, N>(parsed: Parsed<N>, arena: &mut FormattingArena<'arena>) -> FormattingNode<'arena>
where
    N: ToFormattingNode,
{
    match &parsed.outcome {
        Outcome::Valid(node) => node.to_formatting_node(NodeContext::valid(parsed.span), arena),
        Outcome::Recovered(node) => node.to_formatting_node(NodeContext::recovered(parsed.span), arena),
        Outcome::Error(kind) => node_kind_to_formatting_node(*kind, parsed.span, arena),
    }
}
fn node_kind_to_formatting_node<'arena>(
    kind: NodeKind,
    span: Span,
    arena: &mut FormattingArena<'arena>,
) -> FormattingNode<'arena> {
    let name = match kind {
        NodeKind::Scope => "Scope!",
        NodeKind::UseStmt => "UseStmt!",
        NodeKind::FnDef => "FnDef!",
        NodeKind::Ident => "Ident!",
        NodeKind::OpDef => "OpDef!",
        NodeKind::OpPart => "OpPart!",
        NodeKind::Argument => "Argument!",
        NodeKind::LazyArgument => "LazyArgument!",
        NodeKind::Literal => "Literal!",
        NodeKind::Variadic => "Variadic!",
        NodeKind::OpParts => "OpParts!",
        NodeKind::OpBindings => "OpBindings!",
        NodeKind::OpBinding => "OpBinding!",
        NodeKind::OpArrow => "OpArrow!",
        NodeKind::OpArrowLeft => "OpArrowLeft!",
        NodeKind::OpArrowRight => "OpArrowRight!",
        NodeKind::FnBody => "FnBody!",
    };
    FormattingNode {
        text: arena.name_and_span(name, span),
        children: Children::Never,
    }
}

pub struct NodeContext {
    pub span: Span,
    pub validity: Validity,
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
    pub children: Children<'arena>,
}
#[derive(Clone, Copy)]
pub enum Children<'arena> {
    Never,
    Sometimes(&'arena [FormattingNode<'arena>]),
}
impl<'arena> FormattingNode<'arena> {
    pub fn has_children(&self) -> bool {
        match self.children {
            Children::Never => false,
            Children::Sometimes(children) => !children.is_empty(),
        }
    }
}

pub struct FormattingArena<'arena> {
    text: &'arena Arena<u8>,
    children: &'arena Arena<FormattingNode<'arena>>,
    text_scratch_buffer: String,
    children_scratch_buffer: Vec<FormattingNode<'arena>>,
    children_stack_frame_sizes: Vec<usize>,
}
pub struct ChildBuilder<'of, 'arena> {
    arena: &'of mut FormattingArena<'arena>,
}
impl<'arena> FormattingArena<'arena> {
    pub fn new(text: &'arena Arena<u8>, children: &'arena Arena<FormattingNode<'arena>>) -> Self {
        Self {
            text,
            children,
            text_scratch_buffer: String::new(),
            children_scratch_buffer: Vec::new(),
            children_stack_frame_sizes: Vec::new(),
        }
    }
    pub fn name_and_span(&mut self, name: &str, span: Span) -> &'arena str {
        self.text_scratch_buffer.push_str(name);
        _ = write!(&mut self.text_scratch_buffer, "{span:?}");
        let out = self.text.alloc_str(&self.text_scratch_buffer);
        self.text_scratch_buffer.clear();
        out
    }
    pub fn name_and_context(&mut self, name: &str, ctx: NodeContext) -> &'arena str {
        self.text_scratch_buffer.push_str(name);
        if ctx.validity == Validity::Recovered {
            self.text_scratch_buffer.push('~');
        }
        _ = write!(&mut self.text_scratch_buffer, "{:?}", ctx.span);
        let out = self.text.alloc_str(&self.text_scratch_buffer);
        self.text_scratch_buffer.clear();
        out
    }
    pub fn unit(&mut self, name: &str, ctx: NodeContext) -> FormattingNode<'arena> {
        FormattingNode {
            text: self.name_and_context(name, ctx),
            children: Children::Never,
        }
    }
    pub fn child<C>(&mut self, child: &Parsed<C>) -> Children<'arena>
    where
        C: ToFormattingNode,
    {
        self.children_builder().child(child).finish()
    }
    pub fn children<'a, C, N>(&mut self, children: C) -> Children<'arena>
    where
        C: IntoIterator<Item = &'a Parsed<N>>,
        N: ToFormattingNode + 'a,
    {
        self.children_builder().children(children).finish()
    }
    pub fn children_builder(&mut self) -> ChildBuilder<'_, 'arena> {
        self.children_stack_frame_sizes.push(self.children_scratch_buffer.len());
        ChildBuilder { arena: self }
    }
}
impl<'of, 'arena> ChildBuilder<'of, 'arena> {
    pub fn child<C>(&mut self, child: &Parsed<C>) -> &mut Self
    where
        C: ToFormattingNode,
    {
        let child = to_formatting_node(child.as_ref(), self.arena);
        self.arena.children_scratch_buffer.push(child);
        self
    }
    pub fn children<'a, C, N>(&mut self, children: C) -> &mut Self
    where
        C: IntoIterator<Item = &'a Parsed<N>>,
        N: ToFormattingNode + 'a,
    {
        for child in children {
            self.child(child);
        }
        self
    }
    pub fn finish(&mut self) -> Children<'arena> {
        let own_children_start = self.arena.children_stack_frame_sizes.pop().unwrap();
        Children::Sometimes(
            self.arena.children.alloc_extend(self.arena.children_scratch_buffer.drain(own_children_start..)),
        )
    }
}

pub trait ToFormattingNode {
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
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
                .children_builder()
                .children(&self.uses)
                .children(&self.fn_defs)
                .children(&self.children)
                .finish(),
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
            children: arena.children_builder().children(&self.path).finish(),
        }
    }
}
impl ToFormattingNode for FnDef {
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        FormattingNode {
            text: arena.name_and_context("FnDef", ctx),
            children: {
                let mut builder = arena.children_builder();
                builder.child(&self.name);
                if let Some(op_def) = &self.op_def {
                    builder.child(op_def);
                }
                builder.children(&self.bodies).finish()
            },
        }
    }
}
impl ToFormattingNode for FnBody {
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        FormattingNode {
            text: arena.name_and_context("FnBody", ctx),
            children: arena.children_builder().children(&self.args).child(&self.body).finish(),
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
            children: arena.children_builder().child(&self.parts).child(&self.bindings).finish(),
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
            children: arena.children(&self.0),
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
            children: arena.children_builder().child(&self.lhs).child(&self.arrow).child(&self.rhs).finish(),
        }
    }
}
impl ToFormattingNode for OpArrow {
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        let name = match self {
            Self::Left => "OpArrowLeft",
            Self::Right => "OpArrowRight",
        };
        FormattingNode {
            text: arena.name_and_context(name, ctx),
            children: Children::Never,
        }
    }
}
impl ToFormattingNode for OpParts {
    fn to_formatting_node<'arena>(
        &self,
        ctx: NodeContext,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        println!("OpParts::to_formatting_node");
        FormattingNode {
            text: arena.name_and_context("OpParts", ctx),
            children: arena.children(&self.0),
        }
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
            OpPart::Variadic(parts) => {
                println!("variadic");
                FormattingNode {
                    text: arena.name_and_context("Variadic", ctx),
                    children: arena.child(parts),
                }
            }
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
