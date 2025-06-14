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
        Outcome::Valid(node) => node.to_formatting_node(parsed.span, arena),
        Outcome::Error => arena.unit("Error", parsed.span),
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
    fn name_and_span(&mut self, name: &str, span: Span) -> &'arena str {
        self.text_scratch_buffer.push_str(name);
        _ = write!(&mut self.text_scratch_buffer, "{span:?}");
        let out = self.text.alloc_str(&self.text_scratch_buffer);
        self.text_scratch_buffer.clear();
        out
    }
    fn unit(&mut self, name: &str, span: Span) -> FormattingNode<'arena> {
        FormattingNode {
            text: self.name_and_span(name, span),
            children: &[],
        }
    }
    fn add_child<C>(&mut self, child: &Parsed<C>) -> &mut Self
    where
        C: ToFormattingNode,
    {
        let child = to_formatting_node(child.as_ref(), self);
        self.children_scratch_buffer.push(child);
        self
    }
    fn add_children<'a, C, N>(&mut self, children: C) -> &mut Self
    where
        C: IntoIterator<Item = &'a Parsed<N>>,
        N: ToFormattingNode + 'a,
    {
        for child in children {
            self.add_child(child);
        }
        self
    }
    fn finish_children(&mut self) -> &'arena [FormattingNode<'arena>] {
        self.children.alloc_extend(self.children_scratch_buffer.drain(..))
    }
}

pub trait ToFormattingNode {
    fn to_formatting_node<'arena>(
        &self,
        span: Span,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena>;
}
impl ToFormattingNode for Scope {
    fn to_formatting_node<'arena>(
        &self,
        span: Span,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        FormattingNode {
            text: arena.name_and_span("Scope", span),
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
        span: Span,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        FormattingNode {
            text: arena.name_and_span("UseStmt", span),
            children: arena.add_children(&self.path).finish_children(),
        }
    }
}
impl ToFormattingNode for OpDef {
    fn to_formatting_node<'arena>(
        &self,
        span: Span,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        FormattingNode {
            text: arena.name_and_span("OpDef", span),
            children: arena.add_child(&self.bindings).add_child(&self.parts).finish_children(),
        }
    }
}
impl ToFormattingNode for OpBindings {
    fn to_formatting_node<'arena>(
        &self,
        span: Span,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        FormattingNode {
            text: arena.name_and_span("OpBindings", span),
            children: arena.add_children(&self.0).finish_children(),
        }
    }
}
impl ToFormattingNode for OpBinding {
    fn to_formatting_node<'arena>(
        &self,
        span: Span,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        FormattingNode {
            text: arena.name_and_span("OpBinding", span),
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
        span: Span,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        arena.unit("OpArrow", span)
    }
}
impl ToFormattingNode for OpParts {
    fn to_formatting_node<'arena>(
        &self,
        span: Span,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        FormattingNode {
            text: arena.name_and_span("OpParts", span),
            children: arena.add_children(&self.0).finish_children(),
        }
    }
}
impl ToFormattingNode for OpPart {
    fn to_formatting_node<'arena>(
        &self,
        span: Span,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        match self {
            OpPart::Argument => arena.unit("Argument", span),
            OpPart::LazyArgument => arena.unit("LazyArgument", span),
            OpPart::Literal => arena.unit("Literal", span),
            OpPart::Variadic(parsed) => FormattingNode {
                text: arena.name_and_span("Variadic", span),
                children: arena.add_child(parsed).finish_children(),
            },
        }
    }
}
impl ToFormattingNode for Ident {
    fn to_formatting_node<'arena>(
        &self,
        span: Span,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        arena.unit("Ident", span)
    }
}
impl<T> ToFormattingNode for &T
where
    T: ToFormattingNode,
{
    fn to_formatting_node<'arena>(
        &self,
        span: Span,
        arena: &mut FormattingArena<'arena>,
    ) -> FormattingNode<'arena> {
        T::to_formatting_node(self, span, arena)
    }
}
