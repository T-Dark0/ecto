use super::formatting_node::{
    to_formatting_node, Children, FormattingArena, FormattingNode, ToFormattingNode,
};
use crate::scope_tree::ast::Parsed;
use std::marker::PhantomData;
use typed_arena::Arena;

pub fn render<N: ToFormattingNode>(node: Parsed<N>) -> String {
    let text = Arena::new();
    let children = Arena::new();
    let mut arena = FormattingArena::new(&text, &children);
    let node = to_formatting_node(node, &mut arena);
    let mut buffer = String::new();
    RenderState::<Outline, Outline> {
        out: &mut buffer,
        remaining_width: 120,
        total_width: 120,
        indent: 0,
        indent_width: 4,
        _boo: PhantomData,
    }
    .render(node);
    buffer
}

struct RenderState<'out, M, G> {
    out: &'out mut String,
    remaining_width: i32,
    total_width: u32,
    indent: u8,
    indent_width: u8,
    _boo: PhantomData<(M, G)>,
}
impl<'out, M> RenderState<'out, M, Generic>
where
    M: RenderingMode,
{
    fn render(self, node: FormattingNode<'_>) -> Result<Self, M::Error> {
        let state = self.push(node.text)?;
        let state = match node.children {
            Children::Never => state,
            Children::Sometimes([]) => state.push("()")?,
            Children::Sometimes(&[one]) => state.fallback(
                |inline| inline.push("(")?.reserve(1)?.render(one)?.push_reserved(")").ok(),
                |outline| outline.push("(").nest().render(one).unnest().push(")").line().end(),
            ),
            Children::Sometimes(many) => state.fallback(
                |inline| {
                    if many.iter().any(|child| child.has_children()) {
                        return Err(());
                    }
                    let mut inline = inline.push("(")?.reserve(1)?;
                    let mut children = many.iter();
                    if let Some(&first) = children.next() {
                        inline = inline.render(first)?;
                        for &child in children {
                            inline = inline.push(" ")?.render(child)?;
                        }
                    }
                    inline.push_reserved(")").ok()
                },
                |outline| {
                    let mut outline = outline.push("(").nest();
                    let mut children = many.iter();
                    if let Some(&first) = children.next() {
                        outline = outline.render(first);
                        for &child in children {
                            outline = outline.line().render(child)
                        }
                    }
                    outline.unnest().push(")").end()
                },
            ),
        };
        Ok(state)
    }
    fn push(self, text: &str) -> Result<Self, M::Error> {
        Ok(self.reserve(text.len())?.push_reserved(text))
    }
    fn reserve(mut self, width: usize) -> Result<Self, M::Error> {
        let width = width as i32;
        if self.remaining_width < width {
            M::fail()?;
        }
        self.remaining_width -= width;
        Ok(self)
    }
    fn push_reserved(self, text: &str) -> Self {
        self.out.push_str(text);
        self
    }
    fn nest(mut self) -> Self {
        self.indent += 1;
        self.out.push('\n');
        self.indent()
    }
    fn unnest(mut self) -> Self {
        self.indent -= 1;
        self.out.push('\n');
        self.indent()
    }
    fn line(mut self) -> Self {
        self.out.push('\n');
        self.remaining_width = self.total_width as i32;
        self.indent()
    }
    fn indent(mut self) -> Self {
        let width = u16::from(self.indent) * u16::from(self.indent_width);
        for _ in 0..width {
            self.out.push(' ');
        }
        self.remaining_width -= i32::from(width);
        self
    }
}
impl<'out> RenderState<'out, Inline, Inline> {
    fn render(self, node: FormattingNode<'_>) -> Result<Self, ()> {
        self.cast::<Inline, Generic>().render(node).map(RenderState::cast)
    }
    fn push(self, text: &str) -> Result<Self, ()> {
        self.cast::<Inline, Generic>().push(text).map(RenderState::cast)
    }
    fn reserve(self, width: usize) -> Result<Self, ()> {
        self.cast::<Inline, Generic>().reserve(width).map(RenderState::cast)
    }
    fn push_reserved(self, text: &str) -> Self {
        self.cast::<Inline, Generic>().push_reserved(text).cast()
    }
}
impl<'out> RenderState<'out, Outline, Outline> {
    fn render(self, node: FormattingNode<'_>) -> Self {
        let Ok(state) = self.cast::<Outline, Generic>().render(node);
        state.cast()
    }
    fn push(self, text: &str) -> Self {
        let Ok(state) = self.cast::<Outline, Generic>().push(text);
        state.cast()
    }
    fn reserve(self, width: usize) -> Self {
        let Ok(state) = self.cast::<Outline, Generic>().reserve(width);
        state.cast()
    }
    fn push_reserved(self, text: &str) -> Self {
        self.cast::<Outline, Generic>().push_reserved(text).cast()
    }
    fn nest(self) -> Self {
        self.cast::<Outline, Generic>().nest().cast()
    }
    fn unnest(self) -> Self {
        self.cast::<Outline, Generic>().unnest().cast()
    }
    fn line(self) -> Self {
        self.cast::<Outline, Generic>().line().cast()
    }
}
impl<'out, M, G> RenderState<'out, M, G> {
    fn reborrow<'out2>(&'out2 mut self) -> RenderState<'out2, M, G> {
        RenderState {
            out: self.out,
            remaining_width: self.remaining_width,
            total_width: self.total_width,
            indent: self.indent,
            indent_width: self.indent_width,
            _boo: PhantomData,
        }
    }
    fn fallback<I, O>(mut self, inline: I, outline: O) -> Self
    where
        I: FnOnce(RenderState<'_, Inline, Inline>) -> Result<(), ()>,
        O: FnOnce(RenderState<'_, Outline, Outline>),
    {
        let old_len = self.out.len();
        match inline(self.reborrow().cast()) {
            Ok(()) => return self,
            Err(()) => self.out.truncate(old_len),
        }
        outline(self.reborrow().cast());
        self
    }
    fn ok<E>(self) -> Result<(), E> {
        Ok(())
    }
    fn end(self) {}
    fn cast<M2, G2>(self) -> RenderState<'out, M2, G2> {
        RenderState {
            out: self.out,
            remaining_width: self.remaining_width,
            total_width: self.total_width,
            indent: self.indent,
            indent_width: self.indent_width,
            _boo: PhantomData,
        }
    }
}

trait RenderingMode {
    type Success;
    type Error;
    fn fail() -> Result<Self::Success, Self::Error>;
}
struct Inline;
impl RenderingMode for Inline {
    type Success = Never;
    type Error = ();
    fn fail() -> Result<Self::Success, Self::Error> {
        Err(())
    }
}
struct Outline;
impl RenderingMode for Outline {
    type Success = ();
    type Error = Never;
    fn fail() -> Result<Self::Success, Self::Error> {
        Ok(())
    }
}
struct Generic;
enum Never {}
