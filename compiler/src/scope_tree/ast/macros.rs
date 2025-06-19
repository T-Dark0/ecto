macro_rules! ast {
    (Scope [$start:expr, $len:expr] $(($( $node:ident $span:tt $nested:tt )*))? ) => {{
        #[allow(unused_mut, unused_imports, clippy::vec_init_then_push)]
        let scope = {
            use crate::scope_tree::{
                Span,
                ast::{Scope, UseStmt, Ident, OpDef, OpParts, OpPart, OpBindings, OpBinding, Parsed},
            };
            let mut uses = Vec::new();
            let mut op_defs = Vec::new();
            let mut children = Vec::new();
            $($( ast!(@scope $node [uses op_defs children] $span $nested ); )*)?
            Parsed::valid(Span::new($start, $len), Scope { uses, op_defs, children })
        };
        scope
    }};
    (@scope UseStmt [$uses:ident $op_defs:ident $children:ident] [$start:expr, $len:expr] ($( $node:ident $span:tt $( ($($nested:tt)*) )? )*)) => {
        $uses.push({
            let mut path = Vec::new();
            $( ast!(@use $node [path] $span $( ($($nested)*) )? ); )*
            Parsed::valid(Span::new($start, $len), UseStmt { path })
        });
    };
    (@use Ident [$path:ident] [$start:expr, $len:expr]) => {
        $path.push(Parsed::valid(Span::new($start, $len), Ident));
    };
    (@scope OpDef [$uses:ident $op_defs:ident $children:ident] [$start:expr, $len:expr] ($( $node:ident $span:tt $nested:tt )*)) => {
        $op_defs.push({
            let parts;
            let bindings;
            $( ast!(@op-def $node [parts bindings] $span $nested); )*
            Parsed::valid(Span::new($start, $len), OpDef {
                parts,
                bindings,
            })
        });
    };
    (@op-def OpParts [$parts:ident $bindings:ident] [$start:expr, $len:expr] ($( $node:ident $span:tt $( ($($nested:tt)*) )? )*)) => {
        $parts = {
            let mut parts = Vec::new();
            $( ast!(@op-parts $node [parts] $span $( ($($nested)*) )? ); )*
            Parsed::valid(Span::new($start, $len), OpParts(parts))
        };

    };
    (@op-parts Argument [$parts:ident] [$start:expr, $len:expr]) => {
        $parts.push(Parsed::valid(Span::new($start, $len), OpPart::Argument));
    };
    (@op-parts LazyArgument [$parts:ident] [$start:expr, $len:expr]) => {
        $parts.push(Parsed::valid(Span::new($start, $len), OpPart::LazyArgument));
    };
    (@op-parts Literal [$parts:ident] [$start:expr, $len:expr]) => {
        $parts.push(Parsed::valid(Span::new($start, $len), OpPart::Literal));
    };
    (@op-parts Variadic [$parts:ident] [$start:expr, $len:expr] ($( $node:ident $span:tt $nested:tt )+)) => {
        $parts.push({
            let mut parts = Vec::new();
            $( ast!(@op-part $node [parts _] $span $nested ); )+
            valid(span($start, $len), OpPart::Variadic(bounding(parts)))
        });
    };
    (@op-def OpBindings [$parts:ident $bindings:ident] [$start:expr, $len:expr] ($( $node:ident $span:tt $nested:tt )*)) => {
        $bindings = {
            let mut bindings = Vec::new();
            $( ast!(@op-bindings $node [bindings] $span $nested); )*
            Parsed::valid(Span::new($start, $len), OpBindings(bindings))
        };
    };
    (
        @op-bindings OpBinding [$bindings:ident] [$start:expr, $len:expr]
        (
            Ident [$lhs_from:expr, $lhs_len:expr]
            $arrow:ident [$arrow_from:expr, $arrow_len:expr]
            Ident [$rhs_from:expr, $rhs_len:expr]
        )
    ) => {
        $bindings.push({
            let lhs_span = Span::new($lhs_from, $lhs_len);
            let rhs_span = Span::new($rhs_from, $rhs_len);
            Parsed::valid(Span::new($start, $len), OpBinding {
                lhs: Parsed::valid(lhs_span, Ident),
                arrow: Parsed::valid(Span::new($arrow_from, $arrow_len), OpArrow::$arrow),
                rhs: Parsed::valid(rhs_span, Ident),
            })
        });
    };
    (@scope Scope [$uses:ident $op_defs:ident $children:ident] [$start:expr, $len:expr] ($( $node:ident $span:tt $nested:tt )*)) => {
        $children.push({
            let mut uses = Vec::new();
            let mut op_defs = Vec::new();
            let mut children = Vec::new();
            $( ast!(@scope $node [uses op_defs children] $span $nested); )*
            Parsed::valid(Span::new($start, $len), Scope { uses, op_defs, children })
        });
    };
}
pub(crate) use ast;
