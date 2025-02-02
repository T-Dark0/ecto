use super::{
    ast::{
        Ident, OpArrow, OpBinding, OpBindings, OpDef, OpPart, OpParts, Outcome, Parsed, Scope,
        UseStmt,
    },
    lexer::{self, Token, TokenKind},
    Span,
};
use std::slice;
use strum::EnumCount;

pub fn parse(input: &str) -> Parsed<Scope> {
    let tokens = lexer::lex(input);
    let mut context = Context::new(&tokens);
    parse_scope_contents(&mut context)
}
fn parse_scope_contents(context: &mut Context<'_>) -> Parsed<Scope> {
    println!("parse_scope_contents start");
    let mut uses = Vec::new();
    let mut op_defs = Vec::new();
    let mut children = Vec::new();
    let out = spanning(context, |context| loop {
        let tok = context.peek();
        match tok.kind {
            TokenKind::Use => uses.push(parse_use(context)),
            TokenKind::Fn => {
                if let Some(op_def) = parse_fn_for_op(context) {
                    op_defs.push(op_def)
                }
            }
            TokenKind::OpenParen => children.push(parse_scope(context)),
            TokenKind::Eof => {
                break Outcome::Valid(Scope {
                    uses,
                    op_defs,
                    children,
                })
            }
            _ => {
                context.skip();
            }
        }
    });
    println!("parse_scope_contents end");
    out
}

fn parse_use(context: &mut Context<'_>) -> Parsed<UseStmt> {
    println!("parse_use start");
    let mut path = Vec::new();
    let out = spanning(context, |parser| {
        parser.skip();
        let ident = parse_ident(parser);
        if ident.is_error() {
            parser.error(ident.span, ErrorKind::EmptyUse);
            return Outcome::Error;
        }
        path.push(ident);
        loop {
            let separator = parse_token(parser, TokenKind::Dot);
            if separator.is_error() {
                break;
            }

            let ident = parse_ident(parser);
            if ident.is_error() {
                break;
            }
            path.push(ident)
        }
        Outcome::Valid(UseStmt { path })
    });
    let newline = parse_token(context, TokenKind::NewLine);
    if newline.is_error() {
        context.error(newline.span, ErrorKind::MissingNewLineAfterUse)
    }
    println!("parse_use end");
    out
}

fn parse_fn_for_op(context: &mut Context<'_>) -> Option<Parsed<OpDef>> {
    println!("parse_fn_for_op start");
    context.skip();
    parse_ident(context);
    parse_token(context, TokenKind::OpenParen);
    let out = stopping(context, [TokenKind::CloseParen], |context| {
        use FakeTokenKind as F;
        let stop = [F::NewLineOp, F::NewLineColon, F::NewLineEquals];
        let op_def = stopping(context, stop, |context| loop {
            let tok = context.peek();
            match tok.kind {
                TokenKind::Op => break Some(parse_op_def(context)),
                TokenKind::Colon => skip_fn_arm(context),
                TokenKind::Equals => skip_fn_arm(context),
                TokenKind::Eof => break None,
                _ => context.skip(),
            };
        });
        while context.next().kind != TokenKind::Eof {}
        op_def
    });
    println!("parse_fn_for_op end");
    out
}
fn parse_op_def(context: &mut Context<'_>) -> Parsed<OpDef> {
    println!("parse_op_def start");
    let out = spanning(context, |context| {
        let parts = stopping(context, [TokenKind::Semicolon], parse_op_parts);
        let bindings = parse_op_bindings(context);
        Outcome::Valid(OpDef { parts, bindings })
    });
    println!("parse_op_def end");
    out
}
fn parse_op_parts(context: &mut Context<'_>) -> Parsed<OpParts> {
    println!("parse_op_parts start");
    let mut parts = Vec::new();
    let out = spanning(context, |context| loop {
        let tok = context.peek();
        match tok.kind {
            TokenKind::Underscore => parts.push(parse_op_argument(context)),
            TokenKind::Backslash => parts.push(parse_op_lazy_argument(context)),
            TokenKind::NamePart => parts.push(parse_op_name_part(context)),
            TokenKind::Star => {
                let part = parse_op_star(context, parts.pop());
                parts.push(part)
            }
            TokenKind::OpenParen => parts.push(parse_op_group(context)),
            TokenKind::Semicolon => {
                context.skip();
                break Outcome::Valid(OpParts(parts));
            }
            TokenKind::NewLine | TokenKind::CloseParen | TokenKind::Eof => {
                break Outcome::Valid(OpParts(parts))
            }
            _ => {}
        }
    });
    println!("parse_op_parts end");
    out
}
fn parse_op_argument(context: &mut Context<'_>) -> Parsed<OpPart> {
    println!("parse_op_argument start");
    let underscore = context.next().span;
    let out = Parsed::valid(OpPart::Argument, underscore);
    println!("parse_op_argument end");
    out
}
fn parse_op_lazy_argument(parser: &mut Context<'_>) -> Parsed<OpPart> {
    println!("parse_op_lazy_argument start");
    let backslash = parser.next().span;
    let next_tok = parser.peek();
    let out = match next_tok.kind {
        TokenKind::Underscore => {
            parser.skip();
            Parsed::valid(OpPart::LazyArgument, backslash.until(next_tok.span))
        }
        _ => {
            parser.error(backslash, ErrorKind::LazyBeforeNonArg);
            Parsed::error(backslash)
        }
    };
    println!("parse_op_lazy_argument end");
    out
}
fn parse_op_name_part(context: &mut Context<'_>) -> Parsed<OpPart> {
    println!("parse_op_name_part start");
    let literal = context.next().span;
    let out = Parsed::valid(OpPart::Literal, literal);
    println!("parse_op_name_part end");
    out
}
fn parse_op_star(context: &mut Context<'_>, prev_tok: Option<Parsed<OpPart>>) -> Parsed<OpPart> {
    println!("parse_op_star start");
    let star = context.next().span;
    let Some(prev_tok) = prev_tok else {
        context.error(star, ErrorKind::RepetitionOfNothing);
        return Parsed::error(star);
    };
    let make_variadic = |part| {
        Parsed::valid(
            OpPart::Variadic(Parsed::valid(
                OpParts(vec![Parsed::valid(part, prev_tok.span)]),
                prev_tok.span,
            )),
            prev_tok.span.until(star),
        )
    };
    let out = match prev_tok.outcome {
        Outcome::Valid(OpPart::Argument) => make_variadic(OpPart::Argument),
        Outcome::Valid(OpPart::LazyArgument) => make_variadic(OpPart::LazyArgument),
        Outcome::Valid(OpPart::Literal) => make_variadic(OpPart::Literal),
        Outcome::Valid(OpPart::Variadic(_)) => {
            context.error(star, ErrorKind::DoubleRepetition);
            Parsed::error(star)
        }
        Outcome::Error => Parsed::error(prev_tok.span.until(star)),
    };
    println!("parse_op_star end");
    out
}
fn parse_op_group(context: &mut Context<'_>) -> Parsed<OpPart> {
    println!("parse_op_group start");
    let out = spanning(context, |context| {
        let parts = parse_op_parts(context);
        let close_paren = parse_token(context, TokenKind::CloseParen);
        let star = parse_token(context, TokenKind::Star);

        if close_paren.is_error() {
            context.error(close_paren.span, ErrorKind::UnterminatedOpGroup);
        }
        if star.is_error() {
            context.error(star.span, ErrorKind::UnstarredOpGroup);
        }
        Outcome::Valid(OpPart::Variadic(parts))
    });
    println!("parse_op_group end");
    out
}
fn parse_op_bindings(context: &mut Context<'_>) -> Parsed<OpBindings> {
    println!("parse_op_bindings start");
    println!("parse_op_bindings end");
    todo!()
}
fn parse_op_binding(context: &mut Context<'_>) -> Parsed<OpBinding> {
    println!("parse_op_binding start");
    let out = spanning(context, |context| {
        let lhs = parse_ident(context);
        let tok = context.peek();
        let arrow = match tok.kind {
            TokenKind::LeftArrow => {
                context.skip();
                Parsed::valid(OpArrow::Left, tok.span)
            }
            TokenKind::RightArrow => {
                context.skip();
                Parsed::valid(OpArrow::Right, tok.span)
            }
            TokenKind::Identifier => {
                let arrow_span = context.last_span.until_exclusive(tok.span);
                context.error(arrow_span, ErrorKind::ExpectedArrow);
                Parsed::error(arrow_span)
            }
            _ => {
                context.error(tok.span, ErrorKind::InvalidArrow);
                Parsed::error(tok.span)
            }
        };
        let rhs = parse_ident(context);
        Outcome::Valid(OpBinding { lhs, arrow, rhs })
    });
    println!("parse_op_binding end");
    out
}
fn skip_fn_arm(context: &mut Context<'_>) {
    todo!()
}

fn parse_scope(context: &mut Context<'_>) -> Parsed<Scope> {
    println!("parse_scope start");
    let out = spanning(context, |context| {
        context.skip();
        let contents = stopping(context, [TokenKind::CloseParen], parse_scope_contents);
        let close_paren = parse_token(context, TokenKind::CloseParen);
        if close_paren.is_error() {
            context.error(close_paren.span, ErrorKind::UnclosedScope);
        }
        contents.outcome
    });
    println!("parse_scope end");
    out
}

fn parse_ident(context: &mut Context<'_>) -> Parsed<Ident> {
    parse_token(context, TokenKind::Identifier).map(|()| Ident)
}
fn parse_token(context: &mut Context<'_>, expected: TokenKind) -> Parsed<()> {
    println!("parse_token {expected:?} start");
    let Token { kind, span } = context.peek();
    let outcome = match kind == expected {
        true => {
            context.skip();
            Outcome::Valid(())
        }
        false => Outcome::Error,
    };
    println!("parse_token {expected:?} end");
    Parsed { outcome, span }
}
fn spanning<F, R>(context: &mut Context<'_>, f: F) -> Parsed<R>
where
    F: FnOnce(&mut Context<'_>) -> Outcome<R>,
{
    let first_span = context.peek().span;
    let outcome = f(context);
    let last_span = context.last_span;
    let span = if last_span.start < first_span.start {
        last_span.until_exclusive(first_span)
    } else {
        first_span.until(last_span)
    };
    Parsed { outcome, span }
}
fn stopping<F, R, S, const N: usize>(context: &mut Context<'_>, on: [S; N], f: F) -> R
where
    F: FnOnce(&mut Context<'_>) -> R,
    S: Into<FakeTokenKind>,
{
    let on = on.map(Into::into);
    on.iter().for_each(|&on| context.add_stop_on(on));
    let out = f(context);
    on.iter().for_each(|&on| context.remove_stop_on(on));
    out
}

struct Context<'tokens> {
    tokens: slice::Iter<'tokens, Token>,
    errors: Vec<Error>,
    last_span: Span,
    stop_counters: [u64; FakeTokenKind::COUNT],
}
impl<'tokens> Context<'tokens> {
    fn new(source: &'tokens [Token]) -> Self {
        Self {
            tokens: source.iter(),
            errors: Vec::new(),
            last_span: Span { start: 0, len: 0 },
            stop_counters: [0; FakeTokenKind::COUNT],
        }
    }
    fn next(&mut self) -> Token {
        if self.is_at_stop() {
            return self.eof();
        }
        let tok = match self.tokens.next() {
            Some(&tok) => tok,
            None => self.eof(),
        };
        self.last_span = tok.span;
        tok
    }
    fn peek(&self) -> Token {
        if self.is_at_stop() {
            return self.eof();
        }
        let tok = match self.tokens.clone().next() {
            Some(&tok) => tok,
            None => self.eof(),
        };
        tok
    }
    fn skip(&mut self) {
        self.next();
    }
    fn error(&mut self, span: Span, data: ErrorKind) {
        self.errors.push(Error { span, data })
    }
    fn add_stop_on(&mut self, fake: FakeTokenKind) {
        self.stop_counters[fake as usize] += 1;
    }
    fn remove_stop_on(&mut self, fake: FakeTokenKind) {
        self.stop_counters[fake as usize] -= 1;
    }
    fn is_at_stop(&self) -> bool {
        let fake = FakeTokenKind::from(self.tokens.as_slice());
        self.stop_counters[fake as usize] > 0
    }
    fn eof(&self) -> Token {
        Token {
            kind: TokenKind::Eof,
            span: self.last_span.empty_after(),
        }
    }
}
make_fake_token_kind! {
    Fn Op Equals Colon Identifier Underscore NamePart Backslash Star Semicolon LeftArrow RightArrow
    Comma Use Dot NewLine OpenParen CloseParen Eof Error;
    NewLineOp = [NewLine Op]
    NewLineColon = [NewLine Colon]
    NewLineEquals = [NewLine Equals]
}

macro_rules! make_fake_token_kind {
    ($( $real:ident )* ; $( $fake:ident = [$($part:ident)*] )*) => {
        #[derive(Clone, Copy, EnumCount)]
        enum FakeTokenKind {
            $($real,)*
            $($fake,)*
        }
        impl From<TokenKind> for FakeTokenKind {
            fn from(source: TokenKind) -> Self {
                match source {
                    $( TokenKind::$real => FakeTokenKind::$real, )*
                }
            }
        }
        impl<'a> From<&'a [Token]> for FakeTokenKind {
            fn from(source: &'a [Token]) -> Self {
                match source{
                    $( [$(t!(TokenKind::$part),)* ..] => FakeTokenKind::$fake, )*
                    $( [t!(TokenKind::$real), ..] => FakeTokenKind::$real, )*
                    [] => FakeTokenKind::Eof,
                }
            }
        }
    };
}
use make_fake_token_kind;

macro_rules! t {
    ($kind:pat) => {
        Token {
            kind: $kind,
            span: _,
        }
    };
}
use t;

#[derive(Debug, PartialEq, Eq)]
struct Error {
    span: Span,
    data: ErrorKind,
}
#[derive(Debug, PartialEq, Eq)]
enum ErrorKind {
    EmptyUse,
    MissingNewLineAfterUse,
    UnclosedFnDecl,
    LazyBeforeNonArg,
    RepetitionOfNothing,
    DoubleRepetition,
    UnterminatedOpGroup,
    UnstarredOpGroup,
    InvalidArrow,
    ExpectedArrow,
    UnclosedScope,
}
impl Error {
    fn new(span: Span, data: ErrorKind) -> Self {
        Self { span, data }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::scope_tree::lexer;
    use pretty_assertions::assert_eq;

    #[test]
    fn scoped_use() {
        let input = input(
            r#"
            (
                use foo.bar
            )
            "#,
        );
        let mut parser = Context::new(&input);
        let got = parse_scope_contents(&mut parser);
        let expected = ast!(
            Scope[0, 19] (
                Scope[0, 19] (
                    UseStmt[6, 11](Ident[10, 3] Ident[14, 3])
                )
            )
        );
        assert_eq!(expected, got);
        assert_eq!(parser.errors, []);
    }

    #[test]
    fn fn_if() {
        let input = input(
            r#"
            fn if_then_else (
                op "if" _ "then" \_ "else" \_
                : Bool (Lazy a) (Lazy a) -> a
                = True then _ => then ()
                = False  _ else => else ()
            )
            "#,
        );
        let mut parser = Context::new(&input);
        let got = parse_scope_contents(&mut parser);
        let expected = ast!(
            Scope[0, 147](
                OpDef[22, 29](
                    OpParts[25, 26](Literal[25, 4] Argument[30, 1] Literal[32, 6] LazyArgument[39, 2] Literal[42, 6] LazyArgument[49, 2])
                    OpBindings[51, 0]()
                )
            )
        );
        assert_eq!(expected, got);
        assert_eq!(parser.errors, []);
    }

    #[test]
    fn functions_close_properly() {
        let input = input(
            r#"
            fn while (
                : (Lazy Bool) (Lazy ()) -> ()
                = cond body => if cond () then body (); while cond body else ()
            )
            fn add (
                op _ "+" _
                : Num Num -> Num
                = a b => intrinsics.add a b
            )
            "#,
        );
        let mut parser = Context::new(&input);
        let got = parse_scope_contents(&mut parser);
        let expected = ast!(
            Scope[0, 27](
                OpDef[0, 10](
                    OpParts[0, 0](Argument[0, 0] Literal[0, 0] Argument[0, 0])
                    OpBindings[0, 0]()
                )
            )
        );
        assert_eq!(expected, got);
        assert_eq!(parser.errors, []);
    }

    #[test]
    fn one_of_each() {
        let input = input(
            r#"
            use foo.bar
            fn add (
                op _ "+" _; add <- add
            )
            (
                use super.add
            )
            "#,
        );
        let mut parser = Context::new(&input);
        let got = parse_scope_contents(&mut parser);
        let expected = ast!(
            Scope[0, 71](
                UseStmt[0, 11](Ident[4, 3] Ident[8, 3])
                OpDef[25, 22](
                    OpParts[28, 8](Argument[28, 1] Literal[30, 3] Argument[34, 1])
                    OpBindings[37, 10](OpBinding[40, 9](Ident[37, 3] Left[41, 2] Ident[44, 3]))
                )
                Scope[50, 21](
                    UseStmt[56, 13](Ident[60, 5] Ident[66, 3])
                )
            )
        );
        assert_eq!(expected, got);
        assert_eq!(parser.errors, []);
    }

    fn input(str: &str) -> Vec<Token> {
        let indent = str
            .lines()
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.len() - line.trim_start().len())
            .min()
            .unwrap_or(0);
        let str = str
            .lines()
            .filter(|line| !line.trim().is_empty())
            .map(|line| line[indent..].trim_end())
            .collect::<Vec<_>>()
            .join("\n");
        lexer::lex(&str)
    }

    macro_rules! ast {
        (Scope [$start:expr, $len:expr] ($( $node:ident $span:tt $nested:tt )*)) => {{
            #[allow(unused_mut, clippy::vec_init_then_push)]
            let scope = {
                let mut uses = Vec::new();
                let mut op_defs = Vec::new();
                let mut children = Vec::new();
                $( ast!(@scope $node [uses op_defs children] $span $nested ); )*
                valid(span($start, $len), Scope { uses, op_defs, children })
            };
            scope
        }};
        (@scope UseStmt [$uses:ident $op_defs:ident $children:ident] [$start:expr, $len:expr] ($( $node:ident $span:tt $( ($($nested:tt)*) )? )*)) => {
            $uses.push({
                let mut path = Vec::new();
                $( ast!(@use $node [path] $span $( ($($nested)*) )? ); )*
                valid(span($start, $len), UseStmt { path })
            });
        };
        (@use Ident [$path:ident] [$start:expr, $len:expr]) => {
            $path.push(valid(span($start, $len), Ident));
        };
        (@scope OpDef [$uses:ident $op_defs:ident $children:ident] [$start:expr, $len:expr] ($( $node:ident $span:tt $nested:tt )*)) => {
            $op_defs.push({
                let parts;
                let bindings;
                $( ast!(@op-def $node [parts bindings] $span $nested); )*
                valid(span($start, $len), OpDef {
                    parts,
                    bindings,
                })
            });
        };
        (@op-def OpParts [$parts:ident $bindings:ident] [$start:expr, $len:expr] ($( $node:ident $span:tt $( ($($nested:tt)*) )? )*)) => {
            $parts = {
                let mut parts = Vec::new();
                $( ast!(@op-parts $node [parts] $span $( ($($nested)*) )? ); )*
                valid(span($start, $len), OpParts(parts))
            };

        };
        (@op-parts Argument [$parts:ident] [$start:expr, $len:expr]) => {
            $parts.push(valid(span($start, $len), OpPart::Argument));
        };
        (@op-parts LazyArgument [$parts:ident] [$start:expr, $len:expr]) => {
            $parts.push(valid(span($start, $len), OpPart::LazyArgument));
        };
        (@op-parts Literal [$parts:ident] [$start:expr, $len:expr]) => {
            $parts.push(valid(span($start, $len), OpPart::Literal));
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
                valid(span($start, $len), OpBindings(bindings))
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
                let lhs_span = span($lhs_from, $lhs_len);
                let rhs_span = span($rhs_from, $rhs_len);
                let binding_span = lhs_span.until(rhs_span);
                valid(binding_span, OpBinding {
                    lhs: valid(lhs_span, Ident),
                    arrow: valid(span($arrow_from, $arrow_len), OpArrow::$arrow),
                    rhs: valid(rhs_span, Ident),
                })
            });
        };
        (@scope Scope [$uses:ident $op_defs:ident $children:ident] [$start:expr, $len:expr] ($( $node:ident $span:tt $nested:tt )*)) => {
            $children.push({
                let mut uses = Vec::new();
                let mut op_defs = Vec::new();
                let mut children = Vec::new();
                $( ast!(@scope $node [uses op_defs children] $span $nested); )*
                valid(span($start, $len), Scope { uses, op_defs, children })
            });
        };
    }
    use ast;

    fn span(start: u32, len: u16) -> Span {
        Span { start, len }
    }
    fn valid<T>(span: Span, data: T) -> Parsed<T> {
        Parsed::valid(data, span)
    }
}
