use super::{
    context::{Context, ContextState, Hooks},
    end, start, Error, ErrorKind,
};
use crate::scope_tree::{
    ast::{
        Ident, OpArrow, OpBinding, OpBindings, OpDef, OpPart, OpParts, Outcome, Parsed, Scope,
        UseStmt,
    },
    lexer::{self, Token, TokenKind},
};

pub fn parse(input: &str) -> (Parsed<Scope>, Vec<Error>) {
    let lexed = lexer::lex(input);
    let mut state = ContextState::new(lexed.as_slice());
    let context = Context::new(&mut state);
    let parsed = parse_scope_contents(context.set_skip(skip_newline));
    (parsed, state.errors)
}

fn parse_scope_contents(context: Context<impl Hooks>) -> Parsed<Scope> {
    start("parse_scope_contents");
    let mut uses = Vec::new();
    let mut op_defs = Vec::new();
    let mut children = Vec::new();
    let out = spanning(context, |mut context| loop {
        let tok = context.peek();
        match tok.kind {
            TokenKind::Use => uses.push(parse_use(context.reborrow())),
            TokenKind::Fn => {
                if let Some(op_def) = parse_fn_for_op(context.reborrow()) {
                    op_defs.push(op_def)
                }
            }
            TokenKind::OpenParen => children.push(parse_scope(context.reborrow())),
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
    end("parse_scope_contents");
    out
}

fn parse_use(mut context: Context<impl Hooks>) -> Parsed<UseStmt> {
    start("parse_use");
    let mut path = Vec::new();
    let out = spanning(context.reborrow(), |mut context| {
        context.skip();
        let ident = parse_ident(context.reborrow());
        if ident.is_error() {
            context.error(ident.span, ErrorKind::EmptyUse);
            return Outcome::Error;
        }
        path.push(ident);
        loop {
            let separator = parse_token(context.reborrow(), TokenKind::Dot);
            if separator.is_error() {
                break;
            }

            let ident = parse_ident(context.reborrow());
            if ident.is_error() {
                break;
            }
            path.push(ident)
        }
        Outcome::Valid(UseStmt { path })
    });
    let newline = parse_token(context.reborrow(), TokenKind::Semicolon);
    if newline.is_error() {
        context.error(newline.span, ErrorKind::MissingSemiAfterUse)
    }
    end("parse_use");
    out
}

fn parse_fn_for_op(mut context: Context<impl Hooks>) -> Option<Parsed<OpDef>> {
    start("parse_fn_for_op");
    context.skip();
    parse_ident(context.reborrow());
    parse_token(context.reborrow(), TokenKind::OpenParen);
    let mut context = context.set_stop(stop_on_close_paren).set_skip(skip_nothing);
    let mut op_def = None;
    loop {
        let tok = context.peek();
        {
            let mut context = context
                .reborrow()
                .set_skip(skip_newline)
                .add_stop(stop_on_fn_arm_start);
            match tok.kind {
                TokenKind::Op => {
                    let def = parse_op_def(context.reborrow());
                    match op_def {
                        Some(_) => context.error(def.span, ErrorKind::DuplicateOperatorDefinition),
                        None => op_def = Some(def),
                    }
                }
                TokenKind::Colon => skip_fn_arm(context),
                TokenKind::Equals => skip_fn_arm(context),
                TokenKind::Eof => break,
                _ => {}
            }
        }
        context.skip();
    }
    end("parse_fn_for_op");
    op_def
}
fn parse_op_def(context: Context<impl Hooks>) -> Parsed<OpDef> {
    start("parse_op_def");
    let out = spanning(context, |mut context| {
        context.skip();
        let parts = parse_op_parts(context.reborrow().add_stop(stop_on_semicolon));
        let bindings = parse_op_bindings(context);
        Outcome::Valid(OpDef { parts, bindings })
    });
    end("parse_op_def");
    out
}
fn parse_op_parts(context: Context<impl Hooks>) -> Parsed<OpParts> {
    start("parse_op_parts");
    let mut parts = Vec::new();
    let out = spanning(context, |mut context| loop {
        let tok = context.peek();
        match tok.kind {
            TokenKind::Underscore => parts.push(parse_op_argument(context.reborrow())),
            TokenKind::Backslash => parts.push(parse_op_lazy_argument(context.reborrow())),
            TokenKind::NamePart => parts.push(parse_op_name_part(context.reborrow())),
            TokenKind::Star => {
                let part = parse_op_star(context.reborrow(), parts.pop());
                parts.push(part)
            }
            TokenKind::OpenParen => parts.push(parse_op_group(context.reborrow())),
            TokenKind::Eof => {
                context.skip();
                break Outcome::Valid(OpParts(parts));
            }
            _ => context.skip(),
        }
    });
    end("parse_op_parts");
    out
}
fn parse_op_argument(mut context: Context<impl Hooks>) -> Parsed<OpPart> {
    start("parse_op_argument");
    let underscore = context.next();
    let out = Parsed::valid(OpPart::Argument, underscore.span);
    end("parse_op_argument");
    out
}
fn parse_op_lazy_argument(mut context: Context<impl Hooks>) -> Parsed<OpPart> {
    start("parse_op_lazy_argument");
    let backslash = context.next();
    let next_tok = context.peek();
    let out = match next_tok.kind {
        TokenKind::Underscore => {
            context.skip();
            Parsed::valid(OpPart::LazyArgument, backslash.span.until(next_tok.span))
        }
        _ => {
            context.error(backslash.span, ErrorKind::LazyBeforeNonArg);
            Parsed::error(backslash.span)
        }
    };
    end("parse_op_lazy_argument");
    out
}
fn parse_op_name_part(mut context: Context<impl Hooks>) -> Parsed<OpPart> {
    start("parse_op_name_part");
    let literal = context.next();
    let out = Parsed::valid(OpPart::Literal, literal.span);
    end("parse_op_name_part");
    out
}
fn parse_op_star(
    mut context: Context<impl Hooks>,
    prev_tok: Option<Parsed<OpPart>>,
) -> Parsed<OpPart> {
    start("parse_op_star");
    let star = context.next();
    let Some(prev_tok) = prev_tok else {
        context.error(star.span, ErrorKind::RepetitionOfNothing);
        return Parsed::error(star.span);
    };
    let make_variadic = |part| {
        Parsed::valid(
            OpPart::Variadic(Parsed::valid(
                OpParts(vec![Parsed::valid(part, prev_tok.span)]),
                prev_tok.span,
            )),
            prev_tok.span.until(star.span),
        )
    };
    let out = match prev_tok.outcome {
        Outcome::Valid(OpPart::Argument) => make_variadic(OpPart::Argument),
        Outcome::Valid(OpPart::LazyArgument) => make_variadic(OpPart::LazyArgument),
        Outcome::Valid(OpPart::Literal) => make_variadic(OpPart::Literal),
        Outcome::Valid(OpPart::Variadic(_)) => {
            context.error(star.span, ErrorKind::DoubleRepetition);
            Parsed::error(star.span)
        }
        Outcome::Error => Parsed::error(prev_tok.span.until(star.span)),
    };
    end("parse_op_star");
    out
}
fn parse_op_group(context: Context<impl Hooks>) -> Parsed<OpPart> {
    start("parse_op_group");
    let out = spanning(context, |mut context| {
        context.skip();
        let parts = parse_op_parts(context.reborrow());
        let close_paren = parse_token(context.reborrow(), TokenKind::CloseParen);
        let star = parse_token(context.reborrow(), TokenKind::Star);

        if close_paren.is_error() {
            context.error(close_paren.span, ErrorKind::UnterminatedOpGroup);
        }
        if star.is_error() {
            context.error(star.span, ErrorKind::UnstarredOpGroup);
        }
        Outcome::Valid(OpPart::Variadic(parts))
    });
    end("parse_op_group");
    out
}
fn parse_op_bindings(context: Context<impl Hooks>) -> Parsed<OpBindings> {
    start("parse_op_bindings");
    let out = spanning(context, |mut context| {
        let mut bindings = Vec::new();
        loop {
            if context.peek().kind == TokenKind::Eof {
                break;
            }
            bindings.push(parse_op_binding(context.reborrow().add_stop(stop_on_comma)));
            if context.peek().kind == TokenKind::Eof {
                break;
            }
            parse_token(context.reborrow(), TokenKind::Comma);
        }
        Outcome::Valid(OpBindings(bindings))
    });
    end("parse_op_bindings");
    out
}
fn parse_op_binding(context: Context<impl Hooks>) -> Parsed<OpBinding> {
    end("parse_op_binding");
    let out = spanning(context, |mut context| {
        let lhs = parse_ident(context.reborrow());
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
                let arrow_span = context.state.last_span.until_exclusive(tok.span);
                context.error(arrow_span, ErrorKind::ExpectedArrow);
                Parsed::error(arrow_span)
            }
            _ => {
                context.error(tok.span, ErrorKind::InvalidArrow);
                Parsed::error(tok.span)
            }
        };
        let rhs = parse_ident(context.reborrow());
        Outcome::Valid(OpBinding { lhs, arrow, rhs })
    });
    start("parse_op_binding");
    out
}
fn skip_fn_arm(context: Context<impl Hooks>) {
    todo!()
}

fn parse_scope(context: Context<impl Hooks>) -> Parsed<Scope> {
    start("parse_scope");
    let out = spanning(context, |mut context| {
        context.skip();
        let contents = parse_scope_contents(context.reborrow().set_stop(stop_on_close_paren));
        let close_paren = parse_token(context.reborrow(), TokenKind::CloseParen);
        if close_paren.is_error() {
            context.error(close_paren.span, ErrorKind::UnclosedScope);
        }
        contents.outcome
    });
    end("parse_scope");
    out
}

fn parse_ident(context: Context<impl Hooks>) -> Parsed<Ident> {
    parse_token(context, TokenKind::Identifier).map(|()| Ident)
}
fn parse_token(mut context: Context<impl Hooks>, expected: TokenKind) -> Parsed<()> {
    start(format_args!("parse_token {expected:?}"));
    let Token { kind, span } = context.peek();
    let outcome = match kind == expected {
        true => {
            context.skip();
            Outcome::Valid(())
        }
        false => Outcome::Error,
    };
    end(format_args!("parse_token {expected:?}"));
    Parsed { outcome, span }
}
fn spanning<'lexed, H, F, R>(mut context: Context<'_, 'lexed, H>, f: F) -> Parsed<R>
where
    H: Hooks,
    F: FnOnce(Context<'_, 'lexed, H>) -> Outcome<R>,
{
    let first_span = context.peek().span;
    let outcome = f(context.reborrow());
    let last_span = context.state.last_span;
    let span = if last_span.start < first_span.start {
        last_span.until_exclusive(first_span)
    } else {
        first_span.until(last_span)
    };
    Parsed { outcome, span }
}

fn stop_on_close_paren(tokens: &[TokenKind]) -> bool {
    tokens.starts_with(&[TokenKind::CloseParen])
}
fn stop_on_semicolon(tokens: &[TokenKind]) -> bool {
    tokens.starts_with(&[TokenKind::Semicolon])
}
fn stop_on_fn_arm_start(tokens: &[TokenKind]) -> bool {
    if let Some(rest) = tokens.strip_prefix(&[TokenKind::NewLine]) {
        if [TokenKind::Op, TokenKind::Colon, TokenKind::Equals]
            .into_iter()
            .any(|t| rest.starts_with(&[t]))
        {
            return true;
        }
    }
    return false;
}
fn stop_on_comma(tokens: &[TokenKind]) -> bool {
    tokens.starts_with(&[TokenKind::Comma])
}
fn stop_never(tokens: &[TokenKind]) -> bool {
    false
}

fn skip_newline(token: TokenKind) -> bool {
    token == TokenKind::NewLine
}
fn skip_nothing(_: TokenKind) -> bool {
    false
}
