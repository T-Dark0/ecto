#![allow(clippy::unwrap_or_default)]

use indexmap::IndexMap;
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote, quote_spanned};
use syn::{
    Error, Expr, ExprPath, Ident, Pat,
    ext::IdentExt,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    token::{Colon, Comma, FatArrow, If, Or},
};

#[proc_macro]
pub fn select(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match select2(input.into()) {
        Ok(out) => out.into(),
        Err(err) => err.into_compile_error().into(),
    }
}
fn select2(input: TokenStream) -> Result<TokenStream, Error> {
    let select = syn::parse2::<Select>(input)?;
    let parser_name = Ident::new("parser", select.parser.span());
    let parser = &select.parser;
    let tok_name = &select.tok;
    let expected_pat = &select.expected;
    let expected = dedup_expected(&select.arms);
    let arms = expand_arms(&parser_name, &select.arms);
    Ok(quote! {{
        let &mut ref mut #parser_name = #parser;
        let #tok_name = #parser_name.peek();
        let #expected_pat = &[#(#expected),*];
        match #tok_name.kind {
            #(#arms),*
        }
    }})
}
fn dedup_expected(arms: &Punctuated<Arm, Comma>) -> impl Iterator<Item = TokenStream> {
    let mut seen = IndexMap::new();
    for arm in arms.iter().filter(|arm| arm.validity == Validity::Valid) {
        visit_expected(&arm.expected, &mut seen)
    }
    seen.into_values()
}
fn visit_expected(arm: &Expected, seen: &mut IndexMap<String, TokenStream>) {
    match &arm.kind {
        ExpectedKind::Or(cases) => cases.iter().for_each(|case| visit_expected(case, seen)),
        ExpectedKind::Paren(expected) => visit_expected(expected, seen),
        ExpectedKind::Reference(expected) => visit_expected(expected, seen),
        ExpectedKind::Path(expr) => insert(expr, seen),
        ExpectedKind::Wild => (),
    }
}
fn insert<T: ToTokens>(node: T, seen: &mut IndexMap<String, TokenStream>) {
    let stream = node.to_token_stream();
    seen.insert(stream.to_string(), stream);
}
fn expand_arms(parser_name: &Ident, arms: &Punctuated<Arm, Comma>) -> impl Iterator<Item = TokenStream> {
    arms.iter().map(move |arm| {
        let expected = &arm.expected;
        let guard = arm.guard.as_ref().map(|g| quote! { if #g }).unwrap_or(quote! {});
        let body = &arm.body;
        let mode_action = match arm.mode {
            Mode::Next => quote! { #parser_name.next(); },
            Mode::Peek => quote! {},
        };
        quote! { #expected #guard => { #mode_action #body } }
    })
}

struct Select {
    parser: Expr,
    tok: Ident,
    expected: Pat,
    arms: Punctuated<Arm, Comma>,
}
struct Arm {
    mode: Mode,
    validity: Validity,
    expected: Expected,
    guard: Option<Expr>,
    body: Expr,
}
struct Expected {
    span: Span,
    kind: ExpectedKind,
}
enum ExpectedKind {
    Or(Vec<Expected>),
    Paren(Box<Expected>),
    Reference(Box<Expected>),
    Path(ExprPath),
    Wild,
}
enum Mode {
    Next,
    Peek,
}
#[derive(PartialEq, Eq)]
enum Validity {
    Recovery,
    Valid,
}
impl Parse for Select {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let parser = Expr::parse(input)?;
        Comma::parse(input)?;
        Or::parse(input)?;
        let tok = Ident::parse(input)?;
        Comma::parse(input)?;
        let expected = Pat::parse_single(input)?;
        Or::parse(input)?;
        let arms = Punctuated::parse_terminated(input)?;
        Ok(Self {
            parser,
            tok,
            expected,
            arms,
        })
    }
}
impl Parse for Arm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mode = Mode::parse(input)?;
        let validity = Validity::parse(input)?;
        Colon::parse(input)?;
        let expected = Expected::parse(input)?;
        let guard = If::parse(input).ok().map(|_| Expr::parse(input)).transpose()?;
        FatArrow::parse(input)?;
        let body = Expr::parse(input)?;
        Ok(Self {
            mode,
            validity,
            expected,
            guard,
            body,
        })
    }
}
impl Parse for Expected {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        fn limit_pat(pat: Pat) -> Result<Expected, Error> {
            let span = pat.span();
            let kind = match pat {
                Pat::Or(pat) => ExpectedKind::Or(pat.cases.into_iter().map(limit_pat).collect::<Result<_, _>>()?),
                Pat::Paren(pat) => ExpectedKind::Paren(Box::new(limit_pat(*pat.pat)?)),
                Pat::Path(path) => ExpectedKind::Path(path),
                Pat::Reference(pat) => ExpectedKind::Reference(Box::new(limit_pat(*pat.pat)?)),
                Pat::Wild(_) => ExpectedKind::Wild,
                pat => {
                    return Err(Error::new(
                        pat.span(),
                        "Expected a pattern all of whose leaves are paths or wildcards",
                    ));
                }
            };
            Ok(Expected { span, kind })
        }
        Pat::parse_multi_with_leading_vert(input).and_then(limit_pat)
    }
}
impl Parse for Mode {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        match Ident::parse(input).map(|id| id.to_string()).as_deref() {
            Ok("next") => Ok(Mode::Next),
            Ok("peek") => Ok(Mode::Peek),
            Ok(_) | Err(_) => Err(input.error("Expected `next` or `peek`")),
        }
    }
}
impl Parse for Validity {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        match Ident::parse_any(input).map(|id| id.to_string()).as_deref() {
            Ok("try") => Ok(Validity::Recovery),
            _ => Ok(Validity::Valid),
        }
    }
}

impl ToTokens for Expected {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let span = self.span;
        match &self.kind {
            ExpectedKind::Or(cases) => tokens.extend(quote_spanned! {span=> #(#cases)|*}),
            ExpectedKind::Paren(expected) => tokens.extend(quote_spanned! {span=> #expected}),
            ExpectedKind::Reference(expected) => tokens.extend(quote_spanned! {span=> #expected}),
            ExpectedKind::Path(expr) => tokens.extend(quote_spanned! {span=> #expr}),
            ExpectedKind::Wild => tokens.extend(quote_spanned! {span=> _}),
        }
    }
}
