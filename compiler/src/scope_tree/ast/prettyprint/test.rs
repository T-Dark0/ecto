#![cfg(test)]

use super::{parse, render};
use crate::test_util::literal;
use pretty_assertions::assert_eq;

#[test]
fn all_inline() {
    test("Scope[0, 10](UseStmt[1, 5](Ident[5, 5]))")
}

#[test]
fn scope_with_two_uses() {
    test(
        "
            Scope[0, 0](
                UseStmt[2, 2](Ident[3, 3])
                UseStmt[4, 4](Ident[5, 5] Ident[6, 6])
            )
        ",
    )
}

#[test]
fn one_of_each() {
    test(
        "
            Scope[0, 0](
                UseStmt[2, 2](Ident[3, 3] Ident[4, 4])
                OpDef[5, 5](
                    OpParts[6, 6](Argument[7, 7] Literal[8, 8] Argument[9, 9])
                    OpBindings[10, 10](OpBinding[11, 11](Ident[12, 12] OpArrowLeft[13, 13] Ident[14, 14]))
                )
                Scope[15, 15](UseStmt[17, 17](Ident[18, 18] Ident[19, 19] Ident[20, 20]))
            )
        ",
    )
}

#[test]
fn empty_scope_has_parens() {
    test("Scope[0, 0]()")
}

fn test(text: &str) {
    let expected = literal(text);
    let ast = match parse(text) {
        Ok(ast) => ast,
        Err(e) => {
            let range = e.span.to_usize_range();
            let start = range.start.saturating_sub(20);
            let end = Ord::clamp(range.end + 20, 0, text.len());
            panic!(
                "{:#?}\n`{}`\n{}{}",
                e,
                &text[start..end],
                " ".repeat(range.start - start),
                "^".repeat(range.len()),
            )
        }
    };
    let got = render(ast);
    assert_eq!(expected, got)
}
