#![cfg(test)]

use super::render::render;
use crate::scope_tree::ast::ast;
use pretty_assertions::assert_eq;

#[test]
fn basic_prettyprint() {
    let got = render(ast! { Scope[0, 10](UseStmt[1, 9](Ident[1, 9])) });
    let expected = "Scope[0, 10](UseStmt[1, 9](Ident[1, 9]))";
    assert_eq!(expected, got)
}
