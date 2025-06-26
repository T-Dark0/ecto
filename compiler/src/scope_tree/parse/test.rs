#![cfg(test)]

use crate::{scope_tree::ast::prettyprint, test_util::literal};
use pretty_assertions::assert_eq;

#[test]
fn parse_use() {
    test(
        "ScopeContents[0, 16](UseStmt[0, 16](Ident[4, 3] Ident[8, 3] Ident[12, 3]))",
        "use foo.bar.owo,",
    )
}

#[test]
fn parse_fn_for_op() {
    test(
        "
            ScopeContents[0, 91](OpDef[12, 22](
                OpParts[15, 7](Argument[15, 1] Literal[17, 3] Argument[21, 1])
                OpBindings[24, 10](OpBinding[24, 10](Ident[24, 3] OpArrowRight[28, 2] Ident[31, 3]))
            ))
        ",
        r#"
            fn add(
                op _ "+" _; add -> mul
                : Int Int -> Int
                = a b => intrinsics.add(a, b)
            )
        "#,
    )
}

#[test]
fn parse_empty_scope() {
    test(
        "ScopeContents[0, 2](Scope[0, 2](ScopeContents[1, 0]()))",
        "()",
    )
}

fn test(ast: &str, syntax: &str) {
    let expected = prettyprint::parse(ast).unwrap();
    let (got, errors) = super::parse(&literal(syntax));
    assert_eq!(expected, got);
    assert_eq!(errors, []);
}
