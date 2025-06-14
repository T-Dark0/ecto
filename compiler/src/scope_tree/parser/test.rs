#![cfg(test)]
use crate::scope_tree::{
    ast::{
        ast, Ident, OpArrow, OpBinding, OpBindings, OpDef, OpPart, OpParts, Parsed, Scope, UseStmt,
    },
    parser::{parse::parse, Error},
};
use pretty_assertions::assert_eq;

#[test]
fn scoped_use() {
    test(
        r#"
        (
            use foo.bar;
        )
        "#,
        ast!(
            Scope[0,20](
                Scope[0,20](
                    UseStmt[6,12](Ident[10,3] Ident[14,3])
                )
            )
        ),
        [],
    )
}
#[test]
fn many_scopes() {
    test(
        r#"
        (
            (
                use a;
            )
            use aa;
        )
        (
            use aaa;
        )
        "#,
        ast!(
            Scope[0, 59](
                Scope[0, 42](
                    Scope[6, 22](
                        UseStmt[16, 6](Ident[20, 1])
                    )
                    UseStmt[33, 7](Ident[37, 2])
                )
                Scope[43, 16](
                    UseStmt[49, 8](Ident[53, 3])
                )
            )
        ),
        [],
    )
}
#[test]
fn fn_if() {
    test(
        r#"
            fn if_then_else (
                op "if" _ "then" \_ "else" \_
                : Bool (Lazy a) (Lazy a) -> a
                = True then _ => then ()
                = False  _ else => else ()
            )
            "#,
        ast!(
            Scope[0, 147](
                OpDef[22, 29](
                    OpParts[25, 26](Literal[25, 4] Argument[30, 1] Literal[32, 6] LazyArgument[39, 2] Literal[42, 6] LazyArgument[49, 2])
                    OpBindings[51, 0]()
                )
            )
        ),
        [],
    )
}
#[test]
fn functions_close_properly() {
    test(
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
        ast!(
            Scope[0, 193](
                OpDef[128, 10](
                    OpParts[131, 7](Argument[131, 1] Literal[133, 3] Argument[137, 1])
                    OpBindings[138, 0]()
                )
            )
        ),
        [],
    )
}
#[test]
fn one_of_each() {
    test(
        r#"
        use foo.bar;
        fn add (
            op _ "+" _; add <- add
        )
        (
            use super.add;
        )
        "#,
        ast!(
            Scope[0, 73](
                UseStmt[0, 12](Ident[4, 3] Ident[8, 3])
                OpDef[26, 22](
                    OpParts[29, 7](Argument[29, 1] Literal[31, 3] Argument[35, 1])
                    OpBindings[38, 10](OpBinding[38, 10](Ident[38, 3] Left[42, 2] Ident[45, 3]))
                )
                Scope[51, 22](
                    UseStmt[57, 14](Ident[61, 5] Ident[67, 3])
                )
            )
        ),
        [],
    )
}
#[test]
fn multi_bindings() {
    test(
        r#"
        fn add (
            op _ "+" _; add <- add, add -> mul
        )
        "#,
        ast!(
            Scope[0, 49](
                OpDef[13, 34](
                    OpParts[16, 7](Argument[16, 1] Literal[18, 3] Argument[22, 1])
                    OpBindings[25, 22](
                        OpBinding[25, 10](Ident[25, 3] Left[29, 2] Ident[32, 3])
                        OpBinding[37, 10](Ident[37, 3] Right[41, 2] Ident[44, 3])

                    )
                )
            )
        ),
        [],
    )
}
#[test]
fn nested_fn_decl() {
    test(
        r#"
        fn foo (
            : Int Int -> Int
            = a b => (
                fn bar (
                    : Int Int -> Int
                    = a b => a + b
                )
                bar a b
            )
        )
        "#,
        ast!(
            Scope[0, 151]
        ),
        [],
    )
}

fn test<const N: usize>(source: &str, expected_parse: Parsed<Scope>, expected_errors: [Error; N]) {
    let (got, errors) = parse(&input(source));
    assert_eq!(got, expected_parse);
    assert_eq!(errors, expected_errors);
}

fn input(str: &str) -> String {
    let indent = str
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| line.len() - line.trim_start().len())
        .min()
        .unwrap_or(0);
    str.lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| line[indent..].trim_end())
        .collect::<Vec<_>>()
        .join("\n")
}
