#![cfg(test)]

use super::parse::{Error, ErrorKind};
use crate::{
    parsed::Span,
    scope_tree::{lex::TokenKind, prettyprint},
    test_util::literal,
};
use pretty_assertions::Comparison;

#[test]
fn parse_nothing() {
    test("Scope[0, 0]()", "")
}

#[test]
fn parse_use() {
    test(
        "Scope[0, 16](UseStmt[0, 16](Ident[4, 3] Ident[8, 3] Ident[12, 3]))",
        "use foo.bar.owo,",
    )
}

#[test]
fn parse_fn_def() {
    test(
        "
            Scope[0, 91](FnDef[0, 91](
                Ident[3, 3]
                OpDef[12, 22](
                    OpParts[15, 7](Argument[15, 1] Literal[17, 3] Argument[21, 1])
                    OpBindings[24, 10](OpBinding[24, 10](Ident[24, 3] OpArrowRight[28, 2] Ident[31, 3]))
                )
                FnBody[60, 29](
                    Ident[62, 1] 
                    Ident[64, 1] 
                    Scope[69, 20](Scope[83, 6]())
                )
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
    test("Scope[0, 2](Scope[0, 2]())", "()")
}

#[test]
fn parse_one_of_each() {
    test(
        "
            Scope[0, 142](
                UseStmt[0, 16](Ident[4, 3] Ident[8, 3] Ident[12, 3])
                FnDef[18, 102](
                    Ident[21, 3]
                    OpDef[30, 33](
                        OpParts[33, 7](Argument[33, 1] Literal[35, 3] Argument[39, 1])
                        OpBindings[42, 21](
                            OpBinding[42, 10](Ident[42, 3] OpArrowLeft[46, 2] Ident[49, 3])
                            OpBinding[54, 9](Ident[54, 2] OpArrowRight[57, 2] Ident[60, 3])
                        )
                    )
                    FnBody[89, 29](
                        Ident[91, 1]
                        Ident[93, 1]
                        Scope[98, 20](Scope[112, 6]())
                    )
                )
                Scope[122, 20](
                    UseStmt[128, 12](Ident[132, 5] Ident[138, 1])
                )
            )
        ",
        r#"
            use std.num.mul,
            
            fn add(
                op _ "+" _; mul <- add, eq -> add
                : Int Int -> Int
                = a b => intrinsics.add(a, b)
            )

            (
                use super.a,
            )
        "#,
    )
}

#[test]
fn scope_with_irrelevant_syntax() {
    test(
        "
            Scope[0, 73](
                UseStmt[0, 12](Ident[4, 3] Ident[8, 3])
                Scope[13, 7]()
                Scope[37, 23]()
                UseStmt[61, 12](Ident[65, 3] Ident[69, 3])
            )
        ",
        "
            use foo.bar,
            (a + b).print
            struct Foo(owo: U32, uwu: String)
            use owo.uwu,
        ",
    )
}

#[test]
fn unclosed_scope() {
    test_errors(
        "Scope[0, 1](Scope*[0, 1]())",
        "(",
        &[Error {
            span: Span::new(1, 0),
            kind: ErrorKind::UnexpectedToken {
                expected: vec![TokenKind::CloseParen],
                got: TokenKind::Eof,
            },
        }],
    )
}
#[test]
fn list_operator() {
    test(
        "
            Scope[0, 110](
                FnDef[0, 110](
                    Ident[3, 4]
                    OpDef[14, 13](
                        OpParts[17, 10](
                            Literal[17, 3] 
                            Variadic[21, 2](OpParts[21, 1](Argument[21, 1]))
                            Literal[24, 3]
                        )
                        OpBindings[27, 0]()
                    )
                    FnBody[50, 15](Scope[55, 10]())
                    FnBody[70, 38](
                        Ident[72, 4]
                        Ident[77, 4]
                        Scope[85, 23]()
                    )
                )
            )
        ",
        r#"
            fn list (
                op "[" _* "]"
                : a -> List a
                = => List.empty
                = init last => list |&mut> append last
            )
        "#,
    );
}
#[test]
fn repetition_of_nothing() {
    test_errors(
        "
            Scope[0, 20](FnDef[0, 20](
                Ident[3, 3]()
                OpDef[13, 5](
                    OpParts[16, 2](
                        Variadic[16, 1](OpParts![16, 0])
                        Argument[17, 1]
                    )
                    OpBindings[18, 0]()
                )
            ))
        ",
        r#"
            fn foo (
                op *_
            )
        "#,
        &[Error {
            span: Span::new(16, 1),
            kind: ErrorKind::RepetitionOfNothing,
        }],
    );
}

#[test]
fn literal_path() {
    test_errors(
        "Scope[0, 16](UseStmt[0, 16](Ident![4, 5] Ident![10, 5]))",
        r#"
            use "owo"."uwu",
        "#,
        &[
            Error {
                span: Span::new(4, 5),
                kind: ErrorKind::UnexpectedToken {
                    expected: vec![TokenKind::Ident],
                    got: TokenKind::Literal,
                },
            },
            Error {
                span: Span::new(10, 5),
                kind: ErrorKind::UnexpectedToken {
                    expected: vec![TokenKind::Ident],
                    got: TokenKind::Literal,
                },
            },
        ],
    );
}

#[test]
fn nested_function() {
    test(
        "
        Scope[0, 134](FnDef[0, 134](
            Ident[3, 3]
            FnBody[30, 102](
                Ident[32, 3]
                Scope[48, 84](
                    FnDef[48, 70](
                        Ident[51, 3]
                        FnBody[92, 16](Scope[97, 11](Scope[102, 6]()))
                    )
                    Scope[130, 2]()
                )
            )
        ))
        ",
        r#"
        fn foo (
            : Foo -> Bar
            = foo => 
                fn bar (
                    : () -> ()
                    = => print("hi")
                )
                bar()
        )
        "#,
    )
}

#[test]
fn nested_function_named_op() {
    test_errors(
        "
            Scope[0, 96](FnDef[0, 96](
                Ident[3, 3]
                FnBody[30, 64](
                    Ident[32, 3]
                    Scope[39, 55](FnDef[39, 55](
                        Ident*[50, 2]
                        FnBody[80, 8](Ident[82, 1] Scope[87, 1]())
                    ))
                )
            ))
        ",
        "
            fn foo (
                : Foo -> Bar
                = foo => fn
                    op (
                    : a -> a
                    = x => x
                )
            )
        ",
        &[Error {
            kind: ErrorKind::UnexpectedToken {
                expected: vec![TokenKind::Ident],
                got: TokenKind::Op,
            },
            span: Span::new(50, 2),
        }],
    )
}

fn test(ast: &str, syntax: &str) {
    test_errors(ast, syntax, &[]);
}
fn test_errors(ast: &str, syntax: &str, expected_errors: &[Error]) {
    let expected = prettyprint::parse(&literal(ast)).unwrap_or_else(|e| panic!("{e:#?}"));
    let syntax = literal(syntax);
    println!("{syntax}");
    let (got, errors) = super::parse(&syntax);
    if expected_errors != errors || expected != got {
        panic!(
            "{}\n{}",
            Comparison::new(expected_errors, &errors),
            Comparison::new(&expected, &got)
        );
    }
}
