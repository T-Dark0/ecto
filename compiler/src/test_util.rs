#![cfg(test)]

use pretty_assertions::assert_eq;

/// Cleans up a string literal:
/// - Leading and trailing empty lines are stripped, but other empty lines are kept
/// - Shared leading whitespace on all lines is stripped: each line is dedented by an even amount so that at least one line has no leading whitespace.
pub fn literal(text: &str) -> String {
    fn inner(text: &str) -> Option<String> {
        let lines = text.lines().collect::<Vec<_>>();
        let min_indent = lines
            .iter()
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.len() - line.trim_start().len())
            .min()?;
        let first_nonempty = lines.iter().position(|line| !line.trim().is_empty())?;
        let last_nonempty = lines.iter().rposition(|line| !line.trim().is_empty())?;
        let mut lines =
            lines[first_nonempty..=last_nonempty].iter().map(|line| match line.trim() {
                "" => line.get(min_indent..).unwrap_or(""),
                _ => &line[min_indent..],
            });
        let mut out = String::new();
        if let Some(first) = lines.next() {
            out.push_str(first);
            for line in lines {
                out.push('\n');
                out.push_str(line);
            }
        }
        Some(out)
    }
    inner(text).unwrap_or_else(String::new)
}

#[test]
fn test_literal() {
    let got = literal(
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
    );
    let expected = concat!(
        "use std.num.mul,\n",
        "\n",
        "fn add(\n",
        "    op _ \"+\" _; mul <- add, eq -> add\n",
        "    : Int Int -> Int\n",
        "    = a b => intrinsics.add(a, b)\n",
        ")\n",
        "\n",
        "(\n",
        "    use super.a,\n",
        ")",
    );
    assert_eq!(got, expected);
}
