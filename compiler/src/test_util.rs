#![cfg(test)]

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
            lines[first_nonempty..=last_nonempty].iter().map(|line| &line[min_indent..]);
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
