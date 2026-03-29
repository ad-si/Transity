/// Replace all newlines with newlines followed by `indentation` spaces.
pub fn indent_subsequent(indentation: usize, s: &str) -> String {
    let pad = " ".repeat(indentation);
    s.replace('\n', &format!("\n{}", pad))
}
