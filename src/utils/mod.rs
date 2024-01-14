pub fn have_common_prefix(a: &str, b: &str) -> bool {
    let strip_len = usize::min(a.len(), b.len());
    a.as_bytes()[..strip_len] == b.as_bytes()[..strip_len]
}
