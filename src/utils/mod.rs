pub fn have_common_prefix(a: &str, b: &str) -> bool {
    let strip_len = usize::min(a.len(), b.len());
    a.as_bytes()[..strip_len] == b.as_bytes()[..strip_len]
}


pub fn string_escape(s: &str) -> String {
    let mut escaped_string = String::new();

    for c in s.chars() {
        match c {
            hndl @ ('\\' | '\'' | '\"' | '\x07' | '\x08' | '\x0C' | '\n' | '\r' | '\t' | '\x0B' | '\0')
                => escaped_string.push_str(match hndl {
                    '\\' => r"\\",
                    '\'' => r"\'",
                    '\"' => r#"\""#,
                    '\x07' => r"\a",
                    '\x08' => r"\b",
                    '\x0C' => r"\f",
                    '\n' => r"\n",
                    '\r' => r"\r",
                    '\t' => r"\t",
                    '\x0B' => r"\v",
                    '\0' => r"\0",
                    _ => unreachable!()
                }),
            ctrl if c.is_control() => escaped_string.push_str(&match ctrl as u32 {
                n @ ..=0xFF => format!(r"\x{:0>2X}", n),
                n => format!(r"\u{{{:0>4X}}}", n),
            }),
            norm => escaped_string.push_str(norm.to_string().as_str()),
        }
    };

    escaped_string
}
