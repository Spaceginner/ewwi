use std::collections::VecDeque;
use std::fmt;
use std::num::{ParseFloatError, ParseIntError};
use crate::extensions::take::Take;
use crate::utils;

pub enum Word {
    Common {
        pos: usize,
        value: String,
    },
    Key {
        pos: usize,
        value: String,
    },
    Marker {
        pos: usize,
        value: String,
    },
    LiteralString {
        pos: usize,
        value: String,
    },
    LiteralChar {
        pos: usize,
        value: char,
    },
    LiteralInteger {
        pos: usize,
        value: i128,
    },
    LiteralFloat {
        pos: usize,
        value: f64,
    },
    LiteralBoolean {
        pos: usize,
        value: bool,
    },
    Break {
        pos: usize,
    }
}


impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Word::Common { value, pos } => write!(f, "word@{pos} common '{value}'"),
            Word::Key { value, pos } => write!(f, "word@{pos} key '{value}'"),
            Word::Marker { value, pos } => write!(f, "word@{pos} marker '{value}'"),
            Word::Break { pos } => write!(f, "word@{pos} break"),
            Word::LiteralString { pos, value } => write!(f, "word@{pos} literal string \"{}\"", string_escape(value)),
            Word::LiteralChar { pos, value } => write!(f, "word@{pos} literal char '{value}'"),
            Word::LiteralInteger { pos, value } => write!(f, "word@{pos} literal integer {value}"),
            Word::LiteralFloat { pos, value } => write!(f, "word@{pos} literal float {value}"),
            Word::LiteralBoolean { pos, value } => write!(f, "word@{pos} literal boolean {value}"),
        }
    }
}


#[derive(Debug, Default, Copy, Clone, PartialEq)]
enum CommentingMode {
    #[default]
    Disabled,
    Endline,
    Multiline
}


pub const WORD_SEPARATORS: &[char] = &[' ', '\n'];
pub const BOOLEAN_TRUE: &str = "true";
pub const BOOLEAN_FALSE: &str = "false";
pub const KEY_WORDS: &[&str] = &[BOOLEAN_TRUE, BOOLEAN_FALSE, "_", "mut", "pub", "inter", "self", "import", "fnc", "let", "assign", "const", "data", "consts", "signals", "group", "enum", "abst", "fncs", "impl", "return", "call", "once"];
pub const MARKER_WORDS: &[&str] = &[
    "[{", "}]", "#[", "::", "[", "]",
    "$", "{", "}", ":", "|", ",", "%", "!",
    "<", ">", "(", ")", "&", "*", "=", "@",
    "/", ".", "#!", "#@", "%!"
];
pub const BREAK_SYMBOL: char = ';';
pub const STRING_MARKER: char = '"';
pub const CHAR_MARKER: char = '\'';
pub const MARKER_SYMBOLS: &[char] = &[
    '[', ']', '{', '}', '<', '>', '|', '(', ')',
    '$', '#', ':', '!', '%', '@', '=', '&', '*',
    ';', '/', '.', ','
];
pub const DIGITS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
pub const NON_DECIMAL_INTEGER_PREFIX: char = '0';
pub const BINARY_INTEGER_PREFIX: char = 'b';
pub const OCTAL_INTEGER_PREFIX: char = 'o';
pub const HEX_INTEGER_PREFIX: char = 'x';
pub const ALL_DIGITS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
pub const INTEGER_NEGATION_SYMBOL: char = '-';
pub const FLOAT_SEP: char = '.';
pub const FLOAT_EXP_CHAR: char = 'e';
pub const FLOAT_EXP_SIGNS: &[char] = &['+', '-'];
pub const NUMBER_CLARITY_SYMBOL: char = '_';
pub const ESCAPE_SYMBOL: char = '\\';
pub const ENDLINE_COMMENT: &str = "//";
pub const MULTILINE_COMMENT_START: &str = "/*";
pub const MULTILINE_COMMENT_END: &str = "*/";
pub const FORCE_COMMON_WORD_PREFIX: &str = "e^";


#[derive(Debug)]
pub enum LexingError {
    InvalidMarker {
        pos: usize,
        value: String,
    },
    InvalidLiteralInteger {
        pos: usize,
        value: String,
        error: ParseIntError,
    },
    InvalidLiteralFloat {
        pos: usize,
        value: String,
        error: ParseFloatError,
    },
    InvalidEscapeSequence {
        pos: usize,
        value: String,
        error: CharUnescapingError,
    },
    EmptyChar {
        pos: usize,
    },
    DoesNotEndWithNewline,
}


#[derive(Debug, Default, Copy, Clone, PartialEq)]
enum LexingState {
    #[default]
    Normal,
    String,
    Char,
    Integer,
    BinaryInteger,
    OctalInteger,
    HexInteger,
    Float,
}


#[derive(Debug)]
pub enum CharUnescapingError {
    InvalidMode
}


fn unescape_sequence(seq: &str) -> Result<Option<char>, CharUnescapingError> {
    let mut chars = seq.chars();
    let mode = chars.next().unwrap();
    match mode {
        '\n' => Ok(None),
        '\\' => Ok(Some('\\')),
        '\'' => Ok(Some('\'')),
        '\"' => Ok(Some('\"')),
        'a' => Ok(Some('\x07')),
        'b' => Ok(Some('\x08')),
        'f' => Ok(Some('\x0C')),
        'n' => Ok(Some('\n')),
        'r' => Ok(Some('\r')),
        't' => Ok(Some('\t')),
        'v' => Ok(Some('\x0B')),
        '0' => Ok(Some('\0')),
        'o' => todo!(),
        'x' => todo!(),
        'u' => todo!(),
        _ => Err(CharUnescapingError::InvalidMode)
    }
}


fn string_escape(s: &str) -> String {
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


pub fn lex(source: &str) -> Result<Vec<Word>, LexingError> {
    let mut words = Vec::new();

    match source.chars().last() {
        None => return Ok(words),
        Some('\n') => {},
        Some(_) => return Err(LexingError::DoesNotEndWithNewline),
    };

    let mut commenting = CommentingMode::Disabled;
    let mut escaping = false;
    let mut lexing_marker = false;
    let mut lexing_state = LexingState::default();

    let mut word_buffer = String::new();
    let mut chars = source.chars().enumerate();
    let mut char_queue = VecDeque::new();
    while let Some((pos, c)) = char_queue.pop_front().or_else(|| chars.next()) {
        match lexing_state {
            LexingState::Char => {
                if c == ESCAPE_SYMBOL && let Some((escape_pos, escape_c)) = chars.next() {
                    match unescape_sequence(escape_c.to_string().as_str()) {
                        Err(error) => return Err(LexingError::InvalidEscapeSequence { pos: escape_pos - 1, error, value: escape_c.to_string() }),
                        Ok(unescaped) => match unescaped {
                            Some(unescaped_c) => {
                                words.push(Word::LiteralChar { pos: pos - 1, value: unescaped_c });
                                lexing_state = LexingState::Normal;
                            }
                            None => return Err(LexingError::EmptyChar { pos })
                        },
                    };
                } else {
                    words.push(Word::LiteralChar { pos: pos - 1, value: c });
                    lexing_state = LexingState::Normal;
                };
            },
            LexingState::String => {
                if c == ESCAPE_SYMBOL && let Some((escape_pos, escape_c)) = chars.next() {
                    match unescape_sequence(escape_c.to_string().as_str()) {
                        Err(error) => return Err(LexingError::InvalidEscapeSequence { pos: escape_pos - 1, error, value: escape_c.to_string() }),
                        Ok(unescaped) => if let Some(c) = unescaped { word_buffer.push(c) },
                    };
                } else if c == STRING_MARKER {
                    words.push(Word::LiteralString { pos: pos - word_buffer.len() - 1, value: word_buffer.take() });
                    lexing_state = LexingState::Normal;
                } else {
                    word_buffer.push(c);
                };
            },
            LexingState::Integer => {
                if c == FLOAT_SEP {
                    word_buffer.push(c);
                    lexing_state = LexingState::Float;
                } else if c == NUMBER_CLARITY_SYMBOL || ALL_DIGITS.contains(&c) {
                    word_buffer.push(c);
                } else {
                    let value = word_buffer.take();
                    match value.replace(NUMBER_CLARITY_SYMBOL, "").parse() {
                        Ok(n) => words.push(Word::LiteralInteger { pos: pos - value.len(), value: n }),
                        Err(error) => return Err(LexingError::InvalidLiteralInteger { pos: pos - value.len(), value, error }),
                    };

                    char_queue.push_back((pos, c));
                    lexing_state = LexingState::Normal;
                }
            },
            custom_base @ (LexingState::BinaryInteger | LexingState::OctalInteger | LexingState::HexInteger) => {
                let base = match custom_base {
                    LexingState::BinaryInteger => 2,
                    LexingState::OctalInteger => 8,
                    LexingState::HexInteger => 16,
                    _ => unreachable!(),
                };

                if c == NUMBER_CLARITY_SYMBOL || ALL_DIGITS.contains(&c) {
                    word_buffer.push(c);
                } else {
                    let value = word_buffer.take();
                    match i128::from_str_radix(&value.replace(NUMBER_CLARITY_SYMBOL, ""), base) {
                        Ok(n) => words.push(Word::LiteralInteger { pos: pos - value.len(), value: n }),
                        Err(error) => return Err(LexingError::InvalidLiteralInteger { pos: pos - value.len(), value, error }),
                    };

                    char_queue.push_back((pos, c));
                    lexing_state = LexingState::Normal;
                }
            }
            LexingState::Float => {
                if c == NUMBER_CLARITY_SYMBOL || c.to_ascii_lowercase() == FLOAT_EXP_CHAR || DIGITS.contains(&c) || FLOAT_EXP_SIGNS.contains(&c) {
                    word_buffer.push(c);
                } else {
                    let value = word_buffer.take();
                    match value.replace(NUMBER_CLARITY_SYMBOL, "").parse() {
                        Ok(x) => words.push(Word::LiteralFloat { pos: pos - value.len(), value: x }),
                        Err(error) => return Err(LexingError::InvalidLiteralFloat { pos: pos - value.len(), value, error }),
                    };

                    char_queue.push_back((pos, c));
                    lexing_state = LexingState::Normal;
                }
            },
            LexingState::Normal => {
                if word_buffer.starts_with(ENDLINE_COMMENT) {
                    word_buffer.clear();
                    commenting = CommentingMode::Endline;
                } else if commenting == CommentingMode::Endline {
                    if c == '\n' {
                        commenting = CommentingMode::Disabled;
                    };
                } else if word_buffer.starts_with(MULTILINE_COMMENT_START) {
                    word_buffer.clear();
                    commenting = CommentingMode::Multiline;
                } else if commenting == CommentingMode::Multiline {
                    word_buffer.push(c);

                    if !utils::have_common_prefix(&word_buffer, MULTILINE_COMMENT_END) {
                        word_buffer.clear();
                    };
                } else if c == ESCAPE_SYMBOL {
                    escaping = true;
                } else if escaping.take() {
                    word_buffer.push(c);
                } else {
                    if lexing_marker && (!MARKER_SYMBOLS.contains(&c) || c == BREAK_SYMBOL) {
                        let mut buffer = word_buffer.take();
                        let mut next_iter = String::default();
                        let mut last_next_iter_len = 0;
                        loop {
                            if buffer.is_empty() {
                                if next_iter.is_empty() {
                                    break;
                                } else if next_iter.len() == last_next_iter_len {
                                    return Err(LexingError::InvalidMarker { pos: pos - next_iter.len(), value: next_iter.chars().rev().collect() });
                                } else {
                                    last_next_iter_len = next_iter.len();
                                    next_iter = next_iter.chars().rev().collect();
                                    std::mem::swap(&mut buffer, &mut next_iter);
                                };
                            } else if !MARKER_WORDS.contains(&buffer.as_str()) {
                                let (next_buffer, discarded) = buffer.split_at(buffer.len() - 1);
                                next_iter.push_str(discarded);
                                buffer = next_buffer.to_string();
                            } else {
                                words.push(Word::Marker { pos: pos - buffer.len(), value: buffer.take() });
                            };
                        };

                        lexing_marker = false;
                    } else if !word_buffer.is_empty() && (WORD_SEPARATORS.contains(&c) || (!lexing_marker && MARKER_SYMBOLS.contains(&c)) || c == BREAK_SYMBOL) {
                        if KEY_WORDS.contains(&word_buffer.as_str()) {
                            words.push(match word_buffer.take().as_str() {
                                BOOLEAN_FALSE => Word::LiteralBoolean { pos: pos - BOOLEAN_FALSE.len(), value: false },
                                BOOLEAN_TRUE => Word::LiteralBoolean { pos: pos - BOOLEAN_TRUE.len(), value: true },
                                value => Word::Key { pos: pos - value.len(), value: value.to_string() },
                            });
                        } else if let Some(common_keyword) = word_buffer.strip_prefix(FORCE_COMMON_WORD_PREFIX) && KEY_WORDS.contains(&common_keyword) {
                            words.push(Word::Common { pos: pos - word_buffer.len(), value: common_keyword.to_string() });
                            word_buffer.clear();
                        } else {
                            words.push(Word::Common { pos: pos - word_buffer.len(), value: word_buffer.take() });
                        };
                    };

                    if c == BREAK_SYMBOL {
                        words.push(Word::Break { pos });
                    } else if !WORD_SEPARATORS.contains(&c) {
                        if !lexing_marker && MARKER_SYMBOLS.contains(&c) {
                            lexing_marker = true;
                            word_buffer.push(c);
                        } else if lexing_marker {
                            word_buffer.push(c);
                        } else if word_buffer.is_empty() && (DIGITS.contains(&c) || c == INTEGER_NEGATION_SYMBOL) {
                            let (_, num_c) = if c == INTEGER_NEGATION_SYMBOL {
                                word_buffer.push(c);
                                if let Some((pos, c)) = chars.next() {
                                    if DIGITS.contains(&c) {
                                        (pos, c)
                                    } else {
                                        if c == INTEGER_NEGATION_SYMBOL {
                                            words.push(Word::Common { pos: pos - word_buffer.len(), value: word_buffer.take() });
                                        };

                                        char_queue.push_back((pos, c));
                                        continue;
                                    }
                                } else {
                                    continue;
                                }
                            } else {
                                (pos, c)
                            };

                            if num_c == NON_DECIMAL_INTEGER_PREFIX && let Some((_, prefix)) = chars.next() {
                                match prefix.to_ascii_lowercase() {
                                    HEX_INTEGER_PREFIX => { lexing_state = LexingState::HexInteger; },
                                    BINARY_INTEGER_PREFIX => { lexing_state = LexingState::BinaryInteger; },
                                    OCTAL_INTEGER_PREFIX => { lexing_state = LexingState::OctalInteger; },
                                    _ => {
                                        word_buffer.push(num_c);
                                        lexing_state = LexingState::Integer;
                                    }
                                };
                            } else {
                                word_buffer.push(num_c);
                                lexing_state = LexingState::Integer;
                            };
                        } else if c == CHAR_MARKER {
                            lexing_state = LexingState::Char;
                        } else if c == STRING_MARKER {
                            lexing_state = LexingState::String;
                        } else {
                            word_buffer.push(c);
                        };
                    };
                }
            },
        };
    };

    Ok(words)
}
