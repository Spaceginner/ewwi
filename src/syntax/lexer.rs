use std::collections::VecDeque;
use std::fmt;
use std::iter::Enumerate;
use std::num::{ParseFloatError, ParseIntError};
use crate::extensions::take::Take;
use crate::utils;


#[derive(Debug)]
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


pub const WORD_SEPARATORS: &[char] = &[' ', '\t', '\n'];
pub const BOOLEAN_TRUE: &str = "true";
pub const BOOLEAN_FALSE: &str = "false";
pub const KEY_WORDS: &[&str] = &[
    BOOLEAN_TRUE, BOOLEAN_FALSE,
    "pub", "inter", "self",
    "import", "as",
    "fnc", "return", "abst", "_",
    "let", "once", "assign", "call",
    "type", "data", "const", "consts", "signals", "group", "enum",
    "fncs", "impl"
];
pub const MARKER_WORDS: &[&str] = &[
    "[{", "}]", "#[", "::", "[", "]",
    "{", "}", ":", "|", ",", "%", "!",
    "<", ">", "(", ")", "&", "*", "=",
    "@", ".", "#@", "%!", "#{", "}#",
    "$", "/", "#!",
];
pub const BREAK_SYMBOL: char = ';';
pub const STRING_MARKER: char = '"';
pub const CHAR_MARKER: char = '\'';
pub const MARKER_SYMBOLS: &[char] = &[
    '[', ']', '{', '}', '<', '>', '|', '(', ')',
    '$', '#', ':', '!', '%', '@', '=', '&', '*',
    '/', '.', ','
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
pub const COMMENT_PAIRS: &[(&str, &str)] = &[("//", "\n"), ("/*", "*/")];
pub const FORCE_COMMON_WORD_PREFIX: &str = "e^";


#[derive(Debug, Clone)]
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


#[derive(Debug, Clone)]
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


#[derive(Debug)]
pub struct WordStream<'a, C: Iterator<Item = char>> {
    chars: Enumerate<C>,
    char_queue: VecDeque<(usize, char)>,
    word_queue: VecDeque<Word>,
    lexing_error: Option<LexingError>,
    word_buffer: String,
    lexing_state: LexingState,
    comment_end_seq: Option<&'a str>,
    lexing_marker: bool,
}


impl<'a, C> WordStream<'a, C>
    where C: Iterator<Item = char>
{
    pub fn new(chars: C) -> Self {
        Self {
            chars: chars.enumerate(),
            char_queue: VecDeque::new(),
            word_queue: VecDeque::new(),
            lexing_error: None,
            word_buffer: String::new(),
            lexing_state: LexingState::default(),
            comment_end_seq: None,
            lexing_marker: false,
        }
    }
}



impl<'a, C: Iterator<Item = char>> Iterator for WordStream<'a, C> {
    type Item = Word;

    fn next(&mut self) -> Option<Self::Item> {
        if self.lexing_error.is_some() {
            return None;
        };

        if let Some(word) = self.word_queue.pop_front() {
            return Some(word);
        };

        while self.word_queue.is_empty() && let Some((pos, c)) = self.char_queue.pop_front().or_else(|| self.chars.next()) {
            match self.lexing_state {
                LexingState::Char => {
                    if c == ESCAPE_SYMBOL && let Some((escape_pos, escape_c)) = self.chars.next() {
                        match unescape_sequence(escape_c.to_string().as_str()) {
                            Err(error) => { self.lexing_error = Some(LexingError::InvalidEscapeSequence { pos: escape_pos - 1, error, value: escape_c.to_string() }); return None; }
                            Ok(unescaped) => match unescaped {
                                Some(unescaped_c) => {
                                    self.lexing_state = LexingState::Normal;
                                    self.word_queue.push_back(Word::LiteralChar { pos: pos - 1, value: unescaped_c });
                                },
                                None => { self.lexing_error = Some(LexingError::EmptyChar { pos }); return None; },
                            },
                        };
                    } else {
                        self.lexing_state = LexingState::Normal;
                        self.word_queue.push_back(Word::LiteralChar { pos: pos - 1, value: c });
                    };
                }
                LexingState::String => {
                    if c == ESCAPE_SYMBOL && let Some((escape_pos, escape_c)) = self.chars.next() {
                        match unescape_sequence(escape_c.to_string().as_str()) {
                            Err(error) => {
                                self.lexing_error = Some(LexingError::InvalidEscapeSequence { pos: escape_pos - 1, error, value: escape_c.to_string() }); return None; }
                            Ok(unescaped) => if let Some(c) = unescaped { self.word_buffer.push(c) },
                        };
                    } else if c == STRING_MARKER {
                        self.lexing_state = LexingState::Normal;
                        self.word_queue.push_back(Word::LiteralString { pos: pos - self.word_buffer.len() - 1, value: self.word_buffer.take() });
                    } else {
                        self.word_buffer.push(c);
                    };
                }
                LexingState::Integer => {
                    if c == FLOAT_SEP {
                        self.word_buffer.push(c);
                        self.lexing_state = LexingState::Float;
                    } else if c == NUMBER_CLARITY_SYMBOL || ALL_DIGITS.contains(&c) {
                        self.word_buffer.push(c);
                    } else {
                        let value = self.word_buffer.take();
                        let lexed_word = match value.replace(NUMBER_CLARITY_SYMBOL, "").parse() {
                            Ok(n) => Word::LiteralInteger { pos: pos - value.len(), value: n },
                            Err(error) => { self.lexing_error = Some(LexingError::InvalidLiteralInteger { pos: pos - value.len(), value, error }); return None; }
                        };

                        self.char_queue.push_back((pos, c));
                        self.lexing_state = LexingState::Normal;

                        self.word_queue.push_back(lexed_word);
                    }
                }
                custom_base @ (LexingState::BinaryInteger | LexingState::OctalInteger | LexingState::HexInteger) => {
                    let base = match custom_base {
                        LexingState::BinaryInteger => 2,
                        LexingState::OctalInteger => 8,
                        LexingState::HexInteger => 16,
                        _ => unreachable!(),
                    };

                    if c == NUMBER_CLARITY_SYMBOL || ALL_DIGITS.contains(&c) {
                        self.word_buffer.push(c);
                    } else {
                        let value = self.word_buffer.take();
                        let lexed_word = match i128::from_str_radix(&value.replace(NUMBER_CLARITY_SYMBOL, ""), base) {
                            Ok(n) => Word::LiteralInteger { pos: pos - value.len(), value: n },
                            Err(error) => { self.lexing_error = Some(LexingError::InvalidLiteralInteger { pos: pos - value.len(), value, error }); return None; }
                        };

                        self.char_queue.push_back((pos, c));
                        self.lexing_state = LexingState::Normal;

                        self.word_queue.push_back(lexed_word);
                    }
                }
                LexingState::Float => {
                    if c == NUMBER_CLARITY_SYMBOL || c.to_ascii_lowercase() == FLOAT_EXP_CHAR || DIGITS.contains(&c) || FLOAT_EXP_SIGNS.contains(&c) {
                        self.word_buffer.push(c);
                    } else {
                        let value = self.word_buffer.take();
                        let lexed_word = match value.replace(NUMBER_CLARITY_SYMBOL, "").parse() {
                            Ok(x) => Word::LiteralFloat { pos: pos - value.len(), value: x },
                            Err(error) => { self.lexing_error = Some(LexingError::InvalidLiteralFloat { pos: pos - value.len(), value, error }); return None; }
                        };

                        self.char_queue.push_back((pos, c));
                        self.lexing_state = LexingState::Normal;

                        self.word_queue.push_back(lexed_word);
                    }
                }
                LexingState::Normal => {
                    if COMMENT_PAIRS.iter().map(|pair| pair.0).any(|start_seq| start_seq == self.word_buffer) {
                        self.comment_end_seq = Some(COMMENT_PAIRS.iter().find(|pair| pair.0 == self.word_buffer).unwrap().1);
                        self.word_buffer.clear();

                        self.word_buffer.push(c);
                    } else if let Some(end_seq) = self.comment_end_seq {
                        if !utils::have_common_prefix(&self.word_buffer, end_seq) {
                            self.word_buffer.clear();
                        } else if self.word_buffer.len() == end_seq.len() {
                            if self.word_buffer == end_seq {
                                self.comment_end_seq = None;

                                // just because of how junky this whole solution is
                                if !MARKER_SYMBOLS.contains(&c) {
                                    self.lexing_marker = false;
                                };
                            };

                            self.word_buffer.clear();
                        };

                        if self.comment_end_seq.is_some() {
                            self.word_buffer.push(c);
                        } else if !WORD_SEPARATORS.contains(&c) {
                            self.char_queue.push_back((pos, c));
                        };
                    } else {
                        if self.lexing_marker && (!MARKER_SYMBOLS.contains(&c) || c == BREAK_SYMBOL) {
                            let mut buffer = self.word_buffer.take();
                            let mut next_iter = String::default();
                            let mut last_next_iter_len = 0;
                            loop {
                                // FIXME pos tracking
                                if buffer.is_empty() {
                                    if next_iter.is_empty() {
                                        break;
                                    } else if next_iter.len() == last_next_iter_len {
                                        self.lexing_error = Some(LexingError::InvalidMarker { pos: pos - next_iter.len(), value: next_iter.chars().rev().collect() });
                                        return None;
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
                                    self.word_queue.push_back(Word::Marker { pos: pos - buffer.len(), value: buffer.take() });
                                };
                            };

                            self.lexing_marker = false;
                        } else if !self.word_buffer.is_empty() && (WORD_SEPARATORS.contains(&c) || (!self.lexing_marker && MARKER_SYMBOLS.contains(&c)) || c == BREAK_SYMBOL) {
                            if KEY_WORDS.contains(&self.word_buffer.as_str()) {
                                self.word_queue.push_back(match self.word_buffer.take().as_str() {
                                    BOOLEAN_FALSE => Word::LiteralBoolean { pos: pos - BOOLEAN_FALSE.len(), value: false },
                                    BOOLEAN_TRUE => Word::LiteralBoolean { pos: pos - BOOLEAN_TRUE.len(), value: true },
                                    value => Word::Key { pos: pos - value.len(), value: value.to_string() },
                                });
                            } else if let Some(common_keyword) = self.word_buffer.strip_prefix(FORCE_COMMON_WORD_PREFIX) && KEY_WORDS.contains(&common_keyword) {
                                self.word_queue.push_back(Word::Common { pos: pos - self.word_buffer.len(), value: common_keyword.to_string() });
                                self.word_buffer.clear();
                            } else {
                                self.word_queue.push_back(Word::Common { pos: pos - self.word_buffer.len(), value: self.word_buffer.take() });
                            };
                        };

                        if c == BREAK_SYMBOL {
                            self.word_queue.push_back(Word::Break { pos });
                        } else if !WORD_SEPARATORS.contains(&c) {
                            if !self.lexing_marker && MARKER_SYMBOLS.contains(&c) {
                                self.lexing_marker = true;
                                self.word_buffer.push(c);
                            } else if self.lexing_marker {
                                self.word_buffer.push(c);
                            } else if self.word_buffer.is_empty() && (DIGITS.contains(&c) || c == INTEGER_NEGATION_SYMBOL) {
                                let (_, num_c) = if c == INTEGER_NEGATION_SYMBOL {
                                    self.word_buffer.push(c);
                                    if let Some((pos, c)) = self.chars.next() {
                                        if DIGITS.contains(&c) {
                                            (pos, c)
                                        } else {
                                            if c == INTEGER_NEGATION_SYMBOL {
                                                self.word_queue.push_back(Word::Common { pos: pos - self.word_buffer.len(), value: self.word_buffer.take() });
                                            };

                                            self.char_queue.push_back((pos, c));
                                            continue;
                                        }
                                    } else {
                                        continue;
                                    }
                                } else {
                                    (pos, c)
                                };

                                if num_c == NON_DECIMAL_INTEGER_PREFIX && let Some((_, prefix)) = self.chars.next() {
                                    match prefix.to_ascii_lowercase() {
                                        HEX_INTEGER_PREFIX => { self.lexing_state = LexingState::HexInteger; }
                                        BINARY_INTEGER_PREFIX => { self.lexing_state = LexingState::BinaryInteger; }
                                        OCTAL_INTEGER_PREFIX => { self.lexing_state = LexingState::OctalInteger; }
                                        _ => {
                                            self.word_buffer.push(num_c);
                                            self.lexing_state = LexingState::Integer;
                                        }
                                    };
                                } else {
                                    self.word_buffer.push(num_c);
                                    self.lexing_state = LexingState::Integer;
                                };
                            } else if c == CHAR_MARKER {
                                self.lexing_state = LexingState::Char;
                            } else if c == STRING_MARKER {
                                self.lexing_state = LexingState::String;
                            } else {
                                self.word_buffer.push(c);
                            };
                        };
                    };
                },
            };
        };

        self.word_queue.pop_front()
    }
}


pub fn lex(source: &str) -> Result<Vec<Word>, LexingError> {
    match source.chars().last() {
        None => Ok(Vec::new()),
        Some('\n') => {
            let mut lexer = WordStream::new(source.chars());
            let words = lexer.by_ref().collect();

            match lexer.lexing_error {
                Some(error) => Err(error),
                None => Ok(words),
            }
        },
        Some(_) => Err(LexingError::DoesNotEndWithNewline),
    }
}
