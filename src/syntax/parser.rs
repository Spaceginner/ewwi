use std::collections::HashMap;
use crate::syntax::parser::helper::Check;
use super::lexer::{Word, WordStream};


mod helper {
    use super::super::lexer::Word;
    use super::ParsingError;

    pub(super) trait Check
        where Self: Sized
    {
        fn common(self) -> Result<String, ParsingError>;

        fn markers(self, value: &'static [&'static str]) -> Result<(), ParsingError>;

        fn keys(self, values: &'static [&'static str]) -> Result<(), ParsingError>;
    }

    impl Check for Word {
        fn common(self) -> Result<String, ParsingError> {
            match self {
                Word::Common { value, .. } => Ok(value),
                _ => Err(ParsingError::ExpectedCommonWord { got: self }),
            }
        }

        fn markers(self, values: &'static [&'static str]) -> Result<(), ParsingError> {
            match self {
                Word::Marker { pos, value } =>
                    match value.as_str() {
                        v if values.contains(&v) => Ok(()),
                        _ => Err(ParsingError::ExpectedDifferentMarkerWord { pos, got: value, expected: values })
                    },
                _ => Err(ParsingError::ExpectedMarkerWord { got: self }),
            }
        }

        fn keys(self, values: &'static [&'static str]) -> Result<(), ParsingError> {
            match self {
                Word::Key { pos, value } =>
                    match value.as_str() {
                        v if values.contains(&v) => Ok(()),
                        _ => Err(ParsingError::ExpectedDifferentKeyWord { pos, got: value, expected: values })
                    },
                _ => Err(ParsingError::ExpectedKeyWord { got: self }),
            }
        }
    }
}


#[derive(Debug)]
pub enum ParsingError {
    UnexpectedEOS,
    ExpectedCommonWord {
        got: Word,
    },
    ExpectedKeyWord {
        got: Word,
    },
    ExpectedMarkerWord {
        got: Word,
    },
    ExpectedBreakWord {
        got: Word,
    },
    ExpectedDifferentKeyWord {
        pos: usize,
        got: String,
        expected: &'static [&'static str],
    },
    ExpectedDifferentMarkerWord {
        pos: usize,
        got: String,
        expected: &'static [&'static str]
    }
}


#[derive(Debug)]
pub enum Value {
    Integer(i128),
    Float(f64),
    Char(char),
    String(String),
    List(Vec<Value>),
    Tuple(Vec<Value>),
    Dictionary(HashMap<Value, Value>)
}


#[derive(Debug)]
pub struct ModuleIdentifier {
    pub name: String,
    pub path: Vec<String>,
    pub absolute: bool,
}


impl ModuleIdentifier {
    pub const ABSOLUTE: &'static str = "$";
    pub const PATH_SEP: &'static str = "/";
}


#[derive(Debug)]
pub struct Identifier {
    pub name: String,
    pub module: Option<ModuleIdentifier>,
}


impl Identifier {
    pub const MODULE_PRESENT: &'static str = "#!";
    pub const ITEM_SEP: &'static str = "::";
}


#[derive(Debug)]
pub struct Type {
    pub identifier: Identifier,
    pub applied_generics: Vec<AppliedGeneric>,
}


pub struct TypeTuple(pub Vec<Type>);


#[derive(Debug)]
pub enum AppliedGeneric {
    Type(Type),
    TypeTuple(Vec<Type>),
}


#[derive(Debug)]
pub enum Command {
    Declare {
        name: String,
        r#type: Type,
        value: Value,
        rewritable: bool,
    },
    Assign {
        name: String,
        deref: bool,
        value: Value,
    },
    Call {
        name: String,
        deref: bool,
        arguments: Vec<Value>,
    }
}


impl Command {
    pub const DECLARE_KEYWORD: &'static str = "let";
    pub const ASSIGN_KEYWORD: &'static str = "assign";
    pub const CALL_KEYWORD: &'static str = "call";
}


pub struct Parser<'a, C: Iterator<Item = char>> {
    word_stream: WordStream<'a, C>,
}


impl<'a, C: Iterator<Item = char>> Parser<'a, C> {
    pub(self) fn new(word_stream: WordStream<'a, C>) -> Self {
        Self { word_stream }
    }

    fn next(&mut self) -> Result<Word, ParsingError> {
        match self.word_stream.next() {
            Some(word) => Ok(word),
            None => Err(ParsingError::UnexpectedEOS),
        }
    }

    pub fn parse_identifier(&mut self) -> Result<Identifier, ParsingError> {
        let mut word = self.next()?;

        let module = match word.markers(&[Identifier::MODULE_PRESENT]) {
            Ok(()) => {
                let module = {
                    let absolute = match self.next()?.markers(&[ModuleIdentifier::ABSOLUTE]) {
                        Ok(()) => true,
                        Err(ParsingError::ExpectedMarkerWord { got }) => { self.word_stream.redeem(got); false },
                        Err(error) => return Err(error),
                    };

                    let name;
                    let mut path = Vec::new();
                    loop {
                        let ident = self.next()?.common()?;

                        match self.next()? {
                            Word::Marker { pos, value } =>
                                match value.as_str() {
                                    ModuleIdentifier::PATH_SEP => path.push(ident),
                                    Identifier::ITEM_SEP => { name = ident; break; },
                                    _ => return Err(ParsingError::ExpectedDifferentMarkerWord { pos, got: value,
                                        expected: &[ModuleIdentifier::PATH_SEP, Identifier::ITEM_SEP] })
                                },
                            word => return Err(ParsingError::ExpectedMarkerWord { got: word }),
                        };
                    };

                    ModuleIdentifier { path, name, absolute }
                };

                word = self.next()?;

                Some(module)
            },
            Err(ParsingError::ExpectedMarkerWord { got }) => { word = got; None },
            Err(err) => return Err(err),
        };

        Ok(Identifier { module, name: word.common()? })
    }

    fn parse_command(&mut self) -> Result<Command, ParsingError> {
        match self.next()? {
            Word::Key { value, pos } => {
                match value.as_str() {
                    Command::DECLARE_KEYWORD => {
                        todo!()
                    },
                    Command::ASSIGN_KEYWORD => {
                        todo!()
                    },
                    Command::CALL_KEYWORD => {
                        todo!()
                    },
                    _ => Err(ParsingError::ExpectedDifferentKeyWord { pos, got: value,
                        expected: &[Command::DECLARE_KEYWORD, Command::ASSIGN_KEYWORD, Command::CALL_KEYWORD] })
                }
            },
            word => Err(ParsingError::ExpectedKeyWord { got: word })
        }
    }

    pub(self) fn parse(&mut self) -> Result<ModuleIdentifier, ParsingError> {
        todo!()
    }
}


pub fn parse(word_stream: WordStream<'_, impl Iterator<Item = char>>) -> Result<Identifier, ParsingError> {
    Parser::new(word_stream).parse_identifier()
}
