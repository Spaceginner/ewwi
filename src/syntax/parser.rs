use std::fmt;
use super::lexer::{LexingError, Word, WordStream};


use helper::Check;
use crate::utils;

#[macro_use]
mod helper {
    use crate::syntax::lexer::WordStream;
    use super::super::lexer::Word;
    use super::ParsingError;

    pub fn le_convert(ss: &[&str]) -> Vec<String> {
        ss.iter().map(|s| s.to_string()).collect()
    }

    pub(super) trait Check
        where Self: Sized
    {
        fn r#break(self, while_parsing: &'static str) -> Result<(), ParsingError>;

        fn common(self, while_parsing: &'static str) -> Result<String, ParsingError>;

        fn markers(self, while_parsing: &'static str, expected: &[&str], expected_too: &[&str]) -> Result<(), ParsingError>;

        fn keys(self, while_parsing: &'static str, expected: &[&str], expected_too: &[&str]) -> Result<(), ParsingError>;

        fn opt_common(self, while_parsing: &'static str, word_stream: &mut WordStream<'_, impl Iterator<Item = char>>) -> Result<Option<String>, ParsingError> {
            match self.common(while_parsing) {
                Ok(value) => Ok(Some(value)),
                Err(ParsingError::ExpectedCommonWord { got, .. }) => { word_stream.redeem(got); Ok(None) },
                Err(error) => Err(error),
            }
        }

        fn opt_markers(self, while_parsing: &'static str, word_stream: &mut WordStream<'_, impl Iterator<Item = char>>, expected: &[&str], expected_too: &[&str]) -> Result<bool, ParsingError> {
            match self.markers(while_parsing, expected, expected_too) {
                Ok(()) => Ok(true),
                Err(ParsingError::ExpectedMarkerWord { got, .. }) => { word_stream.redeem(got); Ok(false) },
                Err(ParsingError::ExpectedDifferentMarkerWord { pos, got, .. }) => { word_stream.redeem(Word::Marker { pos, value: got }); Ok(false) },
                Err(error) => Err(error),
            }
        }

        fn opt_keys(self, while_parsing: &'static str, word_stream: &mut WordStream<'_, impl Iterator<Item = char>>, expected: &[&str], expected_too: &[&str]) -> Result<bool, ParsingError> {
            match self.keys(while_parsing, expected, expected_too) {
                Ok(()) => Ok(true),
                Err(ParsingError::ExpectedKeyWord { got, .. }) => { word_stream.redeem(got); Ok(false) },
                Err(ParsingError::ExpectedDifferentKeyWord { pos, got, .. }) => { word_stream.redeem(Word::Key { pos, value: got }); Ok(false) },
                Err(error) => Err(error),
            }
        }
    }

    impl Check for Word {
        fn r#break(self, while_parsing: &'static str) -> Result<(), ParsingError> {
            match self {
                Word::Break { .. } => Ok(()),
                word => Err(ParsingError::ExpectedBreakWord { while_parsing, got: word })
            }
        }

        fn common(self, while_parsing: &'static str) -> Result<String, ParsingError> {
            match self {
                Word::Common { value, .. } => Ok(value),
                word => Err(ParsingError::ExpectedCommonWord { while_parsing, got: word }),
            }
        }

        fn markers(self, while_parsing: &'static str, values: &[&str], expected_too: &[&str]) -> Result<(), ParsingError> {
            match self {
                Word::Marker { pos, value } =>
                    match value.as_str() {
                        v if values.contains(&v) => Ok(()),
                        _ => Err(ParsingError::ExpectedDifferentMarkerWord { while_parsing, pos, got: value, optional: false,
                            expected: [le_convert(values), le_convert(expected_too)].concat() })
                    },
                _ => Err(ParsingError::ExpectedMarkerWord { while_parsing, got: self }),
            }
        }

        fn keys(self, while_parsing: &'static str, values: &[&str], expected_too: &[&str]) -> Result<(), ParsingError> {
            match self {
                Word::Key { pos, value } =>
                    match value.as_str() {
                        v if values.contains(&v) => Ok(()),
                        _ => Err(ParsingError::ExpectedDifferentKeyWord { while_parsing, pos, got: value, optional: false,
                            expected: [le_convert(values), le_convert(expected_too)].concat() })
                    },
                _ => Err(ParsingError::ExpectedKeyWord { while_parsing, got: self }),
            }
        }
    }

    macro_rules! join_items {
        ($items:expr, $sep:expr) => {
            $items.iter().map(ToString::to_string).collect::<Vec<_>>().join(&format!("{} ", $sep))
        }
    }

    macro_rules! impl_display_grouping {
        ($t:ty, $always:literal) => {
            impl fmt::Display for $t {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    if $always || !self.0.is_empty() {
                        write!(f, "{}{}{}", Self::PAIR.0, join_items!(self.0, ITEM_SEPARATOR), Self::PAIR.1)?;
                    };

                    Ok(())
                }
            }
        };
    }
}


#[derive(Debug)]
pub enum ParsingError {
    UnexpectedEOS {
        while_parsing: &'static str,
        lexing_error: Option<LexingError>,
    },
    ExpectedCommonWord {
        while_parsing: &'static str,
        got: Word,
    },
    ExpectedKeyWord {
        while_parsing: &'static str,
        got: Word,
    },
    ExpectedMarkerWord {
        while_parsing: &'static str,
        got: Word,
    },
    ExpectedBreakWord {
        while_parsing: &'static str,
        got: Word,
    },
    ExpectedLiteralWord {
        while_parsing: &'static str,
        got: Word,
    },
    ExpectedDifferentKeyWord {
        while_parsing: &'static str,
        pos: usize,
        got: String,
        expected: Vec<String>,
        optional: bool,
    },
    ExpectedDifferentMarkerWord {
        while_parsing: &'static str,
        pos: usize,
        got: String,
        expected: Vec<String>,
        optional: bool,
    }
}


pub const ITEM_SEPARATOR: &str = ",";
pub const BREAK: &str = ";";


#[derive(Debug)]
pub struct Module {
    pub imports: Vec<Import>,
    // FIXME uncomment once done definitions
    // pub definitions: Vec<Definition>
}


#[derive(Debug)]
pub struct Import {
    pub scope: Scope,
    pub module_ident: ModuleIdentifier,
}


#[derive(Debug)]
pub enum Scope {
    Private, Internal, Public
}


#[derive(Debug)]
pub enum Value {
    Boolean(bool),
    Integer(i128),
    Float(f64),
    Char(char),
    String(String),
    List(Vec<Expression>),
    Tuple(Vec<Expression>),
    Dictionary(Vec<(Expression, Expression)>)
}


impl Value {
    pub const BOOLEAN_TRUE: &'static str = "true";
    pub const BOOLEAN_FALSE: &'static str = "false";

    pub const CHAR: &'static str = "'";
    pub const STRING: &'static str = "\"";

    pub const LIST_PAIR: (&'static str, &'static str) = ("[", "]");
    pub const TUPLE_PAIR: (&'static str, &'static str) = ("#[", "]#");
    pub const DICTIONARY_PAIR: (&'static str, &'static str) = ("#{", "}#");
}


impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Boolean(value) => write!(f, "{}", if *value { Self::BOOLEAN_TRUE } else { Self::BOOLEAN_FALSE }),
            Value::Integer(value) => write!(f, "{}", value),
            Value::Float(value) => write!(f, "{:+e}", value),
            Value::Char(value) => write!(f, "{}{}", Self::CHAR, utils::string_escape(value.to_string().as_str())),
            Value::String(value) => write!(f, "{}{}{}", Self::STRING, utils::string_escape(value.as_str()), Self::STRING),
            Value::List(values) => write!(f, "{}{}{}", Self::LIST_PAIR.0, join_items!(values, ITEM_SEPARATOR), Self::LIST_PAIR.1),
            Value::Tuple(values) => write!(f, "{}{}{}", Self::TUPLE_PAIR.0, join_items!(values, ITEM_SEPARATOR), Self::TUPLE_PAIR.1),
            Value::Dictionary(value_pairs) => todo!(),
        }
    }
}


#[derive(Debug)]
pub enum Expression {
    Variable {
        identifier: Identifier,
    },
    Literal {
        value: Value,
    },
    Reference {
        expression: Box<Expression>,
    },
    Dereference {
        expression: Box<Expression>,
    },
    FunctionCall {
        identifier: Identifier,
        arguments: Arguments,
    }
}


impl Expression {
    pub const VARIABLE: &'static str = "%";
    pub const LITERAL: &'static str = ".";
    pub const REFERENCE: &'static str = "&";
    pub const DEREFERENCE: &'static str = "*";
    pub const FUNCTION_CALL: &'static str = "!";
}


impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Variable { identifier } => write!(f, "{}{}", Self::VARIABLE, identifier),
            Expression::Literal { value } => write!(f, "{}{}", Self::LITERAL, value),
            Expression::Reference { expression } => write!(f, "{}{}", Self::REFERENCE, expression),
            Expression::Dereference { expression } => write!(f, "{}{}", Self::DEREFERENCE, expression),
            Expression::FunctionCall { identifier, arguments } => write!(f, "{}{}{}", Self::FUNCTION_CALL, identifier, arguments),
        }
    }
}


#[derive(Debug)]
pub struct ModuleIdentifier {
    pub name: String,
    pub path: Vec<String>,
    pub absolute: bool,
    pub binding: bool,
}


impl ModuleIdentifier {
    pub const BINDING: &'static str = "#@";
    pub const ABSOLUTE: &'static str = "$";
    pub const PATH_SEP: &'static str = "/";
}


impl fmt::Display for ModuleIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.binding {
            write!(f, "{}", Self::BINDING)?;
        };

        if self.absolute {
            write!(f, "{}", Self::ABSOLUTE)?;
        };

        // XXX perhaps there is a better way..?
        write!(f, "{}", [self.path.clone(), vec![self.name.clone()]].concat().join(Self::PATH_SEP))
    }
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


impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(module_ident) = &self.module {
            write!(f, "{}{}{}", Self::MODULE_PRESENT, module_ident, Self::ITEM_SEP)?;
        };

        write!(f, "{}", self.name)
    }
}


#[derive(Debug)]
pub struct TypeTuple(pub Vec<Type>);


impl TypeTuple {
    pub const PAIR: (&'static str, &'static str) = ("<[", "]>");
}


impl_display_grouping!(TypeTuple, true);



#[derive(Debug)]
pub enum AppliedGeneric {
    Type(Type),
    TypeTuple(TypeTuple),
}


impl fmt::Display for AppliedGeneric {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Type(r#type) => write!(f, "{}", r#type),
            Self::TypeTuple(type_tuple) => write!(f, "{}", type_tuple)
        }
    }
}



#[derive(Debug)]
pub struct AppliedGenerics(pub Vec<AppliedGeneric>);

impl AppliedGenerics {
    pub const PAIR: (&'static str, &'static str) = ("<", ">");
}


impl_display_grouping!(AppliedGenerics, false);


#[derive(Debug)]
pub struct Type {
    pub identifier: Identifier,
    pub applied_generics: AppliedGenerics,
}

impl Type {
    pub const ANNOTATION: &'static str = ":";
}


impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.identifier, self.applied_generics)
    }
}


#[derive(Debug)]
pub struct Arguments(pub Vec<Expression>);


impl Arguments {
    pub const PAIR: (&'static str, &'static str) = ("(", ")");
}


impl_display_grouping!(Arguments, true);


#[derive(Debug)]
pub enum Command {
    Declare {
        name: String,
        rewritable: bool,
        r#type: Type,
        expression: Expression,
    },
    Assign {
        to: Expression,
        expression: Expression,
    },
    Call {
        function: Expression,
        arguments: Arguments,
    }
}

impl Command {
    pub const DECLARE_KEYWORD: &'static str = "let";
    pub const ASSIGN_KEYWORD: &'static str = "assign";
    pub const CALL_KEYWORD: &'static str = "call";

    pub const DECLARE_ONCE_KEYWORD: &'static str = "once";

    pub const ASSIGN: &'static str = "=";
}


impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Command::Declare { name, rewritable, r#type, expression } =>
                write!(f, "{}{} {}{} {} {} {}{BREAK}", Self::DECLARE_KEYWORD, if !*rewritable { format!(" {}", Self::DECLARE_ONCE_KEYWORD) } else { String::new() }, name, Type::ANNOTATION, r#type, Self::ASSIGN, expression),
            Command::Assign { to, expression } => write!(f, "{} {to} {} {expression}{BREAK}", Self::ASSIGN_KEYWORD, Self::ASSIGN),
            Command::Call { function, arguments } => write!(f, "{} {function}{arguments}{BREAK}", Self::CALL_KEYWORD),
        }
    }
}


pub struct Parser<'a, C: Iterator<Item = char>> {
    word_stream: WordStream<'a, C>,
}


type Res<T> = Result<T, ParsingError>;


impl<'a, C: Iterator<Item = char>> Parser<'a, C> {
    pub(self) fn new(word_stream: WordStream<'a, C>) -> Self {
        Self { word_stream }
    }

    fn next(&mut self, while_parsing: &'static str) -> Res<Word> {
        match self.word_stream.next() {
            Some(word) => Ok(word),
            None => Err(ParsingError::UnexpectedEOS { while_parsing, lexing_error: self.word_stream.lexing_error.clone() }),
        }
    }

    fn parse_module_identifier(&mut self) -> Res<ModuleIdentifier> {
        let binding = self.next("module identifier [binding]")?.opt_markers("module identifier [binding]", &mut self.word_stream, &[ModuleIdentifier::BINDING], &[ModuleIdentifier::ABSOLUTE])?;
        let absolute = self.next("module identifier [absolute]")?.opt_markers("module identifier [absolute]", &mut self.word_stream, &[ModuleIdentifier::ABSOLUTE], &[])?;

        let name;
        let mut path = Vec::new();
        loop {
            let ident = self.next("module identifier [path/identifier]")?.common("module identifier [path/identifier]")?;

            if self.next("module identifier [path/sep]")?.opt_markers("module identifier [path/sep]", &mut self.word_stream, &[ModuleIdentifier::PATH_SEP], &[])? {
                path.push(ident);
            } else {
                name = ident;
                break;
            };
        };

        Ok(ModuleIdentifier { path, name, absolute, binding })
    }

    fn parse_identifier(&mut self) -> Res<Identifier> {
        let mut word = self.next("identifier [name|module]")?;

        let module = match word.markers("identifier [module]", &[Identifier::MODULE_PRESENT], &[]) {
            Ok(()) => {
                let module = self.parse_module_identifier()?;

                self.next("identifier [item_sep]")?.markers("identifier [item_sep]", &[Identifier::ITEM_SEP], &[ModuleIdentifier::PATH_SEP])?;

                word = self.next("identifier [name]")?;

                Some(module)
            },
            Err(ParsingError::ExpectedMarkerWord { got, .. }) => { word = got; None },
            Err(err) => return Err(err),
        };

        Ok(Identifier { module, name: word.common("identifier [name]")? })
    }

    fn parse_value(&mut self) -> Res<Value> {
        match self.next("value")? {
            Word::LiteralBoolean { value, .. } => Ok(Value::Boolean(value)),
            Word::LiteralInteger { value, .. } => Ok(Value::Integer(value)),
            Word::LiteralFloat { value, .. } => Ok(Value::Float(value)),
            Word::LiteralChar { value, .. } => Ok(Value::Char(value)),
            Word::LiteralString { value, .. } => Ok(Value::String(value)),
            _ => todo!()
        }
    }

    fn parse_arguments(&mut self) -> Res<Arguments> {
        Ok(Arguments(self.parse_items("arguments", Arguments::PAIR, Self::parse_expression)?))
    }

    fn parse_expression(&mut self) -> Res<Expression> {
        match self.next("expression [type]")? {
            Word::Marker { value, pos } =>
                match value.as_str() {
                    Expression::VARIABLE => Ok(Expression::Variable { identifier: self.parse_identifier()? }),
                    Expression::LITERAL => Ok(Expression::Literal { value: self.parse_value()? }),
                    Expression::REFERENCE => Ok(Expression::Reference { expression: Box::new(self.parse_expression()?) }),
                    Expression::DEREFERENCE => Ok(Expression::Dereference { expression: Box::new(self.parse_expression()?) }),
                    Expression::FUNCTION_CALL => Ok(Expression::FunctionCall { identifier: self.parse_identifier()?, arguments: self.parse_arguments()? }),
                    _ => Err(ParsingError::ExpectedDifferentMarkerWord { while_parsing: "expression [type]", pos, got: value, optional: false,
                        expected: helper::le_convert(&[Expression::VARIABLE, Expression::LITERAL, Expression::REFERENCE, Expression::DEREFERENCE, Expression::FUNCTION_CALL]) })
                },
            word => Err(ParsingError::ExpectedMarkerWord { while_parsing: "expression [type]", got: word }),
        }
    }

    fn parse_items<T>(&mut self, while_parsing: &'static str, marker_pair: (&str, &str), parsing_function: impl Fn(&mut Self) -> Res<T>) -> Res<Vec<T>> {
        let mut items = Vec::new();

        self.next(while_parsing)?.markers(while_parsing, &[marker_pair.0], &[])?;

        if self.next(while_parsing)?.opt_markers(while_parsing, &mut self.word_stream, &[marker_pair.1], &[])? {
            return Ok(items);
        };

        // TODO allow for ITEM_SEPARATOR before closing word, ie for arguments: "(.0, )"
        loop {
            items.push(parsing_function(self)?);

            match self.next(while_parsing)? {
                Word::Marker { value, pos } =>
                    match value.as_str() {
                        ITEM_SEPARATOR => {},
                        v if v == marker_pair.1 => break,
                        _ => return Err(ParsingError::ExpectedDifferentMarkerWord { while_parsing, pos, got: value, optional: false,
                            expected: helper::le_convert(&[marker_pair.1, ITEM_SEPARATOR]) }),
                    },
                word => return Err(ParsingError::ExpectedMarkerWord { while_parsing, got: word }),
            };
        };

        Ok(items)
    }

    fn parse_opt_items<T>(&mut self, while_parsing: &'static str, marker_pair: (&str, &str), parsing_function: impl Fn(&mut Self) -> Res<T>) -> Res<Vec<T>> {
        let mut items = Vec::new();

        if self.next(while_parsing)?.opt_markers(while_parsing, &mut self.word_stream, &[marker_pair.0], &[])? {
            if self.next(while_parsing)?.opt_markers(while_parsing, &mut self.word_stream, &[marker_pair.1], &[])? {
                return Ok(items);
            };

            // TODO allow for ITEM_SEPARATOR before closing word, ie for arguments: "(.0, )"
            loop {
                items.push(parsing_function(self)?);

                match self.next(while_parsing)? {
                    Word::Marker { value, pos } =>
                        match value.as_str() {
                            ITEM_SEPARATOR => {},
                            v if v == marker_pair.1 => break,
                            _ => return Err(ParsingError::ExpectedDifferentMarkerWord { while_parsing, pos, got: value, optional: false,
                                expected: helper::le_convert(&[marker_pair.1, ITEM_SEPARATOR]) }),
                        },
                    word => return Err(ParsingError::ExpectedMarkerWord { while_parsing, got: word }),
                };
            };
        };

        Ok(items)
    }

    fn parse_type_tuple(&mut self) -> Res<TypeTuple> {
        Ok(TypeTuple(self.parse_items("type tuple", TypeTuple::PAIR, Self::parse_type)?))
    }

    fn parse_applied_generic(&mut self) -> Res<AppliedGeneric> {
        match self.next("applied generic [<determining type|type_tuple>]")? {
            Word::Marker { value, pos }
                if value.as_str() == TypeTuple::PAIR.0 => {
                    self.word_stream.redeem(Word::Marker { value, pos });
                    Ok(AppliedGeneric::TypeTuple(self.parse_type_tuple()?))
                },
            word => {
                self.word_stream.redeem(word);
                Ok(AppliedGeneric::Type(self.parse_type()?))
            }
        }
    }

    fn parse_applied_generics(&mut self) -> Res<AppliedGenerics> {
        Ok(AppliedGenerics(self.parse_opt_items("applied generics", AppliedGenerics::PAIR, Self::parse_applied_generic)?))
    }

    fn parse_type(&mut self) -> Res<Type> {
        let identifier = self.parse_identifier()?;
        let applied_generics = self.parse_applied_generics()?;

        Ok(Type { identifier, applied_generics })
    }

    fn parse_type_annotation(&mut self) -> Res<Type> {
        self.next("type [annotation]")?.markers("type [annotation]", &[Type::ANNOTATION], &[])?;
        self.parse_type()
    }

    fn parse_command(&mut self) -> Res<Command> {
        match self.next("command [kind]")? {
            Word::Key { value, pos } => {
                match value.as_str() {
                    Command::DECLARE_KEYWORD => {
                        let rewritable = !self.next("command [declare/rewritable]")?.opt_keys("command [declare/rewritable]", &mut self.word_stream,&[Command::DECLARE_ONCE_KEYWORD], &[])?;
                        let name = self.next("command [declare/name]")?.common("command [declare/name]")?;
                        let r#type = self.parse_type_annotation()?;
                        self.next("command [declare/<assign marker>]")?.markers("command [declare/<assign marker>]", &[Command::ASSIGN], &[])?;
                        let expression = self.parse_expression()?;

                        self.next("command [declare/<end>]")?.r#break("command [declare/<end>]")?;

                        Ok(Command::Declare { name, r#type, expression, rewritable })
                    },
                    Command::ASSIGN_KEYWORD => {
                        let to = self.parse_expression()?;
                        self.next("command [assign/<assign marker>]")?.markers("command [assign/<assign marker>]", &[Command::ASSIGN], &[])?;
                        let expression = self.parse_expression()?;

                        self.next("command [assign/<end>]")?.r#break("command [assign/<end>]")?;

                        Ok(Command::Assign { to, expression })
                    },
                    Command::CALL_KEYWORD => {
                        let function = self.parse_expression()?;
                        let arguments = self.parse_arguments()?;

                        self.next("command [call/<end>]")?.r#break("command [call/<end>]")?;

                        Ok(Command::Call { function, arguments })
                    },
                    _ => Err(ParsingError::ExpectedDifferentKeyWord { while_parsing: "command [kind]", pos, got: value, optional: false,
                        expected: helper::le_convert(&[Command::DECLARE_KEYWORD, Command::ASSIGN_KEYWORD, Command::CALL_KEYWORD]) })
                }
            },
            word => Err(ParsingError::ExpectedKeyWord { while_parsing: "command [kind]", got: word })
        }
    }

    fn parse_module(&mut self) -> Res<Module> {
        todo!()
    }
}


pub fn parse(word_stream: WordStream<'_, impl Iterator<Item = char>>) -> Result<Command, ParsingError> {
    Parser::new(word_stream).parse_command()
}
