use std::{collections::HashMap, fmt::Display};

use ordered_float::OrderedFloat;
use phf::phf_map;
use thiserror::Error;

use DoubleCharacterToken::*;
use Keyword::*;
use SingleCharacterToken::*;

/// The location of a [Token]"s lexeme in the
/// source code.
#[derive(Debug, Default, Clone)]
pub struct Location {
    /// Our vertical location in the file
    line_number: u16,

    /// Our horizontal location in the file
    column_number: u16,
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Ln {}, Col {}", self.line_number, self.column_number))
    }
}

impl From<(u16, u16)> for Location {
    fn from((x, y): (u16, u16)) -> Self {
        Location {
            line_number: x,
            column_number: y,
        }
    }
}

impl Location {
    #[inline]
    pub(crate) fn advance_col(&mut self) {
        self.column_number += 1;
    }

    #[inline]
    pub(crate) fn advance_row(&mut self) {
        self.column_number = 0;
        self.line_number += 1;
    }
}

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Unexpected `{character}` at {location}")]
    UnexpectedCharacter { character: char, location: Location },
}

pub static LEXEME_TO_TOKEN_MAPPER: phf::Map<&'static str, Token> = phf_map! {
    // Map Keywords
    "and" => Token::KeywordToken(And),
    "class" => Token::KeywordToken(Class),
    "if" => Token::KeywordToken(If),
    "else" => Token::KeywordToken(Else),
    "true" => Token::KeywordToken(True),
    "false" => Token::KeywordToken(False),
    "fun" => Token::KeywordToken(Fun),
    "for" => Token::KeywordToken(For),
    "while" => Token::KeywordToken(While),
    "var" => Token::KeywordToken(Var),
    "nil" => Token::KeywordToken(Nil),
    "or" => Token::KeywordToken(Or),
    "print" => Token::KeywordToken(Print),
    "return" => Token::KeywordToken(Return),
    "super" => Token::KeywordToken(Super),
    "this" => Token::KeywordToken(This),

    // Map Single and Double character tokens
    "(" => Token::Single(LeftBrace),
    ")" => Token::Single(RightBrace),
    "{" => Token::Single(LeftParenthesis),
    "}" => Token::Single(RightParenthesis),
    "+" => Token::Single(Plus),
    "-" => Token::Single(Minus),
    "," => Token::Single(Comma),
    "." => Token::Single(Dot),
    ";" => Token::Single(SemiColon),
    "*" => Token::Single(Star),
    "!" => Token::Single(Not),
    "/" => Token::Single(Slash),
    "!=" => Token::Double(NotEqual),
    "=" => Token::Single(EqualSign),
    "==" => Token::Double(EqualEqualSign),
    "<" => Token::Single(LessThan),
    "<=" => Token::Double(LessThanOrEqual),
    ">" => Token::Single(GreaterThan),
    ">=" => Token::Double(GreaterThanOrEqual),
};

lazy_static! {
    static ref TOKEN_TO_LEXEME_MAPPER: HashMap<Token, &'static str> = {
        let mut mapper = HashMap::with_capacity(48);

        // Map Keywords
        mapper.insert(Token::KeywordToken(And), "and");
        mapper.insert(Token::KeywordToken(Class), "class");
        mapper.insert(Token::KeywordToken(If), "if");
        mapper.insert(Token::KeywordToken(Else), "else");
        mapper.insert(Token::KeywordToken(True), "true");
        mapper.insert(Token::KeywordToken(False), "false");
        mapper.insert(Token::KeywordToken(Fun), "fun");
        mapper.insert(Token::KeywordToken(For), "for");
        mapper.insert(Token::KeywordToken(While), "while");
        mapper.insert(Token::KeywordToken(Var), "var");
        mapper.insert(Token::KeywordToken(Nil), "nil");
        mapper.insert(Token::KeywordToken(Or), "or");
        mapper.insert(Token::KeywordToken(Print), "print");
        mapper.insert(Token::KeywordToken(Return), "return");
        mapper.insert(Token::KeywordToken(Super), "super");
        mapper.insert(Token::KeywordToken(This), "this");

        // Map Single and Double character tokens
        mapper.insert(Token::Single(LeftBrace), "(");
        mapper.insert(Token::Single(RightBrace), ")");
        mapper.insert(Token::Single(LeftParenthesis), "{");
        mapper.insert(Token::Single(RightParenthesis), "}");
        mapper.insert(Token::Single(Plus), "+");
        mapper.insert(Token::Single(Minus), "-");
        mapper.insert(Token::Single(Comma), ",");
        mapper.insert(Token::Single(Dot), ".");
        mapper.insert(Token::Single(SemiColon), ";");
        mapper.insert(Token::Single(Star), "*");
        mapper.insert(Token::Single(Not), "!");
        mapper.insert(Token::Single(Slash), "/");
        mapper.insert(Token::Double(NotEqual), "!=");
        mapper.insert(Token::Single(EqualSign), "=");
        mapper.insert(Token::Double(EqualEqualSign), "==");
        mapper.insert(Token::Single(LessThan), "<");
        mapper.insert(Token::Double(LessThanOrEqual), "<=");
        mapper.insert(Token::Single(GreaterThan), ">");
        mapper.insert(Token::Double(GreaterThanOrEqual), ">=");

        mapper
    };
}

/// All
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum SingleCharacterToken {
    /// ## (
    LeftParenthesis,
    /// ## )
    RightParenthesis,
    /// ## {
    LeftBrace,
    /// ## }
    RightBrace,
    /// ## ,
    Comma,
    /// ## .
    Dot,
    /// ## -
    Minus,
    /// ## +
    Plus,
    /// ## ;
    SemiColon,
    /// ## /
    Slash,
    /// ## *
    Star,
    /// ## !
    Not,
    /// ## =
    EqualSign,
    /// ## >
    GreaterThan,
    /// ## <
    LessThan,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum DoubleCharacterToken {
    /// ## !=
    NotEqual,
    /// ## ==
    EqualEqualSign,
    /// ## >=
    GreaterThanOrEqual,
    /// ## <=
    LessThanOrEqual,
}

/// Literals can be numbers, variable names, function names, class names, or strings
/// surrounded by double quotes `"`
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Literal {
    Number(OrderedFloat<f32>),
    /// An identifier can be a variable name, a function name ...
    Identifier(String),
    /// A string is anything within double quotes `"<string>"`
    StringLiteral(String),
}

/// Keywords are literals that have been reserved for
/// the language"s internal use
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Keyword {
    /// ## and
    And,
    /// ## class
    Class,
    /// ## if
    If,
    /// ## else
    Else,
    /// ## true
    True,
    /// ## false
    False,
    /// ## fun
    Fun,
    /// ## for
    For,
    /// ## while
    While,
    /// ## var
    Var,
    /// ## nil
    Nil,
    /// ## or
    Or,
    /// ## print
    Print,
    /// ## return
    Return,
    /// ## super
    Super,
    /// ## this
    This,
}

/// All the valid tokens in the `lox` language
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    Eof,
    LiteralToken(Literal),
    KeywordToken(Keyword),
    Single(SingleCharacterToken),
    Double(DoubleCharacterToken),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eof => f.write_str("END_OF_FILE"),
            Self::LiteralToken(literal) => match literal {
                Literal::Number(value) => f.write_str(&value.to_string()),
                Literal::Identifier(value) => f.write_str(value),
                Literal::StringLiteral(value) => f.write_str(value),
            },
            _ => f.write_str(TOKEN_TO_LEXEME_MAPPER.get(self).unwrap()),
        }
    }
}

#[derive(Debug)]
pub struct TokenStream(pub(crate) Vec<Token>);
