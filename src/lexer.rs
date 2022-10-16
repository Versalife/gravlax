use std::{collections::HashMap, fmt::Display, iter::Peekable, str::Chars};

use ordered_float::OrderedFloat;
use phf::phf_map;

use crate::errors::LexerError;

use DoubleCharacterToken::*;
use Keyword::*;
use SingleCharacterToken::*;

static LEXEME_TO_TOKEN_MAPPER: phf::Map<&'static str, Token> = phf_map! {
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
        mapper.insert(Token::KeywordToken(And), "and");
        // TODO: Fill this out
        mapper
    };
}

// static KEYWORD_TO_LEXEME_MAPPER: phf::Map<Keyword, &'static str> = phf_map! {
//     // Map Keywords
//     And=>"and",
//     Class=>"class",
//     If=>"if",
//     Else=>"else",
//     True=>"true",
//     False=>"false",
//     Fun=>"fun",
//     For=>"for",
//     While=>"while",
//     Var=>"var",
//     Nil=>"nil",
//     Or=>"or",
//     Print=>"print",
//     Return=>"return",
//     Super=>"super",
//     This=>"this",
// };

// static CHARACTER_TOKEN_TO_LEXEME_MAPPER: phf::Map<Keyword, &'static str> = phf_map! {
//     // Map Single and Double character tokens
//     Single(LeftBrace) => "(",
//     Single(RightBrace) => ")",
//     Single(LeftParenthesis) => "{",
//     Single(RightParenthesis) => "}",
//     Single(Plus) => "+",
//     Single(Minus) => "-",
//     Single(Comma) => ",",
//     Single(Dot) => ".",
//     Single(SemiColon) => ";",
//     Single(Star) => "*",
//     Single(Not) => "!",
//     Single(Slash) => "/",
//     Double(NotEqual) => "!=",
//     Single(EqualSign) => "=",
//     Double(EqualEqualSign) => "==",
//     Single(LessThan) => "<",
//     Double(LessThanOrEqual) => "<=",
//     Single(GreaterThan) => ">",
//     Double(GreaterThanOrEqual) => ">=",
// };

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

/// Literals can be numbers, variable names, or strings
/// surrounded by double quotes `"`
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Literal {
    Number(OrderedFloat<f32>),
    VariableName(String),
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
    EOF,
    LiteralToken(Literal),
    KeywordToken(Keyword),
    Single(SingleCharacterToken),
    Double(DoubleCharacterToken),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EOF => f.write_str("END_OF_FILE"),
            Self::LiteralToken(literal) => match literal {
                Literal::Number(value) => f.write_str(&value.to_string()),
                Literal::VariableName(value) => f.write_str(value),
                Literal::StringLiteral(value) => f.write_str(value),
            },
            Self::KeywordToken(keyword) => f.write_str("END_OF_FILE"),
            Self::Single(character) => f.write_str("END_OF_FILE"),
            Self::Double(characters) => f.write_str("END_OF_FILE"),
        }
    }
}

/// The location of a [Token]"s lexeme in the
/// source code.
#[derive(Debug, Default, Clone)]
pub struct Location {
    /// Our vertical location in the file
    line_number: usize,

    /// Our horizontal location in the file
    column_number: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Ln {}, Col {}", self.line_number, self.column_number))
    }
}

impl From<(usize, usize)> for Location {
    fn from((x, y): (usize, usize)) -> Self {
        Location {
            line_number: x,
            column_number: y,
        }
    }
}

#[derive(Debug)]
pub struct TokenStream(Vec<Token>);

#[derive(Debug)]
pub struct Lexer {
    source_code: String,
    current_location: Location,
}

impl Lexer {
    /// Create a new lexer for the provided source code
    pub fn new(source_code: String) -> Self {
        Lexer {
            source_code,
            current_location: Location::default(),
        }
    }

    /// Scan the source code to generate a stream of [Token]`s producing
    /// [LexerError] if any errors are encountered.
    pub fn lex(&mut self) -> Result<TokenStream, Vec<LexerError>> {
        let mut errors = Vec::new();
        let mut tokens = Vec::with_capacity(self.source_code.len());
        let mut code_characters = self.source_code.chars().peekable();
        while let Some(character) = code_characters.next() {
            match character {
                '(' => tokens.push(LEXEME_TO_TOKEN_MAPPER.get("(").cloned().unwrap()),
                ')' => tokens.push(LEXEME_TO_TOKEN_MAPPER.get(")").cloned().unwrap()),
                '{' => tokens.push(LEXEME_TO_TOKEN_MAPPER.get("{").cloned().unwrap()),
                '}' => tokens.push(LEXEME_TO_TOKEN_MAPPER.get("}").cloned().unwrap()),
                '+' => tokens.push(LEXEME_TO_TOKEN_MAPPER.get("+").cloned().unwrap()),
                '-' => tokens.push(LEXEME_TO_TOKEN_MAPPER.get("-").cloned().unwrap()),
                ',' => tokens.push(LEXEME_TO_TOKEN_MAPPER.get(",").cloned().unwrap()),
                '.' => tokens.push(LEXEME_TO_TOKEN_MAPPER.get(".").cloned().unwrap()),
                ';' => tokens.push(LEXEME_TO_TOKEN_MAPPER.get(";").cloned().unwrap()),
                '*' => tokens.push(LEXEME_TO_TOKEN_MAPPER.get("*").cloned().unwrap()),
                '!' => Self::probably_add_double_token(&mut tokens, character, &mut code_characters),
                _ => errors.push(LexerError::UnexpectedCharacter {
                    character,
                    location: self.current_location.clone(),
                }),
            }
        }
        tokens.push(Token::EOF);
        if errors.len() == 0 {
            Ok(TokenStream(tokens))
        } else {
            Err(errors)
        }
    }

    /// Look ahead one step. Add a [Token::Double] if the next character matched the expected
    /// character. Otherwise add a [Token::Single]
    fn probably_add_double_token(
        tokens: &mut Vec<Token>,
        current_token: char,
        mut code_characters: &mut Peekable<Chars>,
    ) -> () {
        if Self::one_step_look_ahead(current_token, &mut code_characters) {
            // How to know what to push?
            // Create a static map from str to enum types for token
            // To know what to push we simply look up items in this map
            // use phf
            // tokens.push()
        } else {
        }
    }

    /// Peek at the next character. If it is what we `expect`, we consume it by
    /// advancing the iterator then return `true`. Otherwise, we return false
    fn one_step_look_ahead(expect: char, code_characters: &mut Peekable<Chars>) -> bool {
        if let Some(next_character) = code_characters.peek() {
            match expect.cmp(next_character) {
                std::cmp::Ordering::Equal => {
                    code_characters.next();
                    return true;
                }
                _ => return false,
            }
        }
        // There is no next character. We are at the end of the file.
        return false;
    }
}
