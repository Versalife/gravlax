use crate::errors;

/// All
#[derive(Debug)]
pub enum SingleCharacterToken<'a> {
    /// ## (
    LeftParenthesis(usize, &'a str),
    /// ## )
    RightParenthesis(usize, &'a str),
    /// ## {
    LeftBrace(usize, &'a str),
    /// ## }
    RightBrace(usize, &'a str),
    /// ## ,
    Comma(usize, &'a str),
    /// ## .
    Dot(usize, &'a str),
    /// ## -
    Minus(usize, &'a str),
    /// ## +
    Plus(usize, &'a str),
    /// ## ;
    SemiColon(usize, &'a str),
    /// ## /
    Slash(usize, &'a str),
    /// ## *
    Star(usize, &'a str),
    /// ## !
    Not(usize, &'a str),
    /// ## =
    EqualSign(usize, &'a str),
    /// ## >
    GreaterThan(usize, &'a str),
    /// ## <
    LessThan(usize, &'a str),
}

#[derive(Debug)]
pub enum DoubleCharacterToken<'a> {
    /// ## !=
    NotEqual(usize, &'a str),
    /// ## ==
    EqualEqualSign(usize, &'a str),
    /// ## >=
    GreaterThanOrEqual(usize, &'a str),
    /// ## <=
    LessThanOrEqual(usize, &'a str),
}

/// Literals can be numbers, variable names, or strings
/// surrounded by double quotes `"`
#[derive(Debug)]
pub enum Literal<'a> {
    Number(usize, f32),
    VariableName(usize, &'a str),
    StringLiteral(usize, &'a str),
}

/// Keywords are literals that have been reserved for
/// the language's internal use
#[derive(Debug)]
pub enum Keyword<'a> {
    /// ## and
    And(usize, &'a str),
    /// ## class
    Class(usize, &'a str),
    /// ## if
    If(usize, &'a str),
    /// ## else
    Else(usize, &'a str),
    /// ## true
    True(usize, &'a str),
    /// ## false
    False(usize, &'a str),
    /// ## fun
    Fun(usize, &'a str),
    /// ## for
    For(usize, &'a str),
    /// ## while
    While(usize, &'a str),
    /// ## var
    Var(usize, &'a str),
    /// ## nil
    Nil(usize, &'a str),
    /// ## or
    Or(usize, &'a str),
    /// ## print
    Print(usize, &'a str),
    /// ## return
    Return(usize, &'a str),
    /// ## super
    Super(usize, &'a str),
    /// ## this
    This(usize, &'a str),
}

/// All the valid tokens in the `lox` language
#[derive(Debug)]
pub enum Token<'a> {
    /// The end of the source code marker
    EOF,
    Literal(Literal<'a>),
    Keyword(Keyword<'a>),
    SingleCharacterToken(SingleCharacterToken<'a>),
    DoubleCharacterToken(DoubleCharacterToken<'a>),
}

#[derive(Debug)]
pub struct TokenStream<'a>(Vec<Token<'a>>);

#[derive(Debug)]
pub struct Lexer {}

impl Lexer {
    pub fn lex(source_code: &str) -> Result<TokenStream, errors::LexerError> {
        unimplemented!()
    }
}
