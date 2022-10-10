use crate::errors;

/// All
#[derive(Debug)]
pub enum SingleCharacterToken<'a> {
    LeftParenthesis(usize, &'a str),
    RightParenthesis(usize, &'a str),
    LeftBrace(usize, &'a str),
    RightBrace(usize, &'a str),
    Comma(usize, &'a str),
    Dot(usize, &'a str),
    Minus(usize, &'a str),
    Plus(usize, &'a str),
    SemiColon(usize, &'a str),
    Slash(usize, &'a str),
    Star(usize, &'a str),
    /// !
    Not(usize, &'a str),
    /// =
    EqualSign(usize, &'a str),
    /// >
    GreaterThan(usize, &'a str),
    /// <
    LessThan(usize, &'a str),
}

#[derive(Debug)]
pub enum DoubleCharacterToken<'a> {
    NotEqual(usize, &'a str),
    EqualEqualSign(usize, &'a str),
    GreaterThanOrEqual(usize, &'a str),
    LessThanOrEqual(usize, &'a str),
}

#[derive(Debug)]
pub enum Literal<'a> {
    Number(usize, &'a str),
    VariableName(usize, &'a str),
    StringLiteral(usize, &'a str),
}

#[derive(Debug)]
pub enum Keyword<'a> {
    And(usize, &'a str),
    Class(usize, &'a str),
    If(usize, &'a str),
    Else(usize, &'a str),
    True(usize, &'a str),
    False(usize, &'a str),
    Fun(usize, &'a str),
    For(usize, &'a str),
    While(usize, &'a str),
    Var(usize, &'a str),
    Nil(usize, &'a str),
    Or(usize, &'a str),
    Print(usize, &'a str),
    Return(usize, &'a str),
    Super(usize, &'a str),
    This(usize, &'a str),
}

#[derive(Debug)]
pub enum Token<'a> {
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
