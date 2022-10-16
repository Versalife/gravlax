use crate::lexer::Location;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum GravlaxError {}
#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Unexpected `{character}` at {location}")]
    UnexpectedCharacter { character: char, location: Location },
}

#[derive(Error, Debug)]
pub enum ParserError {}
