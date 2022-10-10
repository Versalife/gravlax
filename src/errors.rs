use thiserror::Error;

#[derive(Error, Debug)]
pub enum GravlaxError {}
#[derive(Error, Debug)]
pub enum LexerError {}

#[derive(Error, Debug)]
pub enum ParserError {}
