/// A Grammar for `lox` expressions
///
/// ```latex
/// Expr -> Literal | Unary | Binary | Grouping
/// Literal -> Number | String | Bool | Nil
/// Bool -> "true" | "false"
/// Nil -> "nil"
/// Grouping -> "(" Expr ")"
/// Unary -> ("-" | "!") Expr
/// Binary -> Expr Operator Expr
/// Operator = "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "/"
/// ```
#[derive(Debug)]
pub enum Expr {
    Unary,
    Binary,
    Literal,
    Grouping,
}
