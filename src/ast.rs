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
enum Expr {
    /// Unary -> ("-" | "!") Expr
    Unary { op: UnOp, expr: Box<Expr> },
    /// Binary -> Expr Operator Expr
    Binary { lhs: Box<Expr>, op: BinOp, rhs: Box<Expr> },
    /// Literal -> Number | String | Bool | Nil
    Literal { val: LiteralVal },
    /// Grouping -> "(" Expr ")"
    Grouping { expr: Box<Expr> },
}

/// Literal -> Number | String | Bool | Nil
#[derive(Debug)]
enum LiteralVal {
    Nil,
    Str(String),
    Number(f32),
    Bool(bool),
}

#[derive(Debug)]
enum UnOp {
    Not,
    Negative,
}

/// Operator = "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "/"
#[derive(Debug)]
enum BinOp {
    EqualTo,
    NotEqualTo,
    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
    Plus,
    Minus,
    DividedBy,
}
