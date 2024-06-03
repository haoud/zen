use crate::lang::{self, Span};

/// An expression. Expressions are evaluated to produce a value that
/// can be used in other expressions or statements. Expressions that
/// do not produce a value are called statements (e.g. `let x = 1;`).
#[derive(Debug, Clone)]
pub struct Expr {
    /// The kind of expression.
    pub kind: ExprKind,

    /// The span of the expression.
    pub span: Span,
}

/// Different kinds of expressions supported by the language.
#[derive(Debug, Clone)]
pub enum ExprKind {
    /// A literal value expression.
    Literal(u64),

    /// A binary operation expression. The first element is the binary
    /// operator, and the second and third elements are the left and
    /// right operands, respectively.
    /// Example: 1 + 2
    Binary(lang::operator::BinaryOp, Box<Expr>, Box<Expr>),

    /// A error expression that is used to indicate a parsing error. This
    /// variant should not be present in the final AST that will be passed
    /// to the code generator.
    Error(Span),
}
