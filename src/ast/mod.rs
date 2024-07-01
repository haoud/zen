use crate::lang::{self, Span};

/// An expression. Expressions are evaluated to produce a value that
/// can be used in other expressions or statements. Expressions that
/// do not produce a value are called statements (e.g. `let x = 1;`).
#[derive(Debug, Clone)]
pub struct Expr<'src> {
    /// The kind of expression.
    pub kind: ExprKind<'src>,

    /// The span of the expression.
    pub span: Span,
}

/// Different kinds of expressions supported by the language.
#[derive(Debug, Clone)]
pub enum ExprKind<'src> {
    /// A literal value expression.
    Literal(u64),

    /// An identifier expression.
    Identifier(&'src str),

    /// A return expression. The element is the expression that is returned.
    Return(Box<Expr<'src>>),

    /// A let expression. The first element is the identifier and the second
    /// element is the expression that is bound to the identifier.
    Let(Box<Expr<'src>>, Box<Expr<'src>>),

    /// A binary operation expression. The first element is the binary
    /// operator, and the second and third elements are the left and
    /// right operands, respectively.
    /// Example: 1 + 2
    Binary(lang::operator::BinaryOp, Box<Expr<'src>>, Box<Expr<'src>>),

    /// A error expression that is used to indicate a parsing error. This
    /// variant should not be present in the final AST that will be passed
    /// to the code generator.
    Error(Span),
}

impl ExprKind<'_> {
    /// Assume that the expression is an identifier and return the identifier
    /// as a string slice. If the expression is not an identifier, then `None`
    /// is returned.
    #[must_use]
    pub fn as_identifier(&self) -> Option<&str> {
        match self {
            ExprKind::Identifier(ident) => Some(ident),
            _ => None,
        }
    }
}
