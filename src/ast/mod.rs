use crate::lang::{self, types::Type, Span};

/// A function prototype. A function prototype is a declaration of a function
/// that specifies the name of the function, the list of parameters that the
/// function takes, and the return type of the function. Function prototypes
/// are used to declare functions before they are defined. This allows functions
/// to be called before they are defined, which is useful when functions call
/// each other interdependently.
#[derive(Debug, Clone)]
pub struct FunctionPrototype<'src> {
    /// The name of the function.
    pub name: &'src str,

    /// A list of parameters that the function takes.
    pub args: Vec<Parameter<'src>>,

    /// The return type of the function.
    pub ret: Type,

    /// The span of the function prototype.
    pub span: Span,
}

/// A function. Functions are the main building blocks of the language.
/// They can be called from other functions or from the entry point of
/// the program. Functions can have a return type and a body that contains
/// a list of expressions that are evaluated when the function is called.
#[derive(Debug, Clone)]
pub struct Function<'src> {
    /// The prototype of the function. The prototype contains the name of
    /// the function, the list of parameters that the function takes, and
    /// the return type of the function.
    pub prototype: FunctionPrototype<'src>,

    /// A list of expressions that make up the body of the function.
    pub body: Vec<Expr<'src>>,

    /// The span of the function.
    pub span: Span,
}

/// A function parameter. Parameters are used to pass values to functions.
#[derive(Debug, Clone)]
pub struct Parameter<'src> {
    /// The name of the parameter.
    pub name: &'src str,

    /// The type of the parameter.
    pub ty: Type,

    /// The span of the parameter.
    pub span: Span,
}

/// An expression. Expressions are evaluated to produce a value that
/// can be used in other expressions or statements. Expressions that
/// do not produce a value are called statements (e.g. `let x = 1;`).
#[derive(Debug, Clone)]
pub struct Expr<'src> {
    /// The kind of expression.
    pub kind: ExprKind<'src>,

    /// The type of the expression.
    pub ty: Type,

    /// The span of the expression.
    pub span: Span,
}

/// Different kinds of expressions supported by the language.
#[derive(Debug, Clone)]
pub enum ExprKind<'src> {
    /// A function call expression. The first element is the function
    /// that is called, and the second element is a list of arguments
    /// that are passed to the function.
    Call(Box<Expr<'src>>, Vec<Expr<'src>>),

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

impl<'src> ExprKind<'src> {
    /// Assume that the expression is an identifier and return the identifier
    /// as a string slice. If the expression is not an identifier, then `None`
    /// is returned.
    #[must_use]
    pub fn as_identifier(&self) -> Option<&'src str> {
        match self {
            ExprKind::Identifier(ident) => Some(ident),
            _ => None,
        }
    }
}
