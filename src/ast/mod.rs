use crate::lang::{self, types::Type, Span};

/// A identifier. Identifiers are used to name variables, functions, and other
/// entities in the language. Identifiers are sequences of letters, numbers,
/// and underscores that start with a letter or an underscore.
#[derive(Debug, Clone)]
pub struct Identifier<'src> {
    /// The name of the identifier.
    pub name: &'src str,

    /// The span of the identifier.
    pub span: Span,
}

/// A variable in the AST. Variables are used to store values that can be
/// used in expressions or statements. A variable is defined by its name
/// and its type, both of which cannot be changed once the variable is
/// defined.
#[derive(Debug, Clone)]
pub struct Variable<'src> {
    /// The name of the variable.
    pub ident: Identifier<'src>,

    /// The type of the variable.
    pub ty: Type,
}

/// A literal value.
///
/// # Type inference
/// Most of the time, the type of the literal is inferred from the context in
/// which it is used. For example, if the literal is used in a binary operation
/// with another literal, the type of the literal is inferred from the type of
/// the other literal. However, literals can have an explicit type, which is
/// useful when the type of the literal cannot be inferred from the context or
/// to enforce a specific type.
#[derive(Debug, Clone)]
pub struct Literal {
    /// The value of the literal. For now, literals are only integers and are
    /// stored as 64-bit unsigned integers in the AST, allowing some indesirable
    /// behavior if the value is larger than its type can hold. Those kind of
    /// errors will be caught by the semantic analysis phase.
    pub value: u64,

    /// The type of the literal. Most of the time, the type of the literal is
    /// inferred from the context in which it is used. However, literals can
    /// have an explicit type, which is useful when the type of the literal
    /// cannot be inferred from the context.
    pub ty: Type,

    /// The span of the literal.
    pub span: Span,
}

/// A function prototype. A function prototype is a declaration of a function
/// that specifies the name of the function, the list of parameters that the
/// function takes, and the return type of the function. Function prototypes
/// are used to declare functions before they are defined. This allows functions
/// to be called before they are defined, which is useful when functions call
/// each other interdependently.
#[derive(Debug, Clone)]
pub struct FunctionPrototype<'src> {
    /// The name of the function.
    pub ident: Identifier<'src>,

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

    /// A list of statements that make up the body of the function.
    pub body: Vec<Stmt<'src>>,

    /// The span of the function.
    pub span: Span,
}

/// A function parameter. Parameters are used to pass values to functions
/// and are used in the function's body to refer to the values that are
/// passed to the function. Parameters are defined by their identifier and
/// their type, both of which cannot be changed once the parameter is defined.
#[derive(Debug, Clone)]
pub struct Parameter<'src> {
    /// The name of the parameter.
    pub ident: Identifier<'src>,

    /// The type of the parameter.
    pub ty: Type,

    /// The span of the parameter.
    pub span: Span,
}

/// An expression. Expressions are evaluated to produce a value that
/// can be used in other expressions or statements. Expressions that
/// do not produce a value are called statements (e.g. `let x = 1;`)
/// and are different from expressions that produce a value (e.g. `1 + 2`).
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

    /// An identifier expression.
    Identifier(Identifier<'src>),

    /// A literal value expression.
    Literal(Literal),

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
    /// Returns the identifier of the expression if the expression is an
    /// identifier. Otherwise, returns `None`.
    #[must_use]
    pub fn as_identifier(&self) -> Option<&Identifier> {
        match self {
            ExprKind::Identifier(ident) => Some(ident),
            _ => None,
        }
    }
}

/// A statement. Statements are fragments of code that are executed in
/// sequence in a program.
#[derive(Debug, Clone)]
pub struct Stmt<'src> {
    /// The kind of statement.
    pub kind: StmtKind<'src>,

    /// The span of the statement.
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind<'src> {
    /// A let statement. The first element is the variable declaration and
    /// the second element is the expression that is assigned to the variable.
    Let(Variable<'src>, Box<Expr<'src>>),

    /// An expression statement. The element is the expression that is
    /// evaluated. Some expressions can be used as statements, such as
    /// function calls.
    Expr(Box<Expr<'src>>),

    /// A return statement. The element is the expression that is returned.
    Return(Box<Expr<'src>>),

    /// A error statement that is used to indicate a parsing error. This
    /// variant should not be present in the final AST that will be passed
    /// to the code generator.
    Error(Span),
}
