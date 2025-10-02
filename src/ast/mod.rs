use crate::lang::{self, Span, Spanned, Type};

/// A identifier. Identifiers are used to name variables, functions, and other
/// entities in the language. Identifiers are sequences of letters, numbers,
/// and underscores that start with a letter or an underscore.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier<'src> {
    /// The name of the identifier.
    pub name: &'src str,
}

/// A literal value.
#[derive(Debug, Clone, Hash)]
pub struct Literal {
    /// The value of the literal. For now, literals are only integers and are
    /// stored as 64-bit unsigned integers in the AST, allowing some indesirable
    /// behavior if the value is larger than its type can hold. Those kind of
    /// errors will be caught by the semantic analysis phase.
    pub value: u64,

    /// The type of the literal. Most of the time, the type of the literal is
    /// inferred from the context in which it is used.
    pub ty: Type,
}

/// A function prototype. A function prototype is a declaration of a function
/// that specifies the name of the function, the list of parameters that the
/// function takes, and the return type of the function. Function prototypes
/// are used to declare functions before they are defined. This allows functions
/// to be called before they are defined, which is useful when functions call
/// each other interdependently.
#[derive(Debug, Clone, Hash)]
pub struct FunctionPrototype<'src> {
    /// The name of the function.
    pub ident: Spanned<Identifier<'src>>,

    /// The return type of the function.
    pub ret: Spanned<Type>,
}

/// A function. Functions are the main building blocks of the language.
/// They can be called from other functions or from the entry point of
/// the program. Functions can have a return type and a body that contains
/// a list of expressions that are evaluated when the function is called.
#[derive(Debug, Clone, Hash)]
pub struct Function<'src> {
    /// The prototype of the function. The prototype contains the name of
    /// the function, the list of parameters that the function takes, and
    /// the return type of the function.
    pub prototype: Spanned<FunctionPrototype<'src>>,

    /// A list of statements that make up the body of the function.
    pub body: Vec<Spanned<Stmt<'src>>>,
}

/// A block of statements. Blocks are used to group statements togethern and are typically
/// used in function bodies, conditional statements, and loops. Blocks can also have a return
/// type, which is the type of the last expression in the block. If the block does not have a
/// return type, it is considered to have a return type of `void`.
#[derive(Debug, Clone, Hash)]
pub struct Block<'src> {
    /// A list of statements that make up the block.
    pub stmts: Vec<Spanned<Stmt<'src>>>,

    /// The return type of the block. If the block does not have a return
    /// type, it is considered to have a return type of `void`.
    pub ty: Type,
}

/// A statement. Statements are fragments of code that are executed in
/// sequence in a program.
#[derive(Debug, Clone, Hash)]
pub struct Stmt<'src> {
    /// The kind of statement.
    pub kind: StmtKind<'src>,

    /// A phantom data to tie the lifetime of the expression to the
    /// source code it was parsed from. This is need for now since
    /// we don't need it yet because our expressions don't contain
    /// any references to the source code, but in the future we will
    /// need it when we add more complex expressions that contain
    /// references to the source code.
    pub _phantom: std::marker::PhantomData<&'src ()>,
}

#[derive(Debug, Clone, Hash)]
pub enum StmtKind<'src> {
    /// A return statement. The element is the expression that is returned.
    Return(Box<Spanned<Expr<'src>>>),

    /// A error statement that is used to indicate a parsing error. This
    /// variant should not be present in the final AST that will be passed
    /// to the code generator.
    Error(Span),
}

/// An expression. Expressions are evaluated to produce a value that
/// can be used in other expressions or statements. Expressions that
/// do not produce a value are called statements (e.g. `let x = 1;`)
/// and are different from expressions that produce a value (e.g. `1 + 2`).
#[derive(Debug, Clone, Hash)]
pub struct Expr<'src> {
    /// The kind of expression.
    pub kind: ExprKind<'src>,

    /// The type of the expression.
    pub ty: Type,
}

/// Different kinds of expressions supported by the language.
#[derive(Debug, Clone, Hash)]
pub enum ExprKind<'src> {
    /// A boolean value expression.
    Bool(Spanned<bool>),

    /// A literal value expression.
    Literal(Spanned<Literal>),

    /// A binary operation expression. The first element is the operator, the second
    /// element is the left-hand side expression, and the third element is the right-hand
    /// side expression.
    Binary(
        lang::BinaryOp,
        Box<Spanned<Expr<'src>>>,
        Box<Spanned<Expr<'src>>>,
    ),

    /// A placeholder expression that is used to avoid the rust compiler telling us that
    /// the lifetime parameter `'src` is unused. This variant should not be present in future
    /// versions of the AST.
    Placeholder(std::marker::PhantomData<&'src ()>),

    /// A error expression that is used to indicate a parsing error. This
    /// variant should not be present in the final AST that will be passed
    /// to the code generator.
    Error(Span),
}
