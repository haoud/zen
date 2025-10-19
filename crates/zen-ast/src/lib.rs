use lang::{self, BinaryOp, Span, Spanned, Type, UnaryOp};

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
pub struct Literal<'src> {
    /// The value of the literal. For now, literals are only integers and are
    /// stored as 64-bit unsigned integers in the AST, allowing some indesirable
    /// behavior if the value is larger than its type can hold. Those kind of
    /// errors will be caught by the semantic analysis phase.
    pub value: lang::Literal<'src>,

    /// The type of the literal. Most of the time, the type of the literal is
    /// inferred from the context in which it is used.
    pub ty: Type,
}

/// A function parameter. Function parameters are used to define the parameters that
/// a function takes. Each parameter is represented by an identifier and a type, and
/// can be marked as mutable if the parameter can be modified within the function body.
#[derive(Debug, Clone, Hash)]
pub struct FunctionParameter<'src> {
    pub ident: Spanned<Identifier<'src>>,
    pub ty: Spanned<Type>,
    pub mutable: bool,
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

    /// The list of parameters that the function takes.
    pub params: Vec<Spanned<FunctionParameter<'src>>>,

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
}

/// Different kinds of statements supported by the language.
#[derive(Debug, Clone, Hash)]
pub enum StmtKind<'src> {
    /// A return statement. The element is an optional expression that is being
    /// returned from the function. If no expression is provided, the expression
    /// type is considered to be `void`.
    Return(Box<Option<Spanned<Expr<'src>>>>),

    /// A let statement. The first element is the identifier being bound, the second
    /// element is the optional type annotation, and the third element is the expression
    /// being assigned to the identifier.
    Let(
        Spanned<Identifier<'src>>,
        Spanned<Type>,
        Box<Spanned<Expr<'src>>>,
    ),

    // A var statement. The first element is the identifier being bound, the second
    // element is the optional type annotation, and the third element is the expression
    // being assigned to the identifier. This is similar to a let statement, but the
    // variable can be mutated after it is declared, unlike a let statement.
    Var(
        Spanned<Identifier<'src>>,
        Spanned<Type>,
        Box<Spanned<Expr<'src>>>,
    ),

    /// An assignment statement. The first element is an optional binary operator (for
    /// compound assignments like `+=`), the second element is the identifier being assigned
    /// to, and the third element is the expression being assigned to the identifier.
    Assign(
        Option<BinaryOp>,
        Box<Spanned<Identifier<'src>>>,
        Box<Spanned<Expr<'src>>>,
    ),

    /// An if statement. The first element is the condition expression, the second element
    /// is the block of statements to execute if the condition is true, and the third element
    /// is an optional block of statements to execute if the condition is false (the else block
    /// is optional). TODO: Support else-if chains.
    If(
        Box<Spanned<Expr<'src>>>,
        Box<Spanned<Block<'src>>>,
        Option<Box<Spanned<Block<'src>>>>,
    ),

    /// An expression statement. This is simply one or more expressions that are terminated
    /// by a semicolon. The element is the expression being evaluated. This is useful for
    /// expressions that have side effects, such as function calls.
    Expr(Box<Spanned<Expr<'src>>>),

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
    /// A boolean value.
    Bool(Spanned<bool>),

    /// A literal value.
    Literal(Spanned<Literal<'src>>),

    /// A string value.
    String(Spanned<String>),

    /// A binary operation. The first element is the operator, the second element is the left-hand
    /// side expression, and the third element is the right-hand side expression.
    Binary(BinaryOp, Box<Spanned<Expr<'src>>>, Box<Spanned<Expr<'src>>>),

    /// A unary operation. The first element is the operator, and the second element is the
    /// expression being operated on.
    Unary(UnaryOp, Box<Spanned<Expr<'src>>>),

    /// An identifier. The element is the identifier being referenced.
    Identifier(Spanned<Identifier<'src>>),

    // A function call. The first element is the identifier of the function being called,
    // and the second element is a list of expressions that are passed as arguments to the function
    FunctionCall(Box<Spanned<Identifier<'src>>>, Vec<Spanned<Expr<'src>>>),

    /// A error that is used to indicate a parsing error. This variant should not be present
    /// in the final AST that will be passed to the code generator.
    Error(Span),
}
