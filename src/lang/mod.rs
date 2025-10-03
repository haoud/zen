use std::{
    fmt::Debug,
    hash::Hash,
    ops::{Deref, DerefMut},
};

/// A simple span type that uses `usize` to represent the start and end of a span.
pub type Span = chumsky::span::SimpleSpan<usize>;

/// A wrapper around an item that includes its span. This struct directly dereferences
/// to the inner item, so it can be used as if it were the item itself, but also allows
/// access to the span information.
pub struct Spanned<T>(pub T, pub Span);

impl<T> Spanned<T> {
    #[must_use]
    pub fn new(item: T, span: Span) -> Self {
        Self(item, span)
    }

    #[must_use]
    pub fn none(item: T) -> Self {
        Self(item, Span::from(0..0))
    }

    /// Get a reference to the inner item.
    #[must_use]
    pub fn inner(&self) -> &T {
        &self.0
    }

    /// Get the span of the item.
    #[must_use]
    pub fn span(&self) -> Span {
        self.1
    }

    /// Consume the `Spanned` and return the inner item, discarding the span.
    #[must_use]
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1)
    }
}

impl<T: Hash> Hash for Spanned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
        self.1.hash(state);
    }
}

impl<T: Debug> std::fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Spanned")
            .field("span", &self.span())
            .field("item", &self.inner())
            .finish()
    }
}

/// The types supported by the language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    /// A type that represents an unknown type. This is primarily used to indicate
    /// an error in type checking, where the type could not be determined. This type
    /// should not appear in the final AST after type checking.
    Unknown,

    /// A special type that indicates the type is to be inferred during type checking. This is
    /// primarily used for integer literals that do not have an explicit type annotation, or for
    /// expressions where the type can be determined from context (e.g., the result of a binary
    /// operation where both operands have the same type). When type inference is complete, all
    /// instances of this type should be replaced with a concrete type, and any remaining instances
    /// of this type indicate a failure to infer the type.
    Infer,

    /// A boolean type. Can be either true or false.
    Bool,

    /// An integer type.
    Int,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unknown => write!(f, "<unknown>"),
            Type::Infer => write!(f, "<infer>"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
        }
    }
}

/// The different binary operators supported by the language.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinaryOp {
    /// Get the string representation of the binary operator.
    #[must_use]
    pub fn as_str(&self) -> &str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
        }
    }
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
