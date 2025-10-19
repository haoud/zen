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

    /// A string type. Currently, strings are represented as a sequence of characters and do not
    /// have a fixed length. Future versions of the language may introduce more complex string
    /// types, such as fixed-length strings or string slices.
    Str,

    /// A boolean type. Can be either true or false.
    Bool,

    /// An signed integer type.
    Int,

    /// Void type. It is only used to indicate that a function does not return a value.
    Void,
}

impl Type {
    /// Check if the type is a boolean type.
    #[must_use]
    pub fn is_boolean(&self) -> bool {
        matches!(self, Type::Bool)
    }

    /// Check if the type is a valid type (i.e., not Unknown or Infer).
    #[must_use]
    pub fn is_valid(&self) -> bool {
        !matches!(self, Type::Unknown | Type::Infer)
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unknown => write!(f, "<unknown>"),
            Type::Infer => write!(f, "<infer>"),
            Type::Str => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Void => write!(f, "void"),
        }
    }
}

/// The different binary operators supported by the language.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    /// Addition operator `+`
    Add,

    /// Subtraction operator `-`
    Sub,

    /// Multiplication operator `*`
    Mul,

    /// Division operator `/`
    Div,

    /// Equality operator `==`
    Eq,

    /// Inequality operator `!=`
    Neq,

    /// Less than operator `<`
    Lt,

    /// Less than or equal to operator `<=`
    Lte,

    /// Greater than operator `>`
    Gt,

    /// Greater than or equal to operator `>=`
    Gte,

    /// Logical AND operator `&&`
    And,

    /// Logical OR operator `||`
    Or,
}

impl BinaryOp {
    /// Check if the binary operator can accept boolean operands. This includes the equality,
    /// inequality, logical AND, and logical OR operators. Other operators, such as arithmetic
    /// and comparison operators, do not accept boolean operands.
    #[must_use]
    pub fn accept_boolean_operands(&self) -> bool {
        matches!(
            self,
            BinaryOp::Eq | BinaryOp::Neq | BinaryOp::And | BinaryOp::Or
        )
    }

    /// Check if the binary operator requires boolean operands. This includes the logical
    /// AND and logical OR operators.
    #[must_use]
    pub fn requires_boolean_operands(&self) -> bool {
        matches!(self, BinaryOp::And | BinaryOp::Or)
    }

    /// Check if the binary operator is a comparison operator. This includes the equality,
    /// inequality, less than, less than or equal to, greater than, and greater than
    /// or equal to operators.
    #[must_use]
    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
            BinaryOp::Eq
                | BinaryOp::Neq
                | BinaryOp::Lt
                | BinaryOp::Lte
                | BinaryOp::Gt
                | BinaryOp::Gte
        )
    }

    /// Check if the binary operator is a logical operator. This includes the logical
    /// AND and logical OR operators.
    #[must_use]
    pub fn is_logical(&self) -> bool {
        matches!(self, BinaryOp::And | BinaryOp::Or)
    }

    /// Get the string representation of the binary operator.
    #[must_use]
    pub fn as_str(&self) -> &str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Lte => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Gte => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        }
    }
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// The different unary operators supported by the language.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl UnaryOp {
    /// Get the string representation of the unary operator.
    #[must_use]
    pub fn as_str(&self) -> &str {
        match self {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
        }
    }
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// A literal represents a fixed value in the source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Literal<'src> {
    literal: &'src str,
    base: LiteralBase,
}

impl<'src> Literal<'src> {
    /// Create a new literal with the given string representation and base.
    #[must_use]
    pub fn new(literal: &'src str, base: LiteralBase) -> Self {
        Self { literal, base }
    }

    /// Get the string representation of the literal
    #[must_use]
    pub fn as_str(&self) -> &str {
        self.literal
    }

    /// Get the base of the literal as it was represented in the source code. For example, if the
    /// literal was represented as `0x1A`, this function will return `LiteralBase::Hexadecimal`.
    ///
    /// # Important
    /// This function returns the base as it was represented in the source code, which may be
    /// incorrect since the lexer does not validate the literal. For example, the lexer may
    /// recognize `0xGHI` as a hexadecimal literal, but this is not a valid hexadecimal number.
    /// The semantic analyzer is responsible for validating the literal and reporting any errors.
    /// Therefore, do not rely on this function to determine the actual base of the literal.
    #[must_use]
    pub fn hinted_base(&self) -> LiteralBase {
        self.base
    }

    /// Parse the literal as a 64-bit unsigned integer. This function will return an error if
    /// the literal is not a valid unsigned integer or if it is out of range for a 64-bit
    /// unsigned integer.
    pub fn parse_u64(&self) -> Result<u64, std::num::ParseIntError> {
        match self.base {
            LiteralBase::Binary => u64::from_str_radix(self.literal, 2),
            LiteralBase::Octal => u64::from_str_radix(self.literal, 8),
            LiteralBase::Decimal => u64::from_str_radix(self.literal, 10),
            LiteralBase::Hexadecimal => u64::from_str_radix(self.literal, 16),
        }
    }

    /// Parse the literal as a 64-bit signed integer. This function will return an error if
    /// the literal is not a valid signed integer or if it is out of range for a 64-bit
    /// signed integer. The `negated` parameter indicates whether the literal is negated (i.e.,
    /// prefixed with a `-` sign). However, this function does not handle the negation itself
    /// since the lexer does not recognize the `-` sign as part of the literal. This parameter is
    /// only used to properly handle the edge case of the minimum value of a signed integer
    /// (i.e., `-9223372036854775808`), which cannot be represented as a positive integer and needs
    /// a special case.
    pub fn parse_i64(&self, negated: bool) -> Result<i64, ()> {
        let num = self.parse_u64().map_err(|_| ())?;
        if negated {
            if num > (i64::MAX as u64) + 1 {
                Err(())
            } else {
                Ok(!(num as i64))
            }
        } else {
            if num > i64::MAX as u64 {
                Err(())
            } else {
                Ok(num as i64)
            }
        }
    }
}

impl core::fmt::Display for Literal<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{}{}", self.base.as_prefix(), self.literal)
    }
}

/// The different bases that a literal can be represented in.
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum LiteralBase {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

impl LiteralBase {
    /// Get the string prefix associated with the literal base.
    #[must_use]
    pub fn as_prefix(&self) -> &'static str {
        match self {
            Self::Binary => "0b",
            Self::Octal => "0o",
            Self::Decimal => "",
            Self::Hexadecimal => "0x",
        }
    }
}

impl core::fmt::Display for LiteralBase {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{}", self.as_prefix())
    }
}
