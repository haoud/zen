use std::{
    fmt::Debug,
    hash::Hash,
    num::{IntErrorKind, ParseIntError},
};

pub mod sym;
pub mod ty;

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
        u64::from_str_radix(self.literal, u32::from(self.base))
    }

    /// Parse the literal as a 64-bit signed integer. This function will return an error if
    /// the literal is not a valid signed integer or if it is out of range for a 64-bit
    /// signed integer. The `negated` parameter indicates whether the literal is negated (i.e.,
    /// prefixed with a `-` sign). However, this function does not handle the negation itself
    /// since the lexer does not recognize the `-` sign as part of the literal. This parameter is
    /// only used to properly handle the edge case of the minimum value of a signed integer
    /// (i.e., `-9223372036854775808`), which cannot be represented as a positive integer and needs
    /// a special case.
    pub fn parse_i64(&self, negated: bool) -> Result<i64, LiteralParseError> {
        let num = self.parse_u64()?;
        if negated {
            if num > (i64::MAX as u64) + 1 {
                Err(LiteralParseError::NegativeOverflow)
            } else {
                Ok(!(num as i64))
            }
        } else if num > i64::MAX as u64 {
            Err(LiteralParseError::PositiveOverflow)
        } else {
            Ok(num as i64)
        }
    }
}

impl core::fmt::Display for Literal<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{}{}", self.base.as_prefix(), self.literal)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum LiteralParseError {
    /// The parsed string was empty.
    Empty,

    /// The parsed string contained an invalid digit for the given base, or if the string
    /// contained a non-ASCII character.
    InvalidDigit,

    /// The parsed integer was too large to fit in the target type.
    PositiveOverflow,

    /// The parsed integer was too small to fit in the target type.
    NegativeOverflow,

    /// The parsed integer was zero, which is not allowed for non-zero types.
    Zero,
}

impl From<ParseIntError> for LiteralParseError {
    fn from(err: ParseIntError) -> Self {
        match err.kind() {
            IntErrorKind::Empty => Self::Empty,
            IntErrorKind::InvalidDigit => Self::InvalidDigit,
            IntErrorKind::PosOverflow => Self::PositiveOverflow,
            IntErrorKind::NegOverflow => Self::NegativeOverflow,
            IntErrorKind::Zero => Self::Zero,
            _ => unimplemented!(),
        }
    }
}

/// The different bases that a literal can be represented in.
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum LiteralBase {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
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

impl From<LiteralBase> for u32 {
    fn from(base: LiteralBase) -> Self {
        base as u32
    }
}

impl core::fmt::Display for LiteralBase {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{}", self.as_prefix())
    }
}
