/// Binary operators supported by the language. The "binary" word means that
/// the operator requires two operands to work (e.g. 1 + 2). The operands are
/// the left and right expressions.
///
/// Please note that the `Sub` variant is used for both subtraction and
/// negation, the difference is that the negation operator only requires
/// one operand (e.g. -1).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
}

impl core::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
        }
    }
}
