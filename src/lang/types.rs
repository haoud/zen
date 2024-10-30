/// A type. Types are used to describe the kind of values that an
/// expression evaluates to. For now, the only type supported is `Int`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    /// A integer type.
    Int,

    /// A unit type. The unit type is used to represent the absence of a value.
    /// This work exactly the same as the unit type in Rust (i.e. `()`) and
    /// greatly simplifies the type system: Any expression that does not return
    /// a significant value (e.g. a function that only prints to the console,
    /// an assignment statement, etc.) will have the unit type.
    Unit,

    /// The `Never` type. This type is used to represent an expression that
    /// cannot be evaluated. This is useful for functions that never return
    /// or for statements that cannot be evaluated (e.g. a `break` or a
    /// `continue` statement). The `Never` type is similar to the `!` type
    /// in Rust, and can be coerced to any other type since it can never
    /// be evaluated.
    Never,

    /// Infer the type during the semantic analysis phase. This is used
    /// when the type of an expression cannot be determined during the
    /// parsing phase. This variant should not be present in the final
    /// AST that will be passed to the code generation phase.
    Infer,
}

impl Type {
    /// Check if the type can be coerced to another type. This is useful
    /// to check if a function that returns a `Never` type can be used in
    /// a context that expects a different type. 
    #[must_use]
    pub fn can_coerce_to(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Never, _) => true,
            (_, Type::Never) => true,
            _ => false,
        }
    }
}

impl core::fmt::Display for Type {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Unit => write!(f, "()"),
            Type::Never => write!(f, "!"),
            Type::Infer => write!(f, "<infer>"),
        }
    }
}
