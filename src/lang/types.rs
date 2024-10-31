/// A type. Types are used to describe the kind of values that an
/// expression evaluates to. For now, the only type supported is `Int`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    /// A signed 8-bit integer type.
    I8,

    /// A unsigned 8-bit integer type.
    U8,

    /// A signed 16-bit integer type.
    I16,

    /// A unsigned 16-bit integer type.
    U16,

    /// A signed 32-bit integer type.
    I32,

    /// A unsigned 32-bit integer type.
    U32,

    /// A signed 64-bit integer type.
    I64,

    /// A unsigned 64-bit integer type.
    U64,

    /// A integer type. The integer type size is platform dependent and
    /// is determined by the target architecture, and is usually the
    /// same as the size of a pointer.
    Int,

    /// A unsigned integer type. The unsigned integer type size is platform
    /// dependent and is determined by the target architecture, and is
    /// usually the same as the size of a pointer.
    Uint,

    /// A boolean type. This type can only have two values: `true` or `false`.
    /// This type is used to represent the result of a comparison operation.
    Bool,

    /// A character type. This type represents a single ASCII character.
    Char,

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
            Type::I8 => write!(f, "i8"),
            Type::U8 => write!(f, "u8"),
            Type::I16 => write!(f, "i16"),
            Type::U16 => write!(f, "u16"),
            Type::I32 => write!(f, "i32"),
            Type::U32 => write!(f, "u32"),
            Type::I64 => write!(f, "i64"),
            Type::U64 => write!(f, "u64"),
            Type::Int => write!(f, "int"),
            Type::Uint => write!(f, "uint"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::Unit => write!(f, "()"),
            Type::Never => write!(f, "!"),
            Type::Infer => write!(f, "<infer>"),
        }
    }
}
