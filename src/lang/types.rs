/// A type. Types are used to describe the kind of values that an
/// expression evaluates to. For now, the only type supported is `Int`.
#[derive(Debug, Clone)]
pub enum Type {
    /// A integer type.
    Int,

    /// Infer the type during the semantic analysis phase. This is used
    /// when the type of an expression cannot be determined during the
    /// parsing phase. This variant should not be present in the final
    /// AST that will be passed to the code generation phase.
    Infer,
}
