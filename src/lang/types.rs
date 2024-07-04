/// A type. Types are used to describe the kind of values that an
/// expression evaluates to. For now, the only type supported is `Int`.
#[derive(Debug, Clone)]
pub enum Type {
    Int,
}
