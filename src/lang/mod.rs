pub mod operator;
pub mod types;

pub use types::Type;

/// A simple span type that uses `usize` to represent the start
/// and end of a span.
pub type Span = chumsky::span::SimpleSpan<usize>;

/// A simple error type that contains a message and a span.
#[derive(Debug, Clone)]
pub struct Error {
    pub msg: String,
    pub span: Span,
}
