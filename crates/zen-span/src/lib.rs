use std::ops::{Deref, DerefMut};

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

impl<T: std::fmt::Debug> std::fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Spanned")
            .field("span", &self.span())
            .field("item", &self.inner())
            .finish()
    }
}
