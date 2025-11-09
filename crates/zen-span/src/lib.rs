/// A simple span type that uses `usize` to represent the start and end of a span.
pub type Span = chumsky::span::SimpleSpan<usize>;

/// A wrapper around an item that includes a span. This is useful for associating source
/// code locations with AST nodes or other items.
pub struct Spanned<T>(T, Span);

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

    /// Get a tuple of references to the inner item and its span.
    #[must_use]
    pub fn as_tuple(&self) -> (&T, &Span) {
        (&self.0, &self.1)
    }

    /// Consume the `Spanned` and return a tuple of the inner item and its span.
    #[must_use]
    pub fn into_tuple(self) -> (T, Span) {
        (self.0, self.1)
    }

    /// Consume the `Spanned` and return the inner item, discarding the span.
    #[must_use]
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> std::ops::DerefMut for Spanned<T> {
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
            .field("item", &self.inner())
            .field("span", &self.span())
            .finish()
    }
}
