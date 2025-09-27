#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    /// The current depth of the scope. This is used to generate the correct
    /// indentation for the generated code, allowing for better readability.
    depth: usize,
}

impl Scope {
    /// Create a new scope object with the given previous scope as its parent.
    #[must_use]
    pub fn new(previous: &Scope) -> Self {
        Self {
            depth: previous.depth + 1,
        }
    }

    /// Create a new global scope object. This is the scope with depth 0, where all the global
    /// variables and functions are defined.
    #[must_use]
    pub fn global() -> Self {
        Self { depth: 0 }
    }

    /// Get the current depth of the scope.
    #[must_use]
    pub fn depth(&self) -> usize {
        self.depth
    }
}
