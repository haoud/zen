use std::collections::HashMap;

use crate::lang::{self, Spanned};

/// A variable symbol
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable<'src> {
    /// The name of the variable.
    pub name: &'src str,

    /// The type of the variable.
    pub ty: lang::Type,

    /// Whether the variable is mutable or not.
    pub mutable: bool,
}

/// A function symbol
#[derive(Debug, Clone, Hash)]

pub struct Function<'src> {
    /// The name of the function.
    pub name: &'src str,

    /// A list of parameters for the function.
    pub params: Vec<Spanned<Variable<'src>>>,

    /// The return type of the function.
    pub ret: lang::Type,
}

/// A symbol table containing variables and functions. It supports insertion and lookup of symbols,
/// but does not handle scope or shadowing.
#[derive(Debug, Clone)]
pub struct SymbolTable<'src> {
    variables: HashMap<&'src str, Spanned<Variable<'src>>>,
    functions: HashMap<&'src str, Spanned<Function<'src>>>,
}

impl<'src> SymbolTable<'src> {
    /// Create an empty symbol table.
    #[must_use]
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    /// Insert a new variable into the symbol table.
    ///
    /// # Panics
    /// This function will panic if a variable with the same name already exists in the symbol
    /// table. This should never happen if the semantic analysis is done correctly.
    pub fn insert_variable(&mut self, variable: Spanned<Variable<'src>>) {
        assert!(!self.variable_exists(variable.name));
        self.variables.insert(variable.name, variable);
    }

    /// Insert a new function into the symbol table.
    ///
    /// # Panics
    /// This function will panic if a function with the same name already exists in the symbol
    /// table. This should never happen if the semantic analysis is done correctly.
    pub fn insert_function(&mut self, function: Spanned<Function<'src>>) {
        assert!(!self.function_exists(function.name));
        self.functions.insert(function.name, function);
    }

    /// Check if a variable with the given name exists in the symbol table.
    #[must_use]
    pub fn variable_exists(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    /// Check if a function with the given name exists in the symbol table.
    #[must_use]
    pub fn function_exists(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }

    /// Get a reference to a variable by name.
    #[must_use]
    pub fn get_variable(&self, name: &str) -> Option<&Spanned<Variable<'src>>> {
        self.variables.get(name)
    }

    /// Get a reference to a function by name.
    #[must_use]
    pub fn get_function(&self, name: &str) -> Option<&Spanned<Function<'src>>> {
        self.functions.get(name)
    }
}

impl Default for SymbolTable<'_> {
    fn default() -> Self {
        Self::new()
    }
}
