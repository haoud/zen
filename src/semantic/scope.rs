use ariadne::{Color, ReportKind};

use crate::{
    ast,
    lang::{Span, Spanned},
    semantic::SemanticError,
};
use std::collections::HashMap;

/// The scope stack for managing variable and function declarations.
#[derive(Debug, Clone)]
pub struct Scope<'src> {
    scopes: Vec<ScopeCtx<'src>>,
}

impl<'src> Scope<'src> {
    /// Create a new scope with an empty global scope and no local scopes.
    #[must_use]
    pub fn new() -> Self {
        Self {
            scopes: vec![ScopeCtx::empty()],
        }
    }

    /// Enter a new local scope.
    pub fn enter_scope(&mut self) {
        self.scopes.push(ScopeCtx::empty());
    }

    /// Exit the current local scope.
    ///
    /// # Panics
    /// This function will panic if there is no local scope to exit. This should never
    /// happen and is a programming error that is fatal and must be fixed.
    ///
    /// # Recommendation
    /// To avoid this panic, it is recommended to prefer using the `scoped` method which
    /// automatically manages entering and exiting scopes.
    pub fn exit_scope(&mut self) {
        assert!(self.scopes.len() > 1);
        self.scopes.pop();
    }

    /// Execute a closure within a new local scope. This method will automatically
    /// enter a new scope before executing the closure and exit the scope after
    /// the closure has been executed.
    pub fn scoped<T, F: FnOnce(&mut Self) -> T>(&mut self, f: F) -> T {
        self.enter_scope();
        let result = f(self);
        self.exit_scope();
        result
    }

    /// Get a mutable reference to the global scope. Since the global scope is always present,
    /// this function will never panic.
    #[must_use]
    pub fn global_scope_mut(&mut self) -> &mut ScopeCtx<'src> {
        &mut self.scopes[0]
    }

    /// Get a reference to the global scope. Since the global scope is always present, this
    /// function will never panic.
    #[must_use]
    pub fn global_scope(&self) -> &ScopeCtx<'src> {
        &self.scopes[0]
    }

    /// Get a reference to the current scope. If there are no local scopes, this will return
    /// the global scope.
    #[must_use]
    pub fn current_scope(&self) -> &ScopeCtx<'src> {
        self.scopes.last().unwrap()
    }

    /// Get a mutable reference to the current scope. If there are no local scopes, this will
    /// return the global scope.
    #[must_use]
    pub fn current_scope_mut(&mut self) -> &mut ScopeCtx<'src> {
        self.scopes.last_mut().unwrap()
    }

    /// Check if a function with the given identifier exists in the current scope.
    #[must_use]
    pub fn function_exists(&self, ident: &ast::Identifier<'src>) -> bool {
        self.scopes
            .iter()
            .rev()
            .any(|scope| scope.function_exists(ident))
    }

    /// Insert a function into the current scope. Because shadowing is not allowed for functions,
    /// this will check all parent scopes to ensure no function with the same identifier exists.
    ///
    /// # Error
    /// If a function with the same identifier already exists in the current scope, returns an error
    /// describing the conflict.
    pub fn insert_function(
        &mut self,
        file_name: &'src str,
        func: &Spanned<ast::Function<'src>>,
    ) -> Result<(), SemanticError<'src>> {
        if self.function_exists(&func.0.prototype.0.ident) {
            let previous_span = self
                .scopes
                .iter()
                .rev()
                .find_map(|scope| {
                    if scope.function_exists(&func.0.prototype.0.ident) {
                        scope.functions.get(&func.0.prototype.0.ident.0)
                    } else {
                        None
                    }
                })
                .unwrap();

            let report = ariadne::Report::build(
                ReportKind::Error,
                (file_name, func.prototype.span().into_range()),
            )
            .with_code(1)
            .with_message(format!(
                "the '{}' function is defined multiple times",
                func.0.prototype.0.ident.name
            ))
            .with_label(
                ariadne::Label::new((file_name, func.span().into_range()))
                    .with_message(format!(
                        "function '{}' redefined here",
                        func.0.prototype.0.ident.name
                    ))
                    .with_color(Color::Red),
            )
            .with_label(
                ariadne::Label::new((file_name, previous_span.into_range()))
                    .with_message("previous definition here")
                    .with_color(Color::Cyan),
            )
            .finish();

            Err(report)
        } else {
            self.current_scope_mut().insert_function(func);
            Ok(())
        }
    }
}

/// Represents a single scope context, containing function declarations.
#[derive(Debug, Clone)]
pub struct ScopeCtx<'src> {
    functions: HashMap<ast::Identifier<'src>, Span>,
}

impl<'src> ScopeCtx<'src> {
    /// Create an empty scope context.
    #[must_use]
    pub fn empty() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    /// Check if a function with the given identifier exists in the current scope.
    #[must_use]
    pub fn function_exists(&self, ident: &ast::Identifier<'src>) -> bool {
        self.functions.contains_key(ident)
    }

    /// Insert a function into the current scope.
    ///
    /// # Panics
    /// This function will panic if a function with the same identifier already exists in the
    /// current scope. This should never happen if the `Scope` methods are used correctly.
    pub fn insert_function(&mut self, func: &Spanned<ast::Function<'src>>) {
        assert!(
            !self.function_exists(&func.0.prototype.0.ident),
            "Function {} already exists in the current scope",
            func.0.prototype.0.ident.name
        );

        // PERF: Cloning here is ideal since identifiers are simply references to the source
        // code annd using references in the hash map would require a more complex lifetime
        // management strategy in addition to probably degrading performance due to the added
        // indirection.
        self.functions
            .insert(func.0.prototype.0.ident.0.clone(), func.span());
    }
}
