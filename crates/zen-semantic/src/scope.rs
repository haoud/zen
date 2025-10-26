use {
    crate::{
        error::SemanticDiagnostic,
        symbol::{self, SymbolTable},
    },
    ast,
    lang::Spanned,
};

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

    /// Enter a new local scope. This will push a new scope context onto the scope stack, which
    /// will be used for any new variable or function declarations until the scope is exited
    /// with [`exit_scope`].
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

    /// Get a mutable reference to the global scope.
    ///
    /// # Panics
    /// This function should never panic since the global scope is always present, and it is
    /// ensured by functions that manipulate the scope stack.
    #[must_use]
    pub fn global_scope_mut(&mut self) -> &mut ScopeCtx<'src> {
        &mut self.scopes[0]
    }

    /// Get a reference to the global scope. Since the global scope is always present, this
    /// function will never panic.
    ///
    /// # Panics
    /// This function should never panic since the global scope is always present, and it is
    /// ensured by functions that manipulate the scope stack.
    #[must_use]
    pub fn global_scope(&self) -> &ScopeCtx<'src> {
        &self.scopes[0]
    }

    /// Get a reference to the current scope. If there are no local scopes, this will return
    /// the global scope.
    ///
    /// # Panics
    /// This function should never panic since at least the global scope is always present, and
    /// it is ensured by functions that manipulate the scope stack.
    #[must_use]
    pub fn current_scope(&self) -> &ScopeCtx<'src> {
        self.scopes.last().unwrap()
    }

    /// Get a mutable reference to the current scope. If there are no local scopes, this will
    /// return the global scope.
    ///
    /// # Panics
    /// This function should never panic since at least the global scope is always present, and
    /// it is ensured by functions that manipulate the scope stack.
    #[must_use]
    pub fn current_scope_mut(&mut self) -> &mut ScopeCtx<'src> {
        self.scopes.last_mut().unwrap()
    }

    /// Check if a function with the given identifier exists in the current scope.
    #[must_use]
    pub fn function_exists(&self, ident: &str) -> bool {
        self.scopes
            .iter()
            .rev()
            .any(|scope| scope.symbols().function_exists(ident))
    }

    /// Check if a variable with the given identifier exists in the current scope.
    #[must_use]
    pub fn variable_exists(&self, ident: &str) -> bool {
        self.scopes
            .iter()
            .rev()
            .any(|scope| scope.symbols().variable_exists(ident))
    }

    /// Get a function with the given identifier from any scope in the stack.
    #[must_use]
    pub fn get_function(&self, ident: &str) -> Option<&Spanned<symbol::Function<'src>>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.symbols().get_function(ident))
    }

    /// Get a variable with the given identifier from any scope in the stack.
    #[must_use]
    pub fn get_variable(&self, ident: &str) -> Option<&Spanned<symbol::Variable<'src>>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.symbols().get_variable(ident))
    }

    /// Insert a function into the current scope. Because shadowing is not allowed for functions,
    /// this will check all parent scopes to ensure no function with the same identifier exists.
    ///
    /// # Error
    /// If a function with the same identifier already exists, an error will be added to the
    /// semantic analysis context.
    pub fn insert_function(
        &mut self,
        errors: &mut SemanticDiagnostic<'src>,
        func: &Spanned<ast::Function<'src>>,
    ) {
        if let Some(function) = self.get_function(func.0.prototype.ident.name) {
            errors.emit_function_redefinition_error(
                &func.0.prototype,
                function.span(),
                func.span(),
            );
        } else {
            // Convert the function parameters to variable symbols.
            let params = func
                .0
                .prototype
                .params
                .iter()
                .map(|param| {
                    Spanned::new(
                        symbol::Variable {
                            mutable: param.mutable,
                            name: param.ident.name,
                            ty: param.ty.0.clone(),
                        },
                        param.span(),
                    )
                })
                .collect::<Vec<_>>();

            self.current_scope_mut()
                .symbols_mut()
                .insert_function(Spanned::new(
                    symbol::Function {
                        name: func.0.prototype.ident.name,
                        ret: func.0.prototype.ret.0.clone(),
                        params,
                    },
                    func.span(),
                ));
        }
    }

    /// Insert a variable into the current scope. Because shadowing is not allowed for variables,
    /// this will check all parent scopes to ensure no variable with the same identifier exists.
    ///
    /// # Error
    /// If a variable with the same identifier already exists, an error will be added to the
    /// semantic analysis context.
    pub fn insert_variable(
        &mut self,
        errors: &mut SemanticDiagnostic<'src>,
        variable: Spanned<symbol::Variable<'src>>,
    ) {
        if let Some(variable) = self.get_variable(variable.name) {
            errors.emit_variable_redefinition_error(variable, variable.span());
        } else {
            self.current_scope_mut()
                .symbols_mut()
                .insert_variable(variable);
        }
    }
}

/// Represents a single scope context, containing function declarations.
#[derive(Debug, Clone)]
pub struct ScopeCtx<'src> {
    symbols: SymbolTable<'src>,
}

impl<'src> ScopeCtx<'src> {
    /// Create an empty scope context.
    #[must_use]
    pub fn empty() -> Self {
        Self {
            symbols: SymbolTable::new(),
        }
    }

    /// Get a reference to the symbol table.
    #[must_use]
    pub fn symbols(&self) -> &SymbolTable<'src> {
        &self.symbols
    }

    /// Get a mutable reference to the symbol table.
    #[must_use]
    pub fn symbols_mut(&mut self) -> &mut SymbolTable<'src> {
        &mut self.symbols
    }
}

impl Default for Scope<'_> {
    fn default() -> Self {
        Self::new()
    }
}
