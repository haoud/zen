//! The semantic module is responsible for the semantic analysis of the source
//! code. It checks for any kind of semantic errors that the lexer and parser
//! might have missed and can make the code generation phase easier by
//! converting the AST into a more structured form that can be easily more
//! easily converted into the target language or bytecode.
//!
//! The semantic analysis is done in three passes:
//! - The first pass checks for any kind of semantic errors that can be
//!   detected without knowing the types of the variables or without a
//!   detailed knowledge of the code context. This includes things like
//!   checking for redeclarations of variables, checking if a variable is
//!   used before it is declared, etc. During this pass, the semantic
//!   analyzer also builds structures that will be used in the second pass
//!   to check for more complex errors.
//!
//! - The second pass checks for more complex errors that require a detailed
//!   knowledge of the code context. This includes things like checking if a
//!   variable is used in a way that is not allowed by its type, checking if
//!   a function is called with the wrong number of arguments, etc.
//!
//! - Finally, the semantic analyzer converts the AST into a more structured
//!   form that can be easily converted into the target language or bytecode.
//!   During this phase, the semantic analyzer can also perform optimizations
//!   that are easier to do at this stage than at the code generation stage
//!   when some informations about the code semantics are lost.

use crate::{
    ast::{self, FunctionPrototype},
    lang,
};
use std::collections::{HashMap, HashSet};

/// Represents a variable in the source code.
#[allow(dead_code)]
#[derive(Debug, Clone)]
struct Variable<'src> {
    /// The name of the variable
    name: &'src str,

    /// The type of the variable
    ty: lang::types::Type,
}

#[derive(Debug)]
struct SemanticAnalyzer<'src> {
    /// A list of variables, separated by scope. The last element of the
    /// vector is the variables defined in the current scope, the second
    /// to last element is the variables defined in the parent scope, etc.
    variables: Vec<HashMap<&'src str, Variable<'src>>>,

    /// A list of functions prototype that the semantic analyzer can use to
    /// check function calls.
    prototypes: Vec<FunctionPrototype<'src>>,

    /// A list of errors that the semantic analyzer found during the
    /// analysis.s
    errors: Vec<lang::Error>,
}

impl<'src> SemanticAnalyzer<'src> {
    /// Create a new semantic analyzer with the given list of functions.
    #[must_use]
    pub fn new(functions: &mut [ast::Function<'src>]) -> Self {
        let prototypes = functions
            .iter()
            .map(|function| function.prototype.clone())
            .collect();

        Self {
            variables: Vec::new(),
            errors: Vec::new(),
            prototypes,
        }
    }

    /// Verify if a variable with the given name exists in the current
    /// scope and above.
    #[must_use]
    pub fn variable_exists(&self, name: &str) -> bool {
        self.variables.iter().any(|scope| scope.contains_key(name))
    }

    /// Start a new scope.
    pub fn start_scope(&mut self) {
        self.variables.push(HashMap::new());
    }

    /// End the current scope.
    pub fn end_scope(&mut self) {
        self.variables.pop();
    }
}

impl<'src> SemanticAnalyzer<'src> {
    /// Add a variable to the current scope.
    pub fn add_variable(&mut self, name: &'src str, ty: lang::types::Type) {
        self.variables
            .last_mut()
            .unwrap()
            .insert(name, Variable { name, ty });
    }

    /// Get a variable by name. The function returns `None` if the variable
    /// is not found in the current scope or above.
    #[must_use]
    pub fn get_variable(&self, name: &str) -> Option<&Variable> {
        for scope in self.variables.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(var);
            }
        }
        None
    }

    /// Get a function by name. The function returns `None` if the function
    /// is not found in the list of functions.
    #[must_use]
    pub fn get_function_prototype(
        &self,
        name: &str,
    ) -> Option<&ast::FunctionPrototype<'src>> {
        self.prototypes.iter().find(|f| f.name == name)
    }

    /// Check a list of functions for semantic errors.
    pub fn check_functions(&mut self, functions: &mut [ast::Function<'src>]) {
        // Verify that each function has a unique name using an hashset.
        let mut names = HashSet::new();
        for function in functions.iter_mut() {
            if !names.insert(function.prototype.name) {
                self.errors.push(lang::Error {
                    msg: format!(
                        "Function {:?} redeclared",
                        function.prototype.name
                    ),
                    span: function.prototype.span,
                });
            }
        }

        for function in functions {
            self.check_function(function);
        }
    }

    /// Start the semantic analysis of a function.
    pub fn check_function(&mut self, f: &mut ast::Function<'src>) {
        // Start a new scope for the function parameters and body.
        self.start_scope();

        // Check that each parameter has an unique name and add it to the
        // parameters map. If a parameter is mut, add an error to the
        // errors vector and do not add it to the parameters map. This means
        // that futher references to this parameter will be considered as
        // a reference to the first declaration.
        for param in &f.prototype.args {
            if self.variable_exists(param.name) {
                self.errors.push(lang::Error {
                    msg: format!("Parameter {:?} redeclared", param.name),
                    span: param.span,
                });
            } else {
                self.add_variable(&param.name, param.ty);
            }
        }

        // Check the function body for semantic errors.
        for expr in &mut f.body {
            self.infer_type(expr);
            self.check_expr(expr);
        }
        self.end_scope();
    }

    /// Check an expression for semantic errors.
    pub fn check_expr(&mut self, expr: &ast::Expr<'src>) {
        match &expr.kind {
            ast::ExprKind::Call(func, args) => {
                // Get the name of the function called. We verify that the
                // function call is an identifier, as we do not support
                // function pointers yet.
                let name = match func.kind.as_identifier() {
                    Some(name) => name,
                    None => {
                        self.errors.push(lang::Error {
                            msg: "Function call must be an identifier"
                                .to_string(),
                            span: func.span,
                        });
                        return;
                    }
                };

                // Get the function information from the functions list. If
                // the function is not declared, add an error to the errors
                // vector and return.
                let fn_info =
                    match self.prototypes.iter().find(|f| f.name == name) {
                        Some(fninfo) => fninfo,
                        None => {
                            self.errors.push(lang::Error {
                                msg: format!(
                                    "Use of undeclared function {:?}",
                                    name
                                ),
                                span: func.span,
                            });
                            return;
                        }
                    };

                // Check if the number of arguments is correct. This is the
                // first step of the type checking process.
                if args.len() != fn_info.args.len() {
                    self.errors.push(lang::Error {
                        msg: format!(
                            "Function {} called with {} arguments, expected {}",
                            name,
                            args.len(),
                            fn_info.args.len()
                        ),
                        span: expr.span,
                    });
                }

                // Check if the arguments are of the correct type. This is
                // the second step of the type checking process.
                for (arg, var) in args.iter().zip(&fn_info.args) {
                    if arg.ty != var.ty {
                        self.errors.push(lang::Error {
                            msg: format!(
                                "Argument of type `{}` passed to function {:?} \
                                that expects type `{}`",
                                arg.ty, name, var.ty
                            ),
                            span: arg.span,
                        });
                    }
                }
            }
            ast::ExprKind::Identifier(ident) => {
                if !self.variable_exists(ident) {
                    self.errors.push(lang::Error {
                        msg: format!(
                            "Use of undeclared identifier {:?}",
                            ident
                        ),
                        span: expr.span,
                    });
                }
            }
            ast::ExprKind::Let(ident, assign) => {
                // Check if the variable declared is a valid identifier.
                // If it is, check if it is already declared in the
                // current scope.
                if let Some(name) = ident.kind.as_identifier() {
                    if self.variable_exists(name) {
                        self.errors.push(lang::Error {
                            msg: format!("Variable {:?} redeclared", name),
                            span: ident.span,
                        });
                    }

                    // FIXME: Add the real variable type here.
                    self.check_expr(assign);
                    self.add_variable(name, lang::types::Type::Int);
                } else {
                    self.errors.push(lang::Error {
                        msg: "Variable name must be an identifier".to_string(),
                        span: ident.span,
                    });
                    self.check_expr(assign);
                }
            }
            ast::ExprKind::Return(expr) => {
                self.check_expr(expr);
            }
            ast::ExprKind::Binary(_, lhs, rhs) => {
                self.check_expr(lhs);
                self.check_expr(rhs);
            }
            ast::ExprKind::Literal(_) => {}
            ast::ExprKind::Error(..) => unreachable!(),
        }
    }

    /// Infer the type of an expression if it is not known. This is used
    /// when the type of an expression cannot be determined during the parsing
    /// phase. If the type of the expression is already known, this function
    /// does nothing.
    pub fn infer_type(&mut self, expr: &mut ast::Expr) {
        if expr.ty != lang::Type::Infer {
            return;
        }

        match &mut expr.kind {
            ast::ExprKind::Call(expr, _) => {
                let name = expr
                    .kind
                    .as_identifier()
                    .expect("Function call must be an identifier");

                let function = self
                    .get_function_prototype(name)
                    .expect("Function not found");

                // Set the type of the expression to the return type
                // of the function.
                expr.ty = function.ret;
            }
            ast::ExprKind::Identifier(name) => {
                // Find the identifier in the variables list and set the type
                // of the expression to the type of the variable.
                expr.ty = self.get_variable(name).unwrap().ty;
            }
            ast::ExprKind::Binary(_, left, right) => {
                self.infer_type(left);
                self.infer_type(right);

                // If the type of the left and right expressions are not the
                // same, add an error to the errors vector.
                if left.ty != right.ty {
                    self.errors.push(lang::Error {
                        msg: "Type mismatch".to_string(),
                        span: expr.span,
                    });
                }

                // Set the type of the expression to the type of the left
                // expression.
                expr.ty = left.ty;
            }

            ast::ExprKind::Let(_, _) => {
                // The type of a let expression is always the `Unit` type
                // since the let expression cannot have a type used in an
                // expression.
                unreachable!();
            }

            ast::ExprKind::Return(_) => {
                // The type of a return expression is always the `Unit` type
                // since the return expression cannot have a type used in
                // an expression.
                unreachable!();
            }

            ast::ExprKind::Literal(_) => {
                // The type of a literal expression is always the `Int` type
                // during the parsing phase and should not be `Infer`.
                unreachable!();
            }

            ast::ExprKind::Error(_) => {
                // An error expression that is used to indicate a parsing
                // error. This variant should not be present in the final AST
                // that will be passed to the code generator.
                unreachable!();
            }
        }
    }
}

/// Analyze the given list of functions for semantic errors and return a list
/// of errors if any are found.
pub fn analyze(
    functions: &mut [ast::Function],
) -> Result<(), Vec<lang::Error>> {
    // Check each function for semantic errors.
    let mut analyzer = SemanticAnalyzer::new(functions);
    analyzer.check_functions(functions);
    match analyzer.errors.is_empty() {
        false => Err(analyzer.errors),
        true => Ok(()),
    }
}
