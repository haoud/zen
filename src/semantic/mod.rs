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

use crate::{ast, lang};
use std::collections::HashMap;

/// Represents a variable in the source code.
#[allow(dead_code)]
struct Variable<'a> {
    /// The name of the variable
    name: &'a str,

    /// The type of the variable
    ty: lang::types::Type,
}

struct SemanticAnalyzer<'a, 'src> {
    /// A list of variables, separated by scope. The last element of the
    /// vector is the variables defined in the current scope, the second
    /// to last element is the variables defined in the parent scope, etc.
    variables: Vec<HashMap<&'a str, Variable<'a>>>,

    /// A list of functions that the semantic analyzer can use to check
    /// function calls.
    functions: &'a [ast::Function<'src>],

    /// A list of errors that the semantic analyzer found during the
    /// analysis.s
    errors: Vec<lang::Error>,
}

impl<'a, 'src> SemanticAnalyzer<'a, 'src> {
    /// Create a new semantic analyzer with the given list of functions.
    #[must_use]
    pub fn new(functions: &'a [ast::Function<'src>]) -> Self {
        Self {
            variables: Vec::new(),
            errors: Vec::new(),
            functions,
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

impl<'a, 'src> SemanticAnalyzer<'a, 'src> {
    /// Add a variable to the current scope.
    pub fn add_variable(&mut self, name: &'a str, ty: lang::types::Type) {
        self.variables
            .last_mut()
            .unwrap()
            .insert(name, Variable { name, ty });
    }

    /// Start the semantic analysis of a function.
    pub fn check_function(&mut self, f: &'a ast::Function) {
        // Start a new scope for the function parameters and body.
        self.start_scope();

        // Check that each parameter has an unique name and add it to the
        // parameters map. If a parameter is redeclared, add an error to the
        // errors vector and do not add it to the parameters map. This means
        // that futher references to this parameter will be considered as
        // a reference to the first declaration.
        for param in &f.args {
            if self.variable_exists(param.name) {
                self.errors.push(lang::Error {
                    msg: format!("Parameter {:?} redeclared", param.name),
                    span: param.span,
                });
            } else {
                self.add_variable(&param.name, param.ty.clone());
            }
        }

        // Check the function body for semantic errors.
        for expr in &f.body {
            self.check_expr(expr);
        }
        self.end_scope();
    }

    /// Check an expression for semantic errors.
    pub fn check_expr(&mut self, expr: &'a ast::Expr) {
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
                    match self.functions.iter().find(|f| f.name == name) {
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
                for (_arg, _var) in args.iter().zip(&fn_info.args) {
                    // TODO: Verify that the argument is of the correct type
                    // and add an error to the errors vector if it is not.
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
}

/// Analyze the given list of functions for semantic errors and return a list
/// of errors if any are found.
pub fn analyze(functions: &[ast::Function]) -> Result<(), Vec<lang::Error>> {
    let mut analyzer = SemanticAnalyzer::new(&functions);

    // Verify that each function has a unique name. This algorithm is very
    // badly optimized, but it is not a problem for now as the number of
    // functions is very small.
    for (i, f) in functions.iter().enumerate() {
        for g in functions.iter().skip(i + 1) {
            if f.name == g.name {
                analyzer.errors.push(lang::Error {
                    msg: format!("Function {:?} redeclared", f.name),
                    span: f.span,
                });
            }
        }
    }

    // Check each function for semantic errors.
    for func in functions {
        analyzer.check_function(func);
    }

    match analyzer.errors.is_empty() {
        false => Err(analyzer.errors),
        true => Ok(()),
    }
}
