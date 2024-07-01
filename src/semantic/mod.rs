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

/// Perform the semantic analysis of the given AST.
///
/// # Errors
/// Returns a vector of errors for all the semantic errors found in the AST.
pub fn check(exprs: &[ast::Expr]) -> Result<(), Vec<lang::Error>> {
    let mut variables = HashMap::new();
    let mut errors = Vec::new();
    for expr in exprs {
        check_identifiers(&mut variables, &mut errors, expr);
    }

    match errors.is_empty() {
        false => Err(errors),
        true => Ok(()),
    }
}

/// Check in the given expression if there are any semantic errors concerning
/// variables identifiers. Currently, this function does the following checks:
/// - Check if a variable is used before it is declared.
/// - Check if a variable is redeclared.
fn check_identifiers<'a>(
    variables: &mut HashMap<&'a str, &'a ast::Expr<'a>>,
    errors: &mut Vec<lang::Error>,
    expr: &'a ast::Expr,
) {
    match &expr.kind {
        ast::ExprKind::Identifier(ident) => {
            if !variables.contains_key(ident) {
                errors.push(lang::Error {
                    msg: format!("Use of undeclared identifier {:?}", ident),
                    span: expr.span,
                });
            }
        }
        ast::ExprKind::Let(ident, assign) => {
            // Check if the variable declared is a valid identifier.
            // If it is, check if it is already declared in the current scope.
            if let Some(name) = ident.kind.as_identifier() {
                if variables.contains_key(name) {
                    // TODO: Add a note to the error message that shows
                    // the previous declaration.
                    errors.push(lang::Error {
                        msg: format!("Variable {:?} redeclared", name),
                        span: ident.span,
                    });
                }
                check_identifiers(variables, errors, assign);
                variables.insert(name, expr);
            } else {
                errors.push(lang::Error {
                    msg: "Variable name must be an identifier".to_string(),
                    span: ident.span,
                });
                check_identifiers(variables, errors, assign);
            }
        }
        ast::ExprKind::Return(expr) => {
            check_identifiers(variables, errors, expr)
        }
        ast::ExprKind::Binary(_, lhs, rhs) => {
            check_identifiers(variables, errors, lhs);
            check_identifiers(variables, errors, rhs);
        }
        ast::ExprKind::Literal(_) => {}
        ast::ExprKind::Error(..) => unreachable!(),
    }
}
