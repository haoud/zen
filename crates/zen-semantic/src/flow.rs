use lang::Spanned;

use crate::error::SemanticDiagnostic;

/// A simple control flow analysis.
#[derive(Debug)]
pub struct ControlFlowAnalysis<'a, 'src> {
    errors: &'a mut SemanticDiagnostic<'src>,
}

impl<'a, 'src> ControlFlowAnalysis<'a, 'src> {
    /// Create a new instance of the control flow analysis. The analysis will report errors to the
    /// given `SemanticDiagnostic`.
    #[must_use]
    pub fn new(errors: &'a mut SemanticDiagnostic<'src>) -> Self {
        Self { errors }
    }

    /// Ensure that all code paths in the given block of statements return a value. Any unreachable
    /// code after a return statement will be signaled as an error.
    pub fn check_block(
        &mut self,
        stmts: &'a [Spanned<ast::Stmt<'src>>],
        function: &Spanned<ast::Function<'src>>,
    ) {
        let mut return_all_paths = false;
        let mut return_span: Option<chumsky::span::SimpleSpan> = None;

        for stmt in stmts.iter() {
            // If we have already determined that all paths return, any subsequent
            // statements are unreachable. We report an error and we skip further
            // analysis of this statement list.
            if return_all_paths {
                self.errors
                    .emit_unreachable_code_error(return_span.unwrap(), stmt.span());
                continue;
            }

            match &stmt.kind {
                ast::StmtKind::If(_, then, or) => {
                    let then_returns = returns_in_all_paths(&then.0.stmts);
                    let else_returns = if let Some(else_branch) = or {
                        returns_in_all_paths(&else_branch.0.stmts)
                    } else {
                        false
                    };

                    if then_returns && else_returns {
                        return_all_paths = true;
                        return_span = Some(stmt.span());
                    }
                }
                ast::StmtKind::Return(_) => {
                    return_all_paths = true;
                    return_span = Some(stmt.span());
                }
                _ => {}
            }
        }

        // If not all paths return a value, and the function is not void, emit an error. If the
        // function is void, it's acceptable for not all paths to have a return statement since
        // the function will implicitly return void.
        if !return_all_paths && function.prototype.ret.0 != lang::ty::Type::Void {
            self.errors
                .emit_not_all_paths_return_value_error(&function.prototype, function.span());
        }
    }
}

/// Check if all paths in the given statements list return a value.
#[must_use]
pub fn returns_in_all_paths(stmts: &[Spanned<ast::Stmt<'_>>]) -> bool {
    for stmt in stmts {
        match &stmt.kind {
            ast::StmtKind::If(_, then, or) => {
                // If an branch of the if statement does return, we need to check other branches
                // to ensure they also return in all paths.
                let then_returns = returns_in_all_paths(&then.0.stmts);
                let else_returns = if let Some(else_branch) = or {
                    returns_in_all_paths(&else_branch.0.stmts)
                } else {
                    false
                };

                // Both branches return, so this if statement returns in all paths. We can stop
                // here and return true.
                if then_returns && else_returns {
                    return true;
                }
            }
            ast::StmtKind::While(_, _) => {
                // Even if the while loop contains a return statement, we cannot guarantee that it
                // will always execute, so we cannot conclude that all paths return.
                continue;
            }
            ast::StmtKind::Return(_) => return true,
            _ => {}
        }
    }

    // If we reach here, not all paths return a value.
    false
}
