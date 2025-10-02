use ariadne::{Color, Report, ReportKind};
use chumsky::span::Span;

use crate::{
    ast,
    lang::{self, Spanned},
};

pub mod infer;
pub mod scope;

pub type SemanticNote<'src> = Report<'src, (&'src str, std::ops::Range<usize>)>;
pub type SemanticWarning<'src> = Report<'src, (&'src str, std::ops::Range<usize>)>;
pub type SemanticError<'src> = Report<'src, (&'src str, std::ops::Range<usize>)>;

/// Represents the semantic analysis context, including scopes and symbol tables.
#[derive(Debug, Clone)]
pub struct SemanticAnalysis<'src> {
    /// The name of the source file being analyzed.
    filename: &'src str,

    /// The scope stack for managing variable and function declarations.
    scope: scope::Scope<'src>,
}

impl<'src> SemanticAnalysis<'src> {
    /// Create a new semantic analysis context with an empty global scope.
    #[must_use]
    pub fn new(filename: &'src str) -> Self {
        Self {
            scope: scope::Scope::new(),
            filename,
        }
    }

    /// Perform semantic analysis on the given list of functions.
    pub fn check_functions(
        &mut self,
        funcs: &mut [Spanned<ast::Function<'src>>],
    ) -> Result<(), Vec<SemanticError<'src>>> {
        let mut errors = Vec::new();

        // Verify that each function has a unique name.
        for function in funcs.iter() {
            if let Err(err) = self.scope.insert_function(self.filename, function) {
                errors.push(err);
            }
        }

        // Perform type inference on the functions.
        if let Err(mut infer_errors) = infer::infer_types(funcs) {
            errors.append(&mut infer_errors);
        }

        // Check each function's body for semantic correctness.
        for function in funcs {
            if let Err(mut func_errors) = self.check_statements(function, &function.body) {
                errors.append(&mut func_errors);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Check the statements within a function for semantic correctness.
    pub fn check_statements(
        &mut self,
        func: &Spanned<ast::Function<'src>>,
        stmts: &[Spanned<ast::Stmt<'src>>],
    ) -> Result<(), Vec<SemanticError<'src>>> {
        let mut base_ret_stmt: Option<&Spanned<ast::Stmt<'src>>> = None;
        let mut errors = Vec::new();

        for stmt in stmts {
            if let Some(ret_stmt) = base_ret_stmt {
                let start = stmt.span().start();
                let end = stmts.last().unwrap().span().end();
                let report = ariadne::Report::build(ReportKind::Error, (self.filename, start..end))
                    .with_code(2)
                    .with_message("unreachable code after return statement")
                    .with_label(
                        ariadne::Label::new((self.filename, ret_stmt.span().into_range()))
                            .with_message("Any code following this return statement is unreachable")
                            .with_color(Color::Cyan),
                    )
                    .with_label(
                        ariadne::Label::new((self.filename, start..end))
                            .with_message("This code will never be executed")
                            .with_color(Color::Red),
                    )
                    .finish();

                // No need to continue checking after finding unreachable code
                errors.push(report);
                break;
            }

            match &stmt.0.kind {
                ast::StmtKind::Return(expr) => {
                    // Check the expression being returned for semantic correctness. If there are
                    // errors, collect them, but still proceed to check the return type.
                    match self.check_expr(expr) {
                        Ok(()) => {}
                        Err(mut expr_errors) => errors.append(&mut expr_errors),
                    }

                    base_ret_stmt = Some(stmt);

                    // Check that the return type matches the function's declared return type. If
                    // not, report an error. However, if the expression's type is `Unknown`, we
                    // skip this check to avoid cascading errors because the type inference was
                    // unable to determine the type and we cannot check it against the function's
                    // return type.
                    if expr.ty != func.prototype.ret.0 && expr.ty != lang::Type::Unknown {
                        let report = ariadne::Report::build(
                            ReportKind::Error,
                            (self.filename, stmt.span().into_range()),
                        )
                        .with_code(4)
                        .with_message(format!(
                            "return type mismatch: expected '{}', found '{}'",
                            func.prototype.ret.0, expr.ty
                        ))
                        .with_label(
                            ariadne::Label::new((
                                self.filename,
                                func.prototype.ret.span().into_range(),
                            ))
                            .with_message(format!(
                                "Expected return type '{}'",
                                func.prototype.ret.0
                            ))
                            .with_color(Color::Cyan),
                        )
                        .with_label(
                            ariadne::Label::new((self.filename, stmt.span().into_range()))
                                .with_message(format!("Found return type '{}'", expr.ty))
                                .with_color(Color::Red),
                        )
                        .finish();
                        errors.push(report);
                    }
                }
                ast::StmtKind::Error(_) => unreachable!(),
            }
        }

        // Ensure that non-void functions have at least one return statement.
        if base_ret_stmt.is_none() {
            let start = func
                .body
                .first()
                .map_or(func.span().start(), |s| s.span().start());
            let end = func
                .body
                .last()
                .map_or(func.span().end(), |s| s.span().end());

            let report = ariadne::Report::build(ReportKind::Error, (self.filename, start..end))
                .with_code(3)
                .with_message(format!(
                    "function '{}' does not have a return statement",
                    func.prototype.ident.name
                ))
                .with_label(
                    ariadne::Label::new((self.filename, func.prototype.ret.span().into_range()))
                        .with_message(format!(
                            "A return statement of type '{}' is required",
                            func.prototype.ret.0
                        ))
                        .with_color(Color::Cyan),
                )
                .finish();
            errors.push(report);
        }
        // TODO: Verify that the return statements match the function's return type.
        // TODO: If the return statement is missing, ensure the function returns void.
        // TODO: If the return statement is present, ensure the function does not return void.
        // TODO: Ensure that no statements exist after a return statement within the same block.
        // TODO: If a return statement is present in deeper scopes, ensure that the function
        // still returns a value on all paths. Also, if all paths return (exemple, if-else with
        // return in both branches), then no need for a return statement at the end of the
        // function.
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Check an expression for semantic correctness, returning any errors found.
    pub fn check_expr(
        &mut self,
        expr: &Spanned<ast::Expr<'src>>,
    ) -> Result<(), Vec<SemanticError<'src>>> {
        let mut errors = Vec::new();

        match &expr.kind {
            ast::ExprKind::Bool(_) => (),
            ast::ExprKind::Literal(_) => (),
            ast::ExprKind::Binary(op, lhs, rhs) => {
                // Verify that both sides of the binary operation are of compatible types.
                if let Err(mut lhs_errors) = self.check_expr(lhs) {
                    errors.append(&mut lhs_errors);
                }
                if let Err(mut rhs_errors) = self.check_expr(rhs) {
                    errors.append(&mut rhs_errors);
                }

                if lhs.ty != rhs.ty {
                    let report = ariadne::Report::build(
                        ReportKind::Error,
                        (self.filename, expr.span().into_range()),
                    )
                    .with_code(5)
                    .with_message(format!(
                        "type mismatch in binary operation '{op}': left is '{}', right is '{}'",
                        lhs.ty, rhs.ty
                    ))
                    .with_label(
                        ariadne::Label::new((self.filename, lhs.span().into_range()))
                            .with_message(format!("Left operand is of type '{}'", lhs.ty))
                            .with_color(Color::Cyan),
                    )
                    .with_label(
                        ariadne::Label::new((self.filename, rhs.span().into_range()))
                            .with_message(format!("Right operand is of type '{}'", rhs.ty))
                            .with_color(Color::Red),
                    )
                    .finish();
                    errors.push(report);
                }

                // Ensure that boolean values are not used in arithmetic operations.
                if lhs.ty == lang::Type::Bool || rhs.ty == lang::Type::Bool {
                    let report = ariadne::Report::build(
                        ReportKind::Error,
                        (self.filename, expr.span().into_range()),
                    )
                    .with_code(6)
                    .with_message(format!(
                        "boolean values cannot be used in arithmetic operation '{op}'"
                    ));

                    // Add a specific labels if the left operand is a boolean.
                    let report = if lhs.ty == lang::Type::Bool {
                        report.with_label(
                            ariadne::Label::new((self.filename, lhs.span().into_range()))
                                .with_message("Left operand is a boolean value")
                                .with_color(Color::Cyan),
                        )
                    } else {
                        report
                    };

                    // Add a specific label if the right operand is a boolean.
                    let report = if rhs.ty == lang::Type::Bool {
                        report.with_label(
                            ariadne::Label::new((self.filename, rhs.span().into_range()))
                                .with_message("Right operand is a boolean value")
                                .with_color(Color::Red),
                        )
                    } else {
                        report
                    };

                    errors.push(report.finish());
                }
            }
            ast::ExprKind::Placeholder(_) => unreachable!(),
            ast::ExprKind::Error(_) => unreachable!(),
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}
