use std::ops::Range;

use ariadne::{Color, Report, ReportKind};
use chumsky::span::Span;

use crate::{ast, lang::Spanned};

pub mod scope;

pub type SemanticNote<'src> = Report<'src, (&'src str, Range<usize>)>;
pub type SemanticWarning<'src> = Report<'src, (&'src str, Range<usize>)>;
pub type SemanticError<'src> = Report<'src, (&'src str, Range<usize>)>;

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
        funcs: &[Spanned<ast::Function<'src>>],
    ) -> Result<(), Vec<SemanticError<'src>>> {
        let mut errors = Vec::new();

        // Verify that each function has a unique name.
        for function in funcs {
            if let Err(err) = self.scope.insert_function(self.filename, function) {
                errors.push(err);
            }
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
                ast::StmtKind::Return(_) => base_ret_stmt = Some(stmt),
                ast::StmtKind::Error(_) => unreachable!(),
            }
        }

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
        // still returns a value on all paths.
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}
