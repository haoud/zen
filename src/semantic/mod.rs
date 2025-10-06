use chumsky::span::Span;

use crate::{
    ast,
    lang::{self, Spanned},
    semantic::error::SemanticDiagnostic,
};

pub mod error;
pub mod scope;
pub mod symbol;

pub type SemanticNote<'src> = ariadne::Report<'src, (&'src str, std::ops::Range<usize>)>;
pub type SemanticWarning<'src> = ariadne::Report<'src, (&'src str, std::ops::Range<usize>)>;
pub type SemanticError<'src> = ariadne::Report<'src, (&'src str, std::ops::Range<usize>)>;

/// Represents the semantic analysis context, including scopes and symbol tables.
#[derive(Debug)]
pub struct SemanticAnalysis<'src> {
    /// The scope stack for managing variable and function declarations.
    scopes: scope::Scope<'src>,

    /// A collection of semantic errors encountered during analysis.
    errors: SemanticDiagnostic<'src>,
}

impl<'src> SemanticAnalysis<'src> {
    /// Create a new semantic analysis context with an empty global scope.
    #[must_use]
    pub fn new(filename: &'src str) -> Self {
        Self {
            errors: SemanticDiagnostic::new(filename),
            scopes: scope::Scope::new(),
        }
    }

    /// Perform semantic analysis on the given list of functions.
    pub fn check_functions(&mut self, funcs: &mut [Spanned<ast::Function<'src>>]) {
        // Verify that each function has a unique name.
        for function in funcs.iter() {
            self.scopes.insert_function(&mut self.errors, function);
        }

        // Check each function's body for semantic correctness.
        for function in funcs {
            let span = function.span();
            self.check_statements(&mut function.0.prototype, &mut function.0.body, span);
        }
    }

    /// Check the statements within a function for semantic correctness.
    pub fn check_statements(
        &mut self,
        proto: &mut Spanned<ast::FunctionPrototype<'src>>,
        stmts: &mut [Spanned<ast::Stmt<'src>>],
        span: lang::Span,
    ) {
        let mut base_ret_stmt_span: Option<lang::Span> = None;

        for stmt in stmts.iter_mut() {
            // Infer the type of the statement and its contents, as it may affect
            // subsequent checks.
            self.infer_stmt(stmt);

            // If we have already encountered a return statement, any subsequent code
            // is unreachable and should be reported as an error.
            if let Some(span) = base_ret_stmt_span {
                self.errors.emit_unreachable_code_error(span, stmt.span());
                break;
            }

            let stmt_span = stmt.span();
            match &mut stmt.0.kind {
                ast::StmtKind::Return(expr) => {
                    // FIXME: Proper escape analysis to ensure all paths return a value, instead of
                    // this simple check that does only work because we don't have any control flow
                    // yet.

                    // Check the expression being returned for semantic correctness. If there are
                    // errors, collect them, but still proceed to check the return type.
                    self.check_expr(expr, false);
                    base_ret_stmt_span = Some(stmt_span);

                    // Check that the return type matches the function's declared return type. If
                    // not, report an error. However, if the expression's type is `Unknown`, we
                    // skip this check to avoid cascading errors because the type inference was
                    // unable to determine the type of the returned expression.
                    if expr.ty != proto.ret.0 && expr.ty != lang::Type::Unknown {
                        self.errors
                            .emit_return_type_mismatch_error(proto, stmt_span, expr.ty);
                    }
                }
                ast::StmtKind::Let(_, ty, expr) => {
                    self.check_expr(expr, false);
                    // Verify that the expression type matches the declared type if provided. If no
                    // type is provided, it should have been inferred during the type inference step
                    // that should have happened before this semantic check.
                    if expr.ty != ty.0 {
                        self.errors
                            .emit_variable_definition_type_mismatch_error(expr, ty, stmt_span);
                    }
                }
                ast::StmtKind::Error(_) => unreachable!(),
            }
        }

        // Ensure that non-void functions have at least one return statement.
        if base_ret_stmt_span.is_none() {
            let start = stmts.first().map_or(span.start(), |s| s.span().start());
            let end = stmts.last().map_or(span.end(), |s| s.span().end());
            let fn_span = lang::Span::from(start..end);
            self.errors.emit_missing_return_error(proto, fn_span);
        }
    }

    /// Check an expression for semantic correctness, returning any errors found.
    pub fn check_expr(&mut self, expr: &mut Spanned<ast::Expr<'src>>, negated: bool) {
        let span = expr.span();

        self.infer_expr(expr);
        match &mut expr.kind {
            ast::ExprKind::Binary(op, lhs, rhs) => {
                // Verify that both sides of the binary operation are of compatible types.
                self.check_expr(lhs, false);
                self.check_expr(rhs, false);
                if lhs.ty != rhs.ty {
                    self.errors
                        .emit_binary_op_type_mismatch_error(*op, lhs, rhs, span);
                }

                // Ensure that boolean values are not used in arithmetic operations.
                if lhs.ty == lang::Type::Bool || rhs.ty == lang::Type::Bool {
                    self.errors
                        .emit_boolean_arithmetic_error(*op, lhs, rhs, span);
                }
            }
            ast::ExprKind::Unary(op, rhs) => {
                // Disallow negation of boolean types.
                if *op == lang::UnaryOp::Neg && rhs.ty == lang::Type::Bool {
                    self.errors.emit_negation_of_non_numeric_type_error(rhs);
                }

                // Recursively check the inner expression, passing along whether we are in
                // a negated context or not. This is needed to correctly handle cases like
                // double negation and to ensure proper overflow/underflow checks.
                if *op == lang::UnaryOp::Neg {
                    self.check_expr(rhs, !negated);
                } else {
                    self.check_expr(rhs, negated);
                }
            }
            ast::ExprKind::Literal(x) => {
                // Ensure that integer literals fit within the bounds of the integer type.
                match x.ty {
                    lang::Type::Int => {
                        if x.value.parse_i64(negated).is_err() {
                            self.errors.emit_literal_overflow_error(x);
                        }
                    }
                    lang::Type::Bool => {
                        // Nothing to check for boolean literals.
                    }
                    lang::Type::Infer | lang::Type::Unknown => {
                        unreachable!("Literal types should be inferred during type inference")
                    }
                }
            }
            ast::ExprKind::Identifier(_) | ast::ExprKind::Bool(_) => {
                // Nothing to check for identifiers and boolean literals.
            }
            ast::ExprKind::Error(_) => unreachable!(),
        }
    }

    /// Infer types for a statement, updating it in place.
    fn infer_stmt(&mut self, stmt: &mut Spanned<ast::Stmt<'src>>) {
        match &mut stmt.kind {
            ast::StmtKind::Return(expr) => self.infer_expr(expr),
            ast::StmtKind::Let(ident, ty, expr) => {
                // Infer the type of the expression if it is not already known.
                if ty.0 == lang::Type::Infer {
                    self.infer_expr(expr);
                    ty.0 = expr.ty;
                }

                // Insert the new variable into the current scope. If a variable with the same
                // name already exists in the current scope, an error will be reported.
                self.scopes.insert_variable(
                    &mut self.errors,
                    Spanned::new(
                        symbol::Variable {
                            name: ident.name,
                            ty: ty.0,
                        },
                        stmt.span(),
                    ),
                );
            }
            ast::StmtKind::Error(_) => unreachable!(),
        }
    }

    /// Infer the type of an expression, updating it in place.
    fn infer_expr(&mut self, expr: &mut Spanned<ast::Expr<'src>>) {
        if expr.ty == lang::Type::Infer {
            match &mut expr.kind {
                ast::ExprKind::Binary(_, lhs, rhs) => {
                    // Recursively infer types of the left and right expressions if needed.
                    self.infer_expr(lhs);
                    self.infer_expr(rhs);

                    // If both sides have the same non-infer type, set the expression's type
                    // to that. However, if either side is still `Infer` or doesn't match, we
                    // set the type to `Unknown` to indicate a type inference failure.
                    // TODO: Implement proper type coercion rules here.
                    if lhs.ty == rhs.ty && lhs.ty != lang::Type::Infer {
                        expr.ty = lhs.ty;
                    } else {
                        expr.ty = lang::Type::Unknown;
                    }
                }
                ast::ExprKind::Unary(_, rhs) => {
                    // Recursively infer the type of the inner expression if needed.
                    self.infer_expr(rhs);
                    expr.ty = rhs.ty;
                }
                ast::ExprKind::Identifier(identifier) => {
                    // Identifier type is always explicited during their declaration. So we can
                    // simply look it up in the symbol table and assign the variable's type to the
                    // expression.
                    if let Some(var) = self.scopes.get_variable(identifier.name) {
                        expr.ty = var.ty;
                    } else {
                        self.errors.emit_undefined_variable_error(identifier);
                        expr.ty = lang::Type::Unknown;
                    }
                }
                ast::ExprKind::Literal(_) => {
                    // Literal types should be inferred based on their value and context. For now,
                    // we assume all literals are integers until we implement proper literal types.
                    todo!("Handle literal type inference");
                }
                ast::ExprKind::Bool(_) => {
                    unreachable!("Boolean literals should always have type Bool after parsing")
                }
                ast::ExprKind::Error(_) => {
                    unreachable!("Error expressions should not appear during type inference")
                }
            }
        }
    }

    /// Finalize the semantic analysis, returning any collected errors.
    pub fn finalize(self) -> Result<(), Vec<SemanticError<'src>>> {
        let errors = self.errors.collect();
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}
