use crate::error::SemanticDiagnostic;
use {
    ast,
    lang::{self, Spanned},
};

pub mod error;
pub mod flow;
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
        for function in funcs.iter_mut() {
            let span = function.span();

            self.scopes.insert_function(&mut self.errors, function);
            self.scopes.enter_scope();
            for param in &function.prototype.params {
                self.scopes.insert_variable(
                    &mut self.errors,
                    Spanned::new(
                        symbol::Variable {
                            mutable: param.mutable,
                            name: param.ident.name,
                            ty: param.ty.0,
                        },
                        param.span(),
                    ),
                );
            }
            self.check_statements(function.prototype.clone(), &mut function.body, span);
            self.scopes.exit_scope();

            // Simple control flow analysis to ensure all code paths return a value and to
            // identify unreachable code.
            let mut cfa = flow::ControlFlowAnalysis::new(&mut self.errors);
            cfa.check_block(&function.0.body, function);
        }
    }

    /// Check the statements within a function for semantic correctness.
    pub fn check_statements(
        &mut self,
        proto: Spanned<ast::FunctionPrototype<'src>>,
        stmts: &mut [Spanned<ast::Stmt<'src>>],
        span: lang::Span,
    ) {
        for stmt in stmts.iter_mut() {
            // Infer the type of the statement and its contents, as it may affect
            // subsequent checks.
            self.infer_stmt(stmt);

            let stmt_span = stmt.span();
            match &mut stmt.0.kind {
                ast::StmtKind::Return(expr) => {
                    self.check_expr(expr, false);

                    // Verify that the return expression type matches the function's return type.
                    if expr.ty != proto.ret.0 && expr.ty.is_valid() {
                        self.errors
                            .emit_return_type_mismatch_error(&proto, stmt_span, expr.ty);
                    }
                }
                ast::StmtKind::Let(_, ty, expr) | ast::StmtKind::Var(_, ty, expr) => {
                    self.check_expr(expr, false);
                    // Verify that the expression type matches the declared type if provided. If no
                    // type is provided, it should have been inferred during the type inference step
                    // that should have happened before this semantic check.
                    if expr.ty != ty.0 {
                        self.errors
                            .emit_variable_definition_type_mismatch_error(expr, ty, stmt_span);
                    }
                }

                ast::StmtKind::Assign(op, ident, expr) => {
                    self.check_expr(expr, false);

                    // If the variable does not exist, report an undefined variable error
                    // and skip further checks for this assignment.
                    if let Some(var) = self.scopes.get_variable(ident.name) {
                        if !var.mutable {
                            self.errors
                                .emit_mutation_of_immutable_variable_error(var, stmt_span);
                        }

                        // Check that the expression type matches the variable's declared type.
                        if expr.ty != var.ty && expr.ty.is_valid() {
                            self.errors
                                .emit_type_mismatch_in_assignment_error(var, expr, span);
                        }

                        // Ensure that compound assignments are not used on boolean variables.
                        if let Some(op) = op {
                            if var.ty.is_boolean() {
                                self.errors
                                    .emit_bool_compound_assignment_error(var, *op, span);
                            }
                        }
                    } else {
                        self.errors.emit_undefined_variable_error(ident);
                    }
                }
                ast::StmtKind::If(cond, then, or) => {
                    // Check the condition expression
                    self.check_expr(cond, false);

                    // Ensure that the condition expression evaluates to a boolean type.
                    if !cond.ty.is_boolean() {
                        self.errors
                            .emit_non_boolean_in_conditional_error(cond, stmt_span);
                    }

                    // Recursively check the statements in the `then` block
                    self.scopes.enter_scope();
                    self.check_statements(proto.clone(), &mut then.stmts, stmt_span);
                    self.scopes.exit_scope();

                    // Recursively check the statements in the optional `else` block
                    if let Some(or) = or {
                        self.scopes.enter_scope();
                        self.check_statements(proto.clone(), &mut or.stmts, stmt_span);
                        self.scopes.exit_scope();
                    }
                }
                ast::StmtKind::Error(_) => unreachable!(),
            }
        }
    }

    /// Check an expression for semantic correctness, returning any errors found.
    pub fn check_expr(&mut self, expr: &mut Spanned<ast::Expr<'src>>, negated: bool) {
        let span = expr.span();

        self.infer_expr(expr);
        match &mut expr.kind {
            ast::ExprKind::Binary(op, lhs, rhs) => {
                let has_boolean_operand = lhs.ty.is_boolean() || rhs.ty.is_boolean();
                self.check_expr(lhs, false);
                self.check_expr(rhs, false);

                // Verify that both sides of the binary operation have the same type. In the
                // future, we may want to implement type coercion rules here.
                if lhs.ty != rhs.ty {
                    if op.is_comparison() {
                        self.errors
                            .emit_comparison_with_incompatible_types_error(*op, lhs, rhs, span);
                    } else {
                        self.errors
                            .emit_binary_op_type_mismatch_error(*op, lhs, rhs, span);
                    }
                }

                // Ensure that boolean values are not used in arithmetic operations.
                if has_boolean_operand && !op.accept_boolean_operands() {
                    self.errors
                        .emit_boolean_arithmetic_error(*op, lhs, rhs, span);
                }

                // Ensure that logical operators are only used with boolean operands.
                if !has_boolean_operand && op.requires_boolean_operands() {
                    self.errors
                        .emit_logical_operator_with_non_boolean_error(*op, lhs, rhs, span);
                }
            }
            ast::ExprKind::Unary(op, rhs) => {
                match op {
                    lang::UnaryOp::Neg => {
                        // Disallow negation of boolean types.
                        if *op == lang::UnaryOp::Neg && rhs.ty.is_boolean() {
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
                    lang::UnaryOp::Not => {
                        // Verify that the operand of the logical NOT operator is a boolean. This
                        // avoids nonsensical expressions like `!42` or `!x` where `x` is an
                        // integer. This is allowed in C-like languages, but not in Zen since it
                        // has a strong type system.
                        self.check_expr(rhs, false);
                        if !rhs.ty.is_boolean() {
                            self.errors.emit_logical_not_with_non_boolean_error(rhs);
                        }
                    }
                }
            }
            ast::ExprKind::FunctionCall(ident, args) => {
                // Check each argument expression for semantic correctness.
                for arg in args.iter_mut() {
                    self.check_expr(arg, false);
                }

                if let Some(func) = self.scopes.get_function(ident.name) {
                    // Check that the number of arguments matches the function's parameter count.
                    if args.len() != func.params.len() {
                        self.errors.emit_argument_count_mismatch_error(
                            ident,
                            func.params.len(),
                            args.len(),
                            span,
                        );
                    }

                    // Check that each argument type matches the corresponding parameter type.
                    for (arg, param) in args.iter_mut().zip(&func.params) {
                        if arg.ty != param.ty && arg.ty.is_valid() {
                            self.errors.emit_argument_type_mismatch_error(
                                param,
                                arg,
                                param.span(),
                                span,
                            );
                        }
                    }

                    expr.ty = func.ret;
                } else {
                    self.errors.emit_undefined_function_error(ident);
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
        let span = stmt.span();
        match &mut stmt.kind {
            ast::StmtKind::Return(expr) => self.infer_expr(expr),
            ast::StmtKind::Let(ident, ty, expr) => {
                self.infer_let_var(ident, expr, &mut ty.0, span, false);
            }
            ast::StmtKind::Var(ident, ty, expr) => {
                self.infer_let_var(ident, expr, &mut ty.0, span, true);
            }
            ast::StmtKind::Assign(_, _, expr) => {
                self.infer_expr(expr);
            }
            ast::StmtKind::If(cond, then, or) => {
                self.infer_expr(cond);
                for stmt in &mut then.stmts {
                    self.infer_stmt(stmt);
                }
                if let Some(or) = or {
                    for stmt in &mut or.stmts {
                        self.infer_stmt(stmt);
                    }
                }
            }
            ast::StmtKind::Error(_) => unreachable!(),
        }
    }

    /// Infer the type of the expression assigned to a `let` or `var` statement, updating both
    /// the variable type and the expression type in place. Then, insert the new variable into
    /// the current scope. If a variable with the same name already exists in the current scope,
    /// an error will be reported.
    fn infer_let_var(
        &mut self,
        ident: &ast::Identifier<'src>,
        expr: &mut Spanned<ast::Expr<'src>>,
        ty: &mut lang::Type,
        span: lang::Span,
        mutable: bool,
    ) {
        // Infer the type of the expression if it is not already known.
        if *ty == lang::Type::Infer {
            self.infer_expr(expr);
            *ty = expr.ty;
        }

        // Insert the new variable into the current scope. If a variable with the same
        // name already exists in the current scope, an error will be reported.
        self.scopes.insert_variable(
            &mut self.errors,
            Spanned::new(
                symbol::Variable {
                    name: ident.name,
                    ty: *ty,
                    mutable,
                },
                span,
            ),
        );
    }

    /// Infer the type of an expression, updating it in place.
    fn infer_expr(&mut self, expr: &mut Spanned<ast::Expr<'src>>) {
        if expr.ty == lang::Type::Infer {
            match &mut expr.kind {
                ast::ExprKind::Binary(op, lhs, rhs) => {
                    // Recursively infer types of the left and right expressions if needed.
                    self.infer_expr(lhs);
                    self.infer_expr(rhs);

                    match op {
                        lang::BinaryOp::And
                        | lang::BinaryOp::Or
                        | lang::BinaryOp::Eq
                        | lang::BinaryOp::Neq
                        | lang::BinaryOp::Lt
                        | lang::BinaryOp::Lte
                        | lang::BinaryOp::Gt
                        | lang::BinaryOp::Gte => {
                            // Logical and comparison operators always yield a boolean result.
                            expr.ty = lang::Type::Bool;
                        }

                        lang::BinaryOp::Add
                        | lang::BinaryOp::Sub
                        | lang::BinaryOp::Mul
                        | lang::BinaryOp::Div => {
                            // If both sides have the same non-infer type, set the expression's type
                            // to that. However, if either side is still `Infer` or doesn't match,
                            // we set the type to `Unknown` to indicate a type inference failure.
                            // TODO: Implement proper type coercion rules here.
                            if lhs.ty == rhs.ty && lhs.ty != lang::Type::Infer {
                                expr.ty = lhs.ty;
                            } else {
                                expr.ty = lang::Type::Unknown;
                            }
                        }
                    }
                }
                ast::ExprKind::Unary(op, rhs) => {
                    match op {
                        lang::UnaryOp::Neg => {
                            // Recursively infer the type of the inner expression if needed.
                            self.infer_expr(rhs);
                            expr.ty = rhs.ty;
                        }
                        lang::UnaryOp::Not => {
                            // The logical NOT operator always yields a boolean result.
                            expr.ty = lang::Type::Bool;
                        }
                    }
                }
                ast::ExprKind::FunctionCall(_ident, _) => {
                    if let Some(func) = self.scopes.get_function(_ident.name) {
                        expr.ty = func.ret;
                    } else {
                        // If the function is not found, we set the expression's type to `Unknown`
                        // but we do not emit an error here, as the function call will be checked
                        // later in the `check_expr` method that will handle the error reporting.
                        expr.ty = lang::Type::Unknown;
                    }
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
