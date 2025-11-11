use lang::Spanned;

use crate::SemanticAnalysis;

impl<'src> SemanticAnalysis<'src> {
    /// Check the statements within a function for semantic correctness.
    pub fn check_statements(
        &mut self,
        proto: Spanned<ast::FunctionPrototype<'src>>,
        stmts: &mut [Spanned<ast::Stmt<'src>>],
    ) {
        for stmt in stmts.iter_mut() {
            // Infer the type of the statement and its contents, as it may affect
            // subsequent checks.
            self.infer_stmt(stmt);

            let stmt_span = stmt.span();
            match &mut stmt.0.kind {
                ast::StmtKind::Return(expr) => {
                    if let Some(expr) = expr.as_mut() {
                        self.check_expr(expr, false);

                        // If the function's return type is void, emitting an error since a value
                        // is being returned. Skip further checks for this return statement.
                        if proto.ret.is_void() {
                            self.errors.emit_return_value_from_void_function_error(
                                proto.span(),
                                stmt_span,
                                expr,
                            );
                            continue;
                        }

                        // Verify that the return expression type matches the function's return type.
                        if expr.ty != proto.ret.0 && expr.ty.is_valid() {
                            self.errors.emit_return_type_mismatch_error(
                                &proto,
                                expr.span(),
                                &expr.ty,
                            );
                        }
                    } else {
                        // Verify that the function's return type is void if no expression
                        // is returned.
                        if proto.ret.is_void() {
                            self.errors
                                .emit_missing_return_expression_error(&proto, stmt_span);
                        }
                    }
                }
                ast::StmtKind::Var(ident, ty, expr, _) => {
                    self.check_expr(expr, false);

                    // Verify that the variable is not declared with type void. If it is, emit an
                    // error and skip further checks for this variable declaration since it is
                    // invalid.
                    if ty.is_void() {
                        self.errors
                            .emit_void_variable_declaration_error(stmt_span, ident.name);
                        continue;
                    }

                    // Verify that the expression type matches the declared type if provided. If no
                    // type is provided, it should have been inferred during the type inference step
                    // that should have happened before this semantic check.
                    if expr.ty != ty.0 && expr.ty.is_valid() {
                        self.errors
                            .emit_variable_definition_type_mismatch_error(expr, ty, stmt_span);
                    }
                }
                ast::StmtKind::Assign(op, lvalue, expr) => {
                    let lvalue_span = lvalue.span();
                    self.check_expr(lvalue, false);
                    self.check_expr(expr, false);

                    match &mut lvalue.kind {
                        ast::ExprKind::Identifier(ident) => {
                            self.check_variable_assign(
                                ident,
                                &expr.ty,
                                *op,
                                lvalue_span,
                                stmt_span,
                                false,
                            );
                        }
                        ast::ExprKind::FieldAccess(lvalue, field) => {
                            self.check_field_access_assign(
                                lvalue, field, expr, *op, stmt_span, false,
                            );
                        }
                        _ => {
                            // Invalid left-hand side in assignment: emit an error and skip further
                            // checks. The left-hand side must be a variable or a field access.
                            self.errors
                                .emit_invalid_assignment_lvalue_error(lvalue, stmt_span);
                            continue;
                        }
                    }

                    if lvalue.ty != expr.ty && lvalue.ty.is_valid() {
                        self.errors.emit_type_mismatch_in_assignment_error(
                            &lvalue.ty,
                            lvalue.span(),
                            &expr.ty,
                            expr.span(),
                            stmt_span,
                        );
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
                    self.check_statements(proto.clone(), &mut then.stmts);
                    self.scopes.exit_scope();

                    // Recursively check the statements in the optional `else` block
                    if let Some(or) = or {
                        self.scopes.enter_scope();
                        self.check_statements(proto.clone(), &mut or.stmts);
                        self.scopes.exit_scope();
                    }
                }
                ast::StmtKind::While(cond, body) => {
                    // Check the condition expression
                    self.check_expr(cond, false);

                    // Ensure that the condition expression evaluates to a boolean type.
                    if !cond.ty.is_boolean() {
                        self.errors
                            .emit_non_boolean_in_conditional_error(cond, stmt_span);
                    }

                    // Check the statements in the loop body
                    self.scopes.enter_scope();
                    self.check_statements(proto.clone(), &mut body.stmts);
                    self.scopes.exit_scope();
                }
                ast::StmtKind::Expr(expr) => {
                    self.check_expr(expr, false);
                    // TODO: Consider warning if the expression's type is not `void` (i.e., its
                    // value is unused), as this may indicate a potential mistake or oversight in
                    // the code. For example, the statement `x + 1;` has no effect and is likely
                    // unintended, although it is syntactically and semantically valid.
                }
                ast::StmtKind::Error(_) => unreachable!(),
            }
        }
    }

    /// Semantically check a field access assignment for correctness. This involves checking
    /// the base expression and ensuring that the field being assigned to is valid. The only
    /// expressions that can be assigned to are identifiers (variables) and (nested) field accesses.
    pub fn check_field_access_assign(
        &mut self,
        lvalue: &mut Spanned<ast::Expr<'src>>,
        field: &mut Spanned<ast::Identifier<'src>>,
        assign_expr: &mut Spanned<ast::Expr<'src>>,
        assign_op: Option<lang::BinaryOp>,
        stmt_span: lang::Span,
        recursion: bool,
    ) {
        self.check_expr(lvalue, false);

        let lvalue_span = lvalue.span();
        let lvalue_ty = &lvalue.ty.clone();

        // If not in recursion, we need to ensure that the field type supports the assignment
        // operation (for compound assignments).
        if !recursion {
            if let Some(op) = assign_op {
                // Get the type of the field being assigned to and verify that it supports the
                // assignment operation. If it does not, emit an error.
                // If the lvalue type does not have the field, skip this check as an error
                // should have already been emitted during expression checking.
                if let Some(ty) = self.types.get_field(lvalue_ty, field.name) {
                    if !self.types.support_binary_op(op, &ty) {
                        self.errors.emit_invalid_binary_operation_for_type_error(
                            op,
                            &ty,
                            field.span(),
                        );
                    }
                }
            }
        }

        // Depending on the kind of the lvalue, perform the appropriate checks.
        match &mut lvalue.kind {
            ast::ExprKind::Identifier(ident) => {
                self.check_variable_assign(
                    ident,
                    lvalue_ty,
                    assign_op,
                    lvalue_span,
                    stmt_span,
                    true,
                );
            }
            ast::ExprKind::FieldAccess(expr, identifier) => {
                self.check_field_access_assign(
                    expr,
                    identifier,
                    assign_expr,
                    assign_op,
                    stmt_span,
                    true,
                );
            }
            _ => {
                unreachable!("Invalid lvalue in field access assignment");
            }
        }
    }

    /// Check a variable assignment for semantic correctness.
    pub fn check_variable_assign(
        &mut self,
        assign_ident: &Spanned<ast::Identifier<'src>>,
        assign_ty: &lang::ty::Type,
        assign_op: Option<lang::BinaryOp>,
        assign_span: lang::Span,
        stmt_span: lang::Span,
        field_access: bool,
    ) {
        // If the variable does not exist, report an undefined variable error
        if let Some(var) = self.scopes.get_variable(assign_ident.name) {
            if !var.mutable {
                self.errors
                    .emit_mutation_of_immutable_variable_error(var, stmt_span);
            }

            // Ensure that the type support the given operation if it's a compound assignment. Only
            // do this check for direct variable assignments, not field accesses since the variable
            // type is not involved in the operation.
            if let Some(op) = assign_op
                && !field_access
            {
                if !self.types.support_binary_op(op, assign_ty) {
                    self.errors.emit_invalid_binary_operation_for_type_error(
                        op,
                        assign_ty,
                        assign_span,
                    );
                }
            }
        } else {
            self.errors.emit_undefined_identifier_error(assign_ident);
        }
    }

    /// Infer types for a statement, updating it in place.
    pub fn infer_stmt(&mut self, stmt: &mut Spanned<ast::Stmt<'src>>) {
        let span = stmt.span();
        match &mut stmt.kind {
            ast::StmtKind::Return(expr) => {
                if let Some(expr) = expr.as_mut() {
                    self.infer_expr(expr, None);
                }
            }
            ast::StmtKind::Var(ident, ty, expr, mutable) => {
                self.infer_let_var(ident, expr, ty, span, *mutable);
            }
            ast::StmtKind::Assign(_, lvalue, expr) => {
                self.infer_expr(lvalue, None);
                self.infer_expr(expr, None);
            }
            ast::StmtKind::If(cond, then, or) => {
                self.infer_expr(cond, None);
                for stmt in &mut then.stmts {
                    self.infer_stmt(stmt);
                }
                if let Some(or) = or {
                    for stmt in &mut or.stmts {
                        self.infer_stmt(stmt);
                    }
                }
            }
            ast::StmtKind::While(cond, body) => {
                self.infer_expr(cond, None);
                for stmt in &mut body.stmts {
                    self.infer_stmt(stmt);
                }
            }
            ast::StmtKind::Expr(expr) => {
                self.infer_expr(expr, None);
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
        ty: &mut Spanned<lang::ty::Type>,
        span: lang::Span,
        mutable: bool,
    ) {
        // Verify that the type exists if it is a user-defined type.
        if let lang::ty::Type::Struct(name) = &ty.0 {
            if !self.types.struct_exists(name) {
                self.errors.emit_unknown_type_error(ty.span(), ty);
            }
        }

        // Infer the type of the expression if it is not already known.
        if ty.0 == lang::ty::Type::Infer {
            self.infer_expr(expr, None);
            ty.0 = expr.ty.clone();
        } else {
            self.infer_expr(expr, Some(&mut ty.0));
        }

        // Insert the new variable into the current scope. If a variable with the same
        // name already exists in the current scope, an error will be reported.
        self.scopes.insert_variable(
            &mut self.errors,
            Spanned::new(
                lang::sym::Variable {
                    name: ident.name,
                    ty: ty.0.clone(),
                    mutable,
                },
                span,
            ),
        );
    }
}
