use lang::Spanned;

use crate::SemanticAnalysis;

impl<'src> SemanticAnalysis<'src> {
    /// Check an expression for semantic correctness, returning any errors found.
    pub fn check_expr(&mut self, expr: &mut Spanned<ast::Expr<'src>>, negated: bool) {
        let span = expr.span();

        self.infer_expr(expr);
        match &mut expr.kind {
            ast::ExprKind::Binary(op, lhs, rhs) => {
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

                // If either side has an invalid type, skip further checks for this binary
                // operation since it is already erroneous.
                if !lhs.ty.is_valid() || !rhs.ty.is_valid() {
                    return;
                }

                // Get the type metadata for the left-hand side type to check if it supports
                // the given binary operation. Since both sides should have the same type, we only
                // need to check one side.
                self.types.try_add_builtin_type(&lhs.ty);
                let ty_metadata = self
                    .types
                    .get_type_metadata(&lhs.ty)
                    .expect("Type metadata should always exist");

                // If the operation is not supported by the type, emit an error.
                if !ty_metadata.support_binary_op(*op) {
                    self.errors
                        .emit_invalid_binary_operation_for_type_error(*op, &lhs.ty, span);
                }
            }
            ast::ExprKind::Unary(op, rhs) => {
                // Recursively check the inner expression, passing along whether we are in
                // a negated context or not. This is needed to correctly handle cases like
                // double negation and to ensure proper overflow/underflow checks.
                if *op == lang::UnaryOp::Neg {
                    self.check_expr(rhs, !negated);
                } else {
                    self.check_expr(rhs, negated);
                }

                // If the inner expression has an invalid type, skip further checks for this
                // unary operation since it is already erroneous.
                if !rhs.ty.is_valid() {
                    return;
                }

                // Get the type metadata for the right-hand side type to check if it supports
                // the given unary operation.
                self.types.try_add_builtin_type(&rhs.ty);
                let ty_metadata = self
                    .types
                    .get_type_metadata(&rhs.ty)
                    .expect("Type metadata should always exist");

                // If the operation is not supported by the type, emit an error.
                if !ty_metadata.support_unary_op(*op) {
                    self.errors
                        .emit_invalid_unary_operation_for_type_error(*op, &rhs.ty, span);
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

                    expr.ty = func.ret.clone();
                } else {
                    self.errors.emit_undefined_function_error(ident);
                }
            }
            ast::ExprKind::IntrinsicCall(ident, args) => {
                // Check each argument expression for semantic correctness.
                for arg in args.iter_mut() {
                    self.check_expr(arg, false);
                }

                match ident.name {
                    "println" | "print" => {
                        // Ensure that at least one argument (the format string) is provided.
                        if args.is_empty() {
                            self.errors.emit_intrinsic_argument_count_mismatch_error(
                                ident, 1, 0, true, span,
                            );
                            return;
                        }

                        // Verify that the first argument is a string literal.
                        let fmt = if let Some(fmt) = args[0].kind.as_string_literal() {
                            fmt
                        } else {
                            self.errors.emit_intrinsic_argument_type_mismatch_error(
                                ident,
                                &args[0],
                                lang::ty::Type::Str,
                            );
                            return;
                        };

                        // Check that the types of the provided arguments are compatible with the
                        // format specifiers in the format string. Currently, we only support `{}`
                        // as a placeholder for any type except void.
                        // TODO: Print a more detailed error message instead of a generic argument
                        // count/type mismatch error.
                        let expected_arg_count = fmt.match_indices("{}").count();
                        let provided_arg_count = args.len() - 1; // Exclude format string
                        if expected_arg_count != provided_arg_count {
                            self.errors.emit_intrinsic_argument_count_mismatch_error(
                                ident,
                                expected_arg_count + 1,
                                provided_arg_count + 1,
                                false,
                                span,
                            );
                        }
                    }
                    _ => {
                        self.errors
                            .emit_unknown_intrinsic_function_error(ident, span);
                    }
                }
            }
            ast::ExprKind::Literal(x) => {
                // Ensure that integer literals fit within the bounds of the integer type.
                match x.ty {
                    lang::ty::Type::Int => {
                        if x.value.parse_i64(negated).is_err() {
                            self.errors.emit_literal_overflow_error(x);
                        }
                    }
                    lang::ty::Type::Void => {
                        unreachable!("Void literals should not exist in the AST")
                    }
                    lang::ty::Type::Str => {
                        unreachable!("String literals should always have type String after parsing")
                    }
                    lang::ty::Type::Bool => {
                        unreachable!("Boolean literals should always have type Bool after parsing")
                    }
                    lang::ty::Type::Array(_, _) => {
                        unreachable!("Array literals should be represented as List expressions")
                    }
                    lang::ty::Type::Struct(_) => {
                        todo!()
                    }
                    lang::ty::Type::Infer | lang::ty::Type::Unknown => {
                        unreachable!("Literal types should be inferred during type inference")
                    }
                }
            }
            ast::ExprKind::List(items) => {
                // Check each item in the initializer list for semantic correctness.
                for item in items.iter_mut() {
                    self.check_expr(item, false);
                }

                // Check for incompatible types in the initializer list
                if let Some(first) = items.first() {
                    // TODO: Currently, we consider that the first item's type is the expected type
                    // for the entire list. In the future, we may want to find the expected type for
                    // the list from the context in which it is used (e.g., variable declaration
                    // type, function parameter type, etc.).
                    for item in items.iter() {
                        if item.ty != first.ty && item.ty.is_valid() {
                            self.errors
                                .emit_incompatible_types_in_initializer_list_error(item, &first.ty);
                        }
                    }
                }
            }
            ast::ExprKind::FieldAccess(_, _) => {
                // Not sure if any specific checks are needed here since type inference already
                // handles field existence and type assignment.
                // TODO: Revisit this if additional checks are needed in the future.
            }
            ast::ExprKind::String(_) => {
                // For now, we do not perform any specific semantic checks on string literals since
                // I'm not sure what constraints I want to enforce yet (UTF-8 validity, length
                // limits, etc.).
            }
            ast::ExprKind::Identifier(_) | ast::ExprKind::Bool(_) => {
                // Nothing to check for identifiers and boolean literals.
            }
            ast::ExprKind::Error(_) => unreachable!(),
        }
    }

    /// Infer the type of an expression, updating it in place.
    ///
    /// # Panics
    /// This function will panic if it encounters an `ast::ExprKind::Error`, as such expressions
    /// should not appear during type inference, or if an expression's type is still `Infer` but
    /// should have been determined during parsing (e.g., string or boolean literals).
    pub fn infer_expr(&mut self, expr: &mut Spanned<ast::Expr<'src>>) {
        // The expression's type is already known, so no inference is needed.
        if expr.ty != lang::ty::Type::Infer {
            return;
        }

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
                        expr.ty = lang::ty::Type::Bool;
                    }

                    lang::BinaryOp::Add
                    | lang::BinaryOp::Sub
                    | lang::BinaryOp::Mul
                    | lang::BinaryOp::Div => {
                        // If both sides have the same non-infer type, set the expression's type
                        // to that. However, if either side is still `Infer` or doesn't match,
                        // we set the type to `Unknown` to indicate a type inference failure.
                        // TODO: Implement proper type coercion rules here.
                        if lhs.ty == rhs.ty && lhs.ty != lang::ty::Type::Infer {
                            expr.ty = lhs.ty.clone();
                        } else {
                            expr.ty = lang::ty::Type::Unknown;
                        }
                    }
                }
            }
            ast::ExprKind::Unary(op, rhs) => {
                self.infer_expr(rhs);
                match op {
                    lang::UnaryOp::Neg => {
                        // Recursively infer the type of the inner expression if needed.
                        expr.ty = rhs.ty.clone();
                    }
                    lang::UnaryOp::Not => {
                        // Logical NOT operator always yields a boolean result.
                        expr.ty = lang::ty::Type::Bool;
                    }
                }
            }
            ast::ExprKind::FunctionCall(ident, args) => {
                // Recursively infer types of each argument expression.
                for argument in args {
                    self.infer_expr(argument);
                }

                if let Some(func) = self.scopes.get_function(ident.name) {
                    expr.ty = func.ret.clone();
                } else {
                    // If the function is not found, we set the expression's type to `Unknown`
                    // but we do not emit an error here, as the function call will be checked
                    // later in the `check_expr` method that will handle the error reporting.
                    expr.ty = lang::ty::Type::Unknown;
                }
            }
            ast::ExprKind::Identifier(identifier) => {
                // Identifier type is always explicited during their declaration. So we can
                // simply look it up in the symbol table and assign the variable's type to the
                // expression. We handle the case where the variable is not found by emitting an
                // undefined variable error and setting the expression's type to `Unknown`. This is
                // done here during inference to avoid having the lookup inside the symbol table
                // multiple times (once during inference and once during checking).
                if let Some(var) = self.scopes.get_variable(identifier.name) {
                    expr.ty = var.ty.clone();
                } else {
                    self.errors.emit_undefined_identifier_error(identifier);
                    expr.ty = lang::ty::Type::Unknown;
                }
            }
            ast::ExprKind::IntrinsicCall(identifier, args) => {
                // Recursively infer types of each argument expression.
                for argument in args {
                    self.infer_expr(argument);
                }

                match identifier.name {
                    // Set the types for known intrinsic functions. If an intrinsic is not listed
                    // here, we default to `Unknown` type.
                    "println" | "print" => {
                        expr.ty = lang::ty::Type::Void;
                    }
                    _ => expr.ty = lang::ty::Type::Unknown,
                }
            }
            ast::ExprKind::List(items) => {
                // Recursively infer types for each item in the list.
                for item in items.iter_mut() {
                    self.infer_expr(item);
                }

                // If all items are of the same type, the list type is that type. Otherwise, we
                // set it to `Unknown` to indicate a type inference failure.
                if let Some(first) = items.first() {
                    if items.iter().all(|item| item.ty == first.ty) {
                        expr.ty =
                            lang::ty::Type::Array(Box::new(first.ty.clone()), items.len() as u64);
                    } else {
                        expr.ty = lang::ty::Type::Unknown;
                    }
                } else {
                    todo!("Implement empty list type inference");
                }
            }
            ast::ExprKind::FieldAccess(expression, field) => {
                self.infer_expr(expression);

                // If the base expression's type is invalid, we cannot proceed with field access
                // inference, so we set the expression's type to `Unknown` and return early. We
                // do not emit an error here, as the inference failure should have already emitted
                // an error earlier.
                if !expression.ty.is_valid() {
                    expr.ty = lang::ty::Type::Unknown;
                    return;
                }

                if let Some(metadata) = self.types.get_type_metadata(&expression.ty) {
                    // Get the field type from the expression's type. If the field exists, set the
                    // expression's type to the field's type. Otherwise, set it to `Unknown` and emit
                    // an error indicating the field does not exist.
                    if let Some(ty) = metadata.fields.get(field.name) {
                        expr.ty = ty.clone();
                    } else {
                        self.errors.emit_unknown_field_access_error(
                            field,
                            &expression.ty,
                            expression.span(),
                        );
                        expr.ty = lang::ty::Type::Unknown;
                    }
                } else {
                    // TODO: Should we emit an error here if the type metadata does not exist? I
                    // don't think so since this means that the type itself is unknown, which should
                    // have already emitted an error earlier.
                    expr.ty = lang::ty::Type::Unknown;
                }
            }
            ast::ExprKind::Literal(_) => {
                // Literal types should be inferred based on their value and context. For now,
                // we assume all literals are integers until we implement proper literal types.
                todo!("Handle literal type inference");
            }
            ast::ExprKind::String(_) => {
                unreachable!("String literals should always have type String after parsing")
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
