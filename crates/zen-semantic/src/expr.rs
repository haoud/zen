use crate::SemanticAnalysis;
use span::Span;

impl<'src> SemanticAnalysis<'src> {
    /// Check an expression for semantic correctness, returning any errors found.
    pub fn check_expr(&mut self, expr: &mut ast::Expr<'src>, negated: bool) {
        self.infer_expr(expr, None);
        match &mut expr.kind {
            ast::ExprKind::Binary(op, lhs, rhs) => {
                self.check_expr(lhs, false);
                self.check_expr(rhs, false);

                // Verify that both sides of the binary operation have the same type. In the
                // future, we may want to implement type coercion rules here.
                if lhs.ty != rhs.ty {
                    if op.is_comparison() {
                        self.errors.emit_comparison_with_incompatible_types_error(
                            *op, lhs, rhs, expr.span,
                        );
                    } else {
                        self.errors
                            .emit_binary_op_type_mismatch_error(*op, lhs, rhs, expr.span);
                    }
                }

                // If either side has an invalid type, skip further checks for this binary
                // operation since it is already erroneous.
                if !lhs.ty.is_valid() || !rhs.ty.is_valid() {
                    return;
                }

                // If the operation is not supported by the type, emit an error.
                if !self.types.support_binary_op(*op, &lhs.ty) {
                    self.errors
                        .emit_invalid_binary_operation_for_type_error(*op, &lhs.ty, expr.span);
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

                // If the operation is not supported by the type, emit an error.
                if !self.types.support_unary_op(*op, &rhs.ty) {
                    self.errors
                        .emit_invalid_unary_operation_for_type_error(*op, &rhs.ty, expr.span);
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
                            expr.span,
                        );
                    }

                    // Check that each argument type matches the corresponding parameter type.
                    for (arg, param) in args.iter_mut().zip(&func.params) {
                        if arg.ty != param.ty && arg.ty.is_valid() {
                            self.errors.emit_argument_type_mismatch_error(
                                param, arg, param.span, expr.span,
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
                                ident, 1, 0, true, expr.span,
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
                                lang::ty::Type::Builtin(lang::ty::BuiltinType::Str),
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
                                expr.span,
                            );
                        }
                    }
                    _ => {
                        self.errors
                            .emit_unknown_intrinsic_function_error(ident, expr.span);
                    }
                }
            }
            ast::ExprKind::Literal(x) => {
                // Ensure that integer literals fit within the bounds of the integer type.
                match &x.ty {
                    lang::ty::Type::Array(_, _) => {
                        unreachable!("Array literals should be represented as List expressions")
                    }
                    lang::ty::Type::Struct(_) => {
                        unreachable!("Struct literals should be represented as field initializers")
                    }
                    lang::ty::Type::Builtin(ty) => match ty {
                        lang::ty::BuiltinType::Int => {
                            if x.value.parse_i64(negated).is_err() {
                                self.errors.emit_literal_overflow_error(x);
                            }
                        }
                        lang::ty::BuiltinType::Void => {
                            unreachable!("Void literals should not exist in the AST")
                        }
                        lang::ty::BuiltinType::Str => {
                            unreachable!(
                                "String literals should always have type String after parsing"
                            )
                        }
                        lang::ty::BuiltinType::Bool => {
                            unreachable!(
                                "Boolean literals should always have type Bool after parsing"
                            )
                        }
                    },
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
    pub fn infer_expr(&mut self, expr: &mut ast::Expr<'src>, target: Option<&lang::ty::Type>) {
        // The expression's type is already known, so no inference is needed.
        if expr.ty != lang::ty::Type::Infer {
            return;
        }

        match &mut expr.kind {
            ast::ExprKind::Binary(op, lhs, rhs) => {
                // Recursively infer types of the left and right expressions if needed.
                self.infer_expr(lhs, None);
                self.infer_expr(rhs, None);

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
                        expr.ty = lang::ty::Type::Builtin(lang::ty::BuiltinType::Bool);
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
                self.infer_expr(rhs, None);
                match op {
                    lang::UnaryOp::Neg => {
                        // Recursively infer the type of the inner expression if needed.
                        expr.ty = rhs.ty.clone();
                    }
                    lang::UnaryOp::Not => {
                        // Logical NOT operator always yields a boolean result.
                        expr.ty = lang::ty::Type::Builtin(lang::ty::BuiltinType::Bool);
                    }
                }
            }
            ast::ExprKind::FunctionCall(ident, args) => {
                // Recursively infer types of each argument expression.
                for argument in args {
                    self.infer_expr(argument, None);
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
                    self.infer_expr(argument, None);
                }

                match identifier.name {
                    // Set the types for known intrinsic functions. If an intrinsic is not listed
                    // here, we default to `Unknown` type.
                    "println" | "print" => {
                        expr.ty = lang::ty::Type::Builtin(lang::ty::BuiltinType::Void);
                    }
                    _ => expr.ty = lang::ty::Type::Unknown,
                }
            }
            ast::ExprKind::List(items) => {
                if let Some(target) = target {
                    // Try to infer the list item types based on the target type.
                    let inferred_type = self.infer_list_items(items, target, expr.span);
                    expr.ty = inferred_type;
                } else {
                    // Recursively infer types for each item in the list.
                    for item in items.iter_mut() {
                        self.infer_expr(item, None);
                    }

                    // If all items are of the same type, the list type is an array of that type.
                    if let Some(first) = items.first() {
                        if items.iter().all(|item| item.ty == first.ty) {
                            expr.ty = lang::ty::Type::Array(
                                Box::new(first.ty.clone()),
                                items.len() as u64,
                            );
                        } else {
                            expr.ty = lang::ty::Type::Unknown;
                        }
                    } else {
                        todo!("Implement empty list type inference");
                    }
                }
            }
            ast::ExprKind::FieldAccess(expression, field) => {
                self.infer_expr(expression, None);

                // If the base expression's type is invalid, we cannot proceed with field access
                // inference, so we set the expression's type to `Unknown` and return early. We
                // do not emit an error here, as the inference failure should have already emitted
                // an error earlier.
                if !expression.ty.is_valid() {
                    expr.ty = lang::ty::Type::Unknown;
                    return;
                }

                // Get the field type from the expression's type. If the field exists, set the
                // expression's type to the field's type. Otherwise, set it to `Unknown` and emit
                // an error indicating the field does not exist.
                if let Some(ty) = self.types.get_field(&expression.ty, field.name) {
                    expr.ty = ty.clone();
                } else {
                    self.errors.emit_unknown_field_access_error(
                        field,
                        &expression.ty,
                        expression.span,
                    );
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

    /// Try to infer list item types to match the target type. If successful, return the inferred
    /// type. If not, return `Unknown`.
    pub fn infer_list_items(
        &mut self,
        items: &mut [ast::Expr<'src>],
        target: &lang::ty::Type,
        span: Span,
    ) -> lang::ty::Type {
        match target {
            lang::ty::Type::Array(ty, len) => {
                // If the target type is an array, we can infer the item types based on the
                // array's element type and length.
                let mut same_length = true;
                let mut same_type = true;

                // Check if the length of the initializer list matches the target array length.
                if items.len() as u64 != *len {
                    same_length = false;
                }

                // Infer each item's type to be the array's element type and check for type
                // compatibility.
                for item in items.iter_mut() {
                    self.infer_expr(item, Some(ty));
                    if item.ty != **ty && item.ty.is_valid() {
                        self.errors
                            .emit_incompatible_types_in_initializer_list_error(item, &ty);
                        same_type = false;
                    }
                }

                // If all items have the same type and the length matches, we can return the array
                // type. If only the types match, we return an array type with the length of the
                // initializer to avoid losing information, but this will not pass type checking
                // later.
                if same_type {
                    if same_length {
                        return lang::ty::Type::Array(ty.clone(), *len);
                    } else {
                        return lang::ty::Type::Array(ty.clone(), items.len() as u64);
                    }
                }

                return lang::ty::Type::Unknown;
            }
            lang::ty::Type::Struct(name) => {
                // If the target type is a structure, we can infer the item types based on the
                // structure's field types.
                if let Some(structure) = self.types.get_struct_metadata(name).cloned() {
                    let mut right_length = true;
                    let mut right_type = true;

                    for (item, i) in items.iter_mut().zip(&structure.fields_order) {
                        let field_type = structure
                            .fields
                            .get(i)
                            .expect("Field type should exist for field name");
                        self.infer_expr(item, Some(field_type));
                        if item.ty != *field_type {
                            self.errors
                                .emit_incompatible_types_in_initializer_list_error(
                                    item, field_type,
                                );
                            right_type = false;
                        }
                    }

                    // If the number of items does not match the number of fields, emit an error,
                    // because this is probably not what the developer intended. C allows this by
                    // default, and I think it is a very bad idea.
                    if items.len() != structure.fields.len() {
                        self.errors.emit_count_mismatch_in_initializer_list_error(
                            structure.fields.len(),
                            items.len(),
                            span,
                        );
                        right_length = false;
                    }

                    if right_type && right_length {
                        return target.clone();
                    }
                }
            }
            lang::ty::Type::Builtin(ty) => {
                // For built-in types, we can only infer the item type if there is exactly one item
                // in the list. We infer that item's type to be the built-in type and check if it
                // matches the target type.
                if let Some(item) = items.first_mut() {
                    self.infer_expr(item, Some(&lang::ty::Type::Builtin(*ty)));
                    if item.ty == *target {
                        if items.len() == 1 {
                            return target.clone();
                        } else {
                            self.errors.emit_count_mismatch_in_initializer_list_error(
                                1,
                                items.len(),
                                span,
                            );
                        }
                    }
                }
            }
            lang::ty::Type::Infer | lang::ty::Type::Unknown => {
                // If the target type is `Infer` or `Unknown`, we cannot infer item types, so we
                // skip inference for each item.
            }
        }
        lang::ty::Type::Unknown
    }
}
