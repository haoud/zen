use crate::{
    ast,
    lang::{self, Spanned},
    semantic::SemanticError,
};

#[derive(Debug)]
struct TypeInference<'src> {
    errors: Vec<SemanticError<'src>>,
}

impl<'src> TypeInference<'src> {
    /// Create a new `TypeInference` instance with an empty list of errors.
    #[must_use]
    fn new() -> Self {
        Self { errors: Vec::new() }
    }

    /// Infer types for a statement, updating it in place.
    ///
    /// # Errors
    /// Errors are collected in the `errors` field and reported at the end of type inference when
    /// [`finalize`](Self::finalize) is called.
    fn infer_stmt(&mut self, stmt: &mut Spanned<ast::Stmt>) {
        match &mut stmt.kind {
            ast::StmtKind::Return(expr) => self.infer_expr(expr),
            ast::StmtKind::Error(_) => unreachable!(),
        }
    }

    /// Infer the type of an expression, updating it in place.
    ///
    /// # Errors
    /// Errors are collected in the `errors` field and reported at the end of type inference
    /// when [`finalize`](Self::finalize) is called.
    fn infer_expr(&mut self, expr: &mut Spanned<ast::Expr>) {
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
                ast::ExprKind::Literal(_) => todo!("Handle literal type inference"),
                ast::ExprKind::Bool(_) => {
                    unreachable!("Boolean literals should always have type Bool after parsing")
                }
                ast::ExprKind::Placeholder(_) => {
                    unreachable!("Placeholders should not appear during type inference")
                }
                ast::ExprKind::Error(_) => {
                    unreachable!("Error expressions should not appear during type inference")
                }
            }
        }
    }

    /// Finalize type inference, returning any collected errors.
    fn finalize(self) -> Result<(), Vec<SemanticError<'src>>> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors)
        }
    }
}

/// Perform type inference on the given functions, updating their ASTs in place.
pub fn infer_types<'src>(
    funcs: &mut [Spanned<ast::Function<'src>>],
) -> Result<(), Vec<SemanticError<'src>>> {
    let mut type_inference = TypeInference::new();

    for func in funcs {
        for stmt in &mut func.body {
            type_inference.infer_stmt(stmt);
        }
    }

    type_inference.finalize()
}
