//! This module defines the different kinds of semantic errors that can occur during
//! semantic analysis, as well as a structure to collect and report these errors and
//! helpers functions to emit specific errors easily with proper formatting without
//! duplicating code and without having big error handling code in the semantic analyzer
//! code itself.
use ariadne::{Color, ReportKind};
use lang::Spanned;

use crate::{SemanticError, symbol};

/// The different kinds of semantic errors that can occur during semantic analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum SemanticErrorKind {
    /// Redefinition of a function with the same name (Zen does not support function overloading).
    FunctionRedefinition = 1,

    /// Code that is determined to be unreachable, such as statements after a return.
    UnreachableCode = 2,

    /// All control paths in a function do not return a value when the function is expected
    /// to return a value.
    MissingReturn = 3,

    /// The type of a return statement does not match the function's declared return type.
    ReturnTypeMismatch = 4,

    /// The types of the operands in a binary operation do not match and does not coerce.
    BinaryOpTypeMismatch = 5,

    /// Attempted to perform arithmetic on boolean values (like who does that ?).
    BooleanArithmetic = 6,

    /// Use of an undefined variable.
    UndefinedVariable = 7,

    /// Redefinition of a variable with the same name (shadowing must be explicit in Zen).
    VariableRedefinition = 8,

    /// The type of a variable definition does not match the variable's declared type.
    VariableDefinitionTypeMismatch = 9,

    /// An integer literal is too large to fit in the specified type.
    LiteralOverflow = 10,

    /// The negation operator '-' is applied to a non-numeric type.
    NegationOfNonNumericType = 11,

    /// Attempted to mutate an immutable variable (declared with `let` instead of `var`).
    MutationOfImmutableVariable = 12,

    /// Type mismatch in assignment (the type of the expression does not match the variable's type).
    TypeMismatchInAssignment = 13,

    /// Attempted to use a compound assignment (like `+=`, `-=`, etc.) on a boolean variable.
    BoolCompoundAssignment = 14,

    /// Call to an undefined function.
    UndefinedFunction = 15,

    /// The number of arguments in a function call does not match the number of parameters
    /// in the function definition.
    ArgumentCountMismatch = 16,

    /// The type of an argument in a function call does not match the type of the corresponding
    /// parameter in the function definition.
    ArgumentTypeMismatch = 17,

    /// Using logical operators (`and`, `or`) with non-boolean operands.
    LogicalOperatorWithNonBoolean = 18,

    /// Using the logical not operator (`not`) with a non-boolean operand.
    LogicalNotWithNonBoolean = 19,

    /// Using comparison operators (`==`, `!=`, `<`, `>`, `<=`, `>=`) with incompatible types.
    ComparisonWithIncompatibleTypes = 20,

    /// Using an non-boolean expression in a conditional statement (like `if` or `while`).
    NonBooleanInConditional = 21,

    /// Not all control paths in a non-void function return a value.
    NotAllPathsReturnValue = 22,

    /// Invalid binary operation attempted on string literals.
    InvalidStringBinaryOperation = 23,

    /// Invalid unary operation attempted on string literals.
    InvalidStringUnaryOperation = 24,

    /// Attempted to return a value from a void function.
    ReturnValueFromVoidFunction = 25,

    /// Missing return expression in a non-void function.
    MissingReturnExpression = 26,

    /// Attempted to declare a variable of type void.
    VoidVariableDeclaration = 27,

    /// Attempted to declare a parameter of type void.
    VoidFunctionParameter = 28,

    /// Call to an unknown intrinsic function.
    UnknownIntrinsicFunction = 29,

    /// Type mismatch in argument provided to an intrinsic function.
    IntrinsicArgumentTypeMismatch = 30,

    /// Incorrect number of arguments provided to an intrinsic function.
    IntrinsicArgumentCountMismatch = 31,
}

/// A collection of semantic errors that can be emitted during semantic analysis.
#[derive(Debug)]
pub struct SemanticDiagnostic<'src> {
    /// The source filename where the errors were found, used for reporting nice error
    /// messages to the user.
    pub filename: &'src str,

    /// The collected semantic errors.
    pub errors: Vec<SemanticError<'src>>,
}

impl<'src> SemanticDiagnostic<'src> {
    /// Create a new `SemanticDiagnostic` with the given source filename and an empty list
    /// of errors.
    #[must_use]
    pub fn new(filename: &'src str) -> Self {
        Self {
            filename,
            errors: Vec::new(),
        }
    }

    /// Consume the `SemanticDiagnostic`, returning the collected errors.
    #[must_use]
    pub fn collect(self) -> Vec<SemanticError<'src>> {
        self.errors
    }

    /// Emit a function redefinition error, given the function prototype, the original span
    /// where it was defined, and the new span where it is being redefined.
    #[inline]
    pub fn emit_function_redefinition_error(
        &mut self,
        proto: &ast::FunctionPrototype<'src>,
        og_span: lang::Span,
        fn_span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, fn_span.into_range()))
                .with_code(SemanticErrorKind::FunctionRedefinition as u32)
                .with_message(format!(
                    "the '{}' function is defined multiple times",
                    proto.ident.name
                ))
                .with_label(
                    ariadne::Label::new((self.filename, fn_span.into_range()))
                        .with_message(format!("function '{}' redefined here", proto.ident.name))
                        .with_color(Color::Red),
                )
                .with_label(
                    ariadne::Label::new((self.filename, og_span.into_range()))
                        .with_message("previous definition here")
                        .with_color(Color::Cyan),
                )
                .finish(),
        );
    }

    /// Emit an unreachable code error, given the span of the return statement that makes
    /// the code unreachable, and the span of the unreachable code itself.
    #[inline]
    pub fn emit_unreachable_code_error(&mut self, ret_span: lang::Span, code_span: lang::Span) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, code_span.into_range()))
                .with_code(SemanticErrorKind::UnreachableCode as u32)
                .with_message("unreachable code after return statement")
                .with_label(
                    ariadne::Label::new((self.filename, ret_span.into_range()))
                        .with_message("Any code following this return statement is unreachable")
                        .with_color(Color::Cyan),
                )
                .with_label(
                    ariadne::Label::new((self.filename, code_span.into_range()))
                        .with_message("This code will never be executed")
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit a missing return error, given the function prototype and the span of the function.
    #[inline]
    pub fn emit_missing_return_error(
        &mut self,
        proto: &ast::FunctionPrototype<'src>,
        fn_span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, fn_span.into_range()))
                .with_code(SemanticErrorKind::MissingReturn as u32)
                .with_message(format!(
                    "function '{}' does not have a return statement",
                    proto.ident.name
                ))
                .with_label(
                    ariadne::Label::new((self.filename, proto.ret.span().into_range()))
                        .with_message(format!(
                            "A return statement of type '{}' is required",
                            proto.ret.0
                        ))
                        .with_color(Color::Cyan),
                )
                .finish(),
        );
    }

    /// Emit a return type mismatch error, given the function prototype, the span of the
    /// return statement, and the actual type of the return statement.
    #[inline]
    pub fn emit_return_type_mismatch_error(
        &mut self,
        proto: &ast::FunctionPrototype<'src>,
        ret_span: lang::Span,
        ret_type: lang::Type,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, ret_span.into_range()))
                .with_code(SemanticErrorKind::ReturnTypeMismatch as u32)
                .with_message(format!(
                    "return type mismatch: expected '{}', found '{ret_type}'",
                    proto.ret.0
                ))
                .with_label(
                    ariadne::Label::new((self.filename, proto.ret.span().into_range()))
                        .with_message(format!("Expected return type '{}'", proto.ret.0))
                        .with_color(Color::Cyan),
                )
                .with_label(
                    ariadne::Label::new((self.filename, ret_span.into_range()))
                        .with_message(format!("Found return type '{ret_type}'"))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit a binary operation type mismatch error, given the binary operator, the left-hand
    /// side expression, the right-hand side expression, and the span of the binary operation.
    #[inline]
    pub fn emit_binary_op_type_mismatch_error(
        &mut self,
        op: lang::BinaryOp,
        lhs: &Spanned<ast::Expr<'src>>,
        rhs: &Spanned<ast::Expr<'src>>,
        span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, span.into_range()))
                .with_code(SemanticErrorKind::BinaryOpTypeMismatch as u32)
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
                .finish(),
        );
    }

    /// Emit a boolean arithmetic error, given the binary operator, the left-hand side
    /// expression, the right-hand side expression, and the span of the binary operation.
    #[inline]
    pub fn emit_boolean_arithmetic_error(
        &mut self,
        op: lang::BinaryOp,
        lhs: &Spanned<ast::Expr<'src>>,
        rhs: &Spanned<ast::Expr<'src>>,
        span: lang::Span,
    ) {
        let mut report =
            ariadne::Report::build(ReportKind::Error, (self.filename, span.into_range()))
                .with_code(SemanticErrorKind::BooleanArithmetic as u32)
                .with_message(format!(
                    "boolean values cannot be used in arithmetic operation '{op}'"
                ));

        // Add a specific label if the left operand is a boolean.
        if lhs.ty.is_boolean() {
            report = report.with_label(
                ariadne::Label::new((self.filename, lhs.span().into_range()))
                    .with_message("Left operand type is 'bool'")
                    .with_color(Color::Cyan),
            );
        }

        // Add a specific label if the right operand is a boolean.
        if rhs.ty.is_boolean() {
            report = report.with_label(
                ariadne::Label::new((self.filename, rhs.span().into_range()))
                    .with_message("Right operand type is 'bool'")
                    .with_color(Color::Red),
            );
        }

        self.errors.push(report.finish());
    }

    /// Emit an undefined variable error, given the spanned identifier of the variable.
    #[inline]
    pub fn emit_undefined_variable_error(&mut self, identifier: &Spanned<ast::Identifier<'src>>) {
        let span = identifier.span();
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, span.into_range()))
                .with_code(SemanticErrorKind::UndefinedVariable as u32)
                .with_message(format!("undeclared identifier '{}'", identifier.name))
                .with_label(
                    ariadne::Label::new((self.filename, span.into_range()))
                        .with_message(format!(
                            "'{}' is not declared in this scope",
                            identifier.name
                        ))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit a variable redefinition error, given the spanned identifier of the variable
    /// and the previous span where it was defined.
    #[inline]
    pub fn emit_variable_redefinition_error(
        &mut self,
        variable: &Spanned<symbol::Variable<'src>>,
        previous_span: lang::Span,
    ) {
        let span = variable.span();
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, span.into_range()))
                .with_code(SemanticErrorKind::VariableRedefinition as u32)
                .with_message(format!("redeclaration of variable '{}'", variable.name))
                .with_label(
                    ariadne::Label::new((self.filename, span.into_range()))
                        .with_message(format!("variable '{}' redeclared here", variable.name))
                        .with_color(Color::Red),
                )
                .with_label(
                    ariadne::Label::new((self.filename, previous_span.into_range()))
                        .with_message("previous declaration here")
                        .with_color(Color::Cyan),
                )
                .finish(),
        );
    }

    /// Emit a variable definition type mismatch error, given the expression assigned to
    /// the variable, the declared type of the variable, and the span of the entire let
    /// statement.
    #[inline]
    pub fn emit_variable_definition_type_mismatch_error(
        &mut self,
        expr: &Spanned<ast::Expr<'src>>,
        ty: &Spanned<lang::Type>,
        stmt_span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, stmt_span.into_range()))
                .with_code(SemanticErrorKind::VariableDefinitionTypeMismatch as u32)
                .with_message(format!(
                    "type mismatch in variable definition: expected '{}', found '{}'",
                    ty.0, expr.ty
                ))
                .with_label(
                    ariadne::Label::new((self.filename, ty.span().into_range()))
                        .with_message(format!("Expected type '{}'", ty.0))
                        .with_color(Color::Cyan),
                )
                .with_label(
                    ariadne::Label::new((self.filename, expr.span().into_range()))
                        .with_message(format!("Found type '{}'", expr.ty))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit an integer literal overflow error, given the spanned literal that caused
    /// the overflow.
    #[inline]
    pub fn emit_literal_overflow_error(&mut self, literal: &Spanned<ast::Literal>) {
        let span = literal.span();
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, span.into_range()))
                .with_code(SemanticErrorKind::LiteralOverflow as u32)
                .with_message(format!(
                    "integer literal '{}' overflows the type '{}'",
                    literal.value, literal.ty
                ))
                .with_label(
                    ariadne::Label::new((self.filename, span.into_range()))
                        .with_message(format!(
                            "the literal '{}' does not fit in the type '{}'",
                            literal.value, literal.ty
                        ))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit a negation of non-numeric type error, given the expression being negated.
    #[inline]
    pub fn emit_negation_of_non_numeric_type_error(&mut self, expr: &Spanned<ast::Expr<'src>>) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, expr.span().into_range()))
                .with_code(SemanticErrorKind::NegationOfNonNumericType as u32)
                .with_message(format!(
                    "negation operator '-' cannot be applied to type '{}'",
                    expr.ty
                ))
                .with_label(
                    ariadne::Label::new((self.filename, expr.span().into_range()))
                        .with_message(format!(
                            "the expression is of type '{}', which is not numeric",
                            expr.ty
                        ))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit a mutation of immutable variable error, given the spanned variable being mutated
    /// (which is not mutable, i.e declared with `let` instead of `var`).
    #[inline]
    pub fn emit_mutation_of_immutable_variable_error(
        &mut self,
        variable: &Spanned<symbol::Variable<'src>>,
        stmt_span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, stmt_span.into_range()))
                .with_code(SemanticErrorKind::MutationOfImmutableVariable as u32)
                .with_message(format!(
                    "cannot assign to immutable variable '{}'",
                    variable.name
                ))
                .with_label(
                    ariadne::Label::new((self.filename, stmt_span.into_range()))
                        .with_message(format!("variable '{}' is not mutable", variable.name))
                        .with_color(Color::Red),
                )
                .with_label(
                    ariadne::Label::new((self.filename, variable.span().into_range()))
                        .with_message("immutable variable declared here")
                        .with_color(Color::Cyan),
                )
                .finish(),
        );
    }

    /// Emit an error when the type of the expression assigned to a variable does not match
    /// the variable's type. It takes the spanned variable, the spanned expression, and the
    /// span of the entire assignment statement.
    #[inline]
    pub fn emit_type_mismatch_in_assignment_error(
        &mut self,
        variable: &Spanned<symbol::Variable<'src>>,
        expr: &Spanned<ast::Expr<'src>>,
        stmt_span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, stmt_span.into_range()))
                .with_code(SemanticErrorKind::TypeMismatchInAssignment as u32)
                .with_message(format!(
                    "type mismatch in assignment to variable '{}': expected '{}', found '{}'",
                    variable.name, variable.ty, expr.ty
                ))
                .with_label(
                    ariadne::Label::new((self.filename, variable.span().into_range()))
                        .with_message(format!(
                            "Expected type '{}' for variable '{}'",
                            variable.ty, variable.name
                        ))
                        .with_color(Color::Cyan),
                )
                .with_label(
                    ariadne::Label::new((self.filename, expr.span().into_range()))
                        .with_message(format!("Found type '{}' in expression", expr.ty))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit an error when a compound assignment (like `+=`, `-=`, etc.) is attempted
    /// on a boolean variable. It takes the spanned variable, the binary operator used
    /// in the compound assignment, and the span of the entire assignment statement.
    #[inline]
    pub fn emit_bool_compound_assignment_error(
        &mut self,
        variable: &Spanned<symbol::Variable<'src>>,
        op: lang::BinaryOp,
        stmt_span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, stmt_span.into_range()))
                .with_code(SemanticErrorKind::BoolCompoundAssignment as u32)
                .with_message(format!(
                    "cannot use compound assignment '{op}'= on boolean variable '{}'",
                    variable.name
                ))
                .with_label(
                    ariadne::Label::new((self.filename, variable.span().into_range()))
                        .with_message("Boolean variable declared here")
                        .with_color(Color::Cyan),
                )
                .finish(),
        );
    }

    /// Emit an error when a unknown function is called, given the spanned identifier
    /// of the function.
    #[inline]
    pub fn emit_undefined_function_error(&mut self, identifier: &Spanned<ast::Identifier<'src>>) {
        let span = identifier.span();
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, span.into_range()))
                .with_code(SemanticErrorKind::UndefinedFunction as u32)
                .with_message(format!("call to undefined function '{}'", identifier.name))
                .with_label(
                    ariadne::Label::new((self.filename, span.into_range()))
                        .with_message(format!(
                            "function '{}' is not declared in this scope",
                            identifier.name
                        ))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit an error when the number of arguments in a function call does not match the
    /// number of parameters in the function definition. It takes the spanned identifier
    /// of the function being called, the expected number of arguments, the actual number
    /// of arguments, and the span of the entire function call expression.
    #[inline]
    pub fn emit_argument_count_mismatch_error(
        &mut self,
        identifier: &Spanned<ast::Identifier<'src>>,
        expected: usize,
        actual: usize,
        call_span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, call_span.into_range()))
                .with_code(SemanticErrorKind::ArgumentCountMismatch as u32)
                .with_message(format!(
                    "argument count mismatch in call to function '{}': expected {}, found {}",
                    identifier.name, expected, actual
                ))
                .with_label(
                    ariadne::Label::new((self.filename, identifier.span().into_range()))
                        .with_message(format!(
                            "function '{}' is called with {} arguments, but it expects {}",
                            identifier.name, actual, expected
                        ))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit an error when the type of an argument in a function call does not match
    /// the type of the corresponding parameter in the function definition. It takes
    /// the parameter variable, the spanned argument expression, and the span of the
    /// entire function call expression.
    #[inline]
    pub fn emit_argument_type_mismatch_error(
        &mut self,
        param: &symbol::Variable<'src>,
        arg: &Spanned<ast::Expr<'src>>,
        param_span: lang::Span,
        call_span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, call_span.into_range()))
                .with_code(SemanticErrorKind::ArgumentTypeMismatch as u32)
                .with_message(format!(
                    "argument type mismatch in call to function: expected '{}', found '{}'",
                    param.ty, arg.ty
                ))
                .with_label(
                    ariadne::Label::new((self.filename, arg.span().into_range()))
                        .with_message(format!("Found type '{}' in argument", arg.ty))
                        .with_color(Color::Red),
                )
                .with_label(
                    ariadne::Label::new((self.filename, param_span.into_range()))
                        .with_message(format!(
                            "Expected type '{}' for parameter '{}'",
                            param.ty, param.name
                        ))
                        .with_color(Color::Cyan),
                )
                .finish(),
        );
    }

    /// Emit an error when logical operators (`&&`, `||`) are used with non-boolean operands.
    #[inline]
    pub fn emit_logical_operator_with_non_boolean_error(
        &mut self,
        op: lang::BinaryOp,
        lhs: &Spanned<ast::Expr<'src>>,
        rhs: &Spanned<ast::Expr<'src>>,
        span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, span.into_range()))
                .with_code(SemanticErrorKind::LogicalOperatorWithNonBoolean as u32)
                .with_message(format!(
                    "logical operator '{op}' requires boolean operands: left is '{}', right is '{}'",
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
                .finish(),
        );
    }

    /// Emit an error when the logical not operator (`!`) is used with a non-boolean operand.
    #[inline]
    pub fn emit_logical_not_with_non_boolean_error(&mut self, expr: &Spanned<ast::Expr<'src>>) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, expr.span().into_range()))
                .with_code(SemanticErrorKind::LogicalOperatorWithNonBoolean as u32)
                .with_message(format!(
                    "logical operator '!' requires a boolean operand: found '{}'",
                    expr.ty
                ))
                .with_label(
                    ariadne::Label::new((self.filename, expr.span().into_range()))
                        .with_message(format!("Operand is of type '{}'", expr.ty))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit an error when comparison operators (`==`, `!=`, `<`, `>`, `<=`, `>=`) are used
    /// with incompatible types (e.g., comparing a boolean with an integer).
    #[inline]
    pub fn emit_comparison_with_incompatible_types_error(
        &mut self,
        op: lang::BinaryOp,
        lhs: &Spanned<ast::Expr<'src>>,
        rhs: &Spanned<ast::Expr<'src>>,
        span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, span.into_range()))
                .with_code(SemanticErrorKind::ComparisonWithIncompatibleTypes as u32)
                .with_message(format!(
                    "comparison operator '{op}' requires compatible operand types: left is '{}', right is '{}'",
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
                .finish(),
        );
    }

    /// Emit an error when a non-boolean expression is used in a conditional statement
    /// (like `if` or `while`). It takes the spanned expression and the span of the entire
    /// conditional statement.
    #[inline]
    pub fn emit_non_boolean_in_conditional_error(
        &mut self,
        expr: &Spanned<ast::Expr<'src>>,
        stmt_span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, stmt_span.into_range()))
                .with_code(SemanticErrorKind::NonBooleanInConditional as u32)
                .with_message(format!(
                    "conditional statement requires a boolean expression: found '{}'",
                    expr.ty
                ))
                .with_label(
                    ariadne::Label::new((self.filename, expr.span().into_range()))
                        .with_message(format!("Expression is of type '{}'", expr.ty))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit an error when not all control paths in a non-void function return a value.
    #[inline]
    pub fn emit_not_all_paths_return_value_error(
        &mut self,
        proto: &ast::FunctionPrototype<'src>,
        fn_span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, fn_span.into_range()))
                .with_code(SemanticErrorKind::NotAllPathsReturnValue as u32)
                .with_message(format!(
                    "not all control paths in function '{}' return a value",
                    proto.ident.name
                ))
                .with_label(
                    ariadne::Label::new((self.filename, fn_span.into_range()))
                        .with_message(format!(
                            "function '{}' must return a value of type '{}'",
                            proto.ident.name, proto.ret.0
                        ))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit an error when an invalid binary operation is attempted on string literals.
    #[inline]
    pub fn emit_invalid_string_binary_operation_error(
        &mut self,
        op: lang::BinaryOp,
        lhs: &Spanned<ast::Expr<'src>>,
        rhs: &Spanned<ast::Expr<'src>>,
        span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, span.into_range()))
                .with_code(SemanticErrorKind::InvalidStringBinaryOperation as u32)
                .with_message(format!(
                    "cannot perform binary operation '{op}' on string literals: left is '{}', right is '{}'",
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
                .finish(),
        );
    }

    /// Emit an error when an invalid unary operation is attempted on string literals.
    #[inline]
    pub fn emit_invalid_string_unary_operation_error(
        &mut self,
        op: lang::UnaryOp,
        expr: &Spanned<ast::Expr<'src>>,
        span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, span.into_range()))
                .with_code(SemanticErrorKind::InvalidStringUnaryOperation as u32)
                .with_message(format!(
                    "cannot perform unary operation '{op}' on string literal",
                ))
                .with_label(
                    ariadne::Label::new((self.filename, expr.span().into_range()))
                        .with_message(format!("Operand is a string literal"))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit an error when a value is returned from a void function.
    #[inline]
    pub fn emit_return_value_from_void_function_error(
        &mut self,
        fn_span: lang::Span,
        ret_span: lang::Span,
        expr: &Spanned<ast::Expr<'src>>
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, ret_span.into_range()))
                .with_code(SemanticErrorKind::ReturnValueFromVoidFunction as u32)
                .with_message("cannot return a value from a void function")
                .with_label(
                    ariadne::Label::new((self.filename, ret_span.into_range()))
                        .with_message(format!("returning a value here of type {}", expr.ty))
                        .with_color(Color::Red),
                )
                .with_label(
                    ariadne::Label::new((self.filename, fn_span.into_range()))
                        .with_message("function is declared as void here")
                        .with_color(Color::Cyan),
                )
                .finish(),
        );
    }

    /// Emit an error when a return expression is missing in a non-void function.
    #[inline]
    pub fn emit_missing_return_expression_error(
        &mut self,
        proto: &ast::FunctionPrototype<'src>,
        fn_span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, fn_span.into_range()))
                .with_code(SemanticErrorKind::MissingReturnExpression as u32)
                .with_message(format!(
                    "missing return expression in non-void function '{}'",
                    proto.ident.name
                ))
                .with_label(
                    ariadne::Label::new((self.filename, fn_span.into_range()))
                        .with_message(format!(
                            "function '{}' must return a value of type '{}'",
                            proto.ident.name, proto.ret.0
                        ))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit an error when a variable of type void is declared.
    #[inline]
    pub fn emit_void_variable_declaration_error(
        &mut self,
        var_span: lang::Span,
        var_name: &str,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, var_span.into_range()))
                .with_code(SemanticErrorKind::VoidVariableDeclaration as u32)
                .with_message(format!(
                    "cannot declare variable '{}' of type void",
                    var_name
                ))
                .with_label(
                    ariadne::Label::new((self.filename, var_span.into_range())) 
                        .with_message(format!("variable '{}' declared as void here", var_name))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit an error when a function parameter is declared with type void.
    #[inline]
    pub fn emit_void_function_parameter_error(
        &mut self,
        param_span: lang::Span,
        proto: &ast::FunctionPrototype<'src>,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, param_span.into_range()))
                .with_code(SemanticErrorKind::VoidFunctionParameter as u32)
                .with_message(format!(
                    "cannot declare parameter of type void in function '{}'",
                    proto.ident.name
                ))
                .with_label(
                    ariadne::Label::new((self.filename, param_span.into_range()))
                        .with_message("parameter declared as void here")
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit an error when an unknown intrinsic function is called.
    #[inline]
    pub fn emit_unknown_intrinsic_function_error(
        &mut self,
        identifier: &Spanned<ast::Identifier<'src>>,
        span: lang::Span,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, span.into_range()))
                .with_code(SemanticErrorKind::UnknownIntrinsicFunction as u32)
                .with_message(format!(
                    "call to unknown intrinsic function '{}'", identifier.name
                ))
                .with_label(
                    ariadne::Label::new((self.filename, identifier.span().into_range()))
                        .with_message(format!(
                            "intrinsic function '{}' does not exist", identifier.name
                        ))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    /// Emit an error when there is a type mismatch in an argument provided to an intrinsic
    /// function.
    #[inline]
    pub fn emit_intrinsic_argument_type_mismatch_error(
        &mut self,
        identifier: &Spanned<ast::Identifier<'src>>,
        expr: &Spanned<ast::Expr<'src>>,
        expected: lang::Type,
    ) {
        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, expr.span().into_range()))
                .with_code(SemanticErrorKind::IntrinsicArgumentTypeMismatch as u32)
                .with_message(format!(
                    "type mismatch in argument for intrinsic function '{}': expected '{}', found '{}'",
                    identifier.name, expected, expr.ty
                ))
                .with_label(
                    ariadne::Label::new((self.filename, expr.span().into_range()))
                        .with_message(format!(
                            "Expected type '{}' for intrinsic function '{}'", expected, identifier.name
                        ))
                        .with_color(Color::Cyan),
                )
                .finish(),
        ); 
    }

    /// Emit an error when there is an incorrect number of arguments provided to an intrinsic
    /// function.
    #[inline]
    pub fn emit_intrinsic_argument_count_mismatch_error(
        &mut self,
        identifier: &Spanned<ast::Identifier<'src>>,
        expected: usize,
        actual: usize,
        or_more: bool,
        call_span: lang::Span,
    ) {
        let message = if or_more {
            format!(
                "incorrect number of arguments for intrinsic function '{}': expected at least {}, found {}",
                identifier.name, expected, actual
            )
        } else {
            format!(
                "incorrect number of arguments for intrinsic function '{}': expected {}, found {}",
                identifier.name, expected, actual
            )
        };

        self.errors.push(
            ariadne::Report::build(ReportKind::Error, (self.filename, call_span.into_range()))
                .with_code(SemanticErrorKind::IntrinsicArgumentCountMismatch as u32)
                .with_message(message)
                .with_label(
                    ariadne::Label::new((self.filename, identifier.span().into_range()))
                        .with_message(format!(
                            "intrinsic function '{}' called with {} arguments, but it expects {}{}",
                            identifier.name,
                            actual,
                            expected,
                            if or_more { " or more" } else { "" }
                        ))
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }    
}