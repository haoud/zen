use lang::{Span, Spanned};

use crate::SemanticAnalysis;

/// A control flow graph node.
#[derive(Debug)]
pub enum ControlFlowNode<'a, 'src> {
    /// A return statement node.
    Return(ControlFlowReturn<'a, 'src>),

    /// A block of statements.
    Stmt(ControlFlowStmts<'a, 'src>),
}

/// A return statement in the control flow graph. This node represents a return statement
/// in the function body, including the expression being returned and any statements that are
/// unreachable after the return statement.
#[derive(Debug)]
pub struct ControlFlowReturn<'a, 'src> {
    /// The expression being returned. This can be a literal, variable, or more complex expression.
    /// The type of this expression should match the function's return type.
    pub expr: &'a ast::Expr<'src>,

    /// The span of the return statement. This is used for error reporting.
    pub span: Span,

    /// Any statements that are unreachable after this return statement. This is optional should be
    /// None if there are no unreachable statements. Any unreachable statements should be reported
    /// as errors during analysis.
    pub unreachable: Option<ControlFlowStmts<'a, 'src>>,
}

/// A list of statements in the control flow graph. This node is used to represent any kind of
/// statements that does not alter the control flow, such as variable declarations, expressions...
#[derive(Debug)]
pub struct ControlFlowStmts<'a, 'src> {
    /// A list of spanned statements.
    pub stmts: &'a [Spanned<ast::Stmt<'src>>],
}

/// Build a control flow graph for a function body. This function scans the statements in the
/// function body and constructs a control flow graph that represents the flow of control
/// through the function.
///
/// TODO: Return a Result if the control flow graph cannot be built due to malformed statements
/// for example when an else statement is found without a matching if statement.
pub fn build_graph<'a, 'src>(stmts: &'a [Spanned<ast::Stmt<'src>>]) -> ControlFlowNode<'a, 'src> {
    for (i, stmt) in stmts.iter().enumerate() {
        if let ast::StmtKind::Return(expr) = &stmt.kind {
            let unreachable = if i + 1 < stmts.len() {
                Some(ControlFlowStmts {
                    stmts: &stmts[i + 1..],
                })
            } else {
                None
            };

            // We found a return statement. We create a ControlFlowReturn node and return it. Any
            // statements after the return are considered unreachable and are included in the
            // unreachable field of the ControlFlowReturn node.
            return ControlFlowNode::Return(ControlFlowReturn {
                expr,
                unreachable,
                span: stmt.span(),
            });
        }
    }

    // If we reach here, there are no return statements in the function body. We simply return
    // all the statements as a single block, which will be analyzed later and will trigger an
    // error if the function is expected to return a value.
    ControlFlowNode::Stmt(ControlFlowStmts { stmts: &stmts[..] })
}

/// Analyze the control flow graph for a function. Any errors found during analysis will be
/// reported to the SemanticAnalysis's error handler.
pub fn analyse_graph<'a, 'src>(
    analysis: &mut SemanticAnalysis<'src>,
    function: &ast::Function<'src>,
    node: ControlFlowNode<'a, 'src>,
) {
    // Analyze the control flow graph. Currently, since we do not have any kind of branching
    // statements (if, while, etc.), this function is very simple and not even recursive. When
    // we add branching statements, this function will need to be updated to handle them and
    // will likely become kinda recursive.
    match node {
        ControlFlowNode::Return(ret) => {
            // Check if the return type matches the function's return type. This is only
            // checked if the return type is concrete to avoid reporting errors when the
            // type inference fails that may cause the return type to be unknown.
            if ret.expr.ty != function.prototype.ret.0 && ret.expr.ty.is_valid() {
                analysis.errors.emit_return_type_mismatch_error(
                    &function.prototype,
                    ret.span,
                    ret.expr.ty,
                );
            }

            // If there are unreachable statements after the return, emit an error for them
            if let Some(unreachable) = ret.unreachable {
                let start = unreachable.stmts.first().unwrap().span().start;
                let end = unreachable.stmts.last().unwrap().span().end;
                let span = Span::from(start..end);
                analysis.errors.emit_unreachable_code_error(ret.span, span);
            }
        }
        ControlFlowNode::Stmt(node) => {
            let start = node.stmts.first().map_or(0, |stmts| stmts.span().start);
            let end = node.stmts.last().map_or(0, |stmts| stmts.span().end);
            let span = Span::from(start..end);
            analysis
                .errors
                .emit_missing_return_error(&function.prototype, span);
        }
    }
}
