use crate::{
    diagnostics::{self, ReportBuilder, WarningKind},
    expr::Expr,
    sources::Span,
};

use super::{parse_ctx, tokens::TokenPayload};

mod charmap;
mod condition;
mod context;
mod data;
mod error;
mod expr;
pub mod fixed_point; // Exporting the fixed-point precision clamp function for the lexer.
mod instructions;
mod macros;
mod opt;
mod print;
mod section;
mod string;
mod symbols;

type ParseError = lalrpop_util::ParseError<usize, TokenPayload, ()>;

/// Utilities used pervasively throughout semantic actions.
impl parse_ctx!() {
    pub(super) fn nth_span(&mut self, span_idx: usize) -> Span {
        // Use a value that isn't supposed to appear at all,
        // so that a double call to this function fails in a conspicuous way even in release mode.
        let span = std::mem::replace(&mut self.line_spans[span_idx], Span::CommandLine);
        debug_assert!(
            matches!(span, Span::Normal(_)),
            "`nth_span` called twice on idx {span_idx}"
        );
        span
    }

    pub(super) fn span_from_to(&self, left_idx: usize, right_idx: usize) -> Span {
        self.line_spans[left_idx].merged_with(&self.line_spans[right_idx])
    }

    pub(super) fn try_const_eval(&self, expr: &Expr) -> Result<(i32, Span), crate::expr::Error> {
        expr.try_const_eval(self.symbols, self.macro_args.last(), self.sections)
    }

    pub(super) fn error<'span, F: FnOnce(&mut ReportBuilder<'span>)>(
        &self,
        span: &'span Span,
        callback: F,
    ) {
        diagnostics::error(span, callback, self.nb_errors_left, self.options);
    }
    pub(super) fn report_expr_error(&self, error: crate::expr::Error) {
        error.report(self.identifiers, self.nb_errors_left, self.options);
    }

    pub(super) fn warn<'span, F: FnOnce(&mut ReportBuilder<'span>)>(
        &self,
        id: WarningKind,
        span: &'span Span,
        callback: F,
    ) {
        diagnostics::warn(id, span, callback, self.nb_errors_left, self.options);
    }
}
