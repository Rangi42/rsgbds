use compact_str::CompactString;

use crate::{
    diagnostics::{self, warning},
    expr::Expr,
    macro_args::MacroArgs,
    sources::{FileNode, NormalSpan, Span, SpanKind},
};

use super::parse_ctx;

impl parse_ctx!() {
    pub fn push_macro_arg(
        &self,
        args: MacroArgs,
        span_idx: usize,
        arg: CompactString,
    ) -> MacroArgs {
        if arg.is_empty() {
            let span = &self.line_spans[span_idx];
            // Do not report this if it originates from combined macro args, as an empty macro arg there
            // must stem from one passed to this macro, which must already have generated a warning.
            if !matches!(
                span,
                Span::Normal(NormalSpan {
                    node: FileNode {
                        kind: SpanKind::CombinedMacroArgs,
                        ..
                    },
                    ..
                })
            ) {
                self.warn(warning!("empty-macro-arg"), span, |warning| {
                    warning.set_message("empty macro argument");
                    warning.add_label(diagnostics::warning_label(span).with_message("here"));
                });
            }
        }

        args.push_arg(arg)
    }

    pub fn shift_macro_args(&mut self, amount: Option<Expr>, span_idx: usize) {
        let span = &self.line_spans[span_idx];

        if let Some(args) = self.macro_args.last_mut() {
            if let Some(expr) = amount {
                match expr.try_const_eval(
                    self.symbols,
                    Some(args),
                    self.sections,
                    |warning, span| warning.report(span, self.nb_errors_left, self.options),
                ) {
                    Ok((value, span)) => {
                        args.shift_by(value as isize, &span, self.nb_errors_left, self.options)
                    }
                    Err(error) => self.report_expr_error(error),
                }
            } else {
                args.shift_by(1, span, self.nb_errors_left, self.options);
            }
        } else {
            diagnostics::error(
                span,
                |error| {
                    error.set_message("cannot `shift` outside of a macro");
                    error.add_label(
                        diagnostics::error_label(span)
                            .with_message("no macro arguments to shift at this point"),
                    );
                },
                self.nb_errors_left,
                self.options,
            );
        }
    }
}
