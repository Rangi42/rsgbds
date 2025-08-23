use crate::{diagnostics, expr::Expr};

use super::parse_ctx;

impl parse_ctx!() {
    pub fn shift_macro_args(&mut self, amount: Option<Expr>, span_idx: usize) {
        let span = &self.line_spans[span_idx];

        if let Some(args) = self.macro_args.last_mut() {
            if let Some(expr) = amount {
                match expr.try_const_eval(self.symbols, Some(args), self.sections) {
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
