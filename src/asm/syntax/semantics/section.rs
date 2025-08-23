use crate::{diagnostics, expr::Expr, warning};

use super::parse_ctx;

impl parse_ctx!() {
    pub fn align_args(&self, align_expr: Expr, ofs_expr: Option<Expr>) -> (u8, u16) {
        let align = match self.try_const_eval(&align_expr) {
            Ok((value, span)) => match value {
                0..=16 => value as u8,
                17.. => {
                    self.error(&span, |error| {
                        error.set_message("alignment cannot be larger than 16");
                        error.add_label(
                            diagnostics::error_label(&span)
                                .with_message(format!("an alignment of {value} is invalid")),
                        );
                    });
                    16
                }
                ..=-1 => {
                    self.error(&span, |error| {
                        error.set_message("alignment cannot be negative");
                        error.add_label(
                            diagnostics::error_label(&span)
                                .with_message(format!("an alignment of {value} is invalid")),
                        );
                    });
                    0
                }
            },
            Err(err) => {
                self.report_expr_error(err);
                0
            }
        };

        let align_size = 1 << align;
        let align_ofs = ofs_expr.and_then(|expr| {
            match self.try_const_eval(&expr) {
                Ok((value, span)) => {
                    if value >= align_size || value <= -align_size {
                        self.warn(warning!("align-ofs"), &span, |warning| {
                            warning.set_message("alignment offset is larger than alignment size");
                            warning.add_label(
                                diagnostics::warning_label(&span)
                                    .with_message(format!("requested an alignment to {align_size} bytes and an offset of {value} bytes")),
                            );
                        });
                    }
                    Some(value.rem_euclid(align_size) as u16)
                },
                Err(err) => {
                    self.report_expr_error(err);
                    None
                }
            }
        }).unwrap_or(0);

        (align, align_ofs)
    }
}
