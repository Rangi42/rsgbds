use std::{cell::Cell, num::FpCategory};

use crate::{diagnostics, expr::Expr, sources::Span, Options};

use super::parse_ctx;

// TODO: the spans passed to `Expr::nothing` could be different, so as to avoid merging spans on syntax errors?
impl parse_ctx!() {
    pub fn fixpoint_round(
        &self,
        expr: Expr,
        precision: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let precision = self.fixpoint_precision(precision);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let Some((value, _span)) = self.fix_to_float(expr, precision) {
            Expr::number(Self::float_to_fix(value.round(), precision), span)
        } else {
            Expr::nothing(span)
        }
    }

    pub fn fixpoint_ceil(
        &self,
        expr: Expr,
        precision: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let precision = self.fixpoint_precision(precision);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let Some((value, _span)) = self.fix_to_float(expr, precision) {
            Expr::number(Self::float_to_fix(value.ceil(), precision), span)
        } else {
            Expr::nothing(span)
        }
    }

    pub fn fixpoint_floor(
        &self,
        expr: Expr,
        precision: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let precision = self.fixpoint_precision(precision);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let Some((value, _span)) = self.fix_to_float(expr, precision) {
            Expr::number(Self::float_to_fix(value.floor(), precision), span)
        } else {
            Expr::nothing(span)
        }
    }

    pub fn fixpoint_mul(
        &self,
        lhs: Expr,
        rhs: Expr,
        precision: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let precision = self.fixpoint_precision(precision);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let (Some((left, _left_span)), Some((right, _right_span))) = (
            self.fix_to_float(lhs, precision),
            self.fix_to_float(rhs, precision),
        ) {
            Expr::number(Self::float_to_fix(left * right, precision), span)
        } else {
            Expr::nothing(span)
        }
    }

    pub fn fixpoint_div(
        &self,
        lhs: Expr,
        rhs: Expr,
        precision: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let precision = self.fixpoint_precision(precision);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let (Some((left, _left_span)), Some((right, right_span))) = (
            self.fix_to_float(lhs, precision),
            self.fix_to_float(rhs, precision),
        ) {
            // Rust defines `-0.0 == +0.0`, so we need not ask for the float's classification.
            if right != 0. {
                Expr::number(Self::float_to_fix(left / right, precision), span)
            } else {
                self.report_expr_error(crate::expr::Error {
                    span: right_span,
                    kind: crate::expr::ErrKind::DivBy0,
                });
                Expr::nothing(span)
            }
        } else {
            Expr::nothing(span)
        }
    }

    pub fn fixpoint_mod(
        &self,
        lhs: Expr,
        rhs: Expr,
        precision: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let precision = self.fixpoint_precision(precision);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let (Some((left, _left_span)), Some((right, right_span))) = (
            self.fix_to_float(lhs, precision),
            self.fix_to_float(rhs, precision),
        ) {
            // Rust defines `-0.0 == +0.0`, so we need not ask for the float's classification.
            if right != 0. {
                Expr::number(Self::float_to_fix(left % right, precision), span)
            } else {
                self.report_expr_error(crate::expr::Error {
                    span: right_span,
                    kind: crate::expr::ErrKind::DivBy0,
                });
                Expr::nothing(span)
            }
        } else {
            Expr::nothing(span)
        }
    }

    pub fn fixpoint_pow(
        &self,
        lhs: Expr,
        rhs: Expr,
        precision: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let precision = self.fixpoint_precision(precision);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let (Some((left, _left_span)), Some((right, _right_span))) = (
            self.fix_to_float(lhs, precision),
            self.fix_to_float(rhs, precision),
        ) {
            Expr::number(Self::float_to_fix(left.powf(right), precision), span)
        } else {
            Expr::nothing(span)
        }
    }

    pub fn fixpoint_log(
        &self,
        lhs: Expr,
        rhs: Expr,
        precision: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let precision = self.fixpoint_precision(precision);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let (Some((left, _left_span)), Some((right, _right_span))) = (
            self.fix_to_float(lhs, precision),
            self.fix_to_float(rhs, precision),
        ) {
            Expr::number(Self::float_to_fix(left.log(right), precision), span)
        } else {
            Expr::nothing(span)
        }
    }

    pub fn fixpoint_sin(
        &self,
        expr: Expr,
        precision: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let precision = self.fixpoint_precision(precision);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let Some((value, _span)) = self.fix_to_float(expr, precision) {
            Expr::number(
                Self::float_to_fix(turns_to_radians(value).sin(), precision),
                span,
            )
        } else {
            Expr::nothing(span)
        }
    }

    pub fn fixpoint_cos(
        &self,
        expr: Expr,
        precision: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let precision = self.fixpoint_precision(precision);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let Some((value, _span)) = self.fix_to_float(expr, precision) {
            Expr::number(
                Self::float_to_fix(turns_to_radians(value).cos(), precision),
                span,
            )
        } else {
            Expr::nothing(span)
        }
    }

    pub fn fixpoint_tan(
        &self,
        expr: Expr,
        precision: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let precision = self.fixpoint_precision(precision);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let Some((value, _span)) = self.fix_to_float(expr, precision) {
            Expr::number(
                Self::float_to_fix(turns_to_radians(value).tan(), precision),
                span,
            )
        } else {
            Expr::nothing(span)
        }
    }

    pub fn fixpoint_asin(
        &self,
        expr: Expr,
        precision: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let precision = self.fixpoint_precision(precision);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let Some((value, _span)) = self.fix_to_float(expr, precision) {
            Expr::number(
                Self::float_to_fix(radians_to_turns(value.asin()), precision),
                span,
            )
        } else {
            Expr::nothing(span)
        }
    }

    pub fn fixpoint_acos(
        &self,
        expr: Expr,
        precision: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let precision = self.fixpoint_precision(precision);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let Some((value, _span)) = self.fix_to_float(expr, precision) {
            Expr::number(
                Self::float_to_fix(radians_to_turns(value.acos()), precision),
                span,
            )
        } else {
            Expr::nothing(span)
        }
    }

    pub fn fixpoint_atan(
        &self,
        expr: Expr,
        precision: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let precision = self.fixpoint_precision(precision);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let Some((value, _span)) = self.fix_to_float(expr, precision) {
            Expr::number(
                Self::float_to_fix(radians_to_turns(value.atan()), precision),
                span,
            )
        } else {
            Expr::nothing(span)
        }
    }

    pub fn fixpoint_atan2(
        &self,
        lhs: Expr,
        rhs: Expr,
        precision: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let precision = self.fixpoint_precision(precision);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        if let (Some((left, _left_span)), Some((right, _right_span))) = (
            self.fix_to_float(lhs, precision),
            self.fix_to_float(rhs, precision),
        ) {
            Expr::number(
                Self::float_to_fix(radians_to_turns(left.atan2(right)), precision),
                span,
            )
        } else {
            Expr::nothing(span)
        }
    }

    fn fixpoint_precision(&self, precision: Option<Expr>) -> u8 {
        precision
            .and_then(|expr| match self.try_const_eval(&expr) {
                Ok((value, span)) => {
                    clamp_fixpoint_precision(value, &span, self.nb_errors_left, self.options)
                }
                Err(err) => {
                    self.report_expr_error(err);
                    None
                }
            })
            .unwrap_or(self.options.runtime_opts.q_precision)
    }
    fn fix_to_float(&self, expr: Expr, precision: u8) -> Option<(f64, Span)> {
        match self.try_const_eval(&expr) {
            Ok((value, span)) => Some((value as f64 / 2.0f64.powi(precision.into()), span)),
            Err(err) => {
                self.report_expr_error(err);
                None
            }
        }
    }
    fn float_to_fix(value: f64, precision: u8) -> i32 {
        match value.classify() {
            FpCategory::Nan => 0,
            FpCategory::Infinite => {
                if value < 0. {
                    i32::MIN
                } else {
                    i32::MAX
                }
            }
            FpCategory::Zero => 0,
            FpCategory::Subnormal | FpCategory::Normal => {
                (value * 2.0f64.powi(precision.into())).round() as i32
            }
        }
    }
}

const TAU: f64 = std::f64::consts::PI * 2.;
fn turns_to_radians(turns: f64) -> f64 {
    turns * TAU
}
fn radians_to_turns(radians: f64) -> f64 {
    radians / TAU
}

// FIXME: the logic of this function is duplicated for CLI parsing; would be nice to merge the two.
pub fn clamp_fixpoint_precision(
    value: i32,
    span: &Span,
    nb_errors_left: &Cell<usize>,
    options: &Options,
) -> Option<u8> {
    match value {
        1..=31 => Some(value as u8),
        _ => {
            diagnostics::error(
                span,
                |error| {
                    error.set_message("invalid fixed-point precision");
                    error.add_label(diagnostics::error_label(span).with_message(format!(
                        "this must be between 1 and 31 inclusive, not {value}"
                    )));
                },
                nb_errors_left,
                options,
            );
            None
        }
    }
}
