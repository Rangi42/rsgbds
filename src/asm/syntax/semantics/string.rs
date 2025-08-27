use std::{cell::Cell, cmp::Ordering};

use compact_str::CompactString;

use crate::{
    charmap::CharMapping,
    diagnostics::{self, warning},
    expr::Expr,
    sources::Span,
    Options,
};

use super::parse_ctx;

/// String functions, which return numeric values.
impl parse_ctx!() {
    pub fn strcmp(
        &self,
        (lhs, _left_span): (CompactString, Span),
        (rhs, _right_span): (CompactString, Span),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let value = match lhs.cmp(&rhs) {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        };
        let span = self.span_from_to(l_span_idx, r_span_idx);
        Expr::number(value, span)
    }

    pub fn strfind(
        &self,
        (lhs, _left_span): (CompactString, Span),
        (rhs, _right_span): (CompactString, Span),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let value = lhs.find(rhs.as_str()).map_or(-1, |pos| pos as i32);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        Expr::number(value, span)
    }

    pub fn strrfind(
        &self,
        (lhs, _left_span): (CompactString, Span),
        (rhs, _right_span): (CompactString, Span),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let value = lhs.rfind(rhs.as_str()).map_or(-1, |pos| pos as i32);
        let span = self.span_from_to(l_span_idx, r_span_idx);
        Expr::number(value, span)
    }

    pub fn strlen(
        &self,
        (expr, _span): (CompactString, Span),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let value = expr.chars().count() as i32;
        let span = self.span_from_to(l_span_idx, r_span_idx);
        Expr::number(value, span)
    }

    pub fn bytelen(
        &self,
        (expr, _span): (CompactString, Span),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let value = expr.len();
        let span = self.span_from_to(l_span_idx, r_span_idx);
        Expr::number(value as i32, span)
    }

    pub fn charlen(
        &self,
        (expr, span): (CompactString, Span),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let charmap = self.charmaps.active_charmap();
        let encoded = charmap.encode(
            (&expr, &span),
            self.charmaps.is_main_charmap_active(),
            self.identifiers,
            self.nb_errors_left,
            self.options,
        );

        let value = encoded.count();
        let span = self.span_from_to(l_span_idx, r_span_idx);
        Expr::number(value as i32, span)
    }

    pub fn incharmap(
        &self,
        (expr, _span): (CompactString, Span),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let charmap = self.charmaps.active_charmap();
        let value = if matches!(charmap.encode_one(&expr), Some((CharMapping::Mapped(_), byte_len)) if byte_len == expr.len())
        {
            1
        } else {
            0
        };

        let span = self.span_from_to(l_span_idx, r_span_idx);
        Expr::number(value, span)
    }

    pub fn charcmp(
        &self,
        (lhs, left_span): (CompactString, Span),
        (rhs, right_span): (CompactString, Span),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let charmap = self.charmaps.active_charmap();
        let mut left = charmap
            .encode(
                (&lhs, &left_span),
                self.charmaps.is_main_charmap_active(),
                self.identifiers,
                self.nb_errors_left,
                self.options,
            )
            .flatten();
        let mut right = charmap
            .encode(
                (&rhs, &right_span),
                self.charmaps.is_main_charmap_active(),
                self.identifiers,
                self.nb_errors_left,
                self.options,
            )
            .flatten();
        let value = loop {
            match (left.next(), right.next()) {
                (None, None) => break 0,
                (None, Some(_)) => break 1,
                (Some(_), None) => break -1,
                (Some(left_value), Some(right_value)) => match left_value.cmp(&right_value) {
                    Ordering::Less => break -1,
                    Ordering::Greater => break 1,
                    Ordering::Equal => {} // Keep checking.
                },
            }
        };

        let span = self.span_from_to(l_span_idx, r_span_idx);
        Expr::number(value, span)
    }

    pub fn charsize(
        &self,
        (expr, _span): (CompactString, Span),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let span = self.span_from_to(l_span_idx, r_span_idx);

        let charmap = self.charmaps.active_charmap();
        match charmap.encode_one(&expr) {
            None => {
                self.error(&span, |error| {
                    error.set_message("empty string doesn't have a charmap size");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("this string is empty"),
                    );
                });
                Expr::nothing(span)
            }
            Some((mapping, byte_len)) => {
                if byte_len != expr.len() {
                    self.error(&span, |error| {
                        error.set_message("string passed to `charsize` is more than one mapping");
                        error.add_label(diagnostics::error_label(&span).with_message(format!(
                            "the longest initial mapping is \"{}\"",
                            &expr[..byte_len],
                        )));
                    });
                }
                let value = match mapping {
                    CharMapping::Mapped(slice) => slice.len(),
                    CharMapping::Passthrough(c) => {
                        // TODO: maybe such a case can be intentional
                        self.error(&span, |error| {
                            error.set_message("string passed to `charsize` isn't mapped");
                            error.add_label(diagnostics::error_label(&span).with_message(format!(
                                "\"{expr}\" isn't in charmap `{}`",
                                self.identifiers.resolve(charmap.name()).unwrap(),
                            )));
                        });
                        c.len_utf8()
                    }
                };
                Expr::number(value as i32, span)
            }
        }
    }

    pub fn charval_single(
        &self,
        (string, _span): (CompactString, Span),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let span = self.span_from_to(l_span_idx, r_span_idx);

        let charmap = self.charmaps.active_charmap();
        match charmap.encode_one(&string) {
            None => {
                self.error(&span, |error| {
                    error.set_message("empty string doesn't have any character values");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("this string is empty"),
                    );
                });
                Expr::nothing(span)
            }
            Some((mapping, byte_len)) => {
                if byte_len != string.len() {
                    self.error(&span, |error| {
                        error.set_message("string passed to `charval` is more than one mapping");
                        error.add_label(diagnostics::error_label(&span).with_message(format!(
                            "the longest initial mapping is \"{}\"",
                            &string[..byte_len],
                        )));
                    });
                }
                fn get_only_value<T: Copy + Into<i32>>(
                    slice: &[T],
                    span: Span,
                    nb_errors_left: &Cell<usize>,
                    options: &Options,
                ) -> Expr {
                    if let [value] = slice {
                        Expr::number((*value).into(), span)
                    } else {
                        diagnostics::error(
                            &span,
                            |error| {
                                error.set_message(
                                    "string passed to `charval` maps to more than one unit",
                                );
                                error.add_label(diagnostics::error_label(&span).with_message(
                                    format!("this string maps to {} units", slice.len()),
                                ))
                            },
                            nb_errors_left,
                            options,
                        );
                        Expr::nothing(span)
                    }
                }
                match mapping {
                    CharMapping::Mapped(slice) => {
                        get_only_value(slice, span, self.nb_errors_left, self.options)
                    }
                    CharMapping::Passthrough(c) => {
                        // TODO: maybe such a case can be intentional
                        self.error(&span, |error| {
                            error.set_message("string passed to `charval` isn't mapped");
                            error.add_label(diagnostics::error_label(&span).with_message(format!(
                                "\"{string}\" isn't in charmap `{}`",
                                self.identifiers.resolve(charmap.name()).unwrap(),
                            )));
                        });
                        let mut buf = [0; 4];
                        get_only_value(
                            c.encode_utf8(&mut buf).as_bytes(),
                            span,
                            self.nb_errors_left,
                            self.options,
                        )
                    }
                }
            }
        }
    }

    pub fn charval(
        &self,
        (string, _span): (CompactString, Span),
        expr: Expr,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let span = self.span_from_to(l_span_idx, r_span_idx);

        let charmap = self.charmaps.active_charmap();
        match charmap.encode_one(&string) {
            None => {
                self.error(&span, |error| {
                    error.set_message("empty string doesn't have any character values");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("this string is empty"),
                    );
                });
                Expr::nothing(span)
            }
            Some((mapping, _byte_len)) => {
                match self.logical_index_to_physical(
                    expr,
                    charmap
                        .encode(
                            (&string, &span),
                            self.charmaps.is_main_charmap_active(),
                            self.identifiers,
                            self.nb_errors_left,
                            self.options,
                        )
                        .count(),
                ) {
                    None => Expr::nothing(span),
                    Some((index, _logical_idx)) => {
                        fn get_nth_value<T: Copy + Into<i32>>(
                            slice: &[T],
                            index: usize,
                            span: Span,
                            nb_errors_left: &Cell<usize>,
                            options: &Options,
                        ) -> Expr {
                            if let Some(value) = slice.get(index) {
                                Expr::number((*value).into(), span)
                            } else {
                                diagnostics::error(
                                    &span,
                                    |error| {
                                        error.set_message("character mapping index out of bounds");
                                        error.add_label(
                                            diagnostics::error_label(&span).with_message(format!(
                                                "attempted to take the {} value, out of {}",
                                                ordinal::Ordinal(index), // TODO: would it make more sense to report `logical_idx` instead?
                                                slice.len(),
                                            )),
                                        )
                                    },
                                    nb_errors_left,
                                    options,
                                );
                                Expr::nothing(span)
                            }
                        }
                        match mapping {
                            CharMapping::Mapped(slice) => {
                                get_nth_value(slice, index, span, self.nb_errors_left, self.options)
                            }
                            CharMapping::Passthrough(c) => {
                                // TODO: maybe such a case can be intentional
                                self.error(&span, |error| {
                                    error.set_message("string passed to `charval` isn't mapped");
                                    error.add_label(diagnostics::error_label(&span).with_message(
                                        format!(
                                            "\"{string}\" isn't in charmap `{}`",
                                            self.identifiers.resolve(charmap.name()).unwrap(),
                                        ),
                                    ));
                                });
                                let mut buf = [0; 4];
                                get_nth_value(
                                    c.encode_utf8(&mut buf).as_bytes(),
                                    index,
                                    span,
                                    self.nb_errors_left,
                                    self.options,
                                )
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn strbyte(
        &self,
        (string, _span): (CompactString, Span),
        expr: Expr,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> Expr {
        let span = self.span_from_to(l_span_idx, r_span_idx);
        match self.logical_index_to_physical(expr, string.len()) {
            None => Expr::nothing(span),
            Some((index, logical_idx)) => {
                let value = match string.as_bytes().get(index) {
                    Some(byte) => *byte as i32,
                    None => {
                        self.warn(warning!("builtin-args"), &span, |warning| {
                            warning.set_message(
                                "specified index is outside of the bounds of the string",
                            );
                            warning.add_label(diagnostics::warning_label(&span).with_message(
                                format!(
                                    "{logical_idx} is not between 0 and {} exclusive",
                                    string.len(),
                                ),
                            ));
                        });
                        0
                    }
                };
                Expr::number(value, span)
            }
        }
    }

    fn logical_index_to_physical(&self, logical: Expr, length: usize) -> Option<(usize, i32)> {
        match self.try_const_eval(&logical) {
            Ok((value, _span)) => Some((
                if value >= 0 {
                    value as usize
                } else {
                    length.wrapping_add_signed(value as isize)
                },
                value,
            )),
            Err(err) => {
                self.report_expr_error(err);
                None
            }
        }
    }
}
