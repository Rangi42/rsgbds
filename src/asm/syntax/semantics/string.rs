use std::{cell::Cell, cmp::Ordering};

use compact_str::{CompactString, ToCompactString};
use either::Either;

use crate::{
    charmap::CharMapping,
    common::S,
    diagnostics::{self, warning},
    expr::Expr,
    format::FormatSpec,
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
}

/// String functions which return strings.
impl parse_ctx!() {
    pub fn strslice(
        &self,
        (string, _span): (CompactString, Span),
        start: Expr,
        end: Option<Expr>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> (CompactString, Span) {
        let span = self.span_from_to(l_span_idx, r_span_idx);

        let string_len = string.chars().count();
        let (start_idx, logical_start) = self
            .logical_index_to_physical(start, string_len)
            .unwrap_or((0, 0));
        let (end_idx, logical_end) = end
            .and_then(|expr| self.logical_index_to_physical(expr, string_len))
            .unwrap_or((string_len, string_len as i32));
        let nb_codepoints = end_idx.checked_sub(end_idx).unwrap_or_else(|| {
            self.warn(warning!("builtin-args"), &span, |warning| {
                warning.set_message("string slice range is backwards");
                warning.add_label(diagnostics::warning_label(&span).with_message(format!(
                    "skipping the first {start_idx} codepoints, stopping at {end_idx} codepoints",
                )))
            });
            0
        });

        if start_idx > string_len {
            self.warn(warning!("builtin-args"), &span, |warning| {
                warning.set_message("start index is past the end of the string");
                warning.add_label(diagnostics::warning_label(&span).with_message(
                    "starting at {start_idx} codepoints, but the string only contains {string_len}",
                ));
            });
        }
        if end_idx > string_len {
            self.warn(warning!("builtin-args"), &span, |warning| {
                warning.set_message("stop index is past the end of the string");
                warning.add_label(diagnostics::warning_label(&span).with_message(
                    "stopping at {end_idx} codepoints, but the string only contains {string_len}",
                ));
            });
        }

        (
            string.chars().skip(start_idx).take(nb_codepoints).collect(),
            span,
        )
    }

    pub fn strchar(
        &self,
        (string, span): (CompactString, Span),
        idx: Expr,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> (CompactString, Span) {
        let span = self.span_from_to(l_span_idx, r_span_idx);

        let charmap = self.charmaps.active_charmap();
        let nb_charmap_units = charmap
            .encode(
                (&string, &span),
                self.charmaps.is_main_charmap_active(),
                self.identifiers,
                self.nb_errors_left,
                self.options,
            )
            .count();
        let string = self
            .logical_index_to_physical(idx, nb_charmap_units)
            .and_then(|(start_idx, logical_idx)| {
                let mut ofs = 0;
                let mut start_ofs = 0;
                for i in 0..=start_idx {
                    let Some((mapping, nb_bytes)) = charmap.encode_one(&string[ofs..]) else {
                        self.warn(warning!("builtin-args"), &span, |error| {
                            error.set_message("index passed to `strchar` is larger than the string");
                            error.add_label(diagnostics::warning_label(&span).with_message(format!(
                                "attempting to get the {} charmap unit of a string that has only {i}",
                                ordinal::Ordinal(start_idx),
                            )));
                        });
                        return None;
                    };
                    start_ofs = ofs;
                    ofs += nb_bytes;
                }
                Some((&string[start_ofs..ofs]).to_compact_string())
            })
            .unwrap_or_default();
        (string, span)
    }

    pub fn revchar(
        &self,
        mapping: Vec<i32>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> (CompactString, Span) {
        let span = self.span_from_to(l_span_idx, r_span_idx);

        let charmap = self.charmaps.active_charmap();
        let string = match charmap.find_mapping(&mapping) {
            None => {
                self.error(&span, |error| {
                    error.set_message("reverse string mapping failed");
                    error.add_label(diagnostics::error_label(&span).with_message(format!(
                        "no mapping found in charmap `{}` for these values",
                        self.identifiers.resolve(charmap.name()).unwrap(),
                    )));
                });
                Default::default()
            }
            Some((string, other_string)) => {
                if let Some(other) = other_string {
                    self.error(&span, |error| {
                        error.set_message("reverse string mapping found more than one result");
                        error.add_label(diagnostics::error_label(&span).with_message(format!(
                            "\"{string}\" matches in charmap `{}`, but so does \"{other}\"",
                            self.identifiers.resolve(charmap.name()).unwrap(),
                        )));
                    });
                }
                string
            }
        };

        (string, span)
    }

    // strcat is implemented directly in the parser, since it's trivial string concat.

    pub fn strupr(
        &self,
        (mut string, _span): (CompactString, Span),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> (CompactString, Span) {
        string.as_mut_str().make_ascii_uppercase();
        let span = self.span_from_to(l_span_idx, r_span_idx);
        (string, span)
    }

    pub fn strlwr(
        &self,
        (mut string, _span): (CompactString, Span),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> (CompactString, Span) {
        string.as_mut_str().make_ascii_lowercase();
        let span = self.span_from_to(l_span_idx, r_span_idx);
        (string, span)
    }

    pub fn strrpl(
        &self,
        (string, _span): (CompactString, Span),
        (needle, needle_span): (CompactString, Span),
        (replacement, _replacement_span): (CompactString, Span),
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> (CompactString, Span) {
        let span = self.span_from_to(l_span_idx, r_span_idx);

        if needle.is_empty() {
            self.warn(warning!("empty-strrpl"), &needle_span, |warning| {
                warning.set_message("cannot search for an empty string");
                warning.add_label(
                    diagnostics::warning_label(&needle_span).with_message("this string is empty"),
                );
            });
            return (string, span);
        }

        let mut input = string.as_str();
        let mut output = CompactString::with_capacity(string.len());
        while let Some(offset) = input.find(needle.as_str()) {
            output.push_str(&input[..offset]);
            output.push_str(&replacement);
            input = &input[offset + needle.len()..];
        }
        output.push_str(input);

        (output, span)
    }

    pub fn strfmt(
        &self,
        (fmt, fmt_span): (CompactString, Span),
        args_vec: Vec<Either<Expr, (CompactString, Span)>>,
        l_span_idx: usize,
        r_span_idx: usize,
    ) -> (CompactString, Span) {
        let mut args = args_vec.iter();
        let mut input = fmt.as_str();
        let mut output = CompactString::default();
        let mut nb_missing_args = 0;
        let mut bad_format_string = false;
        while let Some(ofs) = input.find('%') {
            output.push_str(&input[..ofs]);
            // SAFETY: `input` is derived from `fmt`.
            let idx = unsafe { input.as_ptr().offset_from(fmt.as_ptr()) } as usize + ofs;
            let after_percent = ofs + '%'.len_utf8();
            if input[after_percent..].starts_with('%') {
                output.push('%');
                input = &input[after_percent + '%'.len_utf8()..];
            } else {
                let res = match FormatSpec::parse(
                    &input[after_percent..],
                    self.options.runtime_opts.q_precision,
                ) {
                    Err(err) => {
                        input = &input[after_percent..]; // Process normally the entire (would-be) format spec, since we don't know how long it should be.
                        output.push('%');
                        bad_format_string = true;
                        Err(err)
                    }
                    Ok((spec, rest)) => {
                        input = rest;
                        match args.next() {
                            Some(Either::Left(expr)) => {
                                match self.try_const_eval(expr) {
                                    Err(err) => {
                                        self.report_expr_error(err); // ...and don't add anything to the string.
                                        Ok(()) // No *format* error.
                                    }
                                    Ok((value, _span)) => spec.write_number(
                                        value as u32,
                                        &mut output,
                                        "numeric expression",
                                    ),
                                }
                            }
                            Some(Either::Right((string, _span))) => {
                                spec.write_str(string, &mut output, "string expression")
                            }
                            None => {
                                nb_missing_args += 1;
                                Ok(())
                            }
                        }
                    }
                };
                if let Err(err) = res {
                    self.error(&fmt_span, |error| {
                        error.set_message(&err);
                        error.add_label(diagnostics::error_label(&fmt_span).with_message(format!(
                            "bad format specifier {idx} byte{} into this format string",
                            S::from(idx),
                        )));
                    });
                }
            }
        }
        output.push_str(input);

        // If the format string was bad, we don't know for sure how many arguments were expected, so don't try to check.
        if !bad_format_string {
            if let remaining @ 1.. = args.as_slice().len() {
                self.error(&fmt_span, |error| {
                    error.set_message("not all arguments were used by the format string");
                    error.add_label(diagnostics::error_label(&fmt_span).with_message(format!(
                        "{remaining} argument{} remain unformatted",
                        S::from(remaining)
                    )));
                });
            } else if nb_missing_args != 0 {
                self.error(&fmt_span, |error| {
                    error.set_message("not enough arguments for the format string");
                    error.add_label(diagnostics::error_label(&fmt_span).with_message(format!(
                        "{} argument{} were supplied, {nb_missing_args} more {} missing",
                        args_vec.len(),
                        S::from(args_vec.len()),
                        if nb_missing_args == 1 { "is" } else { "are" },
                    )))
                });
            }
        }

        (output, self.span_from_to(l_span_idx, r_span_idx))
    }
}

/// Utility function.
impl parse_ctx!() {
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
