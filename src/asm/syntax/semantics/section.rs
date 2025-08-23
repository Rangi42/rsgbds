use compact_str::CompactString;

use crate::{
    common::section::MemRegion,
    diagnostics,
    expr::Expr,
    section::{AddrConstraint, SectionAttrs, SectionKind},
    sources::Span,
    warning,
};

use super::parse_ctx;

impl parse_ctx!() {
    pub fn mandatory_section_attrs(
        &self,
        kind: Option<SectionKind>,
        mem_region: MemRegion,
        addr: Option<Expr>,
    ) -> SectionAttrs {
        SectionAttrs {
            kind: kind.unwrap_or(SectionKind::Normal),
            mem_region,
            address: match addr {
                Some(expr) => match self.try_const_eval(&expr) {
                    Ok((addr, span)) => {
                        if (mem_region.min_addr().into()..=mem_region.max_addr().into())
                            .contains(&addr)
                        {
                            AddrConstraint::Addr(addr as u16)
                        } else {
                            self.error(&span, |error| {
                                error.set_message("invalid address constraint for section");
                                error.add_label(diagnostics::error_label(&span).with_message(
                                    format!(
                                    "{} sections can be between ${:04x} and ${:04x}, not ${:04x}",
                                    mem_region.name(),
                                    mem_region.min_addr(),
                                    mem_region.max_addr(),
                                    addr,
                                ),
                                ));
                            });
                            AddrConstraint::Addr(mem_region.min_addr())
                        }
                    }
                    Err(err) => {
                        self.report_expr_error(err);
                        AddrConstraint::Addr(mem_region.min_addr())
                    }
                },
                None => AddrConstraint::None,
            },
            bank: None,
        }
    }
    pub fn set_bank_of(
        &self,
        (mut attrs, name): (SectionAttrs, (CompactString, Span)),
        bank: Expr,
    ) -> (SectionAttrs, (CompactString, Span)) {
        match self.try_const_eval(&bank) {
            Ok((value, span)) => {
                let value = value as u32;
                if (attrs.mem_region.min_bank()..=attrs.mem_region.max_bank()).contains(&value) {
                    match attrs.bank {
                        None => attrs.bank = Some(value),
                        Some(bank) => {
                            if bank != value {
                                self.error(&span, |error| {
                                    error.set_message("conflicting bank constraints for section");
                                    error.add_label(
                                        diagnostics::error_label(&span).with_message(format!(
                                            "specified again as {value} here"
                                        )),
                                    );
                                    error.set_note(format!("previously specified as {bank}"));
                                });
                            }
                        }
                    }
                } else {
                    self.error(&span, |error| {
                        error.set_message("invalid bank constraint for section");
                        error.add_label(diagnostics::error_label(&span).with_message(format!(
                            "{} sections can be between {} and {}, not {}",
                            attrs.mem_region.name(),
                            attrs.mem_region.min_bank(),
                            attrs.mem_region.max_bank(),
                            value,
                        )));
                    });
                }
            }
            Err(err) => self.report_expr_error(err),
        }

        (attrs, name)
    }
    pub fn set_align_of(
        &self,
        (mut attrs, name): (SectionAttrs, (CompactString, Span)),
        (alignment, offset): (u8, u16),
        span_idx: usize,
    ) -> (SectionAttrs, (CompactString, Span)) {
        if let Err(err) = attrs
            .address
            .merge(AddrConstraint::Align(alignment, offset), 0)
        {
            let span = &self.line_spans[span_idx];
            self.error(span, |error| {
                let (msg, label_msg) = err.details();
                error.set_message(msg);
                error.add_label(diagnostics::error_label(span).with_message(label_msg));
            });
        }

        (attrs, name)
    }

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

    pub fn align_section(&mut self, (alignment, offset): (u8, u16), span_idx: usize) {
        let span = &self.line_spans[span_idx];

        if let Some((_data_section, symbol_section)) = self.sections.active_section.as_mut() {
            if let Err(err) = self.sections.sections[symbol_section.id]
                .attrs
                .address
                .merge((alignment, offset).into(), symbol_section.offset)
            {
                self.error(span, |error| {
                    let (msg, label) = err.details();
                    error.set_message(msg);
                    error.add_label(diagnostics::error_label(span).with_message(label));
                });
            }
        } else {
            self.error(span, |error| {
                error.set_message("`align` used outside of a section");
                error.add_label(
                    diagnostics::error_label(span).with_message("this directive is invalid"),
                );
            });
        }
    }

    pub fn end_load_block(&mut self, span_idx: usize) {
        let span = &self.line_spans[span_idx];

        if let Some((data_section, symbol_section)) = self.sections.active_section.as_mut() {
            if !data_section.points_to_same_as(symbol_section) {
                // End the `LOAD` block.
                *symbol_section = data_section.clone();
            } else {
                self.error(span, |error| {
                    error.set_message("`endl` used outside of a `load` block");
                    error.add_label(
                        diagnostics::error_label(span).with_message("this directive is invalid"),
                    );
                });
            }
        } else {
            self.error(span, |error| {
                error.set_message("`endl` used outside of a section");
                error.add_label(
                    diagnostics::error_label(span).with_message("this directive is invalid"),
                );
            });
        }
    }

    pub fn end_section(&mut self, span_idx: usize) {
        let span = &self.line_spans[span_idx];

        self.sections
            .end_section(span, self.nb_errors_left, self.options);
        self.symbols.end_scope();
    }

    pub fn set_symbol_section(
        &mut self,
        (attrs, (name, def_span)): (SectionAttrs, (CompactString, Span)),
    ) {
        let active_section = self.sections.create_if_not_exists(
            name,
            attrs,
            def_span,
            self.nb_errors_left,
            self.options,
        );
        self.sections.active_section = Some((active_section.clone(), active_section));
    }

    pub fn push_section(
        &mut self,
        opt: Option<(SectionAttrs, (CompactString, Span))>,
        span_idx: usize,
    ) {
        let pushs_span = self.nth_span(span_idx);
        self.sections.push_active_section(pushs_span, self.symbols);

        if let Some(stuff) = opt {
            self.create_section(stuff);
        }
    }

    pub fn pop_section(&mut self, span_idx: usize) {
        let span = &self.line_spans[span_idx];

        if self.sections.pop_active_section(self.symbols).is_none() {
            self.error(span, |error| {
                error.set_message("no entries in the section stack");
                error.add_label(diagnostics::error_label(span).with_message("cannot pop"));
            })
        }
    }

    pub fn create_section(&mut self, (attrs, (name, span)): (SectionAttrs, (CompactString, Span))) {
        let active_section = self.sections.create_if_not_exists(
            name,
            attrs,
            span,
            self.nb_errors_left,
            self.options,
        );
        self.sections.active_section = Some((active_section.clone(), active_section));
        self.symbols.end_scope();
    }
}
