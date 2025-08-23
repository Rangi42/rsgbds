use compact_str::CompactString;
use either::Either;

use crate::{diagnostics, expr::Expr, sources::Span};

use super::parse_ctx;

impl parse_ctx!() {
    pub fn allocate_space(
        &mut self,
        len_or_align: Either<Either<Expr, usize>, (u8, u16)>,
        span_idx: usize,
    ) {
        let length = self.ds_length(len_or_align);
        let span = &self.line_spans[span_idx];
        self.sections
            .allocate_space(length, span, self.nb_errors_left, self.options);
    }

    pub fn emit_padding(
        &mut self,
        len_or_align: Either<Either<Expr, usize>, (u8, u16)>,
        bytes: Vec<Expr>,
        span_idx: usize,
    ) {
        let length = self.ds_length(len_or_align);
        let span = &self.line_spans[span_idx];
        self.sections.emit_padding(
            length,
            &bytes,
            span,
            self.identifiers,
            self.symbols,
            self.macro_args.last(),
            self.nb_errors_left,
            self.options,
        );
    }

    /// Also updates the active section's alignment if an alignment is specified.
    fn ds_length(&mut self, len_or_align: Either<Either<Expr, usize>, (u8, u16)>) -> usize {
        match len_or_align {
            Either::Left(Either::Left(expr)) => match self.try_const_eval(&expr) {
                Ok((value, span)) => {
                    if value < 0 {
                        self.error(&span, |error| {
                            error.set_message("negative sizes are not allowed for `ds`");
                            error.add_label(diagnostics::error_label(&span).with_message("cannot emit {value} bytes of padding"));
                            if self.sections
                                .active_section
                                .as_ref()
                                .is_some_and(|(_data_sect, sym_sect)| {
                                    !self.sections.sections[sym_sect.id].attrs.mem_region.has_data()
                                })
                            {
                                error.set_help("if you want multiple blocks of data to share the same space, consider using `union`");
                            }
                        });
                        0
                    } else {
                        value as usize
                    }
                }
                Err(error) => {
                    self.report_expr_error(error);
                    0
                }
            },

            Either::Left(Either::Right(length)) => length,

            Either::Right((alignment, offset)) => {
                if let Some((_data_section, sym_section)) = self.sections.active_section.as_ref() {
                    let section = &mut self.sections.sections[sym_section.id];
                    let length =
                        section.bytes_until_alignment(alignment, offset, sym_section.offset);
                    section
                        .attrs
                        .address
                        .merge((alignment, offset).into(), sym_section.offset + length)
                        .unwrap();
                    length
                } else {
                    0 // The data emission stage will produce an error anyway.
                }
            }
        }
    }

    pub fn emit_bytes(&mut self, bytes: Vec<Either<Expr, (CompactString, Span)>>, span_idx: usize) {
        if bytes.is_empty() {
            self.allocate_space(Either::Left(Either::Right(1)), span_idx);
        } else {
            let keyword_span = &self.line_spans[span_idx];

            for elem in bytes {
                match elem {
                    Either::Left(expr) => self.sections.emit_byte(
                        &expr,
                        keyword_span,
                        self.identifiers,
                        self.symbols,
                        self.macro_args.last(),
                        self.nb_errors_left,
                        self.options,
                    ),
                    Either::Right((string, span)) => self.sections.emit_byte_string(
                        &string,
                        &span,
                        keyword_span,
                        self.identifiers,
                        self.charmaps.active_charmap(),
                        self.nb_errors_left,
                        self.options,
                    ),
                }
            }
        }
    }

    pub fn emit_words(&mut self, words: Vec<Either<Expr, (CompactString, Span)>>, span_idx: usize) {
        if words.is_empty() {
            self.allocate_space(Either::Left(Either::Right(1)), span_idx);
        } else {
            let keyword_span = &self.line_spans[span_idx];

            for elem in words {
                match elem {
                    Either::Left(expr) => self.sections.emit_word(
                        &expr,
                        keyword_span,
                        self.identifiers,
                        self.symbols,
                        self.macro_args.last(),
                        self.nb_errors_left,
                        self.options,
                    ),
                    Either::Right((string, span)) => self.sections.emit_word_string(
                        &string,
                        &span,
                        keyword_span,
                        self.identifiers,
                        self.charmaps.active_charmap(),
                        self.nb_errors_left,
                        self.options,
                    ),
                }
            }
        }
    }

    pub fn emit_longs(&mut self, longs: Vec<Either<Expr, (CompactString, Span)>>, span_idx: usize) {
        if longs.is_empty() {
            self.allocate_space(Either::Left(Either::Right(1)), span_idx);
        } else {
            let keyword_span = &self.line_spans[span_idx];

            for elem in longs {
                match elem {
                    Either::Left(expr) => self.sections.emit_long(
                        &expr,
                        keyword_span,
                        self.identifiers,
                        self.symbols,
                        self.macro_args.last(),
                        self.nb_errors_left,
                        self.options,
                    ),
                    Either::Right((string, _span)) => self.sections.emit_long_string(
                        &string,
                        keyword_span,
                        self.identifiers,
                        self.charmaps.active_charmap(),
                        self.nb_errors_left,
                        self.options,
                    ),
                }
            }
        }
    }
}
