use std::{fmt::Display, fs::File, io::Read};

use compact_str::CompactString;
use either::Either;

use crate::{
    common::S,
    diagnostics::{self, ReportBuilder},
    expr::Expr,
    sources::Span,
};

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
                        self.charmaps.is_main_charmap_active(),
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
                        self.charmaps.is_main_charmap_active(),
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
                    Either::Right((string, span)) => self.sections.emit_long_string(
                        &string,
                        &span,
                        keyword_span,
                        self.identifiers,
                        self.charmaps.active_charmap(),
                        self.charmaps.is_main_charmap_active(),
                        self.nb_errors_left,
                        self.options,
                    ),
                }
            }
        }
    }

    #[allow(clippy::read_zero_byte_vec)] // False positive on Rust 1.73.
    pub fn incbin_file(
        &mut self,
        (path, path_span): (CompactString, Span),
        start_expr: Option<Expr>,
        length_expr: Option<Expr>,
        span_idx: usize,
    ) {
        let length = length_expr.and_then(|expr| match self.try_const_eval(&expr) {
            Ok((value, span)) => {
                if value < 0 {
                    self.error(&span, |error| {
                        error.set_message("negative length given to `incbin`");
                        error.add_label(
                            diagnostics::error_label(&span)
                                .with_message(format!("this evaluates to {value}")),
                        );
                    });
                    None
                } else {
                    Some(value as usize)
                }
            }
            Err(err) => {
                self.report_expr_error(err);
                None
            }
        });
        if length == Some(0) {
            let span = &self.line_spans[span_idx];
            self.sections.check_could_emit_slice(
                span,
                self.identifiers,
                self.nb_errors_left,
                self.options,
            );
            return; // Don't even try to open the file if we would be including none of it.
        }

        let report_io_err =
            |err: std::io::Error, span: &Span, eof_msg: &str, eof_label: EofLabel| {
                self.error(span, |error| {
                    use std::io::ErrorKind;
                    if err.kind() == ErrorKind::UnexpectedEof {
                        error.set_message(eof_msg);
                        error.add_label(diagnostics::error_label(span).with_message(eof_label))
                    } else {
                        error.set_message(format!("failed to read \"{path}\""));
                        error.add_label(diagnostics::error_label(span).with_message(err));
                    }
                })
            };
        enum EofLabel {
            None,
            Start(usize),
            Length(Option<i32>, usize),
        }
        impl Display for EofLabel {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    EofLabel::None => unreachable!(),
                    EofLabel::Start(start) => {
                        write!(f, "cannot skip the first {start} byte{}", S::from(*start))
                    }
                    EofLabel::Length(None, length) => {
                        write!(f, "cannot read {length} byte{}", S::from(*length))
                    }
                    EofLabel::Length(Some(start), length) => {
                        write!(
                            f,
                            "cannot read {length} byte{} (after skipping {start})",
                            S::from(*length)
                        )
                    }
                }
            }
        }

        let mut file = match File::open(&path) {
            Ok(file) => file,
            Err(err) => {
                report_io_err(err, &path_span, "", EofLabel::None);
                return;
            }
        };

        let mut data = vec![];
        let start = start_expr.and_then(|expr| match self.try_const_eval(&expr) {
            Ok((value, span)) => {
                if value < 0 {
                    self.error(&span, |error| {
                        error.set_message("negative start offset given to `incbin`");
                        error.add_label(
                            diagnostics::error_label(&span)
                                .with_message(format!("this evaluates to {value}")),
                        );
                    });
                    Some(0)
                } else if value != 0 {
                    data.resize(value as usize, 0);
                    if let Err(err) = file.read_exact(&mut data) {
                        report_io_err(
                            err,
                            &span,
                            "specified start offset is greater than length of `incbin` file",
                            EofLabel::Start(value as usize),
                        );
                    }
                    Some(value)
                } else {
                    Some(0)
                }
            }
            Err(err) => {
                self.report_expr_error(err);
                None
            }
        });

        let res = match length {
            Some(length) => {
                data.resize(length, 0);
                file.read_exact(&mut data)
            }
            None => {
                data.clear();
                file.read_to_end(&mut data).map(|_| ())
            }
        };
        if let Err(err) = res {
            report_io_err(
                err,
                &path_span,
                "specified length is greater than end of `incbin` file",
                EofLabel::Length(start, length.unwrap_or_default()),
            );
        }

        let span = &self.line_spans[span_idx];
        self.sections.emit_byte_slice(
            &data,
            span,
            self.identifiers,
            self.nb_errors_left,
            self.options,
        );
    }
}
