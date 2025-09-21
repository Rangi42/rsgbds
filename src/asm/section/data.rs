use std::cell::Cell;

use compact_str::CompactString;
use either::Either;
use indexmap::IndexMap;
use rustc_hash::FxBuildHasher;

use crate::{
    charmap::Charmap,
    diagnostics::{self, warning},
    expr::Expr,
    instructions::Instruction,
    macro_args::MacroArgs,
    sources::Span,
    symbols::Symbols,
    Identifiers, Options,
};

use super::{
    ActiveSection, Contents, LinkTimeExpr, Patch, PatchKind, Section, SectionKind, Sections,
    UnionEntry,
};

const MAX_SECTION_SIZE: usize = 0x1_000_000;

/// Top-level data emission functions.
impl Sections {
    // Since this is intended to work even in non-data sections, it cannot use `emit_data`.
    pub fn allocate_space(
        &mut self,
        length: usize,
        keyword_span: &Span,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        let Some(active) = self.active_section.as_mut() else {
            diagnostics::error(
                keyword_span,
                |error| {
                    error.set_message("padding emitted outside of a section");
                    error.add_label(
                        diagnostics::error_label(keyword_span)
                            .with_message("no section is active at this point"),
                    );
                    if self
                        .section_stack
                        .iter()
                        .any(|entry| entry.active_section.is_some())
                    {
                        error.set_help("consider popping a section with `pops` before this");
                    } else {
                        error.set_help("consider opening a section with `section` before this");
                    }
                },
                nb_errors_left,
                options,
            );
            return;
        };

        Self::pad_section(
            &mut self.sections,
            &active.data_section,
            length,
            &active.unions,
            options,
        );
        if active.is_load_block_active() {
            Self::pad_section(
                &mut self.sections,
                &active.sym_section,
                length,
                &active.unions,
                options,
            );
        }

        active.advance_by(length);
    }
    fn pad_section(
        sections: &mut IndexMap<CompactString, Section, FxBuildHasher>,
        active_section: &ActiveSection,
        length: usize,
        unions: &[UnionEntry],
        options: &Options,
    ) {
        let data_sect = &mut sections[active_section.id];
        match &mut data_sect.bytes {
            Contents::Data(data) => {
                if active_section
                    .offset
                    .checked_add(length)
                    .is_some_and(|new_ofs| new_ofs <= MAX_SECTION_SIZE)
                {
                    debug_assert_eq!(data.len(), active_section.offset);
                    data.extend(std::iter::repeat(options.runtime_opts.pad_byte).take(length));
                } else {
                    data.resize(MAX_SECTION_SIZE, options.runtime_opts.pad_byte);
                }
            }
            Contents::NoData(len) => {
                if matches!(data_sect.attrs.kind, SectionKind::Union) || !unions.is_empty() {
                    *len = std::cmp::max(*len, active_section.offset + length);
                } else if active_section
                    .offset
                    .checked_add(length)
                    .is_some_and(|new_ofs| new_ofs <= MAX_SECTION_SIZE)
                {
                    debug_assert_eq!(*len, active_section.offset);
                    *len += length;
                } else {
                    *len = MAX_SECTION_SIZE;
                }
            }
        }
    }

    pub fn emit_padding(
        &mut self,
        length: usize,
        bytes: &[Expr],
        keyword_span: &Span,
        identifiers: &Identifiers,
        symbols: &mut Symbols,
        macro_args: Option<&MacroArgs>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        let eval_res: Vec<_> = bytes
            .iter()
            .map(|expr| {
                expr.prep_for_patch(symbols, macro_args, self, |warning, span| {
                    warning.report(span, nb_errors_left, options)
                })
            })
            .collect();

        self.emit_data(
            "padding",
            length,
            keyword_span,
            identifiers,
            nb_errors_left,
            options,
            |offset, mut ctx| {
                for i in 0..length {
                    // It is important that PC's value remains identical throughout the loop's iterations.
                    Self::push_byte(&eval_res[i % eval_res.len()], offset + i, &mut ctx);
                }
            },
        );
    }

    pub fn emit_byte(
        &mut self,
        byte: &Expr,
        keyword_span: &Span,
        identifiers: &Identifiers,
        symbols: &mut Symbols,
        macro_args: Option<&MacroArgs>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        let eval_res = byte.prep_for_patch(symbols, macro_args, self, |warning, span| {
            warning.report(span, nb_errors_left, options)
        });

        self.emit_data(
            "byte",
            1,
            keyword_span,
            identifiers,
            nb_errors_left,
            options,
            |offset, mut ctx| Self::push_byte(&eval_res, offset, &mut ctx),
        );
    }

    pub fn emit_byte_string(
        &mut self,
        string: &str,
        str_span: &Span,
        keyword_span: &Span,
        identifiers: &Identifiers,
        charmap: &Charmap,
        is_main_charmap: bool,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        self.emit_data(
            "byte",
            Self::string_size(string, charmap),
            keyword_span,
            identifiers,
            nb_errors_left,
            options,
            |_offset, mut ctx| {
                Self::push_byte_string(string, charmap, str_span, is_main_charmap, &mut ctx)
            },
        );
    }

    pub fn emit_word(
        &mut self,
        word: &Expr,
        keyword_span: &Span,
        identifiers: &Identifiers,
        symbols: &mut Symbols,
        macro_args: Option<&MacroArgs>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        let eval_res = word.prep_for_patch(symbols, macro_args, self, |warning, span| {
            warning.report(span, nb_errors_left, options)
        });

        self.emit_data(
            "word",
            2,
            keyword_span,
            identifiers,
            nb_errors_left,
            options,
            |offset, mut ctx| Self::push_word(&eval_res, offset, &mut ctx),
        );
    }

    pub fn emit_word_string(
        &mut self,
        string: &str,
        str_span: &Span,
        keyword_span: &Span,
        identifiers: &Identifiers,
        charmap: &Charmap,
        is_main_charmap: bool,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        self.emit_data(
            "word",
            Self::string_size(string, charmap) * 2,
            keyword_span,
            identifiers,
            nb_errors_left,
            options,
            |_offset, mut ctx| {
                Self::push_word_string(string, charmap, str_span, is_main_charmap, &mut ctx)
            },
        );
    }

    pub fn emit_long(
        &mut self,
        long: &Expr,
        keyword_span: &Span,
        identifiers: &Identifiers,
        symbols: &mut Symbols,
        macro_args: Option<&MacroArgs>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        let eval_res = long.prep_for_patch(symbols, macro_args, self, |warning, span| {
            warning.report(span, nb_errors_left, options)
        });

        self.emit_data(
            "long word",
            4,
            keyword_span,
            identifiers,
            nb_errors_left,
            options,
            |offset, mut ctx| Self::push_long(&eval_res, offset, &mut ctx),
        );
    }

    pub fn emit_long_string(
        &mut self,
        string: &str,
        str_span: &Span,
        keyword_span: &Span,
        identifiers: &Identifiers,
        charmap: &Charmap,
        is_main_charmap: bool,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        self.emit_data(
            "long word",
            Self::string_size(string, charmap) * 4,
            keyword_span,
            identifiers,
            nb_errors_left,
            options,
            |_offset, mut ctx| {
                Self::push_long_string(string, str_span, charmap, is_main_charmap, &mut ctx)
            },
        );
    }

    pub fn emit_instruction(
        &mut self,
        instruction: Instruction,
        identifiers: &Identifiers,
        symbols: &mut Symbols,
        macro_args: Option<&MacroArgs>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        let patch_res = instruction.patch.map(|patch| {
            (
                patch
                    .expr
                    .prep_for_patch(symbols, macro_args, self, |warning, span| {
                        warning.report(span, nb_errors_left, options)
                    }),
                patch,
            )
        });

        self.emit_data(
            "instruction",
            instruction.bytes.len(),
            &instruction.span,
            identifiers,
            nb_errors_left,
            options,
            |offset, ctx| {
                ctx.data.extend_from_slice(&instruction.bytes);

                if let Some((eval_res, patch)) = patch_res {
                    let offset = offset + usize::from(patch.offset);
                    match eval_res {
                        Ok(Either::Left((value, span))) => match patch.kind {
                            PatchKind::Byte => {
                                ctx.check_8_bit(
                                    value,
                                    &span,
                                    "instruction operand",
                                    "expression evaluates to",
                                    Some("you can use the `low()` function to truncate an expression"),
                                );
                                ctx.data[offset] = value as u8;
                            }
                            PatchKind::Word => {
                                ctx.check_16_bit(
                                    value,
                                    &span,
                                    "instruction operand",
                                    "expression evaluates to",
                                    None,
                                );
                                ctx.data[offset..].copy_from_slice(&(value as i16).to_le_bytes());
                            }
                            PatchKind::Long => {
                                ctx.data[offset..].copy_from_slice(&value.to_le_bytes());
                            }
                            // To write the byte, we'd have to know PC's value as well, and we can't reborrow the sections right now.
                            // Thus, defer this to later.
                            PatchKind::Jr => ctx.patches.push(Patch {
                                kind: PatchKind::Jr,
                                rest: LinkTimeExpr {
                                    span: instruction.span.clone(),
                                    expr: Expr::number(value, span),
                                    offset,
                                    pc: Some((ctx.symbol_section.id, ctx.symbol_section.offset)),
                                },
                            }),
                        },

                        Ok(Either::Right(expr)) => ctx.patches.push(Patch {
                            kind: patch.kind,
                            rest: LinkTimeExpr {
                                span: instruction.span.clone(),
                                expr,
                                offset,
                                pc: Some((ctx.symbol_section.id, ctx.symbol_section.offset)),
                            },
                        }),

                        Err(error) => error.report(identifiers, nb_errors_left, options),
                    }
                }
            },
        );
    }

    pub fn emit_byte_slice(
        &mut self,
        slice: &[u8],
        keyword_span: &Span,
        identifiers: &Identifiers,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        self.emit_data(
            "binary file",
            slice.len(),
            keyword_span,
            identifiers,
            nb_errors_left,
            options,
            |_offset, ctx| ctx.data.extend_from_slice(slice),
        );
    }

    pub fn check_could_emit_slice(
        &mut self,
        keyword_span: &Span,
        identifiers: &Identifiers,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        self.emit_data(
            "binary file",
            0,
            keyword_span,
            identifiers,
            nb_errors_left,
            options,
            |_offset, _ctx| {},
        );
    }
}

struct EmissionContext<'patches, 'data, 'sym_sect, 'idents, 'nerrs, 'options> {
    patches: &'patches mut Vec<Patch>,
    data: &'data mut Vec<u8>,
    symbol_section: &'sym_sect mut ActiveSection,
    identifiers: &'idents Identifiers,
    nb_errors_left: &'nerrs Cell<usize>,
    options: &'options Options,
}

/// Common core to all of the data emission functions.
impl Sections {
    fn emit_data<F: FnOnce(usize, EmissionContext)>(
        &mut self,
        name: &str,
        nb_bytes: usize,
        keyword_span: &Span,
        identifiers: &Identifiers,
        nb_errors_left: &Cell<usize>,
        options: &Options,
        emission_callback: F,
    ) {
        let Some(active) = self.active_section.as_mut() else {
            diagnostics::error(
                keyword_span,
                |error| {
                    error.set_message(format!("{name} emitted outside of a section"));
                    error.add_label(
                        diagnostics::error_label(keyword_span)
                            .with_message("no section is active at this point"),
                    );
                    if self
                        .section_stack
                        .iter()
                        .any(|entry| entry.active_section.is_some())
                    {
                        error.set_help("consider popping a section with `pops` before this");
                    } else {
                        error.set_help("consider opening a section with `section` before this");
                    }
                },
                nb_errors_left,
                options,
            );
            return;
        };

        let data_sect = &mut self.sections[active.data_section.id];
        match &mut data_sect.bytes {
            Contents::Data(data) => {
                if active
                    .data_section
                    .offset
                    .checked_add(nb_bytes)
                    .is_some_and(|new_ofs| new_ofs <= MAX_SECTION_SIZE)
                {
                    debug_assert_eq!(data.len(), active.data_section.offset);

                    emission_callback(
                        active.data_section.offset,
                        EmissionContext {
                            patches: &mut data_sect.patches,
                            data,
                            symbol_section: &mut active.sym_section,
                            identifiers,
                            nb_errors_left,
                            options,
                        },
                    );
                } else {
                    data.resize(MAX_SECTION_SIZE, options.runtime_opts.pad_byte);
                }
            }

            Contents::NoData(len) => {
                if active
                    .data_section
                    .offset
                    .checked_add(nb_bytes)
                    .is_some_and(|new_ofs| new_ofs <= MAX_SECTION_SIZE)
                {
                    debug_assert_eq!(*len, active.data_section.offset);

                    diagnostics::error(
                        keyword_span,
                        |error| {
                            error.set_message(format!("{name} emitted outside of ROM"));
                            error.add_label(diagnostics::error_label(keyword_span).with_message(
                                format!(
                                    "a {} section is active here",
                                    data_sect.attrs.mem_region.name(),
                                ),
                            ));
                            error.set_help("you can store the data in ROM while leaving labels pointing to RAM, using `load`");
                        },
                        nb_errors_left,
                        options,
                    );
                    *len += nb_bytes;
                } else {
                    *len = MAX_SECTION_SIZE;
                }
            }
        }
        if active.is_load_block_active() {
            Self::pad_section(
                &mut self.sections,
                &active.sym_section,
                nb_bytes,
                &active.unions,
                options,
            );
        }

        active.advance_by(nb_bytes);
    }
}

/// Internal, per-kind data emission functions.
impl Sections {
    fn push_byte(
        res: &Result<Either<(i32, Span), Expr>, crate::expr::Error>,
        offset: usize,
        ctx: &mut EmissionContext,
    ) {
        match res {
            Ok(Either::Left((value, span))) => {
                ctx.check_8_bit(
                    *value,
                    span,
                    "expression's value",
                    "expression evaluates to",
                    Some("you can use the `low()` function to truncate an expression"),
                );

                ctx.data.push(*value as u8);
            }

            Ok(Either::Right(expr)) => {
                ctx.patches.push(Patch {
                    kind: PatchKind::Byte,
                    rest: LinkTimeExpr {
                        span: expr.overall_span(),
                        expr: expr.clone(),
                        offset,
                        pc: Some((ctx.symbol_section.id, ctx.symbol_section.offset)),
                    },
                });

                ctx.data.push(Default::default());
            }

            Err(error) => {
                error.report(ctx.identifiers, ctx.nb_errors_left, ctx.options);

                ctx.data.push(Default::default());
            }
        }
    }

    fn push_word(
        res: &Result<Either<(i32, Span), Expr>, crate::expr::Error>,
        offset: usize,
        ctx: &mut EmissionContext,
    ) {
        match res {
            Ok(Either::Left((value, span))) => {
                ctx.check_16_bit(
                    *value,
                    span,
                    "expression's value",
                    "expression evaluates to",
                    None,
                );

                ctx.data.extend_from_slice(&(*value as i16).to_le_bytes());
            }

            Ok(Either::Right(expr)) => {
                ctx.patches.push(Patch {
                    kind: PatchKind::Word,
                    rest: LinkTimeExpr {
                        span: expr.overall_span(),
                        expr: expr.clone(),
                        offset,
                        pc: Some((ctx.symbol_section.id, ctx.symbol_section.offset)),
                    },
                });

                ctx.data
                    .extend_from_slice(&[Default::default(), Default::default()]);
            }

            Err(error) => {
                error.report(ctx.identifiers, ctx.nb_errors_left, ctx.options);

                ctx.data
                    .extend_from_slice(&[Default::default(), Default::default()]);
            }
        }
    }

    fn push_long(
        res: &Result<Either<(i32, Span), Expr>, crate::expr::Error>,
        offset: usize,
        ctx: &mut EmissionContext,
    ) {
        match res {
            Ok(Either::Left((value, _span))) => {
                // No truncation checks here, since we are working with 32-bit values already.
                ctx.data.extend_from_slice(&value.to_le_bytes());
            }

            Ok(Either::Right(expr)) => {
                ctx.patches.push(Patch {
                    kind: PatchKind::Long,
                    rest: LinkTimeExpr {
                        span: expr.overall_span(),
                        expr: expr.clone(),
                        offset,
                        pc: Some((ctx.symbol_section.id, ctx.symbol_section.offset)),
                    },
                });

                ctx.data.extend_from_slice(&[
                    Default::default(),
                    Default::default(),
                    Default::default(),
                    Default::default(),
                ]);
            }

            Err(error) => {
                error.report(ctx.identifiers, ctx.nb_errors_left, ctx.options);

                ctx.data.extend_from_slice(&[
                    Default::default(),
                    Default::default(),
                    Default::default(),
                    Default::default(),
                ]);
            }
        }
    }

    fn push_byte_string(
        string: &str,
        charmap: &Charmap,
        str_span: &Span,
        is_main_charmap: bool,
        ctx: &mut EmissionContext,
    ) {
        let mut warned = false;
        for mapping in charmap.encode(
            (string, str_span),
            is_main_charmap,
            ctx.identifiers,
            ctx.nb_errors_left,
            ctx.options,
        ) {
            for value in mapping {
                if !warned {
                    warned = ctx.check_8_bit(
                        value,
                        str_span,
                        "string's encoding",
                        "string's encoding contains",
                        Some("consider emitting the string using `dw` or `dl`"),
                    );
                }

                ctx.data.push(value as u8);
            }
        }
    }

    fn push_word_string(
        string: &str,
        charmap: &Charmap,
        str_span: &Span,
        is_main_charmap: bool,
        ctx: &mut EmissionContext,
    ) {
        let mut warned = false;
        for mapping in charmap.encode(
            (string, str_span),
            is_main_charmap,
            ctx.identifiers,
            ctx.nb_errors_left,
            ctx.options,
        ) {
            for value in mapping {
                if !warned {
                    warned = ctx.check_16_bit(
                        value,
                        str_span,
                        "string's encoding",
                        "string's encoding contains",
                        Some("consider emitting the string using `dl`"),
                    );
                }

                ctx.data.extend_from_slice(&(value as i16).to_le_bytes());
            }
        }
    }

    fn push_long_string(
        string: &str,
        str_span: &Span,
        charmap: &Charmap,
        is_main_charmap: bool,
        ctx: &mut EmissionContext,
    ) {
        for mapping in charmap.encode(
            (string, str_span),
            is_main_charmap,
            ctx.identifiers,
            ctx.nb_errors_left,
            ctx.options,
        ) {
            for value in mapping {
                // No truncation checks here, since we are working with 32-bit values already.
                ctx.data.extend_from_slice(&value.to_le_bytes());
            }
        }
    }

    fn string_size(mut string: &str, charmap: &Charmap) -> usize {
        // Not using `encode` because we don't want to report passthrough characters.
        let mut size = 0;
        while let Some((mapping, byte_len)) = charmap.encode_one(string) {
            size += mapping.into_iter().len();
            string = &string[byte_len..];
        }
        size
    }
}

impl EmissionContext<'_, '_, '_, '_, '_, '_> {
    fn check_8_bit(
        &self,
        value: i32,
        span: &Span,
        what: &'static str,
        eval_msg: &'static str,
        help_text: Option<&'static str>,
    ) -> bool {
        if !(-256..=255).contains(&value) {
            diagnostics::warn(
                warning!("truncation=1"),
                span,
                |warning| {
                    warning.set_message(format!("this {what} is not 8-bit"));
                    warning.add_label(diagnostics::warning_label(span).with_message(format!(
                        "this {eval_msg} {value}, which is not between -256 and 255",
                    )));
                    if let Some(help) = help_text {
                        warning.set_help(help);
                    }
                },
                self.nb_errors_left,
                self.options,
            );
            true
        } else if value < -128 {
            diagnostics::warn(
                warning!("truncation=2"),
                span,
                |warning| {
                    warning.set_message(format!("this {what} doesn't seem to be 8-bit"));
                    warning.add_label(diagnostics::warning_label(span).with_message(format!(
                        "this {eval_msg} {value}, which is not between -128 and 255",
                    )));
                    if let Some(help) = help_text {
                        warning.set_help(help);
                    }
                },
                self.nb_errors_left,
                self.options,
            );
            true
        } else {
            false
        }
    }

    fn check_16_bit(
        &self,
        value: i32,
        span: &Span,
        what: &'static str,
        eval_msg: &'static str,
        help_text: Option<&'static str>,
    ) -> bool {
        if !(-65536..=65535).contains(&value) {
            diagnostics::warn(
                warning!("truncation=1"),
                span,
                |warning| {
                    warning.set_message(format!("this {what} is not 16-bit"));
                    warning.add_label(diagnostics::warning_label(span).with_message(format!(
                        "this {eval_msg} {value}, which is not between -65536 and 65535",
                    )));
                    if let Some(help) = help_text {
                        warning.set_help(help);
                    }
                },
                self.nb_errors_left,
                self.options,
            );
            true
        } else if value < -32767 {
            diagnostics::warn(
                warning!("truncation=2"),
                span,
                |warning| {
                    warning.set_message(format!("this {what} doesn't seem to be 16-bit"));
                    warning.add_label(diagnostics::warning_label(span).with_message(format!(
                        "this {eval_msg} {value}, which is not between -32767 and 65535",
                    )));
                    if let Some(help) = help_text {
                        warning.set_help(help);
                    }
                },
                self.nb_errors_left,
                self.options,
            );
            true
        } else {
            false
        }
    }
}
