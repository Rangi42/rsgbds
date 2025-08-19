use std::cell::Cell;

use either::Either;

use crate::{
    diagnostics::{self, warning},
    expr::Expr,
    instructions::Instruction,
    macro_args::MacroArgs,
    section::{Patch, PatchKind},
    sources::Span,
    symbols::Symbols,
    Identifiers, Options,
};

use super::{ActiveSection, Contents, LinkTimeExpr, SectionKind, Sections};

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
        let Some((data_section, symbol_section)) = self.active_section.as_mut() else {
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

        let data_sect = &mut self.sections[data_section.id];
        match &mut data_sect.bytes {
            Contents::Data(data) => {
                if data_section
                    .offset
                    .checked_add(length)
                    .is_some_and(|new_ofs| new_ofs <= MAX_SECTION_SIZE)
                {
                    debug_assert_eq!(data.len(), data_section.offset);
                    data.extend(std::iter::repeat(options.runtime_opts.pad_byte).take(length));
                } else {
                    data.resize(MAX_SECTION_SIZE, options.runtime_opts.pad_byte);
                }
            }
            Contents::NoData(len) => {
                if !matches!(data_sect.attrs.kind, SectionKind::Union) {
                    if data_section
                        .offset
                        .checked_add(length)
                        .is_some_and(|new_ofs| new_ofs <= MAX_SECTION_SIZE)
                    {
                        debug_assert_eq!(*len, data_section.offset);
                        *len += length;
                    } else {
                        *len = MAX_SECTION_SIZE;
                    }
                } else {
                    *len = std::cmp::max(*len, data_section.offset + length);
                }
            }
        }

        data_section.offset += length;
        symbol_section.offset += length;
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
            .map(|expr| expr.prep_for_patch(symbols, macro_args, self))
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
        let eval_res = byte.prep_for_patch(symbols, macro_args, self);

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
        let eval_res = word.prep_for_patch(symbols, macro_args, self);

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
        let eval_res = long.prep_for_patch(symbols, macro_args, self);

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

    pub fn emit_instruction(
        &mut self,
        instruction: Instruction,
        identifiers: &Identifiers,
        symbols: &mut Symbols,
        macro_args: Option<&MacroArgs>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        let patch_res = instruction
            .patch
            .map(|patch| (patch.expr.prep_for_patch(symbols, macro_args, self), patch));

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
                        Ok(Either::Left((value, _span))) => match patch.kind {
                            PatchKind::Byte => ctx.data[offset] = value as u8,
                            PatchKind::Word => {
                                ctx.data[offset..].copy_from_slice(&(value as i16).to_le_bytes())
                            }
                            PatchKind::Long => {
                                ctx.data[offset..].copy_from_slice(&value.to_le_bytes())
                            }
                            PatchKind::Jr => todo!(),
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
        let Some((data_section, symbol_section)) = self.active_section.as_mut() else {
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

        let data_sect = &mut self.sections[data_section.id];
        match &mut data_sect.bytes {
            Contents::Data(data) => {
                if data_section
                    .offset
                    .checked_add(nb_bytes)
                    .is_some_and(|new_ofs| new_ofs <= MAX_SECTION_SIZE)
                {
                    debug_assert_eq!(data.len(), data_section.offset);

                    emission_callback(
                        data_section.offset,
                        EmissionContext {
                            patches: &mut data_sect.patches,
                            data,
                            symbol_section,
                            identifiers,
                            nb_errors_left,
                            options,
                        },
                    );
                } else {
                    match &mut data_sect.bytes {
                        Contents::Data(data) => {
                            data.resize(MAX_SECTION_SIZE, options.runtime_opts.pad_byte)
                        }
                        Contents::NoData(len) => *len = MAX_SECTION_SIZE,
                    }
                }
            }

            Contents::NoData(len) => {
                if data_section
                    .offset
                    .checked_add(nb_bytes)
                    .is_some_and(|new_ofs| new_ofs <= MAX_SECTION_SIZE)
                {
                    debug_assert_eq!(*len, data_section.offset);

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
                    match &mut data_sect.bytes {
                        Contents::Data(data) => {
                            data.resize(MAX_SECTION_SIZE, options.runtime_opts.pad_byte)
                        }
                        Contents::NoData(len) => *len = MAX_SECTION_SIZE,
                    }
                }
            }
        }

        data_section.offset += nb_bytes;
        symbol_section.offset += nb_bytes;
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
                if *value < -256 || *value > 255 {
                    diagnostics::warn(
                        warning!("truncation=1"),
                        span,
                        |warning| {
                            warning.set_message("this expression's value is not 8-bit");
                            warning.add_label(diagnostics::warning_label(span).with_message(format!("this expression evaluates to {value}, which is not between -256 and 255")));
                            warning.set_help(
                                "you can use the `low()` function to truncate an expression",
                            );
                        },
                        ctx.nb_errors_left,
                        ctx.options,
                    );
                } else if *value < -128 {
                    diagnostics::warn(
                        warning!("truncation=2"),
                        span,
                        |warning| {
                            warning.set_message("this expression's value doesn't seem to be 8-bit");
                            warning.add_label(diagnostics::warning_label(span).with_message(format!("this expression evaluates to {value}, which is not between -128 and 255")));
                            warning.set_help(
                                "you can use the `low()` function to truncate an expression",
                            );
                        },
                        ctx.nb_errors_left,
                        ctx.options,
                    );
                }

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
                if *value < -65536 || *value > 65535 {
                    diagnostics::warn(
                        warning!("truncation=1"),
                        span,
                        |warning| {
                            warning.set_message("this expression's value is not 16-bit");
                            warning.add_label(diagnostics::warning_label(span).with_message(format!("this expression evaluates to {value}, which is not between -65536 and 65535")));
                        },
                        ctx.nb_errors_left,
                        ctx.options,
                    );
                } else if *value < -32767 {
                    diagnostics::warn(
                        warning!("truncation=2"),
                        span,
                        |warning| {
                            warning
                                .set_message("this expression's value doesn't seem to be 16-bit");
                            warning.add_label(diagnostics::warning_label(span).with_message(format!("this expression evaluates to {value}, which is not between -32767 and 65535")));
                        },
                        ctx.nb_errors_left,
                        ctx.options,
                    );
                }

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
}
