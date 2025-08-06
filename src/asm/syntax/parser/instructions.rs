use crate::{
    diagnostics,
    expr::Expr,
    instructions::{Condition, Instruction, Reg16, Reg8},
    sources::Span,
    syntax::{parser::expr, tokens::Token},
};

use super::{expect_one_of, matches_tok, misc, parse_ctx};

macro_rules! operand {
    ($kind:ident::$value:ident) => {
        Operand {
            kind: OperandKind::$kind($kind::$value),
            indirect: false,
            ..
        }
    };
    ([$kind:ident::$value:ident]) => {
        Operand {
            kind: OperandKind::$kind($kind::$value),
            indirect: true,
            ..
        }
    };
}

// Load. There's a lot of variants.

pub(super) fn parse_ld(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (operands, lookahead) =
        misc::parse_comma_list(parse_operand, parse_ctx.next_token(), parse_ctx);

    let res = match operands.as_slice() {
        // ld <reg8>, <reg8>
        [lhs, rhs] if lhs.is_reg8() && rhs.is_reg8() => Some(Instruction::ld_r8_r8(
            lhs.to_reg8(),
            rhs.to_reg8(),
            instr_token.span.merged_with(&rhs.span),
        )),

        // ld <reg8>, <imm>
        [lhs, Operand {
            kind: OperandKind::Expr(_),
            indirect: false,
            span,
        }] if lhs.is_reg8() => {
            let dest = lhs.to_reg8();
            let span = instr_token.span.merged_with(span);
            let Some(Operand {
                kind: OperandKind::Expr(src),
                ..
            }) = operands.into_iter().nth(1)
            else {
                unreachable!()
            };
            Some(Instruction::ld_r8_imm(dest, src, span))
        }

        // ld <reg16>, <imm>
        [Operand {
            kind: OperandKind::Reg16(reg),
            indirect: false,
            ..
        }, Operand {
            kind: OperandKind::Expr(_),
            indirect: false,
            span,
        }] => {
            let dest = *reg;
            let span = instr_token.span.merged_with(span);
            let Some(Operand {
                kind: OperandKind::Expr(src),
                ..
            }) = operands.into_iter().nth(1)
            else {
                unreachable!()
            };

            let instr = Instruction::ld_r16_imm(dest, src, span);
            if let Err(span) = &instr {
                parse_ctx.error(span, |error| {
                    error.set_message("invalid left-hand side operand to `ld`");
                    error.add_label(
                        diagnostics::error_label(span)
                            .with_message("expected `bc`, `de`, `hl`, or `sp`"),
                    );
                });
            }
            instr.ok()
        }

        // `ld a, [<reg16>]`
        [operand!(Reg8::A), Operand {
            kind: OperandKind::Reg16(r16),
            indirect: true,
            span,
        }] => {
            let instr = Instruction::ld_a_r16_ind(false, *r16, instr_token.span.merged_with(span));
            if let Err(span) = &instr {
                parse_ctx.error(span, |error| {
                    error.set_message("invalid right-hand side operand to `ld`");
                    error.add_label(
                        diagnostics::error_label(span)
                            .with_message("expected `bc`, `de`, `hld`, or `hli`"),
                    );
                });
            }
            instr.ok()
        }
        // `ld [<reg16>], a`
        [Operand {
            kind: OperandKind::Reg16(r16),
            indirect: true,
            span,
        }, operand!(Reg8::A)] => {
            let instr = Instruction::ld_a_r16_ind(true, *r16, instr_token.span.merged_with(span));
            if let Err(span) = &instr {
                parse_ctx.error(span, |error| {
                    error.set_message("invalid left-hand side operand to `ld`");
                    error.add_label(
                        diagnostics::error_label(span)
                            .with_message("expected `bc`, `de`, `hld`, or `hli`"),
                    );
                });
            }
            instr.ok()
        }

        // ld a, [<imm>]
        [operand!(Reg8::A), Operand {
            kind: OperandKind::Expr(_),
            indirect: true,
            span,
        }] => {
            let span = instr_token.span.merged_with(span);
            let Some(Operand {
                kind: OperandKind::Expr(addr),
                ..
            }) = operands.into_iter().nth(1)
            else {
                unreachable!()
            };

            Some(Instruction::ld_a_addr(false, addr, span))
        }
        // ld [<imm>], a
        [Operand {
            kind: OperandKind::Expr(_),
            indirect: true,
            span,
        }, operand!(Reg8::A)] => {
            let span = instr_token.span.merged_with(span);
            let Some(Operand {
                kind: OperandKind::Expr(addr),
                ..
            }) = operands.into_iter().next()
            else {
                unreachable!()
            };

            Some(Instruction::ld_a_addr(true, addr, span))
        }

        // `ld sp, hl`
        [operand!(Reg16::Sp), rhs @ operand!(Reg16::Hl)] => Some(Instruction::ld_sp_hl(
            instr_token.span.merged_with(&rhs.span),
        )),

        // `ld hl, sp +/- <ofs>`
        [operand!(Reg16::Hl), Operand {
            kind: OperandKind::SpRel(_),
            indirect: false,
            span,
        }] => {
            let span = instr_token.span.merged_with(span);
            let Some(Operand {
                kind: OperandKind::SpRel(offset),
                ..
            }) = operands.into_iter().nth(1)
            else {
                unreachable!()
            };
            Some(Instruction::ld_hl_sp_ofs(offset, span))
        }

        // ld [<imm>], sp
        [Operand {
            kind: OperandKind::Expr(_),
            indirect: true,
            span,
        }, operand!(Reg16::Sp)] => {
            let span = instr_token.span.merged_with(span);
            let Some(Operand {
                kind: OperandKind::Expr(addr),
                ..
            }) = operands.into_iter().next()
            else {
                unreachable!()
            };

            Some(Instruction::ld_addr_sp(addr, span))
        }

        // TODO: the other variants of `ld`.
        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span.clone(),
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message("invalid operands to `ld`");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("these operands are invalid"),
                    );
                });
            }
            None
        }
    };
    (res, lookahead)
}

pub(super) fn parse_ldh(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (operands, lookahead) =
        misc::parse_comma_list(parse_operand, parse_ctx.next_token(), parse_ctx);

    let res = match operands.as_slice() {
        [lhs @ operand!(Reg8::A), Operand {
            kind: OperandKind::Reg8(Reg8::C),
            indirect: true,
            span,
        }] => Some(Instruction::ldh_c(false, lhs.span.merged_with(span))),
        [Operand {
            kind: OperandKind::Reg8(Reg8::C),
            indirect: true,
            span,
        }, rhs @ operand!(Reg8::A)] => Some(Instruction::ldh_c(true, span.merged_with(&rhs.span))),

        [lhs @ operand!(Reg8::A), Operand {
            kind: OperandKind::Expr(_),
            indirect: true,
            span,
        }] => {
            let span = lhs.span.merged_with(span);
            let Some(Operand {
                kind: OperandKind::Expr(addr),
                ..
            }) = operands.into_iter().nth(1)
            else {
                unreachable!()
            };
            Some(Instruction::ldh(false, addr, instr_token.span, span))
        }
        [Operand {
            kind: OperandKind::Expr(_),
            indirect: true,
            span,
        }, rhs @ operand!(Reg8::A)] => {
            let span = span.merged_with(&rhs.span);
            let Some(Operand {
                kind: OperandKind::Expr(addr),
                ..
            }) = operands.into_iter().next()
            else {
                unreachable!()
            };
            Some(Instruction::ldh(true, addr, instr_token.span, span))
        }

        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span.clone(),
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message("invalid operands to `ldh`");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("expected an address and `a`"),
                    );
                    error.set_help("did you mean to use `ld`?");
                });
            }
            None
        }
    };
    (res, lookahead)
}

// Arithmetic and logic instructions.

pub(super) fn parse_adc(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (rhs, lookahead) = parse_arith_instr_args(&instr_token, parse_ctx.next_token(), parse_ctx);

    let instruction = match rhs.kind {
        OperandKind::Reg8(reg) => Some(Instruction::adc(reg, rhs.span)),
        OperandKind::Expr(expr) => Some(Instruction::adc_imm(expr, rhs.span)),
        OperandKind::Err | OperandKind::Reg16(_) | OperandKind::SpRel(_) => None,
    };
    (instruction, lookahead)
}

pub(super) fn parse_add(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    expect_one_of! { parse_ctx.next_token() => {
        |"hl"| => {
            let lookahead = expect_one_of! { parse_ctx.next_token() => {
                |","| => parse_ctx.next_token(),
                else |unexpected| => {
                    parse_ctx.report_syntax_error(&unexpected, |error, span| {
                        error.add_label(diagnostics::error_label(span).with_message("expected a comma here"));
                    });
                    unexpected
                }
            }};

            let (res, lookahead) = parse_operand(lookahead, parse_ctx);
            let instruction = match res {
                None => {
                    parse_ctx.error(&lookahead.span, |error| {
                        error.set_message("missing right-hand side operand to `add hl`");
                        error.add_label(diagnostics::error_label(&lookahead.span).with_message("expected a 16-bit register after the comma"));
                    });
                    None
                },
                Some(Operand { span, kind: OperandKind::Reg16(reg), indirect: false }) => {
                    let instr = Instruction::add_r16(reg, instr_token.span.merged_with(&span));
                    if let Err(span) = &instr {
                        parse_ctx.error(span, |error| {
                            error.set_message("invalid right-hand side operand to `add hl`");
                            error.add_label(diagnostics::error_label(span).with_message("expected one of `bc`, `de`, `hl`, or `sp`"));
                        });
                    }
                    instr.ok()
                },
                Some(operand) => {
                    if is_valid(std::array::from_ref(&operand)) {
                        parse_ctx.error(&operand.span, |error| {
                            error.set_message("invalid right-hand side operand to `add hl`");
                            error.add_label(diagnostics::error_label(&operand.span).with_message("expected a 16-bit register"));
                        });
                    }
                    None
                }
            };
            (instruction, lookahead)
        },

        |"sp"| => {
            let lookahead = expect_one_of! { parse_ctx.next_token() => {
                |","| => parse_ctx.next_token(),
                else |unexpected| => {
                    parse_ctx.report_syntax_error(&unexpected, |error, span| {
                        error.add_label(diagnostics::error_label(span).with_message("expected a comma here"));
                    });
                    unexpected
                }
            }};

            let (expr, lookahead) = expr::parse_numeric_expr(lookahead, parse_ctx);
            let expr = expr.map(|expr| {
                let span = expr.overall_span();
                Instruction::add_sp(expr, span)
            });
            (expr, lookahead)
        },

        else |unexpected| => {
            let (rhs, lookahead) = parse_arith_instr_args(&instr_token, unexpected, parse_ctx);

            let instruction = match rhs.kind {
                OperandKind::Reg8(reg) => Some(Instruction::add(reg, rhs.span)),
                OperandKind::Expr(expr) => Some(Instruction::add_imm(expr, rhs.span)),
                OperandKind::Err | OperandKind::Reg16(_) | OperandKind::SpRel(_) => None,
            };
            (instruction, lookahead)
        }
    }}
}

pub(super) fn parse_and(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (rhs, lookahead) = parse_arith_instr_args(&instr_token, parse_ctx.next_token(), parse_ctx);

    let instruction = match rhs.kind {
        OperandKind::Reg8(reg) => Some(Instruction::and(reg, rhs.span)),
        OperandKind::Expr(expr) => Some(Instruction::and_imm(expr, rhs.span)),
        OperandKind::Err | OperandKind::Reg16(_) | OperandKind::SpRel(_) => None,
    };
    (instruction, lookahead)
}

pub(super) fn parse_cp(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (rhs, lookahead) = parse_arith_instr_args(&instr_token, parse_ctx.next_token(), parse_ctx);

    let instruction = match rhs.kind {
        OperandKind::Reg8(reg) => Some(Instruction::cp(reg, rhs.span)),
        OperandKind::Expr(expr) => Some(Instruction::cp_imm(expr, rhs.span)),
        OperandKind::Err | OperandKind::Reg16(_) | OperandKind::SpRel(_) => None,
    };
    (instruction, lookahead)
}

pub(super) fn parse_or(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (rhs, lookahead) = parse_arith_instr_args(&instr_token, parse_ctx.next_token(), parse_ctx);

    let instruction = match rhs.kind {
        OperandKind::Reg8(reg) => Some(Instruction::or(reg, rhs.span)),
        OperandKind::Expr(expr) => Some(Instruction::or_imm(expr, rhs.span)),
        OperandKind::Err | OperandKind::Reg16(_) | OperandKind::SpRel(_) => None,
    };
    (instruction, lookahead)
}

pub(super) fn parse_sbc(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (rhs, lookahead) = parse_arith_instr_args(&instr_token, parse_ctx.next_token(), parse_ctx);

    let instruction = match rhs.kind {
        OperandKind::Reg8(reg) => Some(Instruction::sbc(reg, rhs.span)),
        OperandKind::Expr(expr) => Some(Instruction::sbc_imm(expr, rhs.span)),
        OperandKind::Err | OperandKind::Reg16(_) | OperandKind::SpRel(_) => None,
    };
    (instruction, lookahead)
}

pub(super) fn parse_sub(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (rhs, lookahead) = parse_arith_instr_args(&instr_token, parse_ctx.next_token(), parse_ctx);

    let instruction = match rhs.kind {
        OperandKind::Reg8(reg) => Some(Instruction::sub(reg, rhs.span)),
        OperandKind::Expr(expr) => Some(Instruction::sub_imm(expr, rhs.span)),
        OperandKind::Err | OperandKind::Reg16(_) | OperandKind::SpRel(_) => None,
    };
    (instruction, lookahead)
}

pub(super) fn parse_xor(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (rhs, lookahead) = parse_arith_instr_args(&instr_token, parse_ctx.next_token(), parse_ctx);

    let instruction = match rhs.kind {
        OperandKind::Reg8(reg) => Some(Instruction::xor(reg, rhs.span)),
        OperandKind::Expr(expr) => Some(Instruction::xor_imm(expr, rhs.span)),
        OperandKind::Err | OperandKind::Reg16(_) | OperandKind::SpRel(_) => None,
    };
    (instruction, lookahead)
}

fn parse_arith_instr_args(
    instr_token: &Token,
    lookahead: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Operand, Token) {
    let (operands, lookahead) = misc::parse_comma_list(parse_operand, lookahead, parse_ctx);

    let (kind, span) = match operands.as_slice() {
        // `a, <rhs>` is equivalent to `<rhs>`.
        [operand!(Reg8::A), rhs @ Operand { kind, .. }] | [rhs @ Operand { kind, .. }]
            if rhs.is_reg8() || matches!(kind, OperandKind::Expr(_)) =>
        {
            let kind = match kind {
                OperandKind::Expr(expr) => OperandKind::Expr(expr.clone()),
                OperandKind::Reg8(r8) => OperandKind::Reg8(*r8),
                OperandKind::Reg16(Reg16::Hl) => OperandKind::Reg8(Reg8::HlInd),
                _ => unreachable!(),
            };

            (
                kind,
                instr_token.span.merged_with(&operands.last().unwrap().span),
            )
        }

        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span.clone(),
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message(format!("invalid operands to {}", instr_token.payload));
                    error.add_label(diagnostics::error_label(&span).with_message(
                        "expected an 8-bit register or an expression, optionally preceded by `a`",
                    ));
                    if matches!(
                        operands.as_slice(),
                        [operand!(Reg8::A), operand!(Reg16::Hl)] | [operand!(Reg16::Hl)]
                    ) {
                        error.set_help("surround `hl` with brackets to dereference it: `[hl]`");
                    }
                });
            }
            (OperandKind::Err, span)
        }
    };

    (
        Operand {
            span,
            kind,
            indirect: false,
        },
        lookahead,
    )
}

// Increment and decrement.

pub(super) fn parse_dec(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (res, lookahead) = parse_operand(parse_ctx.next_token(), parse_ctx);

    let instruction = match res {
        Some(operand) if operand.is_reg8() => Some(Instruction::dec_r8(
            operand.to_reg8(),
            instr_token.span.merged_with(&operand.span),
        )),
        Some(Operand {
            span,
            kind: OperandKind::Reg16(reg),
            indirect: false,
        }) => {
            let instruction = Instruction::dec_r16(reg, span);
            if let Err(span) = &instruction {
                parse_ctx.error(span, |error| {
                    error.set_message("invalid operand to `dec`");
                    error.add_label(
                        diagnostics::error_label(&instr_token.span)
                            .with_message("expected one of `bc`, `de`, `hl`, or `sp`"),
                    );
                });
            }
            instruction.ok()
        }
        Some(operand) => {
            if is_valid(std::array::from_ref(&operand)) {
                parse_ctx.error(&operand.span, |error| {
                    error.set_message("invalid operand to `dec`");
                    error.add_label(
                        diagnostics::error_label(&operand.span)
                            .with_message("expected an 8-bit or a 16-bit register"),
                    );
                });
            }
            None
        }
        None => {
            parse_ctx.error(&instr_token.span, |error| {
                error.set_message("missing operand to `dec`");
                error.add_label(
                    diagnostics::error_label(&instr_token.span)
                        .with_message("expected an 8-bit or 16-bit register"),
                )
            });
            None
        }
    };

    (instruction, lookahead)
}

pub(super) fn parse_inc(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (res, lookahead) = parse_operand(parse_ctx.next_token(), parse_ctx);

    let instruction = match res {
        Some(operand) if operand.is_reg8() => Some(Instruction::inc_r8(
            operand.to_reg8(),
            instr_token.span.merged_with(&operand.span),
        )),
        Some(Operand {
            span,
            kind: OperandKind::Reg16(reg),
            indirect: false,
        }) => {
            let instruction = Instruction::inc_r16(reg, span);
            if let Err(span) = &instruction {
                parse_ctx.error(span, |error| {
                    error.set_message("invalid operand to `inc`");
                    error.add_label(
                        diagnostics::error_label(&instr_token.span)
                            .with_message("expected one of `bc`, `de`, `hl`, or `sp`"),
                    );
                });
            }
            instruction.ok()
        }
        Some(operand) => {
            if is_valid(std::array::from_ref(&operand)) {
                parse_ctx.error(&operand.span, |error| {
                    error.set_message("invalid operand to `inc`");
                    error.add_label(
                        diagnostics::error_label(&operand.span)
                            .with_message("expected an 8-bit or 16-bit register"),
                    );
                });
            }
            None
        }
        None => {
            parse_ctx.error(&instr_token.span, |error| {
                error.set_message("missing operand to `inc`");
                error.add_label(
                    diagnostics::error_label(&instr_token.span)
                        .with_message("expected an 8-bit or 16-bit register"),
                );
            });
            None
        }
    };

    (instruction, lookahead)
}

// Bit flag instructions.

pub(super) fn parse_bit(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (operands, lookahead) =
        misc::parse_comma_list(parse_operand, parse_ctx.next_token(), parse_ctx);

    let res = match operands.as_slice() {
        [Operand {
            kind: OperandKind::Expr(_),
            indirect: false,
            ..
        }, rhs]
            if rhs.is_reg8() =>
        {
            let reg = rhs.to_reg8();
            let span = instr_token.span.merged_with(&rhs.span);
            let Some(Operand {
                kind: OperandKind::Expr(bit),
                ..
            }) = operands.into_iter().next()
            else {
                unreachable!()
            };
            Some(Instruction::bit(reg, bit, span, instr_token.span))
        }
        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span.clone(),
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message("invalid operands to `bit`");
                    error.add_label(
                        diagnostics::error_label(&span)
                            .with_message("expected the bit number then a 8-bit register"),
                    );
                });
            }
            None
        }
    };
    (res, lookahead)
}

pub(super) fn parse_res(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (operands, lookahead) =
        misc::parse_comma_list(parse_operand, parse_ctx.next_token(), parse_ctx);

    let res = match operands.as_slice() {
        [Operand {
            kind: OperandKind::Expr(_),
            indirect: false,
            ..
        }, rhs]
            if rhs.is_reg8() =>
        {
            let reg = rhs.to_reg8();
            let span = instr_token.span.merged_with(&rhs.span);
            let Some(Operand {
                kind: OperandKind::Expr(bit),
                ..
            }) = operands.into_iter().next()
            else {
                unreachable!()
            };
            Some(Instruction::res(reg, bit, span, instr_token.span))
        }
        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span.clone(),
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message("invalid operands to `res`");
                    error.add_label(
                        diagnostics::error_label(&span)
                            .with_message("expected the bit number then a 8-bit register"),
                    );
                });
            }
            None
        }
    };
    (res, lookahead)
}

pub(super) fn parse_set(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (operands, lookahead) =
        misc::parse_comma_list(parse_operand, parse_ctx.next_token(), parse_ctx);

    let res = match operands.as_slice() {
        [Operand {
            kind: OperandKind::Expr(_),
            indirect: false,
            ..
        }, rhs]
            if rhs.is_reg8() =>
        {
            let reg = rhs.to_reg8();
            let span = instr_token.span.merged_with(&rhs.span);
            let Some(Operand {
                kind: OperandKind::Expr(bit),
                ..
            }) = operands.into_iter().next()
            else {
                unreachable!()
            };
            Some(Instruction::set(reg, bit, span, instr_token.span))
        }
        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span.clone(),
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message("invalid operands to `set`");
                    error.add_label(
                        diagnostics::error_label(&span)
                            .with_message("expected the bit number then a 8-bit register"),
                    );
                });
            }
            None
        }
    };
    (res, lookahead)
}

// Bit shift instructions.

pub(super) fn parse_rlc(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (operands, lookahead) =
        misc::parse_comma_list(parse_operand, parse_ctx.next_token(), parse_ctx);

    let res = match operands.as_slice() {
        [operand] if operand.is_reg8() => Some(Instruction::rlc(
            operand.to_reg8(),
            instr_token.span.merged_with(&operand.span),
        )),
        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span,
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message("invalid operands to `rlc`");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("expected an 8-bit register"),
                    );
                });
            }
            None
        }
    };

    (res, lookahead)
}

pub(super) fn parse_rl(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (operands, lookahead) =
        misc::parse_comma_list(parse_operand, parse_ctx.next_token(), parse_ctx);

    let res = match operands.as_slice() {
        [operand] if operand.is_reg8() => Some(Instruction::rl(
            operand.to_reg8(),
            instr_token.span.merged_with(&operand.span),
        )),
        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span,
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message("invalid operands to `rl`");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("expected an 8-bit register"),
                    );
                });
            }
            None
        }
    };

    (res, lookahead)
}

pub(super) fn parse_rrc(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (operands, lookahead) =
        misc::parse_comma_list(parse_operand, parse_ctx.next_token(), parse_ctx);

    let res = match operands.as_slice() {
        [operand] if operand.is_reg8() => Some(Instruction::rrc(
            operand.to_reg8(),
            instr_token.span.merged_with(&operand.span),
        )),
        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span,
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message("invalid operands to `rrc`");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("expected an 8-bit register"),
                    );
                });
            }
            None
        }
    };

    (res, lookahead)
}

pub(super) fn parse_rr(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (operands, lookahead) =
        misc::parse_comma_list(parse_operand, parse_ctx.next_token(), parse_ctx);

    let res = match operands.as_slice() {
        [operand] if operand.is_reg8() => Some(Instruction::rr(
            operand.to_reg8(),
            instr_token.span.merged_with(&operand.span),
        )),
        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span,
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message("invalid operands to `rr`");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("expected an 8-bit register"),
                    );
                });
            }
            None
        }
    };

    (res, lookahead)
}

pub(super) fn parse_sla(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (operands, lookahead) =
        misc::parse_comma_list(parse_operand, parse_ctx.next_token(), parse_ctx);

    let res = match operands.as_slice() {
        [operand] if operand.is_reg8() => Some(Instruction::sla(
            operand.to_reg8(),
            instr_token.span.merged_with(&operand.span),
        )),
        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span,
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message("invalid operands to `sla`");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("expected an 8-bit register"),
                    );
                });
            }
            None
        }
    };

    (res, lookahead)
}

pub(super) fn parse_sra(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (operands, lookahead) =
        misc::parse_comma_list(parse_operand, parse_ctx.next_token(), parse_ctx);

    let res = match operands.as_slice() {
        [operand] if operand.is_reg8() => Some(Instruction::sra(
            operand.to_reg8(),
            instr_token.span.merged_with(&operand.span),
        )),
        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span,
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message("invalid operands to `sra`");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("expected an 8-bit register"),
                    );
                });
            }
            None
        }
    };

    (res, lookahead)
}

pub(super) fn parse_srl(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (operands, lookahead) =
        misc::parse_comma_list(parse_operand, parse_ctx.next_token(), parse_ctx);

    let res = match operands.as_slice() {
        [operand] if operand.is_reg8() => Some(Instruction::srl(
            operand.to_reg8(),
            instr_token.span.merged_with(&operand.span),
        )),
        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span,
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message("invalid operands to `srl`");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("expected an 8-bit register"),
                    );
                });
            }
            None
        }
    };

    (res, lookahead)
}

pub(super) fn parse_swap(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (operands, lookahead) =
        misc::parse_comma_list(parse_operand, parse_ctx.next_token(), parse_ctx);

    let res = match operands.as_slice() {
        [operand] if operand.is_reg8() => Some(Instruction::swap(
            operand.to_reg8(),
            instr_token.span.merged_with(&operand.span),
        )),
        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span,
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message("invalid operands to `swap`");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("expected an 8-bit register"),
                    );
                });
            }
            None
        }
    };

    (res, lookahead)
}

// Stack instructions.

pub(super) fn parse_pop(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (operands, lookahead) =
        misc::parse_comma_list(parse_operand, parse_ctx.next_token(), parse_ctx);

    let instr = match operands.as_slice() {
        [Operand {
            span,
            kind: OperandKind::Reg16(reg),
            indirect: false,
        }] => {
            let instr = Instruction::pop(*reg, instr_token.span.merged_with(span));
            if let Err(span) = &instr {
                parse_ctx.error(span, |error| {
                    error.set_message("invalid operand to `pop`");
                    error.add_label(
                        diagnostics::error_label(span)
                            .with_message("expected `bc`, `de`, `hl`, or `af`"),
                    );
                });
            }
            instr.ok()
        }
        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span,
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message("invalid operands to `pop`");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("expected a 16-bit register"),
                    );
                });
            }
            None
        }
    };
    (instr, lookahead)
}

pub(super) fn parse_push(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (operands, lookahead) =
        misc::parse_comma_list(parse_operand, parse_ctx.next_token(), parse_ctx);

    let instr = match operands.as_slice() {
        [Operand {
            span,
            kind: OperandKind::Reg16(reg),
            indirect: false,
        }] => {
            let instr = Instruction::push(*reg, instr_token.span.merged_with(span));
            if let Err(span) = &instr {
                parse_ctx.error(span, |error| {
                    error.set_message("invalid operand to `push`");
                    error.add_label(
                        diagnostics::error_label(span)
                            .with_message("expected `bc`, `de`, `hl`, or `af`"),
                    );
                });
            }
            instr.ok()
        }
        _ => {
            let span = match operands.last() {
                Some(last) => instr_token.span.merged_with(&last.span),
                None => instr_token.span,
            };
            if is_valid(&operands) {
                parse_ctx.error(&span, |error| {
                    error.set_message("invalid operands to `push`");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("expected a 16-bit register"),
                    );
                });
            }
            None
        }
    };
    (instr, lookahead)
}

// Control flow instructions.

pub(super) fn parse_call(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (cond, mut lookahead) = expect_one_of! { parse_ctx.next_token() => {
        |"nc"| => (Some(Condition::Nc), parse_ctx.next_token()),
        |"c"| => (Some(Condition::C), parse_ctx.next_token()),
        |"nz"| => (Some(Condition::Nz), parse_ctx.next_token()),
        |"z"| => (Some(Condition::Z), parse_ctx.next_token()),
        else |unexpected| => (None, unexpected)
    }};
    if cond.is_some() {
        lookahead = expect_one_of! { lookahead => {
            |","| => parse_ctx.next_token(),
            else |unexpected| => {
                parse_ctx.error(&unexpected.span, |error| {
                    error.set_message("missing comma after condition");
                    error.add_label(diagnostics::error_label(&unexpected.span).with_message("expected a comma before this"));
                });
                unexpected
            }
        }};
    }

    let (expr, lookahead) = expr::expect_numeric_expr(lookahead, parse_ctx);
    (
        Some(Instruction::call(
            cond,
            instr_token.span.merged_with(expr.last_span()),
            expr,
        )),
        lookahead,
    )
}

pub(super) fn parse_jp(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (cond, mut lookahead) = expect_one_of! { parse_ctx.next_token() => {
        Token { span } @ |"hl"| => return (
            Some(Instruction::jp_hl(instr_token.span.merged_with(&span))),
            parse_ctx.next_token(),
        ),
        |"nc"| => (Some(Condition::Nc), parse_ctx.next_token()),
        |"c"| => (Some(Condition::C), parse_ctx.next_token()),
        |"nz"| => (Some(Condition::Nz), parse_ctx.next_token()),
        |"z"| => (Some(Condition::Z), parse_ctx.next_token()),
        else |unexpected| => (None, unexpected)
    }};
    if cond.is_some() {
        lookahead = expect_one_of! { lookahead => {
            |","| => parse_ctx.next_token(),
            else |unexpected| => {
                parse_ctx.error(&unexpected.span, |error| {
                    error.set_message("missing comma after condition");
                    error.add_label(diagnostics::error_label(&unexpected.span).with_message("expected a comma before this"));
                });
                unexpected
            }
        }}
    }

    let (expr, lookahead) = expr::expect_numeric_expr(lookahead, parse_ctx);
    (
        Some(Instruction::jp(
            cond,
            instr_token.span.merged_with(expr.last_span()),
            expr,
        )),
        lookahead,
    )
}

pub(super) fn parse_jr(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (cond, mut lookahead) = expect_one_of! { parse_ctx.next_token() => {
        |"nc"| => (Some(Condition::Nc), parse_ctx.next_token()),
        |"c"| => (Some(Condition::C), parse_ctx.next_token()),
        |"nz"| => (Some(Condition::Nz), parse_ctx.next_token()),
        |"z"| => (Some(Condition::Z), parse_ctx.next_token()),
        else |unexpected| => (None, unexpected)
    }};
    if cond.is_some() {
        lookahead = expect_one_of! { lookahead => {
            |","| => parse_ctx.next_token(),
            else |unexpected| => {
                parse_ctx.error(&unexpected.span, |error| {
                    error.set_message("missing comma after condition");
                    error.add_label(diagnostics::error_label(&unexpected.span).with_message("expected a comma before this"));
                });
                unexpected
            }
        }}
    }

    let (expr, lookahead) = expr::expect_numeric_expr(lookahead, parse_ctx);
    (
        Some(Instruction::jr(
            cond,
            instr_token.span.merged_with(expr.last_span()),
            expr,
        )),
        lookahead,
    )
}

pub(super) fn parse_ret(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (cond, span, lookahead) = expect_one_of! { parse_ctx.next_token() => {
        Token { span } @ |"nc"| => (Some(Condition::Nc), instr_token.span.merged_with(&span), parse_ctx.next_token()),
        Token { span } @ |"c"| => (Some(Condition::C), instr_token.span.merged_with(&span), parse_ctx.next_token()),
        Token { span } @ |"nz"| => (Some(Condition::Nz), instr_token.span.merged_with(&span), parse_ctx.next_token()),
        Token { span } @ |"z"| => (Some(Condition::Z), instr_token.span.merged_with(&span), parse_ctx.next_token()),
        else |unexpected| => (None, instr_token.span, unexpected)
    }};

    (Some(Instruction::ret(cond, span)), lookahead)
}

pub(super) fn parse_rst(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (expr, lookahead) = expr::expect_numeric_expr(parse_ctx.next_token(), parse_ctx);
    let span = instr_token.span.merged_with(expr.last_span());
    (
        Some(Instruction::rst(expr, instr_token.span, span)),
        lookahead,
    )
}

// `stop` optionally takes a value as an operand.

pub(super) fn parse_stop(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let (opt, lookahead) = expr::parse_numeric_expr(parse_ctx.next_token(), parse_ctx);

    let (span, expr) = match opt {
        Some(expr) => (expr.last_span().clone(), expr),
        None => (
            instr_token.span.clone(),
            Expr::number(
                parse_ctx.options.runtime_opts.pad_byte.into(),
                instr_token.span,
            ),
        ),
    };
    (Some(Instruction::stop(expr, span)), lookahead)
}

// `cpl` optionally takes `a` as an operand.

pub(super) fn parse_cpl(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    let lookahead = parse_ctx.next_token();
    let (operand, lookahead) = expect_one_of! { lookahead => {
        |"end of line" / "end of input"| => (None, lookahead),
        else |unexpected| => parse_operand(unexpected, parse_ctx)
    }};

    let instruction = match operand {
        Some(operand!(Reg8::A)) | None => {
            let span = if let Some(operand) = operand {
                instr_token.span.merged_with(&operand.span)
            } else {
                instr_token.span
            };
            Some(Instruction::cpl(span))
        }
        Some(operand) => {
            parse_ctx.error(&operand.span, |error| {
                error.set_message("invalid operand to `cpl`");
                error.add_label(
                    diagnostics::error_label(&operand.span)
                        .with_message("the only valid operand is `a`"),
                );
            });
            None
        }
    };

    (instruction, lookahead)
}

// Instructions that don't take any operands.

pub(super) fn parse_ccf(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    (
        Some(Instruction::ccf(instr_token.span)),
        parse_ctx.next_token(),
    )
}

pub(super) fn parse_daa(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    (
        Some(Instruction::daa(instr_token.span)),
        parse_ctx.next_token(),
    )
}

pub(super) fn parse_di(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    (
        Some(Instruction::di(instr_token.span)),
        parse_ctx.next_token(),
    )
}

pub(super) fn parse_ei(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    (
        Some(Instruction::ei(instr_token.span)),
        parse_ctx.next_token(),
    )
}

pub(super) fn parse_halt(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    (
        Some(Instruction::halt(instr_token.span)),
        parse_ctx.next_token(),
    )
}

pub(super) fn parse_nop(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    (
        Some(Instruction::nop(instr_token.span)),
        parse_ctx.next_token(),
    )
}

pub(super) fn parse_reti(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    (
        Some(Instruction::reti(instr_token.span)),
        parse_ctx.next_token(),
    )
}

pub(super) fn parse_rla(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    (
        Some(Instruction::rla(instr_token.span)),
        parse_ctx.next_token(),
    )
}

pub(super) fn parse_rlca(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    (
        Some(Instruction::rlca(instr_token.span)),
        parse_ctx.next_token(),
    )
}

pub(super) fn parse_rra(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    (
        Some(Instruction::rra(instr_token.span)),
        parse_ctx.next_token(),
    )
}

pub(super) fn parse_rrca(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    (
        Some(Instruction::rrca(instr_token.span)),
        parse_ctx.next_token(),
    )
}

pub(super) fn parse_scf(
    instr_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<Instruction>, Token) {
    (
        Some(Instruction::scf(instr_token.span)),
        parse_ctx.next_token(),
    )
}

// Utility functions.

fn parse_operand(lookahead: Token, parse_ctx: &mut parse_ctx!()) -> (Option<Operand>, Token) {
    let (opening_bracket, lookahead) = if matches_tok!(lookahead, "[") {
        (Some(lookahead.span), parse_ctx.next_token())
    } else {
        (None, lookahead)
    };

    fn operand<K: Into<OperandKind>>(span: Span, kind: K) -> Operand {
        Operand {
            span,
            kind: kind.into(),
            indirect: false, // Will be adjusted before returning.
        }
    }
    let (mut operand, lookahead) = expect_one_of! { lookahead => {
        Token { span } @ |"b"| => (operand(span, Reg8::B), parse_ctx.next_token()),
        Token { span } @ |"c"| => (operand(span, Reg8::C), parse_ctx.next_token()),
        Token { span } @ |"d"| => (operand(span, Reg8::D), parse_ctx.next_token()),
        Token { span } @ |"e"| => (operand(span, Reg8::E), parse_ctx.next_token()),
        Token { span } @ |"h"| => (operand(span, Reg8::H), parse_ctx.next_token()),
        Token { span } @ |"l"| => (operand(span, Reg8::L), parse_ctx.next_token()),
        Token { span } @ |"a"| => (operand(span, Reg8::A), parse_ctx.next_token()),

        Token { span } @ |"bc"| => (operand(span, Reg16::Bc), parse_ctx.next_token()),
        Token { span } @ |"de"| => (operand(span, Reg16::De), parse_ctx.next_token()),
        Token { span } @ |"hl"| => {
            expect_one_of! { parse_ctx.next_token() => {
                Token { span: minus_span } @ |"-"| => (operand(span.merged_with(&minus_span), Reg16::Hld), parse_ctx.next_token()),
                Token { span: plus_span } @ |"+"| => (operand(span.merged_with(&plus_span), Reg16::Hli), parse_ctx.next_token()),
                else |unexpected| => (operand(span, Reg16::Hl), unexpected)
            }}
        },
        Token { span } @ |"af"| => (operand(span, Reg16::Af), parse_ctx.next_token()),
        Token { span } @ |"hld"| => (operand(span, Reg16::Hld), parse_ctx.next_token()),
        Token { span } @ |"hli"| => (operand(span, Reg16::Hli), parse_ctx.next_token()),
        Token { span } @ |"sp"| => expect_one_of! { parse_ctx.next_token() => {
            |"+"| => {
                let (expr, lookahead) = expr::expect_numeric_expr(parse_ctx.next_token(), parse_ctx);
                (operand(span.merged_with(expr.last_span()), OperandKind::SpRel(expr)), lookahead)
            },
            Token { span: op_span } @ |"-"| => {
                let (expr, lookahead) = expr::expect_numeric_expr(parse_ctx.next_token(), parse_ctx);
                let expr = expr.unary_op(crate::expr::UnOp::Negation, op_span);
                (operand(span.merged_with(expr.last_span()), OperandKind::SpRel(expr)), lookahead)
            },
            else |unexpected| => (operand(span, Reg16::Sp), unexpected)
        }},

        Token { span } @ |"low"| => {
            todo!();
        },

        else |unexpected| => {
            let (res, lookahead) = expr::parse_numeric_expr(unexpected, parse_ctx);
            if let Some(expr) = res {
                (operand(expr.overall_span(), expr), lookahead)
            } else {
                parse_ctx.report_syntax_error(&lookahead, |error, span| {
                    error.add_label(diagnostics::error_label(span).with_message("expected a register or an expression"));
                });
                (operand(lookahead.span.clone(), OperandKind::Err), lookahead)
            }
        }
    }};

    if let Some(opening_span) = opening_bracket {
        operand.indirect = true;

        let lookahead = expect_one_of! { lookahead => {
            Token { span: closing_span } @ |"]"| => {
                operand.span = opening_span.merged_with(&closing_span);

                parse_ctx.next_token()
            },
            else |unexpected| => {
                parse_ctx.report_syntax_error(&unexpected, |error, span| {
                    error.add_labels([
                        diagnostics::error_label(span).with_message("expected a closing bracket..."),
                        diagnostics::error_label(&opening_span).with_message("...to close this one"),
                    ]);
                });
                unexpected
            }
        }};

        (Some(operand), lookahead)
    } else {
        (Some(operand), lookahead)
    }
}
#[derive(Debug)]
struct Operand {
    span: Span,
    kind: OperandKind,
    indirect: bool,
}
#[derive(Debug, derive_more::From)]
enum OperandKind {
    Err,
    Reg8(Reg8),
    Reg16(Reg16),
    #[from(ignore)]
    SpRel(Expr),
    Expr(Expr),
}

impl Operand {
    fn is_reg8(&self) -> bool {
        matches!(
            self,
            Self {
                kind: OperandKind::Reg8(_),
                indirect: false,
                ..
            } | Self {
                // `[hl]` is an 8-bit register.
                kind: OperandKind::Reg16(Reg16::Hl),
                indirect: true,
                ..
            }
        )
    }
    fn to_reg8(&self) -> Reg8 {
        match &self.kind {
            OperandKind::Reg8(reg) => *reg,
            OperandKind::Reg16(Reg16::Hl) => Reg8::HlInd,
            _ => unreachable!(),
        }
    }
}

fn is_valid(operands: &[Operand]) -> bool {
    operands
        .iter()
        .all(|operand| !matches!(operand.kind, OperandKind::Err))
}
