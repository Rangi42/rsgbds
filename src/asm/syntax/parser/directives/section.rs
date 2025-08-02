use compact_str::CompactString;

use crate::{
    common::section::MemRegion,
    diagnostics,
    section::{AddrConstraint, SectionAttrs, SectionKind},
    syntax::tokens::Token,
};

use super::super::{expect_one_of, expr, matches_tok, parse_ctx, require, string};

fn parse_section_attrs(
    first_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (SectionAttrs, CompactString, Token) {
    let mut attrs = SectionAttrs {
        kind: SectionKind::Normal,
        mem_region: MemRegion::Rom0, // Dummy that doesn't matter.
        address: AddrConstraint::None,
        bank: None,
    };

    let lookahead = expect_one_of! { first_token => {
        |"union"| => {
            attrs.kind = SectionKind::Union;
            parse_ctx.next_token()
        },
        |"fragment"| => {
            attrs.kind = SectionKind::Fragment;
            parse_ctx.next_token()
        },
        else |other| => other
    }};

    let (maybe_string, lookahead) = string::expect_string_expr(lookahead, parse_ctx);
    let name = match maybe_string {
        Some((name, _)) => name,
        None => "<error>".into(),
    };

    require! { lookahead => |","| else |other| {
        parse_ctx.report_syntax_error(&other, |error,span| {
            error.add_label(diagnostics::error_label(span).with_message("expected a comma here"));
        });
    }};

    let mut lookahead = expect_one_of! { parse_ctx.next_token() => {
        |"rom0"| => {
            attrs.mem_region = MemRegion::Rom0;
            parse_ctx.next_token()
        },
        |"romx"| => {
            attrs.mem_region = MemRegion::Romx;
            parse_ctx.next_token()
        },
        |"vram"| => {
            attrs.mem_region = MemRegion::Vram;
            parse_ctx.next_token()
        },
        |"sram"| => {
            attrs.mem_region = MemRegion::Sram;
            parse_ctx.next_token()
        },
        |"wram0"| => {
            attrs.mem_region = MemRegion::Wram0;
            parse_ctx.next_token()
        },
        |"wramx"| => {
            attrs.mem_region = MemRegion::Wramx;
            parse_ctx.next_token()
        },
        |"oam"| => {
            attrs.mem_region = MemRegion::Oam;
            parse_ctx.next_token()
        },
        |"hram"| => {
            attrs.mem_region = MemRegion::Hram;
            parse_ctx.next_token()
        },
        else |other, expected| => {
            parse_ctx.report_syntax_error(&other, |error,span| {
                error.add_label(diagnostics::error_label(span).with_message("expected TODO here"))
            });

            // Process the next token if it is expected later in the directive.
            // Otherwise, consume the rest of the line (to avoid reporting an extraneous syntax error), and abort.
            if !matches_tok!(other, "," | "[" | "number"(_) | "bank" | "align") {
                let mut lookahead = other;
                while !matches_tok!(lookahead, "end of input" | "end of line") {
                    lookahead = parse_ctx.next_token();
                }
                return (attrs, name, lookahead);
            }
            other
        }
    }};

    // Address.
    if matches_tok!(lookahead, "[") {
        let (expr, after_expr) = expr::expect_numeric_expr(parse_ctx.next_token(), parse_ctx);

        debug_assert_eq!(attrs.address, AddrConstraint::None);
        match parse_ctx.try_const_eval(&expr) {
            Ok((addr, _)) => match addr.try_into() {
                Ok(addr) => attrs.address = AddrConstraint::Addr(addr),
                Err(err) => todo!(),
            },
            Err(err) => parse_ctx.report_expr_error(err),
        };

        require! { after_expr => |"]"| else |other| {
            parse_ctx.report_syntax_error(&other, |error, span| {
                error.set_message("missing `]` after the section's address");
                error.add_label(
                    diagnostics::error_label(span)
                        .with_message("expected a closing brace here")
                );
            });
        }};

        lookahead = parse_ctx.next_token();
    }

    // Attributes.
    while matches_tok!(lookahead, ",") {
        lookahead = parse_ctx.next_token();
        // Allow a trailing comma.
        if matches_tok!(lookahead, "end of input" | "end of line") {
            break;
        }

        expect_one_of! { lookahead => {
            |"bank"| => {
                expect_one_of! { parse_ctx.next_token() => {
                    |"["| => lookahead = parse_ctx.next_token(),
                    else |other| => {
                        parse_ctx.report_syntax_error(&other, |error, span| {
                            error.add_label(
                                diagnostics::error_label(span)
                                    .with_message("the previous section attribute must be followed by a number between braces")
                            )
                        });
                        lookahead = other; // Keep trying to parse.
                    }
                }};

                let (expr, after_expr) = expr::expect_numeric_expr(lookahead, parse_ctx);
                attrs.bank = Some(match parse_ctx.try_const_eval(&expr) {
                    Ok((value, _span)) => value as u32,
                    Err(err) => {
                        parse_ctx.report_expr_error(err);
                        attrs.mem_region.min_bank()
                    }
                });

                expect_one_of! { after_expr => {
                    |"]"| => lookahead = parse_ctx.next_token(),
                    else |other| => {
                        parse_ctx.report_syntax_error(&other, |error, span| {
                            error.add_label(
                                diagnostics::error_label(span)
                                    .with_message("missing closing brace after the previous section attribute")
                            )
                        });
                        lookahead = other;
                    }
                }};
            },

            Token { span: align_span } |"align"| => {
                expect_one_of! { parse_ctx.next_token() => {
                    |"["| => lookahead = parse_ctx.next_token(),
                    else |other| => {
                        parse_ctx.report_syntax_error(&other, |error, span| {
                            error.add_label(
                                diagnostics::error_label(span)
                                    .with_message("the previous section attribute must be followed by one or two numbers between braces")
                            )
                        });
                        lookahead = other; // Keep trying to parse.
                    }
                }};

                let (expr, after_expr) = expr::expect_numeric_expr(lookahead, parse_ctx);
                let align = match parse_ctx.try_const_eval(&expr) {
                    Ok((value, span)) => if (value as u32) > 16 {
                        parse_ctx.error(&span, |error| {
                            error.set_message("specified alignment is larger than 16");
                            error.add_label(
                                diagnostics::error_label(&span)
                                    .with_message(format!("this expression evaluates to {value}"))
                            );
                        });
                        16
                    } else {
                        value as u8
                    },
                    Err(err) => {
                        parse_ctx.report_expr_error(err);
                        0
                    }
                };

                let align_ofs = if matches_tok!(after_expr, ",") {
                    lookahead = parse_ctx.next_token();
                    // Allow a trailing comma.
                    let (maybe_expr, after_expr) = expr::parse_numeric_expr(lookahead, parse_ctx);
                    lookahead = after_expr;
                    maybe_expr.map_or(0, |expr| {
                        match parse_ctx.try_const_eval(&expr) {
                            Ok((value, span)) => if (value as u32) >= (1 << align) {
                                parse_ctx.error(&span, |error| {
                                    error.set_message("alignment offset is larger than the alignment");
                                    error.add_label(
                                        diagnostics::error_label(&span)
                                            .with_message(format!("this expression evaluates to {value}, which is larger than 1 << {align}"))
                                    );
                                });
                                (1 << align) - 1
                            } else {
                                value as u16
                            },
                            Err(err) => {
                                parse_ctx.report_expr_error(err);
                                0
                            }
                        }
                    })
                } else {
                    lookahead = after_expr;
                    0
                };
                // If there is already a constraint, it must match at the same offset.
                if let Err(err) = attrs.address.merge(AddrConstraint::Align(align, align_ofs), 0) {
                    let (msg, label_msg) = err.details();
                    parse_ctx.error(
                        &align_span,
                        |error| {
                            error.set_message(msg);
                            error.add_label(
                                diagnostics::error_label(&align_span).with_message(label_msg)
                            );
                        },
                    );
                };

                expect_one_of! { lookahead => {
                    |"]"| => lookahead = parse_ctx.next_token(),
                    else |other| => {
                        parse_ctx.report_syntax_error(&other, |error, span| {
                            error.add_label(
                                diagnostics::error_label(span)
                                    .with_message("missing closing brace after the previous section attribute")
                            )
                        });
                        lookahead = other;
                    }
                }};
            },

            else |other| => {
                parse_ctx.report_syntax_error(&other, |error, span| {
                    error.add_label(
                        diagnostics::error_label(span)
                            .with_message("expected a section attribute here (TODO)")
                    );
                });
                lookahead = if matches_tok!(other, ",") {
                    other
                } else {
                    parse_ctx.next_token()
                };
            }
        }};
    }

    (attrs, name, lookahead)
}
pub(in super::super) fn parse_section(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let (attrs, name, lookahead) = parse_section_attrs(parse_ctx.next_token(), parse_ctx);

    let active_section = parse_ctx.sections.create_if_not_exists(name, attrs);
    parse_ctx.sections.active_section = Some((active_section.clone(), active_section));

    lookahead
}

pub(in super::super) fn parse_endsection(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    parse_ctx.sections.active_section = None;

    parse_ctx.next_token()
}

pub(in super::super) fn parse_load(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let (attrs, name, lookahead) = parse_section_attrs(parse_ctx.next_token(), parse_ctx);

    let new_active_section = parse_ctx.sections.create_if_not_exists(name, attrs);
    let Some((data_section, symbol_section)) = parse_ctx.sections.active_section.as_mut() else {
        parse_ctx.error(&keyword.span, |error| {
            error.set_message("`LOAD` used outside of a section");
        });

        return lookahead;
    };
    if new_active_section.points_to_same_as(data_section) {
        parse_ctx.error(&keyword.span, |error| {
            error.set_message("`LOAD` cannot designate the active section");
            error.add_label(
                diagnostics::error_label(&keyword.span)
                    .with_message("section \"{}\" is active here"),
            );
        });

        return lookahead;
    }
    *symbol_section = new_active_section;

    lookahead
}

pub(in super::super) fn parse_endl(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    if let Some((data_section, symbol_section)) = parse_ctx.sections.active_section.as_mut() {
        if !data_section.points_to_same_as(symbol_section) {
            // End the `LOAD` block.
            *symbol_section = data_section.clone();
        } else {
            parse_ctx.error(&_keyword.span, |error| {
                error.set_message("`ENDL` used outside of a `LOAD` block");
                error.add_label(
                    diagnostics::error_label(&_keyword.span)
                        .with_message("this directive is invalid"),
                );
            });
        }
    } else {
        parse_ctx.error(&_keyword.span, |error| {
            error.set_message("`ENDL` used outside of a section");
            error.add_label(
                diagnostics::error_label(&_keyword.span).with_message("this directive is invalid"),
            );
        });
    }

    parse_ctx.next_token()
}

pub(in super::super) fn parse_pushs(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    parse_ctx.sections.push_active_section();

    let lookahead = parse_ctx.next_token();
    if matches_tok!(lookahead, "end of line") {
        parse_ctx.sections.active_section = None;
        return lookahead;
    }

    let (attrs, name, lookahead) = parse_section_attrs(lookahead, parse_ctx);

    let active_section = parse_ctx.sections.create_if_not_exists(name, attrs);
    parse_ctx.sections.active_section = Some((active_section.clone(), active_section));

    lookahead
}

pub(in super::super) fn parse_pops(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    if parse_ctx.sections.pop_active_section().is_none() {
        parse_ctx.error(&keyword.span, |error| {
            error.set_message("no entries in the section stack");
            error.add_label(diagnostics::error_label(&keyword.span).with_message("cannot pop"));
        })
    }

    parse_ctx.next_token()
}
