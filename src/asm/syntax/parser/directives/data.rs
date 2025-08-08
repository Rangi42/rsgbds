use crate::{
    diagnostics,
    syntax::{
        parser::{expect_one_of, expr, misc, parse_ctx, require},
        tokens::Token,
    },
};

pub(in super::super) fn parse_ds(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let (length, lookahead) = expect_one_of! { parse_ctx.next_token() => {
        |"align"| => {
            todo!();
        },
        else |unexpected| => {
            let (res, lookahead) = expr::parse_numeric_expr(unexpected, parse_ctx);

            let length = if let Some(expr) = res {
                match parse_ctx.try_const_eval(&expr) {
                    Ok((value, span)) => if value < 0 {
                        parse_ctx.error(&span, |error| {
                            error.set_message("negative sizes are not allowed for `ds`");
                            error.add_label(diagnostics::error_label(&span).with_message("cannot emit {value} bytes of padding"));
                            // TODO: maybe don't print this in ROM sections?
                            error.set_help("if you want multiple blocks of data to share the same space, consider using `union`");
                        });
                        0
                    } else {
                        value as usize
                    },
                    Err(error) => {
                        parse_ctx.report_expr_error(error);
                        0
                    }
                }
            } else {
                parse_ctx.report_syntax_error(&lookahead, |error, span| {
                    error.add_label(diagnostics::error_label(span).with_message("expected a numeric expression or an `align` specification"));
                });
                0
            };
            (length, lookahead)
        }
    }};

    expect_one_of! { lookahead => {
        |","| => {
            let (bytes, lookahead) = misc::parse_comma_list(expr::parse_numeric_expr, parse_ctx.next_token(), parse_ctx);
            parse_ctx.sections.emit_padding(
                length,
                &bytes,
                &keyword.span,
                parse_ctx.identifiers,
                parse_ctx.symbols,parse_ctx.macro_args.last(),
                parse_ctx.nb_errors_remaining,
                parse_ctx.options,
            );

            lookahead
        },
        else |unexpected| => {
            parse_ctx.sections.allocate_space(length, &keyword.span, parse_ctx.nb_errors_remaining, parse_ctx.options);

            unexpected
        }
    }}
}

pub(in super::super) fn parse_db(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let lookahead = parse_ctx.next_token();
    expect_one_of! { lookahead => {
        |"end of line" / "end of input"| => {
            parse_ctx.sections.allocate_space(1, &keyword.span, parse_ctx.nb_errors_remaining, parse_ctx.options);

            lookahead
        },
        else |unexpected| => {
            // A `Vec<()>` doesn't allocate, so this doesn't cost us an allocation.
            let (_vec, lookahead): (Vec<()>, _) = misc::parse_comma_list(
                |lookahead, parse_ctx| {
                    let (str_or_expr, lookahead) = misc::parse_str_or_expr(lookahead, parse_ctx);
                    let res = str_or_expr.map(|str_or_expr| match str_or_expr {
                        misc::StrOrNum::Num(byte) => parse_ctx.sections.emit_byte(
                            &byte,
                            &keyword.span,
                            parse_ctx.identifiers,
                            parse_ctx.symbols,
                            parse_ctx.macro_args.last(),
                            parse_ctx.nb_errors_remaining,
                            parse_ctx.options,
                        ),
                        misc::StrOrNum::String(string) => todo!(),
                    });
                    (res, lookahead)
                },
                unexpected,
                parse_ctx,
            );

            lookahead
        }
    }}
}

pub(in super::super) fn parse_dw(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let lookahead = parse_ctx.next_token();
    expect_one_of! { lookahead => {
        |"end of line" / "end of input"| => {
            parse_ctx.sections.allocate_space(2, &keyword.span, parse_ctx.nb_errors_remaining, parse_ctx.options);

            lookahead
        },
        else |unexpected| => {
            // A `Vec<()>` doesn't allocate, so this doesn't cost us an allocation.
            let (_vec, lookahead): (Vec<()>, _) = misc::parse_comma_list(
                |lookahead, parse_ctx| {
                    let (str_or_expr, lookahead) = misc::parse_str_or_expr(lookahead, parse_ctx);
                    let res = str_or_expr.map(|str_or_expr| match str_or_expr {
                        misc::StrOrNum::Num(word) => parse_ctx.sections.emit_word(
                            &word,
                            &keyword.span,
                            parse_ctx.identifiers,
                            parse_ctx.symbols,
                            parse_ctx.macro_args.last(),
                            parse_ctx.nb_errors_remaining,
                            parse_ctx.options,
                        ),
                        misc::StrOrNum::String(string) => todo!(),
                    });
                    (res, lookahead)
                },
                unexpected,
                parse_ctx,
            );

            lookahead
        }
    }}
}

pub(in super::super) fn parse_dl(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let lookahead = parse_ctx.next_token();
    expect_one_of! { lookahead => {
        |"end of line" / "end of input"| => {
            parse_ctx.sections.allocate_space(4, &keyword.span, parse_ctx.nb_errors_remaining, parse_ctx.options);

            lookahead
        },
        else |unexpected| => {
            // A `Vec<()>` doesn't allocate, so this doesn't cost us an allocation.
            let (_vec, lookahead): (Vec<()>, _) = misc::parse_comma_list(
                |lookahead, parse_ctx| {
                    let (str_or_expr, lookahead) = misc::parse_str_or_expr(lookahead, parse_ctx);
                    let res = str_or_expr.map(|str_or_expr| match str_or_expr {
                        misc::StrOrNum::Num(long) => parse_ctx.sections.emit_long(
                            &long,
                            &keyword.span,
                            parse_ctx.identifiers,
                            parse_ctx.symbols,
                            parse_ctx.macro_args.last(),
                            parse_ctx.nb_errors_remaining,
                            parse_ctx.options,
                        ),
                        misc::StrOrNum::String(string) => todo!(),
                    });
                    (res, lookahead)
                },
                unexpected,
                parse_ctx,
            );

            lookahead
        }
    }}
}

pub(in super::super) fn parse_incbin(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(in super::super) fn parse_union(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(in super::super) fn parse_nextu(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(in super::super) fn parse_endu(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}
