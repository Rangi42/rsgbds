use crate::{
    diagnostics,
    expr::{BinOp, Expr},
    sources::Span,
    syntax::parser::{
        expect_one_of,
        expr::{self, expect_numeric_expr},
        matches_tok, misc, parse_ctx, require, string, tok, Expected, Token,
    },
};

pub(in super::super) fn parse_def_or_redef(
    keyword: Token,
    parse_ctx: &mut parse_ctx!(),
    exported: Option<Span>,
) -> Token {
    require! { parse_ctx.next_token() => Token { span } @ |"identifier"(ident, _has_colon)| else |unexpected| {
        parse_ctx.report_syntax_error(&unexpected, |error, span| {
            error.add_label(diagnostics::error_label(span).with_message("expected an identifier"));
        });
        return unexpected;
    }}

    let kind_token = parse_ctx.next_token();
    expect_one_of! { kind_token => {
        |"equs"| => {
            let (value, lookahead) = string::expect_string_expr(parse_ctx.next_token(), parse_ctx);

            if let Some((string, _span)) = value {
                parse_ctx.symbols.define_string(
                    ident,
                    parse_ctx.identifiers,
                    keyword.span,
                    string,
                    parse_ctx
                        .sections
                        .active_section
                        .as_ref()
                        .map(|(_data_sect, sym_sect)| sym_sect),
                    parse_ctx.macro_args.last(),
                    parse_ctx.nb_errors_remaining,
                    parse_ctx.options,
                );
            }

            if let Some(span) = exported {
                parse_ctx.error(&span, |error| {
                    error.set_message("cannot export string constants");
                    error.add_label(diagnostics::error_label(&span).with_message("consider removing this"));
                });
            }

            lookahead
        },

        |"equ" / "="| => {
            let (expr, lookahead) = expr::expect_numeric_expr(parse_ctx.next_token(), parse_ctx);

            match parse_ctx.try_const_eval(&expr) {
                Ok((value, _span)) => parse_ctx.symbols.define_constant(
                    ident,
                    parse_ctx.identifiers,
                    keyword.span,
                    value,
                    matches_tok!(kind_token, "="),
                    exported.is_some(),
                    parse_ctx
                        .sections
                        .active_section
                        .as_ref()
                        .map(|(_data_sect, sym_sect)| sym_sect),
                    parse_ctx.macro_args.last(),
                    parse_ctx.nb_errors_remaining,
                    parse_ctx.options,
                ),
                Err(error) => parse_ctx.report_expr_error(error),
            }

            lookahead
        },

        Token { span: op_span } @ |"+=" / "-=" / "*=" / "/=" / "%=" / "^=" / "|=" / "&=" / "<<=" / ">>=" / ">>>=" | => {
            let (expr, lookahead) = expr::expect_numeric_expr(parse_ctx.next_token(), parse_ctx);

            let operator = match &kind_token.payload {
                tok!("+=") => BinOp::Add,
                tok!("-=") => BinOp::Subtract,
                tok!("*=") => BinOp::Multiply,
                tok!("/=") => BinOp::Divide,
                tok!("%=") => BinOp::Modulo,
                tok!("^=") => BinOp::Xor,
                tok!("|=") => BinOp::Or,
                tok!("&=") => BinOp::And,
                tok!("<<=") => BinOp::LeftShift,
                tok!(">>=") => BinOp::RightShift,
                tok!(">>>=") => BinOp::UnsignedRightShift,
                _ => unreachable!(),
            };
            let synthetic_expr = Expr::symbol(ident, span).binary_op(operator, expr, op_span);
            match parse_ctx.try_const_eval(&synthetic_expr) {
                Ok((value, _span)) => {
                    parse_ctx.symbols.define_constant(
                        ident,
                        parse_ctx.identifiers,
                        keyword.span,
                        value,
                        true,
                        exported.is_some(),
                        parse_ctx
                            .sections
                            .active_section
                            .as_ref()
                            .map(|(_data_sect, sym_sect)| sym_sect),
                        parse_ctx.macro_args.last(),
                        parse_ctx.nb_errors_remaining,
                        parse_ctx.options,
                    );
                },
                Err(error) => parse_ctx.report_expr_error(error),
            }

            lookahead
        },

        |"rb" / "rw" / "rl"| => {
            let (expr, lookahead) = expr::expect_numeric_expr(parse_ctx.next_token(), parse_ctx);

            match parse_ctx.try_const_eval(&expr) {
                Ok((value, _span)) => {
                    let rs = parse_ctx.symbols.rs();
                    let stride = match &kind_token.payload {
                        tok!("rb") => value,
                        tok!("rw") => value * 2,
                        tok!("rl") => value * 4,
                        _ => unreachable!(),
                    };
                    let constant_value = std::mem::replace(rs, rs.wrapping_add(stride));

                    parse_ctx.symbols.define_constant(
                        ident,
                        parse_ctx.identifiers,
                        keyword.span,
                        constant_value,
                        false,
                        exported.is_some(),
                        parse_ctx
                            .sections
                            .active_section
                            .as_ref()
                            .map(|(_data_sect, sym_sect)| sym_sect),
                        parse_ctx.macro_args.last(),
                        parse_ctx.nb_errors_remaining,
                        parse_ctx.options,
                    );
                },
                Err(error) => parse_ctx.report_expr_error(error),
            }

            lookahead
        },

        else |unexpected, expected| => {
            parse_ctx.report_syntax_error(&unexpected, |error,span| {
                error.add_label(diagnostics::error_label(span).with_message(Expected(expected)));
            });
            unexpected
        }
    }}
}

pub(in super::super) fn parse_export(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let lookahead = parse_ctx.next_token();

    expect_one_of! { lookahead => {
        |"def" / "redef"| => {
            let span = lookahead.span.clone();
            parse_def_or_redef(lookahead, parse_ctx, Some(span))
        },
        else |unexpected| => {
            let (names, lookahead) = misc::parse_comma_list(misc::parse_identifier, unexpected, parse_ctx);

            for (ident, span) in names {
                parse_ctx.symbols.export(
                    ident,
                    span,
                    parse_ctx.identifiers,
                    parse_ctx.nb_errors_remaining,
                    parse_ctx.options,
                );
            }

            lookahead
        }
    }}
}

pub(in super::super) fn parse_purge(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    // Note that it's important to parse the full list first instead of deleting symbols piecemeal,
    // as the rest of the line may reference symbols purged by an earlier part.
    // Example: `purge STR, {STR}`.
    let (names, lookahead) =
        misc::parse_comma_list(misc::parse_identifier, parse_ctx.next_token(), parse_ctx);

    for (ident, span) in names {
        parse_ctx.symbols.delete(
            ident,
            span,
            parse_ctx.identifiers,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        );
    }

    lookahead
}

pub(in super::super) fn parse_rsreset(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    *parse_ctx.symbols.rs() = 0;

    parse_ctx.next_token()
}

pub(in super::super) fn parse_rsset(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let (expr, lookahead) = expect_numeric_expr(parse_ctx.next_token(), parse_ctx);

    match parse_ctx.try_const_eval(&expr) {
        Ok((value, _span)) => *parse_ctx.symbols.rs() = value,
        Err(error) => parse_ctx.report_expr_error(error),
    }

    lookahead
}
