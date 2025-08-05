use crate::{
    diagnostics,
    sources::Span,
    syntax::parser::{
        expect_one_of, expr, matches_tok, parse_ctx, require, string, Expected, Token,
    },
};

pub(in super::super) fn parse_def_or_redef(
    keyword: Token,
    parse_ctx: &mut parse_ctx!(),
    exported: Option<Span>,
) -> Token {
    require! { parse_ctx.next_token() => |"identifier"(ident, _has_colon)| else |unexpected| {
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
                parse_ctx.symbols.define_string(ident, parse_ctx.identifiers, keyword.span, string, parse_ctx.nb_errors_remaining, parse_ctx.options);
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
                    parse_ctx.nb_errors_remaining,
                    parse_ctx.options,
                ),
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
    // TODO: `export <list>`, but also `export def ...`
    todo!()
}

pub(in super::super) fn parse_purge(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(in super::super) fn parse_rsreset(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(in super::super) fn parse_rsset(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}
