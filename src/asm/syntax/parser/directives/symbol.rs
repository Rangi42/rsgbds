use crate::{
    diagnostics,
    syntax::parser::{expect_one_of, require, string, Expected},
};

use super::{parse_ctx, Token};

pub(in super::super) fn parse_def_or_redef(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    require! { parse_ctx.next_token() => |"identifier"(ident, _has_colon)| else |unexpected| {
        parse_ctx.report_syntax_error(&unexpected, |error, span| {
            error.add_label(diagnostics::error_label(span).with_message("expected an identifier"));
        });
        return unexpected;
    }}

    expect_one_of! { parse_ctx.next_token() => {
        |"equs"| => {
            let (value, lookahead) = string::expect_string_expr(parse_ctx.next_token(), parse_ctx);

            if let Some((string, _span)) = value {
                parse_ctx.symbols.define_string(ident, parse_ctx.identifiers, keyword.span, string, parse_ctx.nb_errors_remaining, parse_ctx.options);
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
