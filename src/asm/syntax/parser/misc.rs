use compact_str::CompactString;

use crate::{diagnostics, syntax::tokens::Token};

use super::{expr, string, ParseCtx};

pub(super) enum StrOrNum {
    Num(i32),
    String(CompactString),
}
pub(super) fn parse_str_or_const_expr<'ctx_stack>(
    first_token: Option<Token<'ctx_stack>>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> (Option<StrOrNum>, Option<Token<'ctx_stack>>) {
    let (maybe_string, lookahead) = string::parse_string_expr(first_token, parse_ctx);
    if let Some(string) = maybe_string {
        return (Some(StrOrNum::String(string)), lookahead);
    }

    let (maybe_expr, lookahead) = expr::parse_numeric_expr(lookahead, parse_ctx);
    if let Some(expr) = maybe_expr {
        if let Ok((value, _span)) = expr.try_const_eval() {
            (Some(StrOrNum::Num(value)), lookahead)
        } else {
            (None, lookahead)
        }
    } else {
        let lookahead = lookahead.unwrap();
        diagnostics::error(
            &lookahead.span,
            |error| {
                error.set_message(format!("Syntax error: unexpected {}", lookahead.payload));
                error.add_label(
                    diagnostics::error_label(lookahead.span.resolve())
                        .with_message("Expected a numeric or string expression"),
                );
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        );
        (None, Some(lookahead))
    }
}
