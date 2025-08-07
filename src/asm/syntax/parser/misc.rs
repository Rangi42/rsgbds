use compact_str::CompactString;

use crate::{diagnostics, expr::Expr, sources::Span, syntax::tokens::Token, Identifier};

use super::{expect_one_of, expr, matches_tok, parse_ctx, string};

pub(super) fn parse_comma_list<T, F: FnMut(Token, &mut parse_ctx!()) -> (Option<T>, Token)>(
    mut parse_element: F,
    mut lookahead: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Vec<T>, Token) {
    let mut elements = vec![];
    loop {
        let (maybe_elem, new_lookahead) = parse_element(lookahead, parse_ctx);
        lookahead = new_lookahead;
        let Some(element) = maybe_elem else {
            break;
        };
        elements.push(element);

        if !matches_tok!(lookahead, ",") {
            break;
        }
        // Consume the comma.
        lookahead = parse_ctx.next_token();
        // Allow trailing commas.
        if matches_tok!(lookahead, "end of line" | ")") {
            break;
        }
    }
    (elements, lookahead)
}
pub(super) fn parse_identifier(
    token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<(Identifier, Span)>, Token) {
    expect_one_of! { token => {
        Token { span } @ |"identifier"(ident, _has_colon)| => (Some((ident, span)), parse_ctx.next_token()),
        else |unexpected| => (None, unexpected)
    }}
}

pub enum StrOrNum {
    Num(Expr),
    String(CompactString),
}
pub(super) fn parse_str_or_expr(
    first_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<StrOrNum>, Token) {
    // It's important to try this one first, as strings are valid numeric expressions.
    let (maybe_string, lookahead) = string::parse_string_expr(first_token, parse_ctx);
    if let Some((string, _span)) = maybe_string {
        return (Some(StrOrNum::String(string)), lookahead);
    }

    let (maybe_expr, lookahead) = expr::parse_numeric_expr(lookahead, parse_ctx);
    if let Some(expr) = maybe_expr {
        (Some(StrOrNum::Num(expr)), lookahead)
    } else {
        parse_ctx.error(&lookahead.span, |error| {
            error.set_message(format!("syntax error: unexpected {}", lookahead.payload));
            error.add_label(
                diagnostics::error_label(&lookahead.span)
                    .with_message("expected a numeric or string expression"),
            );
        });
        (None, lookahead)
    }
}
