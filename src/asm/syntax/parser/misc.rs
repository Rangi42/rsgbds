use compact_str::CompactString;

use crate::{diagnostics, syntax::tokens::Token};

use super::{expr, matches_tok, parse_ctx, string};

pub(super) fn parse_comma_list<
    'ctx_stack,
    T,
    F: FnMut(
        Option<Token<'ctx_stack>>,
        &mut parse_ctx!('ctx_stack),
    ) -> (Option<T>, Option<Token<'ctx_stack>>),
>(
    mut parse_element: F,
    mut lookahead: Option<Token<'ctx_stack>>,
    parse_ctx: &mut parse_ctx!('ctx_stack),
) -> (Vec<T>, Option<Token<'ctx_stack>>) {
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

pub(super) enum StrOrNum {
    Num(i32),
    String(CompactString),
}
pub(super) fn parse_str_or_const_expr<'ctx_stack>(
    first_token: Option<Token<'ctx_stack>>,
    parse_ctx: &mut parse_ctx!('ctx_stack),
) -> (Option<StrOrNum>, Option<Token<'ctx_stack>>) {
    // It's important to try this one first, as strings are valid numeric expressions.
    let (maybe_string, lookahead) = string::parse_string_expr(first_token, parse_ctx);
    if let Some((string, _span)) = maybe_string {
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
                    diagnostics::error_label(&lookahead.span)
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
