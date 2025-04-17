use crate::{diagnostics, syntax::tokens::Token};

use super::super::{matches_tok, misc, string, tok, ParseCtx};

fn parse_print_elem<'ctx_stack>(
    first_token: Option<Token<'ctx_stack>>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> (Option<()>, Option<Token<'ctx_stack>>) {
    let (expr, lookahead) = misc::parse_str_or_const_expr(first_token, parse_ctx);
    match expr {
        None => return (None, lookahead),
        Some(misc::StrOrNum::Num(value)) => print!("${value:X}"),
        Some(misc::StrOrNum::String(string)) => print!("{string}"),
    };
    (Some(()), lookahead)
}
pub(in super::super) fn parse_println<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    let mut lookahead = parse_ctx.next_token();
    // Allow a lack of arguments.
    if !matches_tok!(lookahead, "end of line") {
        let (_, new_lookahead) = misc::parse_comma_list(parse_print_elem, lookahead, parse_ctx);
        lookahead = new_lookahead;
    }
    println!();
    lookahead
}
pub(in super::super) fn parse_print<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    let mut lookahead = parse_ctx.next_token();
    // Allow a lack of arguments.
    if !matches_tok!(lookahead, "end of line") {
        let (_, new_lookahead) = misc::parse_comma_list(parse_print_elem, lookahead, parse_ctx);
        lookahead = new_lookahead;
    }
    lookahead
}

pub(in super::super) fn parse_warn<'ctx_stack>(
    keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    let (maybe_message, lookahead) = string::parse_string_expr(parse_ctx.next_token(), parse_ctx);
    if let Some((message, _span)) = maybe_message {
        diagnostics::warn(
            diagnostics::warning!("user"),
            &keyword.span,
            |warning| {
                warning.set_message(message);
                warning.add_label(diagnostics::warning_label(&keyword.span))
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        );
    }
    lookahead
}
pub(in super::super) fn parse_fail<'ctx_stack>(
    keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    let (maybe_message, lookahead) = string::parse_string_expr(parse_ctx.next_token(), parse_ctx);
    if let Some((message, _span)) = maybe_message {
        diagnostics::error(
            &keyword.span,
            |error| {
                error.set_message(message);
                error.add_label(
                    diagnostics::error_label(&keyword.span).with_message("Assembly aborted here"),
                );
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        );
    }
    todo!(); // Abort parsing *somehow*.
    lookahead
}

pub(in super::super) fn parse_assert<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(in super::super) fn parse_static_assert<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}
