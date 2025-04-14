use crate::{
    diagnostics,
    syntax::tokens::{tok, Token},
};

use super::{expr, matches_tok, misc, string, ParseCtx};

pub(super) fn parse_align<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_break<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_charmap<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_db<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_dl<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_ds<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_dw<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_elif<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_else<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_endsection<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_endc<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_endl<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_endm<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_endr<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_endu<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_export<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_fatal<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_for<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_if<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_incbin<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_include<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_load<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_macro<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_newcharmap<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_nextu<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_opt<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_popc<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_popo<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_pops<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_purge<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_pushc<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_pusho<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_pushs<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_rb<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_rw<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_redef<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_rept<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_rsreset<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_rsset<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_section<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_setcharmap<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_shift<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_union<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

fn parse_print_elem<'ctx_stack>(
    first_token: Option<Token<'ctx_stack>>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> (Option<()>, Option<Token<'ctx_stack>>) {
    let (expr, lookahead) = misc::parse_str_or_const_expr(first_token, parse_ctx);
    match expr {
        None => return (None, lookahead),
        Some(misc::StrOrNum::Num(value)) => print!("${value:X}"),
        Some(misc::StrOrNum::String(string)) => print!("{string}"),
    };
    (Some(()), lookahead)
}
pub(super) fn parse_println<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
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
pub(super) fn parse_print<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    let mut lookahead = parse_ctx.next_token();
    // Allow a lack of arguments.
    if !matches_tok!(lookahead, "end of line") {
        let (_, new_lookahead) = misc::parse_comma_list(parse_print_elem, lookahead, parse_ctx);
        lookahead = new_lookahead;
    }
    lookahead
}

pub(super) fn parse_warn<'ctx_stack>(
    keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    let (maybe_message, lookahead) = string::parse_string_expr(parse_ctx.next_token(), parse_ctx);
    if let Some(message) = maybe_message {
        diagnostics::warn(
            diagnostics::warning!("user"),
            &keyword.span,
            |warning| {
                warning.set_message(message);
                warning.add_label(diagnostics::warning_label(keyword.span.resolve()))
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        );
    }
    lookahead
}
pub(super) fn parse_fail<'ctx_stack>(
    keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    let (maybe_message, lookahead) = string::parse_string_expr(parse_ctx.next_token(), parse_ctx);
    if let Some(message) = maybe_message {
        diagnostics::error(
            &keyword.span,
            |error| {
                error.set_message(message);
                error.add_label(
                    diagnostics::error_label(keyword.span.resolve())
                        .with_message("Assembly aborted here"),
                );
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        );
    }
    lookahead
}

pub(super) fn parse_assert<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_static_assert<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}
