use crate::{diagnostics, syntax::tokens::Token};

use super::super::ParseCtx;

pub(in super::super) fn parse_charmap<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(in super::super) fn parse_newcharmap<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(in super::super) fn parse_setcharmap<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(in super::super) fn parse_pushc<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    parse_ctx.charmaps.push_active_charmap();

    parse_ctx.next_token()
}

pub(in super::super) fn parse_popc<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    if parse_ctx.charmaps.pop_active_charmap().is_none() {
        diagnostics::error(
            &_keyword.span,
            |error| {
                error.set_message("No entries in the charmap stack");
                error
                    .add_label(diagnostics::error_label(&_keyword.span).with_message("Cannot pop"));
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        )
    }

    parse_ctx.next_token()
}
