use crate::syntax::tokens::Token;

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
    todo!()
}

pub(in super::super) fn parse_popc<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}
