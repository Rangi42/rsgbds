use crate::syntax::tokens::Token;

use super::ParseCtx;

pub(super) fn parse_adc<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_add<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_and<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_bit<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_call<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_ccf<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_cp<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_cpl<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_daa<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_dec<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_di<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_ei<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_halt<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_inc<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_jp<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_jr<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_ldd<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_ldh<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_ldi<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_ld<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_nop<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_or<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_pop<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_push<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_res<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_reti<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_ret<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_rla<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_rlca<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_rlc<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_rl<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_rra<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_rrca<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_rrc<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_rr<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_rst<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_sbc<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_scf<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_set<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_sla<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_sra<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_srl<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_stop<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_sub<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_swap<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(super) fn parse_xor<'ctx_stack>(
    instr_token: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    todo!()
}
