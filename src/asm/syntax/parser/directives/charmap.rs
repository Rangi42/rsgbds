use crate::{diagnostics, syntax::tokens::Token};

use super::super::{expect_one_of, ParseCtx};

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
    expect_one_of! { parse_ctx.next_token() => {
        Token { span } |"identifier"(ident)| => {
            let charmap_name = parse_ctx.symbols.resolve(ident);
            if parse_ctx.charmaps.switch_to(charmap_name).is_none() {
                diagnostics::error(
                    &span,
                    |error| {
                        error.set_message(format!("No charmap named \"{charmap_name}\" exists"));
                        error.add_label(diagnostics::error_label(&span).with_message("Cannot switch to this charmap"));
                    },
                    parse_ctx.sources,
                    parse_ctx.nb_errors_remaining,
                    parse_ctx.options,
                );
            }

            parse_ctx.next_token()
        },
        else |other| => {
            parse_ctx.report_syntax_error(other.as_ref(), |error, span| {
                error.add_label(diagnostics::error_label(span).with_message("Expected a charmap name here"));
            });
            other
        }
    }}
}

pub(in super::super) fn parse_pushc<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    parse_ctx.charmaps.push_active_charmap();

    expect_one_of! { parse_ctx.next_token() => {
        Token { span } |"identifier"(ident)| => {
            let charmap_name = parse_ctx.symbols.resolve(ident);
            if parse_ctx.charmaps.switch_to(charmap_name).is_none() {
                diagnostics::error(
                    &span,
                    |error| {
                        error.set_message(format!("No charmap named \"{charmap_name}\" exists"));
                        error.add_label(diagnostics::error_label(&span).with_message("Cannot switch to this charmap"));
                    },
                    parse_ctx.sources,
                    parse_ctx.nb_errors_remaining,
                    parse_ctx.options,
                );
            }

            parse_ctx.next_token()
        },
        else |other| => other
    }}
}

pub(in super::super) fn parse_popc<'ctx_stack>(
    keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    if parse_ctx.charmaps.pop_active_charmap().is_none() {
        diagnostics::error(
            &keyword.span,
            |error| {
                error.set_message("No entries in the charmap stack");
                error.add_label(diagnostics::error_label(&keyword.span).with_message("Cannot pop"));
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        )
    }

    parse_ctx.next_token()
}
