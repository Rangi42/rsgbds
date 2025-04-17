use crate::{diagnostics, syntax::tokens::Token};

use super::super::{expect_one_of, expr, misc, require, string, ParseCtx};

pub(in super::super) fn parse_charmap<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    let (maybe_string, lookahead) = string::expect_string_expr(parse_ctx.next_token(), parse_ctx);
    let Some((string, span)) = maybe_string else {
        return lookahead;
    };
    require! { lookahead => |","| else |other| {
        parse_ctx.report_syntax_error(other.as_ref(), |error, span| {
            error.add_label(diagnostics::error_label(span).with_message("Expected a comma here"));
        });
        return other;
    }};

    let (values, lookahead) = misc::parse_comma_list(
        |lookahead, parse_ctx| {
            let (expr, lookahead) = expr::expect_numeric_expr(lookahead, parse_ctx);
            (
                match expr.try_const_eval() {
                    Ok((value, _span)) => Some(value),
                    Err(err) if err.is_nothing() => None,
                    Err(err) => {
                        parse_ctx.report_expr_error(err);
                        Some(0)
                    }
                },
                lookahead,
            )
        },
        parse_ctx.next_token(),
        parse_ctx,
    );
    if parse_ctx
        .charmaps
        .active_charmap_mut()
        .add_mapping(&string, values)
        .is_none()
    {
        diagnostics::error(
            &span,
            |error| {
                error.set_message("Cannot charmap an empty string");
                diagnostics::error_label(&span).with_message("This string shouldn't be empty");
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        );
    }
    lookahead
}

pub(in super::super) fn parse_newcharmap<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    require! { parse_ctx.next_token() => Token { span } @ |"identifier"(ident)| else |other| {
        parse_ctx.report_syntax_error(other.as_ref(), |error, span| {
            error.add_label(
                diagnostics::error_label(span).with_message("Expected a charmap name here"),
            );
        });
        return other;
    }};
    let def_span = span;
    let charmap_name = parse_ctx.symbols.resolve(ident).into();

    require! { parse_ctx.next_token() => |","| else |other| {
        // Just `newcharmap <name>`: create it from scratch.
        if let Err(err) = parse_ctx.charmaps.make_new(charmap_name, def_span) {
            diagnostics::error(
                err.diag_span(),
                |error| {
                    err.make_diag(error);
                },
                parse_ctx.sources,
                parse_ctx.nb_errors_remaining,
                parse_ctx.options
            )
        }

        return other;
    }};

    require! { parse_ctx.next_token() => Token { span } @ |"identifier"(ident)| else |other| {
        parse_ctx.report_syntax_error(other.as_ref(), |error, span| {
            error.add_label(
                diagnostics::error_label(span).with_message("Expected a charmap name here"),
            );
        });
        return other;
    }};

    let target_name = parse_ctx.symbols.resolve(ident);
    if let Err(err) = parse_ctx
        .charmaps
        .make_copy(charmap_name, def_span, target_name)
    {
        diagnostics::error(
            err.diag_span(),
            |error| {
                err.make_diag(error);
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        );
    }

    parse_ctx.next_token()
}

pub(in super::super) fn parse_setcharmap<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    require! { parse_ctx.next_token() => Token { span } @ |"identifier"(ident)| else |other| {
        parse_ctx.report_syntax_error(other.as_ref(), |error, span| {
            error.add_label(
                diagnostics::error_label(span).with_message("Expected a charmap name here"),
            );
        });
        return other;
    }};

    let charmap_name = parse_ctx.symbols.resolve(ident);
    if parse_ctx.charmaps.switch_to(charmap_name).is_none() {
        diagnostics::error(
            &span,
            |error| {
                error.set_message(format!("No charmap named \"{charmap_name}\" exists"));
                error.add_label(
                    diagnostics::error_label(&span).with_message("Cannot switch to this charmap"),
                );
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        );
    }

    parse_ctx.next_token()
}

pub(in super::super) fn parse_pushc<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    parse_ctx.charmaps.push_active_charmap();

    require! { parse_ctx.next_token() => Token { span } @ |"identifier"(ident)| else |other| {
        return other;
    }};

    let charmap_name = parse_ctx.symbols.resolve(ident);
    if parse_ctx.charmaps.switch_to(charmap_name).is_none() {
        diagnostics::error(
            &span,
            |error| {
                error.set_message(format!("No charmap named \"{charmap_name}\" exists"));
                error.add_label(
                    diagnostics::error_label(&span).with_message("Cannot switch to this charmap"),
                );
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        );
    }

    parse_ctx.next_token()
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
