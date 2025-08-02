use crate::{diagnostics, syntax::tokens::Token};

use super::super::{expr, misc, parse_ctx, require, string};

pub(in super::super) fn parse_charmap(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let (maybe_string, lookahead) = string::expect_string_expr(parse_ctx.next_token(), parse_ctx);
    let Some((string, span)) = maybe_string else {
        return lookahead;
    };
    require! { lookahead => |","| else |other| {
        parse_ctx.report_syntax_error(&other, |error, span| {
            error.add_label(diagnostics::error_label(span).with_message("expected a comma here"));
        });
        return other;
    }};

    let (values, lookahead) = misc::parse_comma_list(
        |lookahead, parse_ctx| {
            let (expr, lookahead) = expr::expect_numeric_expr(lookahead, parse_ctx);
            (
                match parse_ctx.try_const_eval(&expr) {
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
        parse_ctx.error(&span, |error| {
            error.set_message("cannot charmap an empty string");
            diagnostics::error_label(&span).with_message("this string shouldn't be empty");
        });
    }
    lookahead
}

pub(in super::super) fn parse_newcharmap(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    require! { parse_ctx.next_token() => Token { span } @ |"identifier"(ident, _colon)| else |other| {
        parse_ctx.report_syntax_error(&other, |error, span| {
            error.add_label(
                diagnostics::error_label(span).with_message("expected a charmap name here"),
            );
        });
        return other;
    }};
    let def_span = span;
    let charmap_name = parse_ctx.identifiers.resolve(ident).unwrap().into();

    require! { parse_ctx.next_token() => |","| else |other| {
        // Just `newcharmap <name>`: create it from scratch.
        if let Err(err) = parse_ctx.charmaps.make_new(charmap_name, def_span) {
            parse_ctx.error(err.diag_span(), |error| {
                err.make_diag(error);
            })
        }

        return other;
    }};

    require! { parse_ctx.next_token() => |"identifier"(ident, _colon)| else |other| {
        parse_ctx.report_syntax_error(&other, |error, span| {
            error.add_label(
                diagnostics::error_label(span).with_message("expected a charmap name here"),
            );
        });
        return other;
    }};

    let target_name = parse_ctx.identifiers.resolve(ident).unwrap();
    if let Err(err) = parse_ctx
        .charmaps
        .make_copy(charmap_name, def_span, target_name)
    {
        parse_ctx.error(err.diag_span(), |error| {
            err.make_diag(error);
        });
    }

    parse_ctx.next_token()
}

pub(in super::super) fn parse_setcharmap(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    require! { parse_ctx.next_token() => Token { span } @ |"identifier"(ident, _colon)| else |other| {
        parse_ctx.report_syntax_error(&other, |error, span| {
            error.add_label(
                diagnostics::error_label(span).with_message("expected a charmap name here"),
            );
        });
        return other;
    }};

    let charmap_name = parse_ctx.identifiers.resolve(ident).unwrap();
    if parse_ctx.charmaps.switch_to(charmap_name).is_none() {
        parse_ctx.error(&span, |error| {
            error.set_message(format!("no charmap named \"{charmap_name}\" exists"));
            error.add_label(
                diagnostics::error_label(&span).with_message("cannot switch to this charmap"),
            );
        });
    }

    parse_ctx.next_token()
}

pub(in super::super) fn parse_pushc(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    parse_ctx.charmaps.push_active_charmap();

    require! { parse_ctx.next_token() => Token { span } @ |"identifier"(ident, _colon)| else |other| {
        return other;
    }};

    let charmap_name = parse_ctx.identifiers.resolve(ident).unwrap();
    if parse_ctx.charmaps.switch_to(charmap_name).is_none() {
        parse_ctx.error(&span, |error| {
            error.set_message(format!("no charmap named \"{charmap_name}\" exists"));
            error.add_label(
                diagnostics::error_label(&span).with_message("cannot switch to this charmap"),
            );
        });
    }

    parse_ctx.next_token()
}

pub(in super::super) fn parse_popc(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    if parse_ctx.charmaps.pop_active_charmap().is_none() {
        parse_ctx.error(&keyword.span, |error| {
            error.set_message("no entries in the charmap stack");
            error.add_label(diagnostics::error_label(&keyword.span).with_message("cannot pop"));
        })
    }

    parse_ctx.next_token()
}
