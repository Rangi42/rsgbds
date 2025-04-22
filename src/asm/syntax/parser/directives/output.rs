use crate::{diagnostics, syntax::tokens::Token};

use super::super::{expect_one_of, matches_tok, misc, parse_ctx, string};

fn parse_print_elem<'ctx_stack>(
    first_token: Option<Token<'ctx_stack>>,
    parse_ctx: &mut parse_ctx!('ctx_stack),
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
    parse_ctx: &mut parse_ctx!('ctx_stack),
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
    parse_ctx: &mut parse_ctx!('ctx_stack),
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
    parse_ctx: &mut parse_ctx!('ctx_stack),
) -> Option<Token<'ctx_stack>> {
    let (maybe_message, lookahead) = string::expect_string_expr(parse_ctx.next_token(), parse_ctx);
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
    parse_ctx: &mut parse_ctx!('ctx_stack),
) {
    let (maybe_message, lookahead) = string::expect_string_expr(parse_ctx.next_token(), parse_ctx);

    expect_one_of! {lookahead => {
        None => {},
        |"end of line"| => {},
        else |unexpected| => {
            parse_ctx.report_syntax_error(unexpected.as_ref(), |error, span| {
                error.add_label(diagnostics::error_label(span).with_message("Expected nothing else on this line"))
            });
        }
    }};

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
}

pub(in super::super) fn parse_assert<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut parse_ctx!('ctx_stack),
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(in super::super) fn parse_static_assert<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut parse_ctx!('ctx_stack),
) -> Option<Token<'ctx_stack>> {
    todo!()
}
