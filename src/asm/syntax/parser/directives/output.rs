use crate::{diagnostics, syntax::tokens::Token};

use super::super::{expect_one_of, matches_tok, misc, parse_ctx, string};

fn parse_print_elem(first_token: Token, parse_ctx: &mut parse_ctx!()) -> (Option<()>, Token) {
    let (expr, lookahead) = misc::parse_str_or_const_expr(first_token, parse_ctx);
    match expr {
        None => return (None, lookahead),
        Some(misc::StrOrNum::Num(value)) => print!("${value:X}"),
        Some(misc::StrOrNum::String(string)) => print!("{string}"),
    };
    (Some(()), lookahead)
}
pub(in super::super) fn parse_println(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let mut lookahead = parse_ctx.next_token();
    // Allow a lack of arguments.
    if !matches_tok!(lookahead, "end of line") {
        let (_, new_lookahead) = misc::parse_comma_list(parse_print_elem, lookahead, parse_ctx);
        lookahead = new_lookahead;
    }
    println!();
    lookahead
}
pub(in super::super) fn parse_print(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let mut lookahead = parse_ctx.next_token();
    // Allow a lack of arguments.
    if !matches_tok!(lookahead, "end of line") {
        let (_, new_lookahead) = misc::parse_comma_list(parse_print_elem, lookahead, parse_ctx);
        lookahead = new_lookahead;
    }
    lookahead
}

pub(in super::super) fn parse_warn(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let (maybe_message, lookahead) = string::expect_string_expr(parse_ctx.next_token(), parse_ctx);
    if let Some((message, _span)) = maybe_message {
        parse_ctx.warn(diagnostics::warning!("user"), &keyword.span, |warning| {
            warning.set_message(message);
            warning.add_label(diagnostics::warning_label(&keyword.span))
        });
    }
    lookahead
}
pub(in super::super) fn parse_fail(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Option<Token> {
    let (maybe_message, lookahead) = string::expect_string_expr(parse_ctx.next_token(), parse_ctx);

    expect_one_of! {lookahead => {
        |"end of input"| => {},
        |"end of line"| => {},
        else |unexpected| => {
            parse_ctx.report_syntax_error(&unexpected, |error, span| {
                error.add_label(diagnostics::error_label(span).with_message("expected nothing else on this line"))
            });
        }
    }};

    if let Some((message, _span)) = maybe_message {
        parse_ctx.error(&keyword.span, |error| {
            error.set_message(message);
            error.add_label(
                diagnostics::error_label(&keyword.span).with_message("assembly aborted here"),
            );
        });
    }

    None // Abort further parsing.
}

pub(in super::super) fn parse_assert(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(in super::super) fn parse_static_assert(
    _keyword: Token,
    parse_ctx: &mut parse_ctx!(),
) -> Token {
    todo!()
}
