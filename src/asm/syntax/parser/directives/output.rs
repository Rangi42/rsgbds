use compact_str::CompactString;

use crate::{
    diagnostics,
    section::{AssertLevel, Assertion, LinkTimeExpr},
    sources::Span,
    syntax::{
        parser::{expect_eol, expr},
        tokens::Token,
    },
};

use super::super::{expect_one_of, matches_tok, misc, parse_ctx, string};

fn parse_print_elem(first_token: Token, parse_ctx: &mut parse_ctx!()) -> (Option<()>, Token) {
    let (expr, lookahead) = misc::parse_str_or_expr(first_token, parse_ctx);
    match expr {
        None => return (None, lookahead),
        Some(misc::StrOrNum::Num(expr)) => match parse_ctx.try_const_eval(&expr) {
            Ok((value, _span)) => print!("${value:X}"),
            Err(error) => parse_ctx.report_expr_error(error),
        },
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

    expect_eol(lookahead, parse_ctx);

    if let Some((message, _span)) = maybe_message {
        parse_ctx.error(&keyword.span, |error| {
            error.set_message(message);
            error.add_label(
                diagnostics::error_label(&keyword.span)
                    .with_message("assembly aborted because of this"),
            );
        });
    }

    None // Abort further parsing.
}

pub(in super::super) fn parse_assert(
    keyword: Token,
    parse_ctx: &mut parse_ctx!(),
) -> Option<Token> {
    let (level, lookahead) = expect_one_of! { parse_ctx.next_token() => {
        |"warn"| => {
            let lookahead = expect_one_of! { parse_ctx.next_token() => {
                |","| => parse_ctx.next_token(),
                else |unexpected| => {
                    parse_ctx.report_syntax_error(&unexpected, |error, span| {
                        error.add_label(diagnostics::error_label(span).with_message("expected a comma here"));
                    });
                    unexpected
                }
            }};
            (AssertLevel::Warn, lookahead)
        },
        |"fail"| => {
            let lookahead = expect_one_of! { parse_ctx.next_token() => {
                |","| => parse_ctx.next_token(),
                else |unexpected| => {
                    parse_ctx.report_syntax_error(&unexpected, |error, span| {
                        error.add_label(diagnostics::error_label(span).with_message("expected a comma here"));
                    });
                    unexpected
                }
            }};
            (AssertLevel::Error, lookahead)
        },
        |"fatal"| => {
            let lookahead = expect_one_of! { parse_ctx.next_token() => {
                |","| => parse_ctx.next_token(),
                else |unexpected| => {
                    parse_ctx.report_syntax_error(&unexpected, |error, span| {
                        error.add_label(diagnostics::error_label(span).with_message("expected a comma here"));
                    });
                    unexpected
                }
            }};
            (AssertLevel::Fatal, lookahead)
        },
        else |unexpected| => (AssertLevel::Error, unexpected)
    }};

    let (expr, lookahead) = expr::expect_numeric_expr(lookahead, parse_ctx);
    let (msg, lookahead) = expect_one_of! { lookahead => {
        |","| => {
            let (msg, lookahead) = string::expect_string_expr(parse_ctx.next_token(), parse_ctx);
            (msg, lookahead)
        },
        else |unexpected| => (None, unexpected)
    }};

    match expr.prep_for_patch(
        parse_ctx.symbols,
        parse_ctx.macro_args.last(),
        parse_ctx.sections,
    ) {
        Ok((0, span)) => assert_failure(level, msg.as_ref(), &span, parse_ctx),
        Ok(_) => Some(()), // OK
        Err(Err(error)) => {
            parse_ctx.report_expr_error(error);
            Some(())
        }
        Err(Ok(expr)) => {
            parse_ctx.sections.assertions.push(Assertion {
                level,
                rest: LinkTimeExpr {
                    span: keyword.span,
                    expr,
                    offset: 0, // TODO: unused, move it to the `Patch` instead
                    pc: parse_ctx
                        .sections
                        .active_section
                        .as_ref()
                        .map(|(_data_sect, sym_sect)| (sym_sect.id, sym_sect.offset)),
                },
            });
            Some(())
        }
    }
    .map(|()| lookahead)
}

pub(in super::super) fn parse_static_assert(
    _keyword: Token,
    parse_ctx: &mut parse_ctx!(),
) -> Option<Token> {
    let (level, lookahead) = expect_one_of! { parse_ctx.next_token() => {
        |"warn"| => {
            let lookahead = expect_one_of! { parse_ctx.next_token() => {
                |","| => parse_ctx.next_token(),
                else |unexpected| => {
                    parse_ctx.report_syntax_error(&unexpected, |error, span| {
                        error.add_label(diagnostics::error_label(span).with_message("expected a comma here"));
                    });
                    unexpected
                }
            }};
            (AssertLevel::Warn, lookahead)
        },
        |"fail"| => {
            let lookahead = expect_one_of! { parse_ctx.next_token() => {
                |","| => parse_ctx.next_token(),
                else |unexpected| => {
                    parse_ctx.report_syntax_error(&unexpected, |error, span| {
                        error.add_label(diagnostics::error_label(span).with_message("expected a comma here"));
                    });
                    unexpected
                }
            }};
            (AssertLevel::Error, lookahead)
        },
        |"fatal"| => {
            let lookahead = expect_one_of! { parse_ctx.next_token() => {
                |","| => parse_ctx.next_token(),
                else |unexpected| => {
                    parse_ctx.report_syntax_error(&unexpected, |error, span| {
                        error.add_label(diagnostics::error_label(span).with_message("expected a comma here"));
                    });
                    unexpected
                }
            }};
            (AssertLevel::Fatal, lookahead)
        },
        else |unexpected| => (AssertLevel::Error, unexpected)
    }};

    let (expr, lookahead) = expr::expect_numeric_expr(lookahead, parse_ctx);
    let (msg, lookahead) = expect_one_of! { lookahead => {
        |","| => {
            let (msg, lookahead) = string::expect_string_expr(parse_ctx.next_token(), parse_ctx);
            (msg, lookahead)
        },
        else |unexpected| => (None, unexpected)
    }};

    match parse_ctx.try_const_eval(&expr) {
        Ok((0, span)) => assert_failure(level, msg.as_ref(), &span, parse_ctx),
        Ok(_) => Some(()), // OK
        Err(error) => {
            parse_ctx.report_expr_error(error);
            Some(())
        }
    }
    .map(|()| lookahead)
}

fn assert_failure(
    level: AssertLevel,
    msg: Option<&(CompactString, Span)>,
    span: &Span,
    parse_ctx: &mut parse_ctx!(),
) -> Option<()> {
    match level {
        AssertLevel::Warn => {
            parse_ctx.error(span, |error| {
                error.set_message("assertion failed");
                error.add_label(diagnostics::error_label(span).with_message(match msg {
                    Some((text, _span)) => text,
                    None => "this expression evaluates to 0",
                }));
            });
            Some(())
        }
        AssertLevel::Error | AssertLevel::Fatal => {
            parse_ctx.error(span, |error| {
                error.set_message("assertion failed");
                error.add_label(diagnostics::error_label(span).with_message(match msg {
                    Some((text, _span)) => text,
                    None => "this expression evaluates to 0",
                }));
            });
            matches!(level, AssertLevel::Error).then_some(())
        }
    }
}
