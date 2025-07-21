use std::fmt::Display;

use crate::{diagnostics, syntax::tokens::Token};

use super::super::{parse_ctx, Span};

fn process_option(string: &str, span: Span, parse_ctx: &mut parse_ctx!()) {
    match string.chars().next() {
        Some('b') => handle_error(
            parse_ctx.options.runtime_opts.parse_b(&string[1..]),
            &span,
            parse_ctx,
        ),
        Some('g') => handle_error(
            parse_ctx.options.runtime_opts.parse_g(&string[1..]),
            &span,
            parse_ctx,
        ),
        Some('p') => handle_error(
            parse_ctx.options.runtime_opts.parse_p(&string[1..]),
            &span,
            parse_ctx,
        ),
        Some('Q') => handle_error(
            parse_ctx.options.runtime_opts.parse_q(&string[1..]),
            &span,
            parse_ctx,
        ),
        Some('r') => handle_error(
            parse_ctx.options.runtime_opts.parse_r(&string[1..]),
            &span,
            parse_ctx,
        ),
        Some('W') => handle_error(
            parse_ctx.options.runtime_opts.parse_w(&string[1..]),
            &span,
            parse_ctx,
        ),
        Some(c) => parse_ctx.error(&span, |error| {
            error.set_message(format!("Unknown option '{}'", c.escape_default()));
            error.add_label(
                diagnostics::error_label(&span).with_message("This option string is invalid"),
            );
        }),
        None => parse_ctx.error(&span, |error| {
            error.set_message("Empty option string");
            error.add_label(
                diagnostics::error_label(&span).with_message("An option string is expected here"),
            );
        }),
    }

    fn handle_error<'ctx_stack, E: Display>(
        r: Result<(), E>,
        span: &Span,
        parse_ctx: &mut parse_ctx!(),
    ) {
        if let Err(err) = r {
            parse_ctx.error(span, |error| {
                error.set_message(err);
            })
        }
    }
}

pub(in super::super) fn parse_opt(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    match parse_ctx.next_token_raw() {
        None => parse_ctx.error(&keyword.span, |error| {
            error.set_message("Syntax error: missing option string");
            error.add_label(
                diagnostics::error_label(&keyword.span)
                    .with_message("`OPT` must be followed by at least one option"),
            );
        }),
        Some((first, span)) => {
            process_option(&first, span, parse_ctx);
            while let Some((option, span)) = parse_ctx.next_token_raw() {
                process_option(&option, span, parse_ctx);
            }
        }
    }

    parse_ctx.next_token()
}

pub(in super::super) fn parse_pusho(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    // Save the options before modifying any of them.
    parse_ctx
        .options
        .runtime_opt_stack
        .push(parse_ctx.options.runtime_opts.clone());

    // The rest is as if we were `opt`, except that we don't *require* any options.
    while let Some((option, span)) = parse_ctx.next_token_raw() {
        process_option(&option, span, parse_ctx);
    }

    parse_ctx.next_token()
}

pub(in super::super) fn parse_popo(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    match parse_ctx.options.runtime_opt_stack.pop() {
        Some(runtime_opts) => parse_ctx.options.runtime_opts = runtime_opts,
        None => parse_ctx.error(&keyword.span, |error| {
            error.set_message("Cannot pop from empty option stack");
            error.add_label(
                diagnostics::error_label(&keyword.span).with_message("Attempting to pop here"),
            );
        }),
    }

    parse_ctx.next_token()
}
