use std::fmt::Display;

use crate::{diagnostics, syntax::tokens::Token};

use super::super::{ParseCtx, Span};

fn process_option<'ctx_stack>(
    string: &str,
    span: Span<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) {
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
        Some(c) => diagnostics::error(
            &span,
            |error| {
                error.set_message(format!("Unknown option '{}'", c.escape_default()));
                error.add_label(
                    diagnostics::error_label(&span).with_message("This option string is invalid"),
                );
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        ),
        None => diagnostics::error(
            &span,
            |error| {
                error.set_message("Empty option string");
                error.add_label(
                    diagnostics::error_label(&span)
                        .with_message("An option string is expected here"),
                );
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        ),
    }

    fn handle_error<'ctx_stack, E: Display>(
        r: Result<(), E>,
        span: &Span<'ctx_stack>,
        parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
    ) {
        if let Err(err) = r {
            diagnostics::error(
                span,
                |error| {
                    error.set_message(err);
                },
                parse_ctx.sources,
                parse_ctx.nb_errors_remaining,
                parse_ctx.options,
            )
        }
    }
}

pub(in super::super) fn parse_opt<'ctx_stack>(
    keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    match parse_ctx.next_token_raw() {
        None => diagnostics::error(
            &keyword.span,
            |error| {
                error.set_message("Syntax error: missing option string");
                error.add_label(
                    diagnostics::error_label(&keyword.span)
                        .with_message("`OPT` must be followed by at least one option"),
                );
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        ),
        Some((first, span)) => {
            process_option(&first, span, parse_ctx);
            while let Some((option, span)) = parse_ctx.next_token_raw() {
                process_option(&option, span, parse_ctx);
            }
        }
    }

    parse_ctx.next_token()
}

pub(in super::super) fn parse_pusho<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
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

pub(in super::super) fn parse_popo<'ctx_stack>(
    keyword: Token<'ctx_stack>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> Option<Token<'ctx_stack>> {
    match parse_ctx.options.runtime_opt_stack.pop() {
        Some(runtime_opts) => parse_ctx.options.runtime_opts = runtime_opts,
        None => diagnostics::error(
            &keyword.span,
            |error| {
                error.set_message("Cannot pop from empty option stack");
                error.add_label(
                    diagnostics::error_label(&keyword.span).with_message("Attempting to pop here"),
                );
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        ),
    }

    parse_ctx.next_token()
}
