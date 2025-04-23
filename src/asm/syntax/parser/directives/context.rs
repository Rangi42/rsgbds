use std::path::Path;

use crate::diagnostics;

use super::super::{expect_one_of, expr, matches_tok, parse_ctx, require, string, Token};

pub(in super::super) fn parse_include<'ctx_stack>(
    keyword: Token<'ctx_stack>,
    parse_ctx: &mut parse_ctx!('ctx_stack),
) -> Option<Token<'ctx_stack>> {
    let (maybe_string, mut lookahead) =
        string::expect_string_expr(parse_ctx.next_token(), parse_ctx);

    // Immediately look for the end of line, since we are about to change the parse context.
    if lookahead.is_some() && !matches_tok!(lookahead, "end of line") {
        parse_ctx.report_syntax_error(lookahead.as_ref(), |error, span| {
            error.add_label(
                diagnostics::error_label(span).with_message("Expected nothing else on this line"),
            )
        });
        while !matches_tok!(lookahead, "end of line") {
            lookahead = parse_ctx.next_token();
        }
    }

    if let Some((string, span)) = maybe_string {
        let path = string.as_ref();
        match parse_ctx.sources.load_file(path) {
            Ok(source) => parse_ctx
                .ctx_stack
                .sources_mut()
                .push_file_context(source, &keyword.span),
            Err(err) => diagnostics::error(
                &span,
                |error| {
                    error.set_message(format!("Unable to read path \"{}\"", path.display()));
                    error.add_label(diagnostics::error_label(&span).with_message(err));
                },
                parse_ctx.sources,
                parse_ctx.nb_errors_remaining,
                parse_ctx.options,
            ),
        };
    }

    lookahead
}

pub(in super::super) fn parse_macro<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut parse_ctx!('ctx_stack),
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(in super::super) fn parse_rept<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut parse_ctx!('ctx_stack),
) -> Option<Token<'ctx_stack>> {
    todo!()
}

pub(in super::super) fn parse_for<'ctx_stack>(
    _keyword: Token<'ctx_stack>,
    parse_ctx: &mut parse_ctx!('ctx_stack),
) -> Option<Token<'ctx_stack>> {
    todo!()
}
