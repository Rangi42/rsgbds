use std::rc::Rc;

use crate::{
    diagnostics,
    sources::{Source, Span},
};

use super::super::{expect_one_of, expr, matches_tok, parse_ctx, require, string, Token};

pub(in super::super) fn parse_include(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let (maybe_string, mut lookahead) =
        string::expect_string_expr(parse_ctx.next_token(), parse_ctx);

    // Immediately look for the end of line, since we are about to change the parse context.
    if !matches_tok!(lookahead, "end of line" | "end of input") {
        parse_ctx.report_syntax_error(&lookahead, |error, span| {
            error.add_label(
                diagnostics::error_label(span).with_message("Expected nothing else on this line"),
            );
        });
        // Discard the rest of the line.
        while !matches_tok!(lookahead, "end of line" | "end of input") {
            lookahead = parse_ctx.next_token();
        }
    }

    if let Some((string, span)) = maybe_string {
        match Source::load_file(&string) {
            Ok(source) => {
                let Span::Normal(span) = &keyword.span else {
                    unreachable!()
                };
                parse_ctx
                    .lexer
                    .push_file(source, Some(Rc::new(span.clone())))
            }
            Err(err) => parse_ctx.error(&span, |error| {
                error.set_message(format!("Unable to read path \"{}\"", string));
                error.add_label(diagnostics::error_label(&span).with_message(err));
            }),
        };
    }

    lookahead
}

pub(in super::super) fn parse_macro(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let mut lookahead = parse_ctx.next_token();
    require! { lookahead => Token { span } @ |"identifier"(name, _colon)| else |unexpected| {
        parse_ctx.report_syntax_error(&unexpected, |error, span| {
            error.add_label(diagnostics::error_label(span).with_message("Expected the macro's name"));
        });

        let mut lookahead = unexpected;
        while !matches_tok!(lookahead, "end of line" | "end of input") {
            lookahead = parse_ctx.next_token();
        }
        return lookahead;
    } }

    lookahead = parse_ctx.next_token();
    require! { lookahead => |"end of line"| else |unexpected| {
        parse_ctx.report_syntax_error(&unexpected, |error, span| {
            error.add_label(diagnostics::error_label(span).with_message("Expected nothing else on this line"));
        });

        let mut lookahead = unexpected;
        while !matches_tok!(lookahead, "end of line" | "end of input") {
            lookahead = parse_ctx.next_token();
        }
        return lookahead;
    } }

    let (body, res) = parse_ctx.lexer.capture_until_keyword(
        "ENDM",
        "macro definition",
        &parse_ctx.nb_errors_remaining,
        &parse_ctx.options,
    );

    parse_ctx.next_token()
}

pub(in super::super) fn parse_rept(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(in super::super) fn parse_for(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}
