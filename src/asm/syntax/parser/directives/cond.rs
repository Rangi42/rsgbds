use crate::{
    diagnostics,
    syntax::parser::{expect_eol, expr},
};

use super::{parse_ctx, Token};

pub(in super::super) fn parse_if(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let (expr, lookahead) = expr::expect_numeric_expr(parse_ctx.next_token(), parse_ctx);
    let should_skip = match expr.try_const_eval() {
        Ok((value, _span)) => value == 0,
        Err(err) => {
            parse_ctx.report_expr_error(err);
            false // Default to executing the `if`.
        }
    };

    // Immediately look for the end of line, since we may be about to skip the block.
    let lookahead = expect_eol(lookahead, parse_ctx);

    if should_skip {
        if let Err(err) = parse_ctx.lexer.skip_conditional_block() {
            parse_ctx.error(&keyword.span, |error| {
                error.set_message(&err);
                error.add_label(
                    diagnostics::error_label(&keyword.span)
                        .with_message("This `IF` is missing a corresponding `ENDC`"),
                );
            });
            parse_ctx.lexer.cond_stack.pop(); // Avoid reporting the error a second time when the lexer context is exited.
        }
    }

    // This consumes the span, so it's done last.
    crate::cond::enter_conditional(&mut parse_ctx.lexer.cond_stack, keyword.span, !should_skip);

    lookahead
}

pub(in super::super) fn parse_elif(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let (lookahead, res) = match crate::cond::active_condition_mut(&mut parse_ctx.lexer) {
        Some(cond) if cond.entered_block => {
            // HACK: if we were executing the previous block, we need not to lex the remainder of the block!
            //       This enables code like:
            // ```
            // IF _NARG < 1
            // ; ...
            // ELIF \1 == 0
            // ```
            parse_ctx.lexer.skip_to_eol();
            let lookahead = parse_ctx.next_token();
            (lookahead, parse_ctx.lexer.skip_conditional_block())
        }
        _ => {
            let (expr, lookahead) = expr::expect_numeric_expr(parse_ctx.next_token(), parse_ctx);
            let should_skip = match expr.try_const_eval() {
                Ok((value, _span)) => value == 0,
                Err(err) => {
                    parse_ctx.report_expr_error(err);
                    false // Default to executing the `elif`.
                }
            };

            (
                lookahead,
                if should_skip {
                    parse_ctx.lexer.skip_conditional_block()
                } else {
                    if let Some(condition) = crate::cond::active_condition_mut(&mut parse_ctx.lexer)
                    {
                        condition.entered_block = true;
                    }
                    Ok(())
                },
            )
        }
    };

    if let Err(err) = res {
        parse_ctx.error(&keyword.span, |error| {
            error.set_message(&err);
            error.add_label(
                diagnostics::error_label(&keyword.span)
                    .with_message("This `ELIF` is missing a corresponding `ENDC`"),
            );
        });
        parse_ctx.lexer.cond_stack.pop(); // Avoid reporting the error a second time when the lexer context is exited.
    } else if let Some(cond) = crate::cond::active_condition_mut(&mut parse_ctx.lexer) {
        if let Some(span) = &cond.else_span {
            diagnostics::error(
                &keyword.span,
                |error| {
                    error.set_message("`ELIF` found after `ELSE`");
                    error.add_labels([
                        diagnostics::error_label(&keyword.span).with_message("This `ELIF`..."),
                        diagnostics::error_label(span)
                            .with_message("...cannot be after this `ELSE`"),
                    ])
                },
                parse_ctx.nb_errors_remaining,
                parse_ctx.options,
            )
        }
    } else {
        parse_ctx.error(&keyword.span, |error| {
            error.set_message("`ELIF` found outside of a conditional block");
            error.add_label(
                diagnostics::error_label(&keyword.span).with_message("No `IF` matches this"),
            );
        });
    }

    lookahead
}

pub(in super::super) fn parse_else(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let lookahead = expect_eol(parse_ctx.next_token(), parse_ctx);

    if let Some(cond) = crate::cond::active_condition_mut(&mut parse_ctx.lexer) {
        if let Some(span) = &cond.else_span {
            diagnostics::error(
                &keyword.span,
                |error| {
                    error.set_message("`ELSE` found after another `ELSE`");
                    error.add_labels([
                        diagnostics::error_label(&keyword.span).with_message("This `ELSE`..."),
                        diagnostics::error_label(span)
                            .with_message("...cannot be after this `ELSE`"),
                    ]);
                },
                parse_ctx.nb_errors_remaining,
                parse_ctx.options,
            );
        }

        cond.else_span = Some(keyword.span.clone());

        if cond.entered_block {
            if let Err(err) = parse_ctx.lexer.skip_conditional_block() {
                diagnostics::error(
                    &keyword.span,
                    |error| {
                        error.set_message(&err);
                        error.add_label(
                            diagnostics::error_label(&keyword.span)
                                .with_message("This `ELSE` is missing a corresponding `ENDC`"),
                        );
                    },
                    parse_ctx.nb_errors_remaining,
                    parse_ctx.options,
                );
            }
        }
    } else {
        parse_ctx.error(&keyword.span, |error| {
            error.set_message("`ENDC` found outside of a conditional block");
            error.add_label(
                diagnostics::error_label(&keyword.span).with_message("No `IF` matches this"),
            );
        });
    }

    lookahead
}

pub(in super::super) fn parse_endc(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let lookahead = expect_eol(parse_ctx.next_token(), parse_ctx);

    if !crate::cond::exit_conditional(&mut parse_ctx.lexer.cond_stack) {
        parse_ctx.error(&keyword.span, |error| {
            error.set_message("`ENDC` found outside of a conditional block");
            error.add_label(
                diagnostics::error_label(&keyword.span).with_message("No `IF` matches this"),
            );
        });
    }

    lookahead
}
