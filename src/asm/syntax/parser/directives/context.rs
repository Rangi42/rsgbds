use std::{cmp::Ordering, rc::Rc};

use crate::{
    diagnostics::{self, warning},
    macro_args::MacroArgs,
    sources::{NormalSpan, Source, Span, SpanKind},
    syntax::{
        lexer::LoopInfo,
        parser::{discard_rest_of_line, expect_eol},
    },
    Identifier,
};

use super::super::{expect_one_of, expr, matches_tok, parse_ctx, require, string, Token};

pub(in super::super) fn parse_include(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let (maybe_string, lookahead) = string::expect_string_expr(parse_ctx.next_token(), parse_ctx);

    // Immediately look for the end of line, since we are about to capture a block.
    let lookahead = expect_eol(lookahead, parse_ctx);

    if let Some((string, span)) = maybe_string {
        match Source::load_file(&string) {
            Ok(source) => {
                let Span::Normal(span) = &keyword.span else {
                    unreachable!()
                };
                parse_ctx.push_file(source, Some(Rc::new(span.clone())))
            }
            Err(err) => parse_ctx.error(&span, |error| {
                error.set_message(format!("unable to read path \"{}\"", string));
                error.add_label(diagnostics::error_label(&span).with_message(err));
            }),
        };
    }

    lookahead
}

pub(in super::super) fn parse_macro(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let lookahead = parse_ctx.next_token();
    require! { lookahead => Token { span } @ |"identifier"(name, _colon)| else |unexpected| {
        parse_ctx.report_syntax_error(&unexpected, |error, span| {
            error.add_label(diagnostics::error_label(span).with_message("expected the macro's name"));
        });

        return discard_rest_of_line(unexpected, parse_ctx);
    } }

    let lookahead = expect_eol(parse_ctx.next_token(), parse_ctx);

    let (body, res) = parse_ctx
        .lexer
        .capture_until_keyword("ENDM", &[], "macro definition"); // TODO: consider reintroducing nested macro definitions?
    if let Err(err) = res {
        parse_ctx.error(&span, |error| {
            error.set_message(&err);
            error.add_label(
                diagnostics::error_label(&span).with_message("macro definition starting here"),
            );
        })
    }

    parse_ctx.symbols.define_macro(
        name,
        parse_ctx.identifiers,
        span,
        body,
        parse_ctx.nb_errors_remaining,
        parse_ctx.options,
    );

    lookahead
}

pub(in super::super) fn parse_rept(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let (expr, lookahead) = expr::expect_numeric_expr(parse_ctx.next_token(), parse_ctx);

    // Immediately look for the end of line, since we are about to capture a block.
    let _ = expect_eol(lookahead, parse_ctx);

    let (body, res) = parse_ctx
        .lexer
        .capture_until_keyword("endr", &["rept", "for"], "loop");
    if let Err(err) = res {
        parse_ctx.error(&keyword.span, |error| {
            error.set_message(&err);
            error.add_label(
                diagnostics::error_label(&keyword.span).with_message("loop starting here"),
            );
        })
    }

    let lookahead = parse_ctx.next_token(); // Lex this token before switching parse contexts!

    match parse_ctx.try_const_eval(&expr) {
        Ok((0, _span)) => {} // `REPT 0` acts like `IF 0`.
        Ok((nb_iters, _span)) => {
            let Span::Normal(span) = &keyword.span else {
                unreachable!()
            };
            parse_ctx.push_loop(
                body,
                Rc::new(span.clone()),
                LoopInfo {
                    nb_iters: nb_iters as u32,
                    for_var: None,
                    ..Default::default()
                },
            );
        }
        Err(err) => parse_ctx.report_expr_error(err),
    }

    lookahead
}

pub(in super::super) fn parse_for(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let mut step_span = None;

    let (ident_maybe, start, stop, step, lookahead) = expect_one_of! { parse_ctx.next_token() => {
        Token { span } @ |"identifier"(ident, _)| => {
            expect_one_of! { parse_ctx.next_token() => {
                |","| => {
                    let (start_expr, lookahead) = expr::expect_numeric_expr(parse_ctx.next_token(), parse_ctx);
                    let start = match parse_ctx.try_const_eval(&start_expr) {
                        Ok((value, _span)) => value,
                        Err(err) => {
                            parse_ctx.report_expr_error(err);
                            0
                        },
                    };

                    let (start, end, lookahead) = if matches_tok!(lookahead, ",") {
                        let (end_expr, lookahead) = expr::expect_numeric_expr(parse_ctx.next_token(), parse_ctx);
                        match parse_ctx.try_const_eval(&end_expr) {
                            Ok((value, _span)) => (start, value, lookahead),
                            Err(err) => {
                                parse_ctx.report_expr_error(err);
                                (start, start, lookahead)
                            }
                        }
                    } else {
                        (0, start, lookahead)
                    };

                    let (step, lookahead) = if matches_tok!(lookahead, ",") {
                        let (step_expr, lookahead) = expr::expect_numeric_expr(parse_ctx.next_token(), parse_ctx);
                        match parse_ctx.try_const_eval(&step_expr) {
                            Ok((value, span)) => {
                                step_span = Some(span);
                                (value, lookahead)
                            }
                            Err(err) => {
                                parse_ctx.report_expr_error(err);
                                (1, lookahead)
                            }
                        }
                    } else {
                        (1, lookahead)
                    };

                    (Some((ident, span)), start, end, step, expect_eol(lookahead, parse_ctx))
                },
                else |unexpected| => {
                    parse_ctx.report_syntax_error(&unexpected, |error,span| {
                        error.add_label(diagnostics::error_label(span).with_message("expected a comma after the identifier here"))
                    });

                    (None, 0, 0, 1, discard_rest_of_line(unexpected, parse_ctx))
                }
            }}
        },
        else |unexpected| => {
            parse_ctx.report_syntax_error(&unexpected, |error,span| {
                error.add_label(diagnostics::error_label(span).with_message("expected an identifier here"))
            });

            (None, 0, 0, 1, discard_rest_of_line(unexpected, parse_ctx))
        }
    }};
    debug_assert!(matches_tok!(lookahead, "end of input" | "end of line"));

    let (body, res) = parse_ctx
        .lexer
        .capture_until_keyword("endr", &["rept", "for"], "loop");
    if let Err(err) = res {
        parse_ctx.error(&keyword.span, |error| {
            error.set_message(&err);
            error.add_label(
                diagnostics::error_label(&keyword.span).with_message("loop starting here"),
            );
        })
    }

    let lookahead = parse_ctx.next_token();

    // Parsing is (finally!) done; time to push the loop.

    let ident = if let Some((ident, span)) = ident_maybe {
        parse_ctx.symbols.define_constant(
            ident,
            parse_ctx.identifiers,
            span,
            start,
            true,
            false,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        );
        Some(ident)
    } else {
        None
    };

    let nb_iters = match (step.cmp(&0), stop.cmp(&start)) {
        (Ordering::Equal, _) => {
            let span = step_span.unwrap();
            parse_ctx.error(&span, |error| {
                error.set_message("`for` loops cannot have a step value of 0");
                error.add_label(
                    diagnostics::error_label(&span).with_message("this step is invalid"),
                );
            });
            0 // Don't run the loop.
        }
        (_, Ordering::Equal) => 0, // If `start == stop`, the loop isn't run at all.
        (Ordering::Greater, Ordering::Greater) => ((stop - start - 1) / step) as u32 + 1,
        (Ordering::Less, Ordering::Less) => ((start - stop - 1) / -step) as u32 + 1,
        (Ordering::Less, Ordering::Greater) | (Ordering::Greater, Ordering::Less) => {
            parse_ctx.warn(warning!("backwards-for"), &keyword.span, |warning| {
                warning.set_message("`for` loop is backwards");
                warning.add_label(
                    diagnostics::warning_label(&keyword.span).with_message(format!(
                        "this loop wants to go from {start} to {stop} in increments of {step}"
                    )),
                )
            });
            0
        }
    };

    // A `for` that doesn't run is equivalent to a `if 0`, except it also defines a variable.
    if nb_iters != 0 {
        let Span::Normal(span) = keyword.span else {
            unreachable!()
        };
        parse_ctx.push_loop(
            body,
            Rc::new(span),
            LoopInfo {
                nb_iters,
                for_var: ident,
                for_value: start,
                for_step: step,
            },
        );
    }

    lookahead
}

pub(in super::super) fn parse_break(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    // Parse up to EOL right now, since we're about to switch contexts.
    let lookahead = expect_eol(parse_ctx.next_token(), parse_ctx);

    if let Err(loop_context_somewhere) = parse_ctx.lexer.break_loop() {
        parse_ctx.error(&keyword.span, |error| {
            error.set_message("`break` used outside of a loop");
            error.add_label(diagnostics::error_label(&keyword.span).with_message("not currently inside of a loop"));
            if loop_context_somewhere {
                error.set_help("one of the parent contexts is a loop, but `break` must be used directly from the loop");
            }
        });
    }

    lookahead
}

impl parse_ctx!() {
    pub fn push_file(&mut self, file: Rc<Source>, parent: Option<Rc<NormalSpan>>) {
        self.lexer
            .push_file(file, parent, self.nb_errors_remaining, self.options);
        // Preserve the active macro args.
    }

    pub fn push_macro(
        &mut self,
        macro_name: Identifier,
        contents: NormalSpan,
        parent: Rc<NormalSpan>,
        args: MacroArgs,
    ) {
        self.lexer.push_macro(
            macro_name,
            contents,
            parent,
            self.nb_errors_remaining,
            self.options,
        );
        self.macro_args.push(args);
        self.unique_id.enter_unique_ctx();
    }

    pub fn push_loop(&mut self, contents: NormalSpan, parent: Rc<NormalSpan>, loop_info: LoopInfo) {
        self.lexer.push_loop(
            loop_info,
            contents,
            parent,
            self.nb_errors_remaining,
            self.options,
        );
        self.unique_id.enter_unique_ctx();
    }

    pub fn pop_context(&mut self) -> bool {
        let ctx = self.lexer.top_context();
        match &mut ctx.span.kind {
            SpanKind::File => self
                .lexer
                .pop_context(self.nb_errors_remaining, self.options),

            SpanKind::Macro(..) => {
                self.unique_id.exit_unique_ctx();
                let args = self.macro_args.pop();
                debug_assert!(args.is_some());
                let has_more = self
                    .lexer
                    .pop_context(self.nb_errors_remaining, self.options);
                debug_assert!(has_more);
                has_more
            }

            SpanKind::Loop(nth) => {
                self.unique_id.exit_unique_ctx();
                let loop_state = &mut ctx.loop_state;
                if *nth == loop_state.nb_iters - 1 {
                    let has_more = self
                        .lexer
                        .pop_context(self.nb_errors_remaining, self.options);
                    debug_assert!(has_more);
                    has_more
                } else {
                    *nth += 1;
                    if let Some(ident) = loop_state.for_var {
                        loop_state.for_value =
                            loop_state.for_value.wrapping_add(loop_state.for_step);

                        let span = ctx
                            .span
                            .parent
                            .as_deref()
                            .expect("Loop context should have a parent")
                            .clone();
                        self.symbols.define_constant(
                            ident,
                            self.identifiers,
                            Span::Normal(span),
                            loop_state.for_value,
                            true,  // Mutable.
                            false, // Unexported.
                            self.nb_errors_remaining,
                            self.options,
                        );
                    }
                    self.lexer
                        .reset_loop_context(self.nb_errors_remaining, self.options);
                    self.unique_id.enter_unique_ctx();
                    true // We haven't popped this context, so there's more to process.
                }
            }

            _ => unreachable!(),
        }
    }
}
