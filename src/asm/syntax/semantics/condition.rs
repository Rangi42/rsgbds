use crate::{diagnostics, expr::Expr};

use super::parse_ctx;

impl parse_ctx!() {
    pub fn process_if(&mut self, span_idx: usize, expr: Expr) {
        let should_skip = match self.try_const_eval(&expr) {
            Ok((value, _span)) => value == 0,
            Err(err) => {
                self.report_expr_error(err);
                false // Default to executing the `if`.
            }
        };

        let span = self.nth_span(span_idx);

        if should_skip {
            if let Err(err) = self.lexer.skip_conditional_block() {
                self.error(&span, |error| {
                    error.set_message(&err);
                    error.add_label(
                        diagnostics::error_label(&span)
                            .with_message("this `if` is missing a corresponding `endc`"),
                    );
                });
                return; // Avoid reporting the error a second time when the lexer context is exited.
            }
        }

        // This consumes the span, so it's done last.
        crate::cond::enter_conditional(&mut self.lexer.cond_stack, span, !should_skip);
    }

    pub fn process_elif(&mut self, span_idx: usize, cond_expr: Option<Expr>) {
        let res = match cond_expr {
            // This is `None` if we are coming from a `if`, and thus the condition was skipped at a lexical level.
            None => self.lexer.skip_conditional_block(),

            Some(expr) => {
                let should_skip = match self.try_const_eval(&expr) {
                    Ok((value, _span)) => value == 0,
                    Err(err) => {
                        self.report_expr_error(err);
                        false // Default to executing the `elif`.
                    }
                };
                if should_skip {
                    self.lexer.skip_conditional_block()
                } else {
                    if let Some(condition) = self.lexer.active_condition_mut() {
                        condition.entered_block = true;
                    }
                    Ok(())
                }
            }
        };

        let span = &self.line_spans[span_idx];

        if let Err(err) = res {
            self.error(span, |error| {
                error.set_message(&err);
                error.add_label(
                    diagnostics::error_label(span)
                        .with_message("this `elif` is missing a corresponding `endc`"),
                );
            });
            // Avoid reporting the error a second time.
            let min_stack_depth = self.lexer.top_context_mut().cond_stack_depth;
            crate::cond::exit_conditional(
                &mut self.lexer.cond_stack,
                min_stack_depth,
                span,
                self.nb_errors_left,
                self.options,
            );
        } else if let Some(cond) = self.lexer.active_condition_mut() {
            if let Some(else_span) = &cond.else_span {
                // Can't borrow all of `self`, so we must explicitly borrow the parts of `self`.
                diagnostics::error(
                    span,
                    |error| {
                        error.set_message("`elif` found after `else`");
                        error.add_labels([
                            diagnostics::error_label(span).with_message("this `elif`..."),
                            diagnostics::error_label(else_span)
                                .with_message("...cannot be after this `else`"),
                        ])
                    },
                    self.nb_errors_left,
                    self.options,
                );
            }
        } else {
            self.error(span, |error| {
                error.set_message("`elif` found outside of a conditional block");
                error
                    .add_label(diagnostics::error_label(span).with_message("no `if` matches this"));
            });
        }
    }

    pub fn process_else(&mut self, span_idx: usize) {
        let span = self.nth_span(span_idx);

        if let Some(cond) = self.lexer.active_condition_mut() {
            if let Some(else_span) = &cond.else_span {
                diagnostics::error(
                    &span,
                    |error| {
                        error.set_message("`else` found after another `else`");
                        error.add_labels([
                            diagnostics::error_label(&span).with_message("this `else`..."),
                            diagnostics::error_label(else_span)
                                .with_message("...cannot be after this `else`"),
                        ]);
                    },
                    self.nb_errors_left,
                    self.options,
                );
            }

            cond.else_span = Some(span.clone());

            if cond.entered_block {
                if let Err(err) = self.lexer.skip_conditional_block() {
                    diagnostics::error(
                        &span,
                        |error| {
                            error.set_message(&err);
                            error.add_label(
                                diagnostics::error_label(&span)
                                    .with_message("this `else` is missing a corresponding `endc`"),
                            );
                        },
                        self.nb_errors_left,
                        self.options,
                    );
                }
            }
        } else {
            self.error(&span, |error| {
                error.set_message("`else` found outside of a conditional block");
                error.add_label(
                    diagnostics::error_label(&span).with_message("no `if` matches this"),
                );
            });
        }
    }

    pub fn process_endc(&mut self, span_idx: usize) {
        let span = &self.line_spans[span_idx];

        let min_stack_depth = self.lexer.top_context_mut().cond_stack_depth;
        crate::cond::exit_conditional(
            &mut self.lexer.cond_stack,
            min_stack_depth,
            span,
            self.nb_errors_left,
            self.options,
        );
    }
}
