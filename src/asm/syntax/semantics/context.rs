use std::{cmp::Ordering, rc::Rc};

use compact_str::CompactString;

use crate::{
    diagnostics::{self, warning},
    expr::Expr,
    macro_args::MacroArgs,
    sources::{NormalSpan, Source, Span, SpanKind},
    syntax::lexer::LoopInfo,
    Identifier,
};

use super::parse_ctx;

impl parse_ctx!() {
    pub fn call_macro(&mut self, name: Identifier, name_span_id: usize, args: MacroArgs) {
        let name_span = self.nth_span(name_span_id);

        let name_str = self.identifiers.resolve(name).unwrap();
        match self.symbols.find_macro(&name) {
            Err(maybe_deleted) => self.error(&name_span, |error| {
                error.set_message(format!("macro `{name_str}` does not exist"));
                error.add_label(
                    diagnostics::error_label(&name_span)
                        .with_message("attempting to call the macro here"),
                );
                if let Some(del_span) = maybe_deleted {
                    error.add_label(
                        diagnostics::error_label(del_span).with_message("it has been deleted here"),
                    );
                }
            }),
            Ok(Err(other)) => self.error(&name_span, |error| {
                error.set_message(format!("`{name_str}` is not a macro"));
                error.add_labels([
                    diagnostics::error_label(&name_span).with_message("Macro call here"),
                    diagnostics::note_label(other.def_span())
                        .with_message("a symbol by this name was defined here"),
                ]);
            }),
            Ok(Ok(slice)) => {
                push_macro(
                    self,
                    name,
                    slice.clone(),
                    Rc::new(name_span.extract_normal()),
                    args,
                );
            }
        }
    }

    pub fn capture_macro_def(&mut self, name: Identifier, span_idx: usize) {
        let def_span = self.nth_span(span_idx);

        let (body, res) = self
            .lexer
            .capture_until_keyword("endm", &[], "macro definition"); // TODO: consider reintroducing nested macro defs?
        if let Err(err) = res {
            self.error(&def_span, |error| {
                error.set_message(&err);
                error.add_label(
                    diagnostics::error_label(&def_span)
                        .with_message("macro definition starting here"),
                );
            });
        }

        self.symbols.define_macro(
            name,
            self.identifiers,
            def_span,
            body,
            self.sections
                .active_section
                .as_ref()
                .map(|active| &active.sym_section),
            self.macro_args.last(),
            self.nb_errors_left,
            self.options,
        );
    }

    pub fn capture_loop(
        &mut self,
        for_var: Option<(Identifier, Span)>,
        (start, end, step): (Option<Expr>, Expr, Option<Expr>),
        span: Span,
    ) {
        let start = match start.map(|expr| self.try_const_eval(&expr)) {
            Some(Ok((value, _span))) => value,
            Some(Err(err)) => {
                self.report_expr_error(err);
                0
            }
            None => 0,
        };
        let (step, step_span) = match step.map(|expr| self.try_const_eval(&expr)) {
            Some(Ok((value, span))) => (value, Some(span)),
            Some(Err(err)) => {
                self.report_expr_error(err);
                (1, None)
            }
            None => (1, None),
        };
        let end = match self.try_const_eval(&end) {
            Ok((value, _span)) => value,
            Err(err) => {
                self.report_expr_error(err);
                start + step // Try to have the loop run at least once.
            }
        };

        let (body, res) = self
            .lexer
            .capture_until_keyword("endr", &["rept", "for"], "loop");
        if let Err(err) = res {
            self.error(&span, |error| {
                error.set_message(&err);
                error.add_label(diagnostics::error_label(&span).with_message("loop starting here"));
            });
        }

        let ident = for_var.map(|(ident, span)| {
            self.symbols.define_constant(
                ident,
                self.identifiers,
                span,
                start,
                true,  // Mutable.
                false, // Unexported.
                false, // Not redefining.
                self.sections
                    .active_section
                    .as_ref()
                    .map(|active| &active.sym_section),
                self.macro_args.last(),
                self.nb_errors_left,
                self.options,
            );
            ident
        });

        let nb_iters = match (step.cmp(&0), end.cmp(&start)) {
            (Ordering::Equal, _) => {
                let span = step_span.unwrap();
                self.error(&span, |error| {
                    error.set_message("`for` loops cannot have a step value of 0");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("this step is invalid"),
                    );
                });
                0 // Don't run the loop.
            }
            (_, Ordering::Equal) => 0, // If `start == end`, the loop isn't run at all.
            (Ordering::Greater, Ordering::Greater) => ((end - start - 1) / step) as u32 + 1,
            (Ordering::Less, Ordering::Less) => ((start - end - 1) / -step) as u32 + 1,
            (Ordering::Less, Ordering::Greater) | (Ordering::Greater, Ordering::Less) => {
                self.warn(warning!("backwards-for"), &span, |warning| {
                    warning.set_message("`for` loop is backwards");
                    warning.add_label(diagnostics::warning_label(&span).with_message(format!(
                        "this loop wants to go from {start} to {end} in increments of {step}"
                    )))
                });
                0
            }
        };

        // A loop that doesn't run is equivalent to `if 0`, except that a `for` still creates its variable.
        if nb_iters != 0 {
            let span = span.extract_normal();
            push_loop(
                self,
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
    }

    pub fn include_file(&mut self, (path, span): (CompactString, Span)) {
        match Source::load_file(&path) {
            Ok(source) => push_file(self, source, Some(Rc::new(span.extract_normal()))),
            Err(err) => self.error(&span, |error| {
                error.set_message("unable to read the file at \"{path}\"");
                error.add_label(diagnostics::error_label(&span).with_message(err));
            }),
        }
    }
}

fn push_file(parse_ctx: &mut parse_ctx!(), file: Rc<Source>, parent: Option<Rc<NormalSpan>>) {
    // We don't do anything if this fails to push.
    let _ = parse_ctx
        .lexer
        .push_file(file, parent, parse_ctx.nb_errors_left, parse_ctx.options);
    // Preserve the active macro args and unique ID.
}

fn push_macro(
    parse_ctx: &mut parse_ctx!(),
    macro_name: Identifier,
    contents: NormalSpan,
    parent: Rc<NormalSpan>,
    args: MacroArgs,
) {
    if parse_ctx
        .lexer
        .push_macro(
            macro_name,
            contents,
            parent,
            parse_ctx.nb_errors_left,
            parse_ctx.options,
        )
        .is_ok()
    {
        parse_ctx.macro_args.push(args);
        parse_ctx.unique_id.enter_unique_ctx();
    }
}

fn push_loop(
    parse_ctx: &mut parse_ctx!(),
    contents: NormalSpan,
    parent: Rc<NormalSpan>,
    loop_info: LoopInfo,
) {
    if parse_ctx
        .lexer
        .push_loop(
            loop_info,
            contents,
            parent,
            parse_ctx.nb_errors_left,
            parse_ctx.options,
        )
        .is_ok()
    {
        parse_ctx.unique_id.enter_unique_ctx();
    }
}

impl parse_ctx!() {
    pub fn pop_context(&mut self) -> bool {
        let ctx = self.lexer.top_context();
        match &mut ctx.span.node.kind {
            SpanKind::File => self.lexer.pop_context(self.nb_errors_left, self.options),

            SpanKind::Macro(..) => {
                self.unique_id.exit_unique_ctx();
                let args = self.macro_args.pop();
                debug_assert!(args.is_some());
                let has_more = self.lexer.pop_context(self.nb_errors_left, self.options);
                debug_assert!(has_more);
                has_more
            }

            SpanKind::Loop(nth) => {
                self.unique_id.exit_unique_ctx();
                let loop_state = &mut ctx.loop_state;

                if let Some(ident) = loop_state.for_var {
                    loop_state.for_value = loop_state.for_value.wrapping_add(loop_state.for_step);

                    let span = ctx
                        .span
                        .node
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
                        false, // Not redefining.
                        self.sections
                            .active_section
                            .as_ref()
                            .map(|active| &active.sym_section),
                        self.macro_args.last(),
                        self.nb_errors_left,
                        self.options,
                    );
                }

                if *nth == loop_state.nb_iters - 1 {
                    let has_more = self.lexer.pop_context(self.nb_errors_left, self.options);
                    debug_assert!(has_more);
                    has_more
                } else {
                    *nth += 1;
                    self.lexer
                        .reset_loop_context(self.nb_errors_left, self.options);
                    self.unique_id.enter_unique_ctx();
                    true // We haven't popped this context, so there's more to process.
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn break_loop(&mut self, span_idx: usize) {
        let span = self.nth_span(span_idx);

        match self.lexer.break_loop() {
            Ok(()) => self.unique_id.exit_unique_ctx(),

            Err(is_some_parent_a_loop) => self.error(&span, |error| {
                error.set_message("`break` used outside of a loop");
                error.add_label(diagnostics::error_label(&span).with_message("this is not directly inside of a loop"));
                if is_some_parent_a_loop {
                    error.set_help("a parent context is a loop, but `break` must be used directly inside of a loop");
                }
            }),
        }
    }
}
