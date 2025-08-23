use std::{cell::Cell, fmt::Display};

use crate::{diagnostics, sources::Span, Options};

use super::parse_ctx;

impl parse_ctx!() {
    pub fn process_option(&mut self, string: &str, span_idx: usize) {
        let span = &self.line_spans[span_idx];

        let mut chars = string.chars();
        let option_char = chars.next();
        let option_arg = chars
            .as_str()
            .trim_start_matches(crate::syntax::lexer::is_whitespace);

        match option_char {
            Some('b') => handle_error(
                self.options.runtime_opts.parse_b(option_arg),
                span,
                self.nb_errors_left,
                self.options,
            ),
            Some('g') => handle_error(
                self.options.runtime_opts.parse_g(option_arg),
                span,
                self.nb_errors_left,
                self.options,
            ),
            Some('p') => handle_error(
                self.options.runtime_opts.parse_p(option_arg),
                span,
                self.nb_errors_left,
                self.options,
            ),
            Some('Q') => handle_error(
                self.options.runtime_opts.parse_q(option_arg),
                span,
                self.nb_errors_left,
                self.options,
            ),
            Some('r') => handle_error(
                self.options
                    .runtime_opts
                    .parse_r(option_arg)
                    .map(|new_depth| {
                        self.lexer.set_recursion_depth(
                            new_depth,
                            span,
                            self.nb_errors_left,
                            self.options,
                        )
                    }),
                span,
                self.nb_errors_left,
                self.options,
            ),
            Some('W') => handle_error(
                self.options.runtime_opts.parse_w(option_arg),
                span,
                self.nb_errors_left,
                self.options,
            ),
            Some(c) => self.error(span, |error| {
                error.set_message(format!("unknown option '{}'", c.escape_default()));
                error.add_label(
                    diagnostics::error_label(span).with_message("this option string is invalid"),
                );
            }),
            None => self.error(span, |error| {
                error.set_message("empty option string");
                error.add_label(
                    diagnostics::error_label(span)
                        .with_message("an option string is expected here"),
                );
            }),
        }

        fn handle_error<E: Display>(
            r: Result<(), E>,
            span: &Span,
            nb_errors_left: &Cell<usize>,
            options: &Options,
        ) {
            if let Err(err) = r {
                diagnostics::error(
                    span,
                    |error| {
                        error.set_message(err);
                    },
                    nb_errors_left,
                    options,
                );
            }
        }
    }

    pub fn push_options(&mut self) {
        self.options
            .runtime_opt_stack
            .push(self.options.runtime_opts.clone());
    }

    pub fn pop_options(&mut self, span_idx: usize) {
        let span = &self.line_spans[span_idx];

        match self.options.runtime_opt_stack.pop() {
            Some(runtime_opts) => self.options.runtime_opts = runtime_opts,
            None => self.error(span, |error| {
                error.set_message("cannot pop from empty option stack");
                error.add_label(
                    diagnostics::error_label(span).with_message("attempting to pop here"),
                );
            }),
        }
    }
}
