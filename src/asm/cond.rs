use std::cell::Cell;

use crate::{diagnostics, sources::Span, Options};

#[derive(Debug)]
pub struct Condition {
    pub opening_span: Span,
    pub entered_block: bool,
    pub else_span: Option<Span>,
}

pub fn enter_conditional(conditions: &mut Vec<Condition>, opening_span: Span, entered_block: bool) {
    conditions.push(Condition {
        opening_span,
        entered_block,
        else_span: None,
    });
}

pub fn exit_conditional(
    conditions: &mut Vec<Condition>,
    min_cond_depth: usize,
    span: &Span,
    nb_errors_left: &Cell<usize>,
    options: &Options,
) {
    debug_assert!(conditions.len() >= min_cond_depth);
    if conditions.len() == min_cond_depth {
        diagnostics::error(
            span,
            |error| {
                error.set_message("`endc` found outside of a conditional block");
                error
                    .add_label(diagnostics::error_label(span).with_message("no `if` matches this"));
                // TODO: if a conditional is active in an outer scope, mention it
            },
            nb_errors_left,
            options,
        );
    } else {
        let res = conditions.pop();
        debug_assert!(res.is_some());
    }
}
