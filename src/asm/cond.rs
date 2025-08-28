use crate::{sources::Span, syntax::lexer::Lexer};

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

#[must_use]
pub fn exit_conditional(conditions: &mut Vec<Condition>) -> bool {
    conditions.pop().is_some()
}
