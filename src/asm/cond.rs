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

pub fn active_condition_mut(lexer: &mut Lexer) -> Option<&mut Condition> {
    let min_depth = lexer.top_context().cond_stack_depth;
    lexer.cond_stack[min_depth..].last_mut()
}

#[must_use]
pub fn exit_conditional(conditions: &mut Vec<Condition>) -> bool {
    conditions.pop().is_some()
}
