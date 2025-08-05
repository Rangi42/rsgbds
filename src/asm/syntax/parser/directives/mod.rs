use crate::{diagnostics, syntax::tokens::Token};

use super::parse_ctx;

pub(super) fn parse_align(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(super) fn parse_break(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(super) fn parse_db(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(super) fn parse_dl(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(super) fn parse_ds(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(super) fn parse_dw(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(super) fn parse_endm(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    parse_ctx.report_syntax_error(&keyword, |error, span| {
        error.add_label(
            diagnostics::error_label(span).with_message("this `ENDM` is outside of a macro"),
        );
    });

    parse_ctx.next_token()
}

pub(super) fn parse_endr(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    parse_ctx.report_syntax_error(&keyword, |error, span| {
        error.add_label(
            diagnostics::error_label(span).with_message("this `ENDR` is outside of a loop"),
        );
    });

    parse_ctx.next_token()
}

pub(super) fn parse_endu(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(super) fn parse_incbin(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(super) fn parse_nextu(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(super) fn parse_shift(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(super) fn parse_union(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub mod charmap;
pub mod cond;
pub mod context;
pub mod opt;
pub mod output;
pub mod section;
pub mod symbol;
