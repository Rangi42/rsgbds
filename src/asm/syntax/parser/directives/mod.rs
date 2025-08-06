use crate::{diagnostics, syntax::tokens::Token};

use super::{expr, parse_ctx};

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

pub(super) fn parse_shift(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let (expr, lookahead) = expr::parse_numeric_expr(parse_ctx.next_token(), parse_ctx);

    if let Some(args) = parse_ctx.macro_args.last_mut() {
        if let Some(expr) = expr {
            match expr.try_const_eval(parse_ctx.symbols, Some(args), parse_ctx.sections) {
                Ok((value, span)) => args.shift_by(
                    value as isize,
                    &span,
                    parse_ctx.nb_errors_remaining,
                    parse_ctx.options,
                ),
                Err(error) => parse_ctx.report_expr_error(error),
            }
        } else {
            args.shift_by(
                1,
                &keyword.span,
                parse_ctx.nb_errors_remaining,
                parse_ctx.options,
            );
        }
    } else {
        diagnostics::error(
            &keyword.span,
            |error| {
                error.set_message("cannot `shift` outside of a macro");
                error.add_label(
                    diagnostics::error_label(&keyword.span)
                        .with_message("no macro arguments to shift at this point"),
                );
            },
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        );
    }

    lookahead
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
