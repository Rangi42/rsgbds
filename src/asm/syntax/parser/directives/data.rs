use crate::{
    diagnostics,
    syntax::{
        parser::{expect_one_of, expr, misc, parse_ctx, require},
        tokens::Token,
    },
};

pub(in super::super) fn parse_ds(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let length = expect_one_of! { parse_ctx.next_token() => {
        |"align"| => {
            todo!();
        },
        else |unexpected| => {
            let (res, lookahead) = expr::parse_numeric_expr(unexpected, parse_ctx);

            if let Some(expr) = res {
                match parse_ctx.try_const_eval(&expr) {
                    Ok((value, _span)) => value as usize,
                    Err(error) => {
                        parse_ctx.report_expr_error(error);
                        0
                    }
                }
            } else {
                parse_ctx.report_syntax_error(&lookahead, |error, span| {
                    error.add_label(diagnostics::error_label(span).with_message("expected a numeric expression or an `align` specification"));
                });
                0
            }
        }
    }};

    expect_one_of! { parse_ctx.next_token() => {
        |","| => {
            let (bytes, lookahead) = misc::parse_comma_list(expr::parse_numeric_expr, parse_ctx.next_token(), parse_ctx);
            todo!();
        },
        else |unexpected| => {
            todo!();

            unexpected
        }
    }};
}

pub(in super::super) fn parse_db(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(in super::super) fn parse_dl(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(in super::super) fn parse_dw(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(in super::super) fn parse_incbin(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(in super::super) fn parse_union(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(in super::super) fn parse_nextu(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

pub(in super::super) fn parse_endu(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}
