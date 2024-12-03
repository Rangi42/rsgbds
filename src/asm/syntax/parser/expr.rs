use rgbds::rpn::Rpn;

use crate::syntax::tokens::{tok, Token, TokenPayload};

use super::{expect_one_of, ParseCtx};

// The implementation strategy is a Pratt parser.
//
// [1]: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
// [2]: https://martin.janiczek.cz/2023/07/03/demystifying-pratt-parsers.html
pub(super) fn parse_numeric_expr<'ctx_stack>(
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> (Option<Rpn>, Option<Token<'ctx_stack>>) {
    fn parse_subexpr<'ctx_stack>(
        parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
        min_binding_power: u8,
    ) -> (Option<Rpn>, Option<Token<'ctx_stack>>) {
        let (mut lhs, mut next_token) = expect_one_of!(parse_ctx.next_token() => {
            |"("| => {
                let (res, next_token) = parse_subexpr(parse_ctx, 0); // The inner expression's minimum power is reset, due to the parens' grouping behavior.
                let next_token = expect_one_of!(next_token => {
                    |")"| => parse_ctx.next_token(),
                    else |unexpected| => {
                        // TODO: would be nice to make this say "Syntax error: unclosed parenthesis" somehow
                        parse_ctx.report_syntax_error(unexpected.as_ref());
                        unexpected
                    }
                });
                // TODO: what to do if `res` is `None`?
                todo!();
            },

            |"number"(number)| => {
                let next_token = parse_ctx.next_token();
                (Rpn::constant(number as u32), next_token)
            },
            |"identifier"(ident)| => {
                let next_token = parse_ctx.next_token();
                if matches!(next_token, Some(Token { payload: tok!("("), .. })) {
                    todo!(); // Function call.
                } else {
                    (Rpn::symbol(todo!()), next_token)
                }
            },
            |"string"(string)| => {
                let next_token = parse_ctx.next_token();
                (Rpn::constant(todo!()), next_token)
            },

            // TODO: all of the function calls...

            else |other| => {
                let Some(((), right_power)) = other.as_ref().and_then(|token| prefix_binding_power(&token.payload)) else {
                    // The next token is neither a prefix operator, nor a terminal / "atom".
                    // Thus, there is no expression to parse.
                    return (None, other);
                };
                let (rhs, next_token) = parse_subexpr(parse_ctx, right_power);
                // TODO: what to do if `res` is `None`?
                todo!()
            }
        });

        while let Some((left_power, right_power)) = next_token
            .as_ref()
            .and_then(|token| infix_binding_power(&token.payload))
        {
            if left_power < min_binding_power {
                break;
            }
            let (rhs, tok) = parse_subexpr(parse_ctx, right_power);
            next_token = tok;
            // What if `rhs` is `None`?
            todo!();
        }

        (Some(lhs), next_token)
    }

    parse_subexpr(parse_ctx, 0)
}

fn prefix_binding_power(kind: &TokenPayload) -> Option<((), u8)> {
    match kind {
        tok!("~") | tok!("+") | tok!("-") | tok!("!") => Some(((), 15)),
        _ => None,
    }
}

fn infix_binding_power(kind: &TokenPayload) -> Option<(u8, u8)> {
    match kind {
        tok!("**") => Some((18, 17)),
        tok!("*") | tok!("/") | tok!("%") => Some((13, 14)),
        tok!("<<") | tok!(">>") | tok!(">>>") => Some((11, 12)),
        tok!("&") | tok!("|") | tok!("^") => Some((9, 10)),
        tok!("+") | tok!("-") => Some((7, 8)),
        tok!("!=") | tok!("==") | tok!("<=") | tok!("<") | tok!(">=") | tok!(">") => Some((5, 6)),
        tok!("&&") => Some((3, 4)),
        tok!("||") => Some((1, 2)),
        _ => None,
    }
}
