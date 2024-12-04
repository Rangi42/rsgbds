use rgbds::rpn::Rpn;

use crate::{
    expr::{BinOp, Expr, UnOp},
    syntax::tokens::{tok, Token, TokenPayload},
};

use super::{expect_one_of, ParseCtx};

pub fn expect_numeric_expr<'ctx_stack>(maybe_expr: Option<Expr<'ctx_stack>>) -> Expr<'ctx_stack> {
    match maybe_expr {
        Some(expr) => expr,
        None => Expr::nothing(),
    }
}

// The implementation strategy is a Pratt parser.
//
// [1]: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
// [2]: https://martin.janiczek.cz/2023/07/03/demystifying-pratt-parsers.html
pub(super) fn parse_numeric_expr<'ctx_stack>(
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> (Option<Expr<'ctx_stack>>, Option<Token<'ctx_stack>>) {
    fn parse_subexpr<'ctx_stack>(
        parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
        min_binding_power: u8,
    ) -> (Option<Expr<'ctx_stack>>, Option<Token<'ctx_stack>>) {
        // First, attempt to parse an "atom" (in short, a sub-expression that will maybe serve
        // as an operand to some operator).
        let (mut lhs, mut token) = expect_one_of!(parse_ctx.next_token() => {
            |"("| => {
                let (res, lookahead) = parse_subexpr(parse_ctx, 0); // The inner expression's minimum power is reset, due to the parens' grouping behavior.
                let lookahead = expect_one_of!(lookahead => {
                    |")"| => parse_ctx.next_token(),
                    else |unexpected| => {
                        // TODO: would be nice to make this say "Syntax error: unclosed parenthesis" somehow
                        parse_ctx.report_syntax_error(unexpected.as_ref());
                        unexpected
                    }
                });
                (expect_numeric_expr(res), lookahead)
            },

            Token { span } |"number"(number)| => {
                let lookahead = parse_ctx.next_token();
                (Expr::number(number, span), lookahead)
            },
            Token { span } |"identifier"(ident)| => {
                let lookahead = parse_ctx.next_token();
                if matches!(lookahead, Some(Token { payload: tok!("("), .. })) {
                    todo!(); // Function call.
                } else {
                    (Expr::symbol(ident, span), lookahead)
                }
            },
            Token { span } |"string"(string)| => {
                let lookahead = parse_ctx.next_token();
                (Expr::number(todo!(), span), lookahead)
            },

            // TODO: all of the function calls...

            else |other| => {
                // If the next token is neither a prefix operator, nor a terminal / "atom",
                // then there is no expression to parse.
                let Some(op_token) = other else {
                    return (None, None);
                };
                let Some(operator) = UnOp::from_token(&op_token.payload) else {
                    return (None, Some(op_token));
                };

                let ((), right_power) = operator.binding_power();
                let (rhs, lookahead) = parse_subexpr(parse_ctx, right_power);
                (expect_numeric_expr(rhs).unary_op(operator, op_token.span), lookahead)
            }
        });

        let lookahead = loop {
            // If at end of input, the expression is over.
            let Some(op_token) = token else {
                break None;
            };
            // If the next token is not a binary operator, stop parsing the expression.
            let Some(operator) = BinOp::from_token(&op_token.payload) else {
                break Some(op_token);
            };
            let (left_power, right_power) = operator.binding_power();
            // Compare the binding powers surrounding the last "atom" (left = `min_binding_power`,
            // the right-hand power of the last operator shifted in; right = `left_power`).
            // If said "atom" is bound more tightly to its left than to its right,
            // "wrap up" the current expression.
            if left_power < min_binding_power {
                break Some(op_token);
            }

            let (rhs, lookahead) = parse_subexpr(parse_ctx, right_power);
            let rhs = expect_numeric_expr(rhs);

            token = lookahead;
            lhs = lhs.binary_op(operator, rhs);
        };

        (Some(lhs), lookahead)
    }

    parse_subexpr(parse_ctx, 0)
}
