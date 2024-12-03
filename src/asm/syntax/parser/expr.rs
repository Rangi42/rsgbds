use rgbds::rpn::Rpn;

use crate::syntax::tokens::Token;

use super::{expect_one_of, ParseCtx};

// The implementation strategy is a Pratt parser:
//
// [1]: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
// [2]: https://martin.janiczek.cz/2023/07/03/demystifying-pratt-parsers.html
pub(super) fn parse_numeric_expr<'ctx_stack>(
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> (Result<Rpn, ()>, Option<Token<'ctx_stack>>) {
    fn parse_subexpr<'ctx_stack>(
        parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
        min_binding_power: u8,
    ) -> (Result<Rpn, ()>, Option<Token<'ctx_stack>>) {
        expect_one_of!(parse_ctx.next_token() => {
            |"("| => {
                let (res, tok) = parse_subexpr(parse_ctx, 0); // The inner expression's minimum power is reset, due to the parens' grouping behavior.
            },
            |"number"(number)| => todo!(),
            else |unexpected, _expected| => todo!(),
        });

        loop {
            todo!()
        }
    }

    parse_subexpr(parse_ctx, 0)
}
