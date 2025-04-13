use compact_str::CompactString;

use crate::syntax::tokens::Token;

use super::{expect_one_of, tok, ParseCtx};

pub(super) fn parse_string_expr<'ctx_stack>(
    first_token: Option<Token<'ctx_stack>>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_>,
) -> (Option<CompactString>, Option<Token<'ctx_stack>>) {
    expect_one_of! {
        first_token => {
            |"string"(string)| => {
                (Some(string), parse_ctx.next_token())
            },
            |"strslice"| => {
                todo!();
            },
            |"strsub"| => {
                todo!();
            },
            |"strchar"| => {
                todo!();
            },
            |"charsub"| => {
                todo!();
            },
            |"revchar"| => {
                todo!();
            },
            |"strcat"| => {
                todo!();
            },
            |"strupr"| => {
                todo!();
            },
            |"strlwr"| => {
                todo!();
            },
            |"strrpl"| => {
                todo!();
            },
            |"strfmt"| => {
                todo!();
            },
            |"section"| => {
                todo!();
            },
            Token { span } |"identifier"(ident)| => {
                // If the identifier resolves to a string symbol, then it expands to its value.
                if let Some(sym) = parse_ctx.symbols.find_interned(&ident) {
                    if let Some(string) = sym.get_string() {
                        return (Some(string), parse_ctx.next_token())
                    }
                }
                (None, Some(Token { payload: tok!("identifier")(ident), span }))
            },
            else |unexpected| => {
                (None, unexpected)
            }
        }
    }
}
