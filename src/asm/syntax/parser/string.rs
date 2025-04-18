use compact_str::CompactString;

use crate::{context_stack::Span, diagnostics, syntax::tokens::Token};

use super::{expect_one_of, parse_ctx, tok};

pub(super) fn parse_string_expr<'ctx_stack>(
    first_token: Option<Token<'ctx_stack>>,
    parse_ctx: &mut parse_ctx!('ctx_stack),
) -> (
    Option<(CompactString, Span<'ctx_stack>)>,
    Option<Token<'ctx_stack>>,
) {
    expect_one_of! {
        first_token => {
            Token { span } |"string"(string)| => {
                (Some((string, span)), parse_ctx.next_token())
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
                        return (Some((string, span)), parse_ctx.next_token())
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

pub(super) fn expect_string_expr<'ctx_stack>(
    first_token: Option<Token<'ctx_stack>>,
    parse_ctx: &mut parse_ctx!('ctx_stack),
) -> (
    Option<(CompactString, Span<'ctx_stack>)>,
    Option<Token<'ctx_stack>>,
) {
    let (maybe_string, lookahead) = parse_string_expr(first_token, parse_ctx);
    (
        match maybe_string {
            Some((string, span)) => Some((string, span)),
            None => {
                parse_ctx.report_syntax_error(lookahead.as_ref(), |error, span| {
                    error.add_label(
                        diagnostics::error_label(span)
                            .with_message("Expected a string expression here"),
                    )
                });
                None
            }
        },
        lookahead,
    )
}
