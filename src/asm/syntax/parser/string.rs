use compact_str::CompactString;

use crate::{diagnostics, sources::Span, syntax::tokens::Token};

use super::{expect_one_of, parse_ctx, tok};

pub(super) fn parse_string_expr(
    first_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<(CompactString, Span)>, Token) {
    expect_one_of! {
        first_token => {
            Token { span } @ |"string"(string)| => {
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
            Token { span } @ |"identifier"(ident, has_colon)| => {
                // If the identifier resolves to a string symbol, then it expands to its value.
                if let Some(sym) = parse_ctx.symbols.find(&ident) {
                    if let Some(string) = sym.get_string() {
                        return (Some((string, span)), parse_ctx.next_token())
                    }
                }
                (None, Token { payload: tok!("identifier")(ident, has_colon), span })
            },
            else |unexpected| => {
                (None, unexpected)
            }
        }
    }
}

pub(super) fn expect_string_expr(
    first_token: Token,
    parse_ctx: &mut parse_ctx!(),
) -> (Option<(CompactString, Span)>, Token) {
    let (maybe_string, lookahead) = parse_string_expr(first_token, parse_ctx);
    (
        match maybe_string {
            Some((string, span)) => Some((string, span)),
            None => {
                parse_ctx.report_syntax_error(&lookahead, |error, span| {
                    error.add_label(
                        diagnostics::error_label(span)
                            .with_message("expected a string expression here"),
                    )
                });
                None
            }
        },
        lookahead,
    )
}
