use std::cell::Cell;

use crate::{
    charmap::{CharMapping, Charmap},
    context_stack::Span,
    diagnostics,
    expr::{BinOp, Expr, UnOp},
    source_store::SourceStore,
    syntax::tokens::{tok, Token},
    Options,
};

use super::{expect_one_of, string, ParseCtx};

// The implementation strategy is a Pratt parser.
//
// [1]: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
// [2]: https://martin.janiczek.cz/2023/07/03/demystifying-pratt-parsers.html
pub(super) fn parse_numeric_expr<'ctx_stack>(
    lookahead: Option<Token<'ctx_stack>>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> (Option<Expr<'ctx_stack>>, Option<Token<'ctx_stack>>) {
    parse_subexpr(lookahead, parse_ctx, 0)
}

fn parse_subexpr<'ctx_stack>(
    lookahead: Option<Token<'ctx_stack>>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
    min_binding_power: u8,
) -> (Option<Expr<'ctx_stack>>, Option<Token<'ctx_stack>>) {
    let (mut lhs, mut token) = expect_one_of!(lookahead => {
        Token { span } |"("| => {
            // The inner expression's minimum power is reset, due to the parens' grouping behavior.
            let (expr, lookahead) = expect_subexpr(parse_ctx.next_token(),parse_ctx, 0);
            let (lookahead, end_span) = expect_one_of!(lookahead => {
                Token { span } |")"| => (parse_ctx.next_token(), span),
                else |unexpected| => {
                    parse_ctx.report_syntax_error(unexpected.as_ref(), |error, cur_span| {
                        error.set_message("Syntax error: unclosed parenthesis");
                        error.add_labels([
                            diagnostics::error_label(&span)
                                .with_message("This parenthesis should be closed..."),
                            diagnostics::note_label(cur_span)
                                .with_message("...before this point"),
                        ]);
                    });
                    (unexpected, parse_ctx.current_span()) // TODO: no, bad. This should span until the last token in the subexpr!
                }
            });
            (expr, lookahead)
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

        // TODO: all of the function calls...

        else |other| => {
            // If the next token is neither a prefix operator, nor a terminal / "atom",
            // then there is no expression to parse.

            // Note that string expressions are atoms.
            // Note that identifiers are matched above, so they are treated as a regular numeric expression.
            // This is intentional, as symbols are resolved only at evaluation time.
            // This does have the side effect that the charmap in force at *evaluation time* is used, instead of the
            // one in force at the time that *this function* is called. I see this as a good thing.
            let (maybe_string, lookahead) = string::parse_string_expr(other, parse_ctx);
            if let Some((string, span)) = maybe_string {
                let charmap = parse_ctx.charmaps.active_charmap();
                match convert_string_to_numeric(
                    &string,
                    &span,
                    charmap,
                    parse_ctx.sources,
                    parse_ctx.nb_errors_remaining,
                    parse_ctx.options,
                ) {
                    Ok(value) => (Expr::number(value, span), lookahead),
                    Err(nb_values) => {
                        diagnostics::error(
                            &span,
                            |error| {
                                error.set_message("Invalid string-to-number conversion");
                                error.add_label(diagnostics::error_label(&span)
                                    .with_message(format!("This string encodes to {nb_values} charmap units instead of 1")));
                            },
                            parse_ctx.sources,
                            parse_ctx.nb_errors_remaining,
                            parse_ctx.options
                        );
                        (Expr::nothing(span), lookahead)
                    }
                }
            } else {
                let Some(op_token) = lookahead else {
                    return (None, None);
                };

                let Some(operator) = UnOp::from_token(&op_token.payload) else {
                    return (None, Some(op_token));
                };

                let ((), right_power) = operator.binding_power();
                let (rhs, lookahead) = expect_subexpr(parse_ctx.next_token(), parse_ctx, right_power);
                (rhs.unary_op(operator, op_token.span), lookahead)
            }
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

        let (rhs, lookahead) = expect_subexpr(parse_ctx.next_token(), parse_ctx, right_power);

        token = lookahead;
        lhs = lhs.binary_op(operator, rhs);
    };

    (Some(lhs), lookahead)
}

pub(super) fn expect_numeric_expr<'ctx_stack>(
    lookahead: Option<Token<'ctx_stack>>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
) -> (Expr<'ctx_stack>, Option<Token<'ctx_stack>>) {
    expect_subexpr(lookahead, parse_ctx, 0)
}

fn expect_subexpr<'ctx_stack>(
    lookahead: Option<Token<'ctx_stack>>,
    parse_ctx: &mut ParseCtx<'ctx_stack, '_, '_, '_, '_, '_>,
    min_binding_power: u8,
) -> (Expr<'ctx_stack>, Option<Token<'ctx_stack>>) {
    let (res, lookahead) = parse_subexpr(lookahead, parse_ctx, min_binding_power);
    (
        res.unwrap_or_else(|| {
            parse_ctx.report_syntax_error(lookahead.as_ref(), |error, span| {
                error.add_label(
                    diagnostics::error_label(span)
                        .with_message("Expected a number or an expression here"),
                )
            });
            // If no expression has been parsed, then the lookahead must point to a non-expression
            // token, which is suitable for an "expression expected here" error.
            Expr::nothing(match lookahead.as_ref() {
                Some(token) => token.span.clone(),
                None => parse_ctx.current_span(),
            })
        }),
        lookahead,
    )
}

fn convert_string_to_numeric<'ctx_stack>(
    string: &str,
    span: &Span<'ctx_stack>,
    charmap: &Charmap<'ctx_stack>,
    sources: &SourceStore,
    nb_errors_left: &Cell<usize>,
    options: &Options,
) -> Result<i32, usize> {
    let mut encoder = charmap.encode(string);
    match (encoder.next(), encoder.next()) {
        (None, _) => Err(0),
        (Some(first), Some(second)) => {
            Err(first.len() + second.len() + encoder.fold(0, |acc, mapping| acc + mapping.len()))
        }
        (Some(mapping), None) => match mapping {
            CharMapping::Mapped(values) => {
                if let &[value] = values {
                    Ok(value)
                } else {
                    Err(values.len())
                }
            }
            CharMapping::Passthrough(c) => {
                charmap.warn_on_passthrough(c, span, sources, nb_errors_left, options);
                Ok(c as u32 as i32)
            }
        },
    }
}
