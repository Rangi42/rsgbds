use std::rc::Rc;

use crate::{
    diagnostics,
    macro_args::MacroArgs,
    sources::{NormalSpan, Source, Span, SpanKind},
    syntax::{lexer::LoopInfo, parser::discard_rest_of_line},
    Identifier,
};

use super::super::{expect_one_of, expr, matches_tok, parse_ctx, require, string, Token};

pub(in super::super) fn parse_include(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let (maybe_string, mut lookahead) =
        string::expect_string_expr(parse_ctx.next_token(), parse_ctx);

    // Immediately look for the end of line, since we are about to change the parse context.
    if !matches_tok!(lookahead, "end of line" | "end of input") {
        parse_ctx.report_syntax_error(&lookahead, |error, span| {
            error.add_label(
                diagnostics::error_label(span).with_message("Expected nothing else on this line"),
            );
        });
        // Discard the rest of the line.
        lookahead = discard_rest_of_line(lookahead, parse_ctx);
    }

    if let Some((string, span)) = maybe_string {
        match Source::load_file(&string) {
            Ok(source) => {
                let Span::Normal(span) = &keyword.span else {
                    unreachable!()
                };
                parse_ctx.push_file(source, Some(Rc::new(span.clone())))
            }
            Err(err) => parse_ctx.error(&span, |error| {
                error.set_message(format!("Unable to read path \"{}\"", string));
                error.add_label(diagnostics::error_label(&span).with_message(err));
            }),
        };
    }

    lookahead
}

pub(in super::super) fn parse_macro(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let mut lookahead = parse_ctx.next_token();
    require! { lookahead => Token { span } @ |"identifier"(name, _colon)| else |unexpected| {
        parse_ctx.report_syntax_error(&unexpected, |error, span| {
            error.add_label(diagnostics::error_label(span).with_message("Expected the macro's name"));
        });

        return discard_rest_of_line(unexpected, parse_ctx);
    } }

    lookahead = parse_ctx.next_token();
    require! { lookahead => |"end of line"| else |unexpected| {
        parse_ctx.report_syntax_error(&unexpected, |error, span| {
            error.add_label(diagnostics::error_label(span).with_message("Expected nothing else on this line"));
        });

        return discard_rest_of_line(unexpected, parse_ctx);
    } }

    let (body, res) = parse_ctx.lexer.capture_until_keyword(
        "ENDM",
        &[], // TODO: consider reintroducing nested macro definitions?
        "macro definition",
        parse_ctx.nb_errors_remaining,
        parse_ctx.options,
    );
    if let Err(err) = res {
        parse_ctx.error(&span, |error| {
            error.set_message(&err);
            error.add_label(
                diagnostics::error_label(&span).with_message("Macro definition starting here"),
            );
        })
    }

    parse_ctx.symbols.define_macro(
        name,
        parse_ctx.identifiers,
        span,
        body,
        parse_ctx.nb_errors_remaining,
        parse_ctx.options,
    );

    parse_ctx.next_token()
}

pub(in super::super) fn parse_rept(keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    let (expr, lookahead) = expr::expect_numeric_expr(parse_ctx.next_token(), parse_ctx);

    expect_one_of! { lookahead => {
        |"end of line" / "end of input"| => {},
        else |unexpected| => {
            parse_ctx.report_syntax_error(&unexpected, |error, span| {
                error.add_label(diagnostics::error_label(span).with_message("Expected nothing else on this line"));
            });

            discard_rest_of_line(unexpected, parse_ctx);
        }
    } };

    let (body, res) = parse_ctx.lexer.capture_until_keyword(
        "ENDR",
        &["REPT", "FOR"],
        "loop",
        parse_ctx.nb_errors_remaining,
        parse_ctx.options,
    );
    if let Err(err) = res {
        parse_ctx.error(&keyword.span, |error| {
            error.set_message(&err);
            error.add_label(
                diagnostics::error_label(&keyword.span).with_message("Loop starting here"),
            );
        })
    }

    let lookahead = parse_ctx.next_token(); // Lex this token before switching parse contexts!

    match expr.try_const_eval() {
        Ok((0, _span)) => {} // `REPT 0` acts like `IF 0`.
        Ok((nb_iters, _span)) => {
            let Span::Normal(span) = &keyword.span else {
                unreachable!()
            };
            parse_ctx.push_loop(
                body,
                Rc::new(span.clone()),
                LoopInfo {
                    nb_iters: nb_iters as u32,
                    for_var: None,
                    ..Default::default()
                },
            );
        }
        Err(err) => parse_ctx.report_expr_error(err),
    }

    lookahead
}

pub(in super::super) fn parse_for(_keyword: Token, parse_ctx: &mut parse_ctx!()) -> Token {
    todo!()
}

impl parse_ctx!() {
    pub fn push_file(&mut self, file: Rc<Source>, parent: Option<Rc<NormalSpan>>) {
        self.lexer
            .push_file(file, parent, self.nb_errors_remaining, self.options);
        // Preserve the active macro args.
    }

    pub fn push_macro(
        &mut self,
        macro_name: Identifier,
        contents: NormalSpan,
        parent: Rc<NormalSpan>,
        args: MacroArgs,
    ) {
        self.lexer.push_macro(
            macro_name,
            contents,
            parent,
            self.nb_errors_remaining,
            self.options,
        );
        self.macro_args.push(args);
        self.unique_id.enter_unique_ctx();
    }

    pub fn push_loop(&mut self, contents: NormalSpan, parent: Rc<NormalSpan>, loop_info: LoopInfo) {
        self.lexer.push_loop(
            loop_info,
            contents,
            parent,
            self.nb_errors_remaining,
            self.options,
        );
        self.unique_id.enter_unique_ctx();
    }

    pub fn pop_context(&mut self) -> bool {
        let ctx = self.lexer.top_context();
        match &mut ctx.span.kind {
            SpanKind::File => self.lexer.pop_context(),

            SpanKind::Macro(..) => {
                self.unique_id.exit_unique_ctx();
                let args = self.macro_args.pop();
                debug_assert!(args.is_some());
                let has_more = self.lexer.pop_context();
                debug_assert!(has_more);
                has_more
            }

            SpanKind::Loop(nth) => {
                self.unique_id.exit_unique_ctx();
                let mut loop_state = &mut ctx.loop_state;
                if *nth == loop_state.nb_iters - 1 {
                    let has_more = self.lexer.pop_context();
                    debug_assert!(has_more);
                    has_more
                } else {
                    *nth += 1;
                    if let Some(ident) = loop_state.for_var {
                        todo!()
                    }
                    ctx.reset();
                    self.unique_id.enter_unique_ctx();
                    true // We haven't popped this context, so there's more to process.
                }
            }

            _ => unreachable!(),
        }
    }
}
