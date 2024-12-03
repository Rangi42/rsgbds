/*! A manually-implemented rgbasm language parser.

Historically, we have used a parser generator ([Bison]), and written rgbasm's language as a LR(1) grammar: [\[1\]].
*Unfortunately*, rgbasm's grammar is not context-free, so this actually requires several lexer hacks (where the parser communicates information back to the lexer).

We originally tried [using a LR(1) parser generator][rgbasm-lalrpop], but this turned to require *heavily* working around its lookahead behaviour, and extensive use of [`RefCell`][std::cell::RefCell], to the point of unmaintainability.
So instead, this takes the approach of a manually-written parser, which can handle all of those edge cases much more sanely.

The extra boilerplate is counter-balanced by how much the aforementioned workarounds required, and the input syntax was fairly well-understood before we switched to a manual parser, so we can expect that there shouldn't be any grammar ambiguities.

[\[1\]]: https://github.com/gbdev/rgbds/blob/15919e550ffe4461e3c7d908897db324d48500a6/src/asm/parser.y
[Bison]: https://www.gnu.org/software/bison/
[rgbasm-lalrpop]: https://github.com/ISSOtm/rsgbds/blob/4cd81e2920b71b335f4be744adcc3f307bdd5fd7/src/asm/language/parser.lalrpop
*/

use std::{cell::Cell, rc::Rc};

use compact_str::CompactString;

use crate::{
    context_stack::ContextStack,
    diagnostics::{self, warning},
    macro_args::MacroArgs,
    source_store::{SourceHandle, SourceStore},
    symbols::Symbols,
    syntax::{
        lexer,
        tokens::{tok, Token},
    },
    Options,
};

mod expr;

macro_rules! expect_one_of {
    ($payload:expr => {
        $( None => $if_none:expr, )?
        $( |$($token:ident:)? $($kind:tt $(($fields:tt))?)/+| => $then:expr, )+
        else |$unexpected:pat_param $(, $expected:pat_param)? $(,)?| => $handle_default:expr $(,)?
    }) => {match $payload {
        $( None => $if_none, )?
        $( Some($($token @)? Token {
            payload: $( $crate::syntax::tokens::tok!($kind $(($fields))?) )|+,
            ..
        }) => $then, )+
        $unexpected => {
            // TODO: handle `if_none`, adding "end of file" to the list
            expect_one_of!(@expected [$($( $kind )+),+] $($expected)?); // Internal call.
            $handle_default
        }
    }};

    // Using internal rules rather than calling other macros allows the macro to remain self-contained.
    // (This does make the macro's name irrelevant or even confusing, unfortunately.)
    //
    // [1]: https://danielkeep.github.io/tlborm/book/pat-internal-rules.html
    (@expected $kinds:tt $($name:tt)?) => {
        $(let $name = &$kinds;)?
    };
}
use expect_one_of; // Allow this macro to be used by children modules.

pub fn parse_file<'ctx_stack>(
    source: SourceHandle,
    ctx_stack: &'ctx_stack ContextStack,
    sources: &SourceStore,
    symbols: &mut Symbols<'ctx_stack>,
    nb_errors_remaining: &Cell<usize>,
    options: &Options,
) {
    let mut parse_ctx = ParseCtx {
        ctx_stack,
        sources,
        symbols,
        nb_errors_remaining,
        options,
    };

    ctx_stack.sources_mut().push_file_context(source);
    while ctx_stack.sources_mut().active_context().is_some() {
        while let Some(mut first_token) = parse_ctx.next_token() {
            // TODO: handle leading diff marks
            // TODO: handle Git conflict markers
            if let tok!("identifier"(ident)) = first_token.payload {
                // Identifiers at the beginning of the line can be two things.
                // Either a label name, if it's *directly* followed by a colon; or the name of a macro.
                if parse_ctx.is_next_char_a_colon() {
                    expect_one_of!(parse_ctx.next_token() => {
                        None => unreachable!(),
                        |":"| => {
                            // TODO
                            eprintln!("Defining label {}", parse_ctx.symbols.resolve(ident));
                            match parse_ctx.next_token() {
                                Some(token) => first_token = token,
                                None => break,
                            };
                        },
                        |"::"| => {
                            // TODO
                            eprintln!("Defining exported label {}", parse_ctx.symbols.resolve(ident));
                            match parse_ctx.next_token() {
                                Some(token) => first_token = token,
                                None => break,
                            };
                        },
                        else |token, _expected| => {
                            // Try continuing the parse with this token as the first in the line.
                            // We know the token can't be empty, because the lexer just reported that its next character would be a colon.
                            first_token = token.unwrap();
                        }
                    });
                }

                // TODO: handle directives that cannot be preceded by a label def.
            }

            match first_token.payload {
                tok!("end of line") => {} // Empty line.

                tok!("identifier"(ident)) => {
                    // Macro call.
                    // Get the macro's arguments.
                    let args = Rc::new(MacroArgs::new(
                        std::iter::from_fn(|| parse_ctx.next_token_raw()).collect(),
                    ));

                    // TODO: consume the newline, if `next_raw` doesn't already?

                    let name = parse_ctx.symbols.resolve(ident);
                    match parse_ctx.symbols.find_macro_interned(&ident) {
                        None => diagnostics::error(
                            &first_token.span,
                            |error| {
                                error
                                    .with_message(format!("Macro `{name}` does not exist"))
                                    .with_label(
                                        diagnostics::error_label(first_token.span.resolve())
                                            .with_message("Attempting to call the macro here"),
                                    )
                            },
                            sources,
                            nb_errors_remaining,
                            options,
                        ),
                        Some(Err(other)) => diagnostics::error(
                            &first_token.span,
                            |error| {
                                error
                                    .with_message(format!("`{name}` is not a macro"))
                                    .with_label(
                                        diagnostics::error_label(first_token.span.resolve())
                                            .with_message("Attempting to call the macro here"),
                                    )
                            },
                            sources,
                            nb_errors_remaining,
                            options,
                        ),
                        Some(Ok(slice)) => {
                            ctx_stack
                                .sources_mut()
                                .push_macro_context(name.into(), slice, args)
                        }
                    }
                }

                // These are not valid after a label.
                tok!("macro") => todo!(),
                tok!("endm") => todo!(),
                tok!("rept") => todo!(),
                tok!("for") => todo!(),
                tok!("endr") => todo!(),
                tok!("if") => todo!(),
                tok!("elif") => todo!(),
                tok!("else") => todo!(),
                tok!("endc") => todo!(),

                tok!("adc") => todo!(),
                tok!("add") => todo!(),
                tok!("and") => todo!(),
                tok!("bit") => todo!(),
                tok!("call") => todo!(),
                tok!("ccf") => todo!(),
                tok!("cp") => todo!(),
                tok!("cpl") => todo!(),
                tok!("daa") => todo!(),
                tok!("dec") => todo!(),
                tok!("di") => todo!(),
                tok!("ei") => todo!(),
                tok!("halt") => todo!(),
                tok!("inc") => todo!(),
                tok!("jp") => todo!(),
                tok!("jr") => todo!(),
                tok!("ldd") => todo!(),
                tok!("ldh") => todo!(),
                tok!("ldi") => todo!(),
                tok!("ld") => todo!(),
                tok!("nop") => todo!(),
                tok!("or") => todo!(),
                tok!("pop") => todo!(),
                tok!("push") => todo!(),
                tok!("res") => todo!(),
                tok!("reti") => todo!(),
                tok!("ret") => todo!(),
                tok!("rla") => todo!(),
                tok!("rlca") => todo!(),
                tok!("rlc") => todo!(),
                tok!("rl") => todo!(),
                tok!("rra") => todo!(),
                tok!("rrca") => todo!(),
                tok!("rrc") => todo!(),
                tok!("rr") => todo!(),
                tok!("rst") => todo!(),
                tok!("sbc") => todo!(),
                tok!("scf") => todo!(),
                tok!("set") => todo!(),
                tok!("sla") => todo!(),
                tok!("sra") => todo!(),
                tok!("srl") => todo!(),
                tok!("stop") => todo!(),
                tok!("sub") => todo!(),
                tok!("swap") => todo!(),
                tok!("xor") => todo!(),

                tok!("align") => todo!(),
                tok!("assert") => todo!(),
                tok!("break") => todo!(),
                tok!("charmap") => todo!(),
                tok!("db") => todo!(),
                tok!("dl") => todo!(),
                tok!("ds") => todo!(),
                tok!("dw") => todo!(),
                tok!("endsection") => todo!(),
                tok!("endl") => todo!(),
                tok!("endu") => todo!(),
                tok!("export") => todo!(),
                tok!("fail") => todo!(),
                tok!("fatal") => todo!(),
                tok!("incbin") => todo!(),
                tok!("include") => todo!(),
                tok!("load") => todo!(),
                tok!("newcharmap") => todo!(),
                tok!("nextu") => todo!(),
                tok!("opt") => todo!(),
                tok!("popc") => todo!(),
                tok!("popo") => todo!(),
                tok!("pops") => todo!(),
                tok!("println") => {
                    let (expr, next) = expr::parse_numeric_expr(&mut parse_ctx);
                    if let Some(expr) = expr {
                        match expr.try_eval::<std::convert::Infallible, _>(|id| todo!()) {
                            Ok(n) => println!("{n}"),
                            Err(err) => todo!(),
                        }
                    }
                }
                tok!("print") => todo!(),
                tok!("purge") => todo!(),
                tok!("pushc") => todo!(),
                tok!("pusho") => todo!(),
                tok!("pushs") => todo!(),
                tok!("rb") => todo!(),
                tok!("rw") => todo!(),
                tok!("redef") => todo!(),
                tok!("rsreset") => todo!(),
                tok!("rsset") => todo!(),
                tok!("section") => todo!(),
                tok!("setcharmap") => todo!(),
                tok!("shift") => todo!(),
                tok!("static_assert") => todo!(),
                tok!("union") => todo!(),
                tok!("warn") => todo!(),

                _ => {
                    parse_ctx.report_syntax_error(Some(&first_token));

                    // Discard the rest of the line.
                    while let Some(token) = parse_ctx.next_token() {
                        if matches!(token.payload, tok!("end of line")) {
                            break;
                        }
                    }
                }
            }
        }

        // We're done parsing from this context, so end it.
        // (This will make REPT/FOR loop if possible, and pop everything else.)
        ctx_stack.sources_mut().end_current_context();
    }
}

struct ParseCtx<'ctx_stack, 'sources, 'symbols, 'nb_errs, 'options> {
    ctx_stack: &'ctx_stack ContextStack,
    sources: &'sources SourceStore,
    symbols: &'symbols mut Symbols<'ctx_stack>,
    nb_errors_remaining: &'nb_errs Cell<usize>,
    options: &'options Options,
}

impl<'ctx_stack> ParseCtx<'ctx_stack, '_, '_, '_, '_> {
    fn next_token(&mut self) -> Option<Token<'ctx_stack>> {
        lexer::next_token(
            self.ctx_stack,
            self.sources,
            self.symbols,
            self.nb_errors_remaining,
            self.options,
        )
    }
    fn next_token_raw(&mut self) -> Option<CompactString> {
        lexer::next_token_raw(
            self.ctx_stack,
            self.sources,
            self.symbols,
            self.nb_errors_remaining,
            self.options,
        )
    }

    fn is_next_char_a_colon(&mut self) -> bool {
        lexer::is_next_char_a_colon(
            self.ctx_stack,
            self.sources,
            self.symbols,
            self.nb_errors_remaining,
            self.options,
        )
    }

    fn report_syntax_error(&self, token: Option<&Token>) {
        match token {
            Some(Token { payload, span }) => diagnostics::error(
                span,
                |error| {
                    error
                        .with_message(format!("Syntax error: unexpected {payload}"))
                        .with_label(
                            diagnostics::error_label(span.resolve()).with_message("Expected TODO"),
                        )
                },
                self.sources,
                self.nb_errors_remaining,
                self.options,
            ),
            None => {
                let span = lexer::current_span(self.ctx_stack);
                diagnostics::error(
                    &span,
                    |error| {
                        error
                            .with_message("Syntax error: unexpected end of input")
                            .with_label(
                                diagnostics::error_label(span.resolve())
                                    .with_message("Expected TODO"),
                            )
                    },
                    self.sources,
                    self.nb_errors_remaining,
                    self.options,
                );
            }
        }
    }
}
