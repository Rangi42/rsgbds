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

use ariadne::ReportBuilder;
use compact_str::CompactString;

use crate::{
    charmap::Charmaps,
    context_stack::{ContextStack, Span},
    diagnostics,
    macro_args::MacroArgs,
    section::Sections,
    source_store::{RawSpan, SourceHandle, SourceStore},
    symbols::Symbols,
    syntax::{
        lexer,
        tokens::{tok, Token},
    },
    Options,
};

mod directives;
mod expr;
mod instructions;
mod misc;
mod string;

macro_rules! expect_one_of {
    ($payload:expr => {
        $( None => $if_none:expr, )?
        $( $( Token {$( $captured_fields:ident $(: $capture_pat:pat)? ),+} )? |$($kind:tt $(($fields:tt))?)/+| => $then:expr, )+
        else |$unexpected:pat_param $(, $expected:pat_param)? $(,)?| => $handle_default:expr
    }) => {match $payload {
        $( None => $if_none, )?
        $( Some(Token {
            payload: $( $crate::syntax::tokens::tok!($kind $(($fields))?) )|+,
            $($($captured_fields $(: $capture_pat)?,)+)?
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

macro_rules! matches_tok {
    ($value:expr, $($name:tt)|+) => {
        matches!($value, Some(Token { payload: $(tok!($name))|+, .. }))
    };
}
use matches_tok;

macro_rules! require {
    ($payload:expr => $( Token {$( $captured_fields:ident ),+} @ )? |$($kind:tt $(($fields:tt))?)/+|
        else |$unexpected:pat_param $(, $expected:pat_param)? $(,)?| $handle_default:expr
    ) => {
        let ($($($captured_fields,)+)? $($($fields,)?)+) = match $payload {
            Some(Token {
                payload: $( crate::syntax::tokens::tok!($kind $(($fields))?) )|+,
                $($($captured_fields,)+)?
                ..
            }) => ($($($captured_fields,)+)? $($($fields,)?)+),
            $unexpected => $handle_default,
        };
    }
}
use require;

pub fn parse_file<'ctx_stack>(
    source: SourceHandle,
    ctx_stack: &'ctx_stack ContextStack,
    sections: &mut Sections<'ctx_stack>,
    sources: &SourceStore,
    charmaps: &mut Charmaps<'ctx_stack>,
    symbols: &mut Symbols<'ctx_stack>,
    nb_errors_remaining: &Cell<usize>,
    options: &mut Options,
) {
    let mut parse_ctx = ParseCtx {
        ctx_stack,
        charmaps,
        sections,
        sources,
        symbols,
        nb_errors_remaining,
        options,
    };

    ctx_stack.sources_mut().push_file_context(source);
    while ctx_stack.sources_mut().active_context().is_some() {
        while let Some(first_token) = parse_ctx.next_token() {
            let Some(()) = parse_line(first_token, &mut parse_ctx, ctx_stack) else {
                return;
            };
        }

        // We're done parsing from this context, so end it.
        // (This will make REPT/FOR loop if possible, and pop everything else.)
        ctx_stack.sources_mut().end_current_context();
    }
}

fn parse_line<'ctx_stack>(
    mut first_token: Token<'ctx_stack>,
    parse_ctx: &mut parse_ctx!('ctx_stack),
    ctx_stack: &'ctx_stack ContextStack,
) -> Option<()> {
    match &first_token.payload {
        tok!("+") | tok!("-") => {
            diagnostics::error(
                &first_token.span,
                |error| {
                    error.set_message("Leftover diff marker");
                    error.add_label(
                        diagnostics::error_label(&first_token.span)
                            .with_message("Extraneous character here"),
                    );
                    error.set_help("Consider applying patches using `patch` or `git apply`");
                },
                parse_ctx.sources,
                parse_ctx.nb_errors_remaining,
                parse_ctx.options,
            );
            match parse_ctx.next_token() {
                None => return Some(()),
                Some(token) => first_token = token,
            };
        }
        // TODO: handle Git conflict markers
        _ => {}
    }

    let mut label_span = None;
    if let tok!("identifier"(ident)) = first_token.payload {
        // Identifiers at the beginning of the line can be two things.
        // Either a label name, if it's *directly* followed by a colon; or the name of a macro.
        if parse_ctx.is_next_char_a_colon() {
            expect_one_of!(parse_ctx.next_token() => {
                None => unreachable!(),
                |":"| => {
                    label_span = Some(first_token.span.clone());
                    parse_ctx.symbols.define_label(
                        ident,
                        first_token.span,
                        false,
                        parse_ctx.sources,
                        parse_ctx.nb_errors_remaining,
                        parse_ctx.options
                    );
                    match parse_ctx.next_token() {
                        Some(token) => first_token = token,
                        None => return Some(()),
                    };
                },
                |"::"| => {
                    label_span = Some(first_token.span.clone());
                    parse_ctx.symbols.define_label(
                        ident,
                        first_token.span,
                        true,
                        parse_ctx.sources,
                        parse_ctx.nb_errors_remaining,
                        parse_ctx.options
                    );
                    match parse_ctx.next_token() {
                        Some(token) => first_token = token,
                        None => return Some(()),
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

    let eol_token = match first_token.payload {
        tok!("end of line") => Some(first_token), // Empty line.

        tok!("identifier"(ident)) => {
            // Macro call.
            // Get the macro's arguments.
            let args = Rc::new(MacroArgs::new(
                std::iter::from_fn(|| parse_ctx.next_token_raw().map(|(arg, _span)| arg)).collect(),
            ));

            let name = parse_ctx.symbols.resolve(ident);
            match parse_ctx.symbols.find_macro_interned(&ident) {
                None => diagnostics::error(
                    &first_token.span,
                    |error| {
                        error.set_message(format!("Macro `{name}` does not exist"));
                        error.add_label(
                            diagnostics::error_label(&first_token.span)
                                .with_message("Attempting to call the macro here"),
                        );
                    },
                    parse_ctx.sources,
                    parse_ctx.nb_errors_remaining,
                    parse_ctx.options,
                ),
                Some(Err(other)) => diagnostics::error(
                    &first_token.span,
                    |error| {
                        error.set_message(format!("`{name}` is not a macro"));
                        error.add_labels([
                            diagnostics::error_label(&first_token.span)
                                .with_message("Macro call here"),
                            diagnostics::note_label(other.def_span())
                                .with_message("A symbol by this name was defined here"),
                        ]);
                    },
                    parse_ctx.sources,
                    parse_ctx.nb_errors_remaining,
                    parse_ctx.options,
                ),
                Some(Ok(slice)) => {
                    ctx_stack
                        .sources_mut()
                        .push_macro_context(name.into(), slice, args)
                }
            }

            parse_ctx.next_token()
        }

        // These are not valid after a label.
        tok!("macro") => {
            reject_prior_label_def(parse_ctx, label_span, &first_token.span, "macro");
            directives::parse_macro(first_token, parse_ctx)
        }
        tok!("endm") => {
            reject_prior_label_def(parse_ctx, label_span, &first_token.span, "endm");
            directives::parse_endm(first_token, parse_ctx)
        }
        tok!("rept") => {
            reject_prior_label_def(parse_ctx, label_span, &first_token.span, "rept");
            directives::parse_rept(first_token, parse_ctx)
        }
        tok!("for") => {
            reject_prior_label_def(parse_ctx, label_span, &first_token.span, "for");
            directives::parse_for(first_token, parse_ctx)
        }
        tok!("endr") => {
            reject_prior_label_def(parse_ctx, label_span, &first_token.span, "endr");
            directives::parse_endr(first_token, parse_ctx)
        }
        tok!("if") => {
            reject_prior_label_def(parse_ctx, label_span, &first_token.span, "if");
            directives::parse_if(first_token, parse_ctx)
        }
        tok!("elif") => {
            reject_prior_label_def(parse_ctx, label_span, &first_token.span, "elif");
            directives::parse_elif(first_token, parse_ctx)
        }
        tok!("else") => {
            reject_prior_label_def(parse_ctx, label_span, &first_token.span, "else");
            directives::parse_else(first_token, parse_ctx)
        }
        tok!("endc") => {
            reject_prior_label_def(parse_ctx, label_span, &first_token.span, "endc");
            directives::parse_endc(first_token, parse_ctx)
        }

        tok!("adc") => instructions::parse_adc(first_token, parse_ctx),
        tok!("add") => instructions::parse_add(first_token, parse_ctx),
        tok!("and") => instructions::parse_and(first_token, parse_ctx),
        tok!("bit") => instructions::parse_bit(first_token, parse_ctx),
        tok!("call") => instructions::parse_call(first_token, parse_ctx),
        tok!("ccf") => instructions::parse_ccf(first_token, parse_ctx),
        tok!("cp") => instructions::parse_cp(first_token, parse_ctx),
        tok!("cpl") => instructions::parse_cpl(first_token, parse_ctx),
        tok!("daa") => instructions::parse_daa(first_token, parse_ctx),
        tok!("dec") => instructions::parse_dec(first_token, parse_ctx),
        tok!("di") => instructions::parse_di(first_token, parse_ctx),
        tok!("ei") => instructions::parse_ei(first_token, parse_ctx),
        tok!("halt") => instructions::parse_halt(first_token, parse_ctx),
        tok!("inc") => instructions::parse_inc(first_token, parse_ctx),
        tok!("jp") => instructions::parse_jp(first_token, parse_ctx),
        tok!("jr") => instructions::parse_jr(first_token, parse_ctx),
        tok!("ldd") => instructions::parse_ldd(first_token, parse_ctx),
        tok!("ldh") => instructions::parse_ldh(first_token, parse_ctx),
        tok!("ldi") => instructions::parse_ldi(first_token, parse_ctx),
        tok!("ld") => instructions::parse_ld(first_token, parse_ctx),
        tok!("nop") => instructions::parse_nop(first_token, parse_ctx),
        tok!("or") => instructions::parse_or(first_token, parse_ctx),
        tok!("pop") => instructions::parse_pop(first_token, parse_ctx),
        tok!("push") => instructions::parse_push(first_token, parse_ctx),
        tok!("res") => instructions::parse_res(first_token, parse_ctx),
        tok!("reti") => instructions::parse_reti(first_token, parse_ctx),
        tok!("ret") => instructions::parse_ret(first_token, parse_ctx),
        tok!("rla") => instructions::parse_rla(first_token, parse_ctx),
        tok!("rlca") => instructions::parse_rlca(first_token, parse_ctx),
        tok!("rlc") => instructions::parse_rlc(first_token, parse_ctx),
        tok!("rl") => instructions::parse_rl(first_token, parse_ctx),
        tok!("rra") => instructions::parse_rra(first_token, parse_ctx),
        tok!("rrca") => instructions::parse_rrca(first_token, parse_ctx),
        tok!("rrc") => instructions::parse_rrc(first_token, parse_ctx),
        tok!("rr") => instructions::parse_rr(first_token, parse_ctx),
        tok!("rst") => instructions::parse_rst(first_token, parse_ctx),
        tok!("sbc") => instructions::parse_sbc(first_token, parse_ctx),
        tok!("scf") => instructions::parse_scf(first_token, parse_ctx),
        tok!("set") => instructions::parse_set(first_token, parse_ctx),
        tok!("sla") => instructions::parse_sla(first_token, parse_ctx),
        tok!("sra") => instructions::parse_sra(first_token, parse_ctx),
        tok!("srl") => instructions::parse_srl(first_token, parse_ctx),
        tok!("stop") => instructions::parse_stop(first_token, parse_ctx),
        tok!("sub") => instructions::parse_sub(first_token, parse_ctx),
        tok!("swap") => instructions::parse_swap(first_token, parse_ctx),
        tok!("xor") => instructions::parse_xor(first_token, parse_ctx),

        tok!("align") => directives::parse_align(first_token, parse_ctx),
        tok!("assert") => directives::output::parse_assert(first_token, parse_ctx),
        tok!("break") => directives::parse_break(first_token, parse_ctx),
        tok!("charmap") => directives::charmap::parse_charmap(first_token, parse_ctx),
        tok!("db") => directives::parse_db(first_token, parse_ctx),
        tok!("dl") => directives::parse_dl(first_token, parse_ctx),
        tok!("ds") => directives::parse_ds(first_token, parse_ctx),
        tok!("dw") => directives::parse_dw(first_token, parse_ctx),
        tok!("endsection") => directives::parse_endsection(first_token, parse_ctx),
        tok!("endl") => directives::parse_endl(first_token, parse_ctx),
        tok!("endu") => directives::parse_endu(first_token, parse_ctx),
        tok!("export") => directives::parse_export(first_token, parse_ctx),
        tok!("fail") => {
            directives::output::parse_fail(first_token, parse_ctx);
            return None;
        }
        tok!("fatal") => directives::parse_fatal(first_token, parse_ctx),
        tok!("incbin") => directives::parse_incbin(first_token, parse_ctx),
        tok!("include") => directives::parse_include(first_token, parse_ctx),
        tok!("load") => directives::parse_load(first_token, parse_ctx),
        tok!("newcharmap") => directives::charmap::parse_newcharmap(first_token, parse_ctx),
        tok!("nextu") => directives::parse_nextu(first_token, parse_ctx),
        tok!("opt") => directives::opt::parse_opt(first_token, parse_ctx),
        tok!("popc") => directives::charmap::parse_popc(first_token, parse_ctx),
        tok!("popo") => directives::opt::parse_popo(first_token, parse_ctx),
        tok!("pops") => directives::parse_pops(first_token, parse_ctx),
        tok!("println") => directives::output::parse_println(first_token, parse_ctx),
        tok!("print") => directives::output::parse_print(first_token, parse_ctx),
        tok!("purge") => directives::parse_purge(first_token, parse_ctx),
        tok!("pushc") => directives::charmap::parse_pushc(first_token, parse_ctx),
        tok!("pusho") => directives::opt::parse_pusho(first_token, parse_ctx),
        tok!("pushs") => directives::parse_pushs(first_token, parse_ctx),
        tok!("rb") => directives::parse_rb(first_token, parse_ctx),
        tok!("rw") => directives::parse_rw(first_token, parse_ctx),
        tok!("redef") => directives::parse_redef(first_token, parse_ctx),
        tok!("rsreset") => directives::parse_rsreset(first_token, parse_ctx),
        tok!("rsset") => directives::parse_rsset(first_token, parse_ctx),
        tok!("section") => directives::parse_section(first_token, parse_ctx),
        tok!("setcharmap") => directives::charmap::parse_setcharmap(first_token, parse_ctx),
        tok!("shift") => directives::parse_shift(first_token, parse_ctx),
        tok!("static_assert") => directives::output::parse_static_assert(first_token, parse_ctx),
        tok!("union") => directives::parse_union(first_token, parse_ctx),
        tok!("warn") => directives::output::parse_warn(first_token, parse_ctx),

        _ => {
            parse_ctx.report_syntax_error(Some(&first_token), |error, span| {
                error.add_label(
                    diagnostics::error_label(span)
                        .with_message("Expected an instruction or a directive here"),
                )
            });

            // Discard the rest of the line.
            loop {
                match parse_ctx.next_token() {
                    None => break None,
                    Some(
                        token @ Token {
                            payload: tok!("end of line"),
                            ..
                        },
                    ) => break Some(token),
                    Some(_) => {}
                }
            }
        }
    };

    expect_one_of!(eol_token => {
        None => {},
        |"end of line"| => {},
        else |unexpected| => {
            parse_ctx.report_syntax_error(unexpected.as_ref(), |error, span| {
                error.add_label(diagnostics::error_label(span).with_message("Expected nothing else on this line"))
            });
        }
    });

    Some(())
}

fn reject_prior_label_def<'ctx_stack>(
    parse_ctx: &mut parse_ctx!('ctx_stack),
    label_span: Option<Span<'ctx_stack>>,
    directive_span: &Span<'ctx_stack>,
    directive_name: &str,
) {
    if let Some(span) = label_span {
        diagnostics::error(
            &span,
            |error| {
                error.set_message("A label is not allowed here");
                error.add_labels([
                    diagnostics::error_label(&span)
                        .with_message("This label cannot be on the same line..."),
                    diagnostics::note_label(directive_span)
                        .with_message(format!("...as a `{directive_name}` directive")),
                ]);
            },
            parse_ctx.sources,
            parse_ctx.nb_errors_remaining,
            parse_ctx.options,
        )
    }
}

struct ParseCtx<'ctx_stack, 'charmaps, 'sections, 'sources, 'symbols, 'nb_errs, 'options> {
    ctx_stack: &'ctx_stack ContextStack,
    charmaps: &'charmaps mut Charmaps<'ctx_stack>,
    sections: &'sections mut Sections<'ctx_stack>,
    sources: &'sources SourceStore,
    symbols: &'symbols mut Symbols<'ctx_stack>,
    nb_errors_remaining: &'nb_errs Cell<usize>,
    options: &'options mut Options,
}
macro_rules! parse_ctx {
    () => {
        $crate::syntax::parser::ParseCtx<'_, '_, '_, '_, '_, '_, '_>
    };
    ($ctx_stack:lifetime) => {
        $crate::syntax::parser::ParseCtx<$ctx_stack, '_, '_, '_, '_, '_, '_>
    };
}
use parse_ctx;

impl<'ctx_stack> parse_ctx!('ctx_stack) {
    fn next_token(&mut self) -> Option<Token<'ctx_stack>> {
        lexer::next_token(
            true,
            self.ctx_stack,
            self.sources,
            self.symbols,
            self.nb_errors_remaining,
            self.options,
        )
    }
    fn next_token_unexpanded(&mut self) -> Option<Token<'ctx_stack>> {
        lexer::next_token(
            false,
            self.ctx_stack,
            self.sources,
            self.symbols,
            self.nb_errors_remaining,
            self.options,
        )
    }
    fn next_token_raw(&mut self) -> Option<(CompactString, Span<'ctx_stack>)> {
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

    fn current_span(&self) -> Span<'ctx_stack> {
        lexer::current_span(self.ctx_stack)
    }
    fn extended_to(
        &self,
        span: &Span<'ctx_stack>,
        token: Option<&Token<'ctx_stack>>,
    ) -> Span<'ctx_stack> {
        match token {
            Some(tok) => span.merged_with(&tok.span),
            None => span.merged_with(&self.current_span()),
        }
    }

    fn report_syntax_error<F: FnOnce(&mut ReportBuilder<'_, RawSpan>, &Span)>(
        &self,
        token: Option<&Token>,
        callback: F,
    ) {
        match token {
            Some(Token { payload, span }) => diagnostics::error(
                span,
                |error| {
                    error.set_message(format!("Syntax error: unexpected {payload}"));
                    callback(error, span);
                },
                self.sources,
                self.nb_errors_remaining,
                self.options,
            ),
            None => {
                let span = self.current_span();
                diagnostics::error(
                    &span,
                    |error| {
                        error.set_message("Syntax error: unexpected end of input");
                        callback(error, &span);
                    },
                    self.sources,
                    self.nb_errors_remaining,
                    self.options,
                );
            }
        }
    }
    fn report_expr_error(&self, error: crate::expr::Error<'ctx_stack>) {
        error.report(self.sources, self.nb_errors_remaining, self.options);
    }
}
