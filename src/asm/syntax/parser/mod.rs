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

use std::{cell::Cell, path::Path, rc::Rc};

use compact_str::CompactString;

use crate::{
    charmap::Charmaps,
    diagnostics::{self, ReportBuilder, WarningKind},
    macro_args::MacroArgs,
    section::Sections,
    sources::{Source, Span},
    symbols::Symbols,
    syntax::{
        lexer,
        tokens::{tok, Token},
    },
    Identifiers, Options,
};

mod directives;
mod expr;
mod instructions;
mod misc;
mod string;

macro_rules! expect_one_of {
    ($payload:expr => {
        $( $( Token {$( $captured_fields:ident $(: $capture_pat:pat)? ),+} )? |$($kind:tt $(($($fields:tt),+))?)/+| => $then:expr, )+
        else |$unexpected:pat_param $(, $expected:pat_param)? $(,)?| => $handle_default:expr
    }) => {match $payload {
        $( Token {
            payload: $( $crate::syntax::tokens::tok!($kind $(($($fields),+))?) )|+,
            $($($captured_fields $(: $capture_pat)?,)+)?
            ..
        } => $then, )+
        $unexpected => {
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
    ($value:expr, $($name:tt $(($field:pat))?)|+) => {
        matches!($value, Token { payload: $($crate::syntax::parser::tok!($name $(($field))?))|+, .. })
    };
}
use matches_tok;

macro_rules! require {
    ($payload:expr => $( Token {$( $captured_fields:ident ),+} @ )? |$($kind:tt $(($($fields:tt),+))?)/+|
        else |$unexpected:pat_param $(, $expected:pat_param)? $(,)?| $handle_default:expr
    ) => {
        let ($($($captured_fields,)+)? $($($($fields),+,)?)+) = match $payload {
            Token {
                payload: $( crate::syntax::tokens::tok!($kind $(($($fields),+))?) )|+,
                $($($captured_fields,)+)?
                ..
            } => ($($($captured_fields,)+)? $($($($fields),+,)?)+),
            $unexpected => $handle_default,
        };
    }
}
use require;

pub fn parse_file(
    path: &Path,
    identifiers: &mut Identifiers,
    sections: &mut Sections,
    charmaps: &mut Charmaps,
    symbols: &mut Symbols,
    nb_errors_remaining: &Cell<usize>,
    options: &mut Options,
) {
    let file = match Source::load_file(path) {
        Ok(file) => file,
        Err(err) => {
            diagnostics::error(
                &Span::CommandLine,
                |error| {
                    error.set_message(format!("Failed to open \"{}\"", path.display()));
                    error.add_note(err);
                },
                &nb_errors_remaining,
                &options,
            );
            return;
        }
    };

    let mut parse_ctx = ParseCtx {
        lexer: Lexer::new(),
        macro_args: Vec::with_capacity(1),
        identifiers,
        charmaps,
        sections,
        symbols,
        nb_errors_remaining,
        options,
    };
    parse_ctx.lexer.push_file(file, None);

    loop {
        loop {
            let first_token = parse_ctx.next_token();
            if matches_tok!(first_token, "end of input") {
                break;
            }
            let Some(()) = parse_line(first_token, &mut parse_ctx) else {
                // Fatal error!
                return;
            };
        }

        // We're done parsing from this context, so end it.
        // (This will make REPT/FOR loop if possible, and pop everything else.)
        if !parse_ctx.lexer.pop_context() {
            break;
        }
    }
}

fn parse_line(mut first_token: Token, parse_ctx: &mut parse_ctx!()) -> Option<()> {
    match &first_token.payload {
        tok!("+") | tok!("-") => {
            parse_ctx.error(&first_token.span, |error| {
                error.set_message("Leftover diff marker");
                error.add_label(
                    diagnostics::error_label(&first_token.span)
                        .with_message("Extraneous character here"),
                );
                error.set_help("Consider applying patches using `patch` or `git apply`");
            });
            first_token = parse_ctx.next_token();
        }
        // TODO: handle Git conflict markers
        _ => {}
    }

    let mut label_span = None;
    if let tok!("identifier"(ident, is_followed_by_colon)) = first_token.payload {
        // Identifiers at the beginning of the line can be two things.
        // Either a label name, if it's *directly* followed by a colon; or the name of a macro.
        if is_followed_by_colon {
            expect_one_of!(parse_ctx.next_token() => {
                |":"| => {
                    label_span = Some(first_token.span.clone());
                    parse_ctx.symbols.define_label(
                        ident,
                        parse_ctx.identifiers,
                        first_token.span,
                        false,
                        parse_ctx.nb_errors_remaining,
                        parse_ctx.options
                    );
                    first_token= parse_ctx.next_token();
                },
                |"::"| => {
                    label_span = Some(first_token.span.clone());
                    parse_ctx.symbols.define_label(
                        ident,
                        parse_ctx.identifiers,
                        first_token.span,
                        true,
                        parse_ctx.nb_errors_remaining,
                        parse_ctx.options
                    );
                    first_token= parse_ctx.next_token();
                },
                else |token, _expected| => {
                    // Try continuing the parse with this token as the first in the line.
                    // We know the token can't be empty, because the lexer just reported that its next character would be a colon.
                    first_token = token;
                }
            });
        }

        // TODO: handle directives that cannot be preceded by a label def.
    }

    let eol_token = match first_token.payload {
        tok!("end of line") | tok!("end of input") => first_token, // Empty line.

        tok!("identifier"(ident, _)) => {
            // Macro call.
            // Get the macro's arguments.
            let args =
                std::iter::from_fn(|| parse_ctx.next_token_raw().map(|(arg, _span)| arg)).collect();

            let name = parse_ctx.identifiers.resolve(ident).unwrap();
            match parse_ctx.symbols.find_macro(&ident) {
                None => parse_ctx.error(&first_token.span, |error| {
                    error.set_message(format!("Macro `{name}` does not exist"));
                    error.add_label(
                        diagnostics::error_label(&first_token.span)
                            .with_message("Attempting to call the macro here"),
                    );
                }),
                Some(Err(other)) => parse_ctx.error(&first_token.span, |error| {
                    error.set_message(format!("`{name}` is not a macro"));
                    error.add_labels([
                        diagnostics::error_label(&first_token.span).with_message("Macro call here"),
                        diagnostics::note_label(other.def_span())
                            .with_message("A symbol by this name was defined here"),
                    ]);
                }),
                Some(Ok(slice)) => {
                    let Span::Normal(span) = first_token.span else {
                        unreachable!();
                    };
                    parse_ctx
                        .lexer
                        .push_macro(slice.clone(), Rc::new(span.clone()));
                    parse_ctx.macro_args.push(args);
                }
            }

            parse_ctx.next_token()
        }

        // These are not valid after a label.
        tok!("macro") => {
            reject_prior_label_def(parse_ctx, label_span, &first_token.span, "macro");
            directives::context::parse_macro(first_token, parse_ctx)
        }
        tok!("endm") => {
            reject_prior_label_def(parse_ctx, label_span, &first_token.span, "endm");
            directives::parse_endm(first_token, parse_ctx)
        }
        tok!("rept") => {
            reject_prior_label_def(parse_ctx, label_span, &first_token.span, "rept");
            directives::context::parse_rept(first_token, parse_ctx)
        }
        tok!("for") => {
            reject_prior_label_def(parse_ctx, label_span, &first_token.span, "for");
            directives::context::parse_for(first_token, parse_ctx)
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
        tok!("endsection") => directives::section::parse_endsection(first_token, parse_ctx),
        tok!("endl") => directives::section::parse_endl(first_token, parse_ctx),
        tok!("endu") => directives::parse_endu(first_token, parse_ctx),
        tok!("export") => directives::parse_export(first_token, parse_ctx),
        tok!("fail") => {
            directives::output::parse_fail(first_token, parse_ctx);
            return None;
        }
        tok!("fatal") => directives::parse_fatal(first_token, parse_ctx),
        tok!("incbin") => directives::parse_incbin(first_token, parse_ctx),
        tok!("include") => directives::context::parse_include(first_token, parse_ctx),
        tok!("load") => directives::section::parse_load(first_token, parse_ctx),
        tok!("newcharmap") => directives::charmap::parse_newcharmap(first_token, parse_ctx),
        tok!("nextu") => directives::parse_nextu(first_token, parse_ctx),
        tok!("opt") => directives::opt::parse_opt(first_token, parse_ctx),
        tok!("popc") => directives::charmap::parse_popc(first_token, parse_ctx),
        tok!("popo") => directives::opt::parse_popo(first_token, parse_ctx),
        tok!("pops") => directives::section::parse_pops(first_token, parse_ctx),
        tok!("println") => directives::output::parse_println(first_token, parse_ctx),
        tok!("print") => directives::output::parse_print(first_token, parse_ctx),
        tok!("purge") => directives::parse_purge(first_token, parse_ctx),
        tok!("pushc") => directives::charmap::parse_pushc(first_token, parse_ctx),
        tok!("pusho") => directives::opt::parse_pusho(first_token, parse_ctx),
        tok!("pushs") => directives::section::parse_pushs(first_token, parse_ctx),
        tok!("rb") => directives::parse_rb(first_token, parse_ctx),
        tok!("rw") => directives::parse_rw(first_token, parse_ctx),
        tok!("redef") => directives::parse_redef(first_token, parse_ctx),
        tok!("rsreset") => directives::parse_rsreset(first_token, parse_ctx),
        tok!("rsset") => directives::parse_rsset(first_token, parse_ctx),
        tok!("section") => directives::section::parse_section(first_token, parse_ctx),
        tok!("setcharmap") => directives::charmap::parse_setcharmap(first_token, parse_ctx),
        tok!("shift") => directives::parse_shift(first_token, parse_ctx),
        tok!("static_assert") => directives::output::parse_static_assert(first_token, parse_ctx),
        tok!("union") => directives::parse_union(first_token, parse_ctx),
        tok!("warn") => directives::output::parse_warn(first_token, parse_ctx),

        _ => {
            parse_ctx.report_syntax_error(&first_token, |error, span| {
                error.add_label(
                    diagnostics::error_label(span)
                        .with_message("Expected an instruction or a directive here"),
                )
            });

            // Discard the rest of the line.
            loop {
                if let token @ Token {
                    payload: tok!("end of line") | tok!("end of input"),
                    ..
                } = parse_ctx.next_token()
                {
                    break token;
                }
            }
        }
    };

    expect_one_of!(eol_token => {
        |"end of line" / "end of input"| => {},
        else |unexpected| => {
            parse_ctx.report_syntax_error(&unexpected, |error, span| {
                error.add_label(diagnostics::error_label(span).with_message("Expected nothing else on this line"))
            });
        }
    });

    Some(())
}

fn reject_prior_label_def(
    parse_ctx: &mut parse_ctx!(),
    label_span: Option<Span>,
    directive_span: &Span,
    directive_name: &str,
) {
    if let Some(span) = label_span {
        parse_ctx.error(&span, |error| {
            error.set_message("A label is not allowed here");
            error.add_labels([
                diagnostics::error_label(&span)
                    .with_message("This label cannot be on the same line..."),
                diagnostics::note_label(directive_span)
                    .with_message(format!("...as a `{directive_name}` directive")),
            ]);
        })
    }
}

struct ParseCtx<'idents, 'charmaps, 'sections, 'symbols, 'nb_errs, 'options> {
    lexer: Lexer,
    identifiers: &'idents mut Identifiers,
    macro_args: Vec<MacroArgs>,
    charmaps: &'charmaps mut Charmaps,
    sections: &'sections mut Sections,
    symbols: &'symbols mut Symbols,
    nb_errors_remaining: &'nb_errs Cell<usize>,
    options: &'options mut Options,
}
macro_rules! parse_ctx {
    () => {
        $crate::syntax::parser::ParseCtx<'_, '_, '_, '_, '_, '_>
    };
}
use parse_ctx;

use super::lexer::Lexer;

impl parse_ctx!() {
    fn next_token(&mut self) -> Token {
        self.lexer.next_token(
            self.identifiers,
            self.symbols,
            self.macro_args.last(),
            self.nb_errors_remaining,
            self.options,
        )
    }
    fn next_token_raw(&mut self) -> Option<(CompactString, Span)> {
        self.lexer.next_token_raw(
            self.identifiers,
            self.symbols,
            self.macro_args.last(),
            self.nb_errors_remaining,
            self.options,
        )
    }

    fn extended_to(&self, span: &Span, token: &Token) -> Span {
        span.merged_with(&token.span)
    }

    fn error<'span, F: FnOnce(&mut ReportBuilder<'span>)>(&self, span: &'span Span, callback: F) {
        diagnostics::error(span, callback, self.nb_errors_remaining, self.options);
    }
    fn report_syntax_error<'span, F: FnOnce(&mut ReportBuilder<'span>, &'span Span)>(
        &self,
        token: &'span Token,
        callback: F,
    ) {
        self.error(&token.span, |error| {
            error.set_message(format!("Syntax error: unexpected {}", &token.payload));
            callback(error, &token.span);
        })
    }
    fn report_expr_error(&self, error: crate::expr::Error) {
        error.report(self.nb_errors_remaining, self.options);
    }

    fn warn<'span, F: FnOnce(&mut ReportBuilder<'span>)>(
        &self,
        id: WarningKind,
        span: &'span Span,
        callback: F,
    ) {
        diagnostics::warn(id, span, callback, self.nb_errors_remaining, self.options);
    }
}
