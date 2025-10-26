use std::{cell::Cell, fs::File, path::Path};

use compact_str::{CompactString, ToCompactString};

use crate::{
    charmap::Charmaps,
    diagnostics,
    macro_args::{MacroArgs, UniqueId},
    section::Sections,
    sources::{Source, Span},
    symbols::Symbols,
    Identifiers, Options,
};

pub mod lexer;
lalrpop_util::lalrpop_mod!(parser, "/asm/syntax/parser.rs");
mod semantics;
pub mod tokens;
use tokens::tok;

pub fn parse_file(
    path: &Path,
    with_search_path: bool,
    identifiers: &mut Identifiers,
    sections: &mut Sections,
    charmaps: &mut Charmaps,
    symbols: &mut Symbols,
    nb_errors_left: &Cell<usize>,
    options: &mut Options,
) {
    let res = if with_search_path {
        options.search_file(path)
    } else {
        let pathbuf = path.to_owned();
        match File::open(path) {
            Ok(file) => Some(Ok((file, pathbuf))),
            Err(err) => Some(Err((err, pathbuf))), // Pass `NotFound` as-is, so that it's not reported as a *search* error.
        }
    };
    let res = match res {
        None => {
            options.report_file_not_found_error(nb_errors_left, &Span::CommandLine, path.display());
            return;
        }
        Some(Ok((file, loaded_path))) => {
            Source::load_file(file, loaded_path.display().to_compact_string())
        }
        Some(Err((err, err_path))) => Err((err, err_path.display().to_compact_string())),
    };
    let src = match res {
        Ok(src) => src,
        Err((err, loaded_path)) => {
            diagnostics::error(
                &Span::CommandLine,
                |error| {
                    error.set_message(format!("failed to open \"{loaded_path}\""));
                    error.add_note(err);
                },
                nb_errors_left,
                options,
            );
            return;
        }
    };

    let mut parse_ctx = ParseCtx {
        lexer: lexer::Lexer::new(),
        // TODO(perf): tweak those capacities
        line_spans: vec![],
        macro_args: Vec::with_capacity(1),
        unique_id: UniqueId::new(),
        identifiers,
        charmaps,
        sections,
        symbols,
        nb_errors_left,
        options,
    };
    if let Err(()) =
        parse_ctx
            .lexer
            .push_file(src, None, parse_ctx.nb_errors_left, parse_ctx.options)
    {
        return;
    }

    let mut line_tokens = vec![];
    loop {
        while !parse_ctx.lexer.top_context_mut().is_empty() {
            // Lex a line, and then parse it.
            // Since the lexing of line N+1 is dependent on the contents of line N,
            // such an incremental approach is required.
            // (That, or `RefCell`ing all the things, but that sucks majorly.)
            line_tokens.clear();
            parse_ctx.line_spans.clear();

            // There are two kinds of lines: regular lines, and lines with a “raw” portion.
            // The latter is macro invocations and `opt` directives;
            // thus, the lexing phase has to do some parsing to figure out its lexing strategy.
            let mut token = parse_ctx.next_token(true);
            if matches!(token.payload, tok!("+") | tok!("-")) {
                // Leftover diff marker.
                parse_ctx.push_line_token(token, &mut line_tokens);
                token = parse_ctx.next_token(true);
            }
            if match &token.payload {
                tok!("identifier"(_)) => parse_ctx.lexer.next_char_is_a_colon(),
                tok!("local identifier"(_)) => true,
                _ => false,
            } {
                // Label definition.
                parse_ctx.push_line_token(token, &mut line_tokens);
                token = parse_ctx.next_token(true);
                if matches!(token.payload, tok!(":") | tok!("::")) {
                    parse_ctx.push_line_token(token, &mut line_tokens);
                    token = parse_ctx.next_token(true);
                }
            }

            match token.payload {
                tok!("end of line") => {} // No more tokens on the line (but some may have been pushed earlier).
                tok!("elif")
                    if parse_ctx
                        .lexer
                        .active_condition_mut()
                        .is_some_and(|cond| cond.entered_block) =>
                {
                    parse_ctx.push_line_token(token, &mut line_tokens);
                    // HACK: if we were executing the previous block, we need to not lex the remainder of the line!
                    //       This enables code like:
                    // ```
                    // if _NARG < 1
                    // ; ...
                    // elif \1 == 0
                    // ```
                    parse_ctx.lexer.skip_to_eol();
                    // The parser still expects to read something after the `elif`, so we push a token that normally doesn't occur: an end of line.
                    let token = parse_ctx.next_token(false);
                    debug_assert!(matches!(token.payload, tok!("end of line")));
                    parse_ctx.push_line_token(token, &mut line_tokens);
                }
                tok!("identifier"(_)) | tok!("opt") | tok!("pusho") => {
                    // Raw line.
                    parse_ctx.push_line_token(token, &mut line_tokens);
                    while let Some((string, span)) = parse_ctx.next_token_raw() {
                        parse_ctx.push_line_token(
                            tokens::Token {
                                payload: tok!("string"(string)),
                                span,
                            },
                            &mut line_tokens,
                        );
                    }
                }
                _ => {
                    // Regular line.
                    let mut enable_equs_expansion = true;
                    loop {
                        match &token.payload {
                            tok!("purge") | tok!("for") | tok!("def") | tok!("redef") => enable_equs_expansion = false,
                            tok!(")") // The closing parens of a `def()`...
                            | tok!(",") // ...the comma of a `for`...
                            | tok!("equ") // ...or a symbol definition keyword.
                            | tok!("equs")
                            | tok!("rb")
                            | tok!("rw")
                            | tok!("rl")
                            | tok!("=")
                            | tok!("+=")
                            | tok!("-=")
                            | tok!("*=")
                            | tok!("/=")
                            | tok!("%=")
                            | tok!("&=")
                            | tok!("|=")
                            | tok!("^=") => enable_equs_expansion = true,
                            _ => {}
                        }
                        parse_ctx.push_line_token(token, &mut line_tokens);
                        token = parse_ctx.next_token(enable_equs_expansion);
                        if matches!(token.payload, tok!("end of line")) {
                            break;
                        }
                    }
                }
            }

            // We accept empty lines, but LALRPOP doesn't allow empty productions.
            if !line_tokens.is_empty() {
                if let Err(err) =
                    parser::LineParser::new().parse(&mut parse_ctx, line_tokens.drain(..))
                {
                    if !parse_ctx.report_parse_error(err) {
                        break;
                    }
                }
            }
        }

        // Pop the current context, since it's empty; and if there are no more contexts, exit.
        if !parse_ctx.pop_context() {
            break;
        }
    }

    parse_ctx.lexer.debug_check_done();
    parse_ctx.unique_id.debug_check_empty();
    debug_assert!(parse_ctx.macro_args.is_empty());
}

pub struct ParseCtx<'idents, 'charmaps, 'sections, 'symbols, 'nb_errs, 'options> {
    lexer: lexer::Lexer,
    line_spans: Vec<Span>,
    identifiers: &'idents mut Identifiers,
    macro_args: Vec<MacroArgs>,
    unique_id: UniqueId,
    charmaps: &'charmaps mut Charmaps,
    sections: &'sections mut Sections,
    symbols: &'symbols mut Symbols,
    nb_errors_left: &'nb_errs Cell<usize>,
    options: &'options mut Options,
}
/// Shorthand for a [`ParseCtx`] with all lifetimes elided.
macro_rules! parse_ctx {
    () => {
        $crate::syntax::ParseCtx<'_, '_, '_, '_, '_, '_>
    };
}
use parse_ctx;

impl parse_ctx!() {
    fn next_token(&mut self, with_equs_expansion: bool) -> tokens::Token {
        loop {
            let token = self.lexer.next_token(
                self.identifiers,
                self.symbols,
                self.macro_args.last_mut(),
                &mut self.unique_id,
                self.sections,
                self.nb_errors_left,
                self.options,
            );
            if with_equs_expansion && !self.lexer.last_ident_was_raw {
                if let tok!("identifier"(ident)) = &token.payload {
                    if let Some(sym) = self.symbols.find(ident) {
                        if let Some(res) =
                            sym.get_string(self.sections.active_section.as_ref(), self.identifiers)
                        {
                            match res {
                                Ok(string) => self.lexer.expand_equs_symbol(
                                    *ident,
                                    string,
                                    token.span.extract_normal(),
                                    self.identifiers,
                                    self.nb_errors_left,
                                    self.options,
                                ),
                                Err(err) => self.error(&token.span, |error| {
                                    error.set_message(err);
                                    error.add_label(
                                        diagnostics::error_label(&token.span)
                                            .with_message("this expansion is invalid"),
                                    );
                                }),
                            }
                            continue;
                        }
                    }
                }
            }
            break token;
        }
    }

    fn next_token_raw(&mut self) -> Option<(CompactString, Span)> {
        self.lexer.next_token_raw(
            self.identifiers,
            self.symbols,
            self.macro_args.last_mut(),
            &mut self.unique_id,
            self.sections,
            self.nb_errors_left,
            self.options,
        )
    }

    fn push_line_token(
        &mut self,
        token: tokens::Token,
        line_tokens: &mut Vec<(usize, tokens::TokenPayload, usize)>,
    ) {
        let nb_tokens = self.line_spans.len();
        line_tokens.push((nb_tokens, token.payload, nb_tokens));
        self.line_spans.push(token.span);
    }
}
