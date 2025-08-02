//! The innermost part of the language's processing.
//!
//! `rgbasm`'s lexer is unusual because of the language's design.
//! In particular, macro arguments (`\1` etc.) and interpolation (`{DUCK}`) work at a textual level;
//! this is enforced by their semantics, particularly the implicit token pasting.
//!
//! The first (known) design of the lexer handled them in the various token functions,
//! but this meant that there was a lot of duplicated handling, and this caused a lot of bugs ([#63], [#362], [#531]...)
//! Instead, we handle them at the lowest level, thus handling them in a manner transparent to the rest of the lexer.
//!
//! TODO: describe the "char stream" / remainder split more
//!
//! [#63]: https://github.com/gbdev/rgbds/issues/63
//! [#362]: https://github.com/gbdev/rgbds/issues/362
//! [#531]: https://github.com/gbdev/rgbds/issues/531

use std::{cell::Cell, iter::Peekable, ops::Deref, rc::Rc, str::CharIndices};

use compact_str::CompactString;
use unicase::UniCase;

use crate::{
    cond::{self, Condition},
    diagnostics::{self, warning, ReportBuilder},
    format::FormatSpec,
    macro_args::{MacroArgs, UniqueId},
    section::Sections,
    sources::{NormalSpan, Source, Span, SpanKind},
    symbols::Symbols,
    Identifier, Identifiers, Options,
};

use super::tokens::{tok, Token, TokenPayload, KEYWORDS};

#[derive(Debug)]
pub struct Lexer {
    contexts: Vec<Context>,
    pub cond_stack: Vec<Condition>,
}

#[derive(Debug)]
pub struct Context {
    pub span: NormalSpan,
    cur_byte: usize,
    /// Bytes before this one have already been scanned for expansions.
    /// Note that this is only updated when necessary, so it may be lagging behind `cur_byte`.
    ///
    /// This is akin to the C preprocessor's “blue paint”.
    ofs_scanned_for_expansion: usize,
    /// This is only used by loop nodes.
    pub loop_state: LoopInfo,
    /// Depth of the conditional stack on entry.
    pub cond_stack_depth: usize,
}
#[derive(Debug, Default)]
pub struct LoopInfo {
    pub nb_iters: u32,
    pub for_var: Option<Identifier>,
    pub for_value: i32,
    pub for_step: i32,
}

impl Lexer {
    pub fn new() -> Self {
        Self {
            contexts: vec![],
            cond_stack: vec![],
        }
    }

    pub fn push_file(
        &mut self,
        file: Rc<Source>,
        parent: Option<Rc<NormalSpan>>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        debug_assert_eq!(parent.is_none(), self.contexts.is_empty());

        self.push_context(
            NormalSpan::new(file, SpanKind::File, parent),
            Default::default(),
            nb_errors_left,
            options,
        );
    }

    pub fn push_macro(
        &mut self,
        macro_name: Identifier,
        mut contents: NormalSpan,
        parent: Rc<NormalSpan>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        contents.parent = Some(parent);
        contents.kind = SpanKind::Macro(macro_name);
        self.push_context(contents, Default::default(), nb_errors_left, options);
    }

    pub fn push_loop(
        &mut self,
        loop_info: LoopInfo,
        mut contents: NormalSpan,
        parent: Rc<NormalSpan>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        contents.parent = Some(parent);
        contents.kind = SpanKind::Loop(0);
        self.push_context(contents, loop_info, nb_errors_left, options);
    }
}

struct LexerParams<'idents, 'sym, 'mac_args, 'uniq_id, 'sections, 'nb_err, 'opts> {
    identifiers: &'idents mut Identifiers,
    symbols: &'sym Symbols,
    macro_args: Option<&'mac_args mut MacroArgs>,
    unique_id: &'uniq_id mut UniqueId,
    sections: &'sections Sections,
    nb_errors_left: &'nb_err Cell<usize>,
    options: &'opts Options,
}

macro_rules! chars {
    (whitespace) => {
        ' ' | '\t'
    };
    (newline) => {
        '\n' | '\r'
    };
    (line_cont) => {
        chars!(whitespace) | chars!(newline) | ';'
    };
    (ident_start) => {
        'a'..='z' | 'A'..='Z' | '_'
    };
    (ident) => {
        chars!(ident_start) | '0'..='9' | '$' | '@' | '#'
    }
}
pub fn is_whitespace(ch: char) -> bool {
    matches!(ch, chars!(whitespace))
}
pub fn is_newline(ch: char) -> bool {
    matches!(ch, chars!(newline))
}

impl Context {
    fn is_empty(&self) -> bool {
        debug_assert!(self.span.is_offset_valid(self.cur_byte));
        self.cur_byte == self.span.bytes.end
    }

    fn remaining_text(&self) -> &str {
        &self.span.src.contents.text()[self.cur_byte..self.span.bytes.end]
    }

    fn new_span(&self) -> NormalSpan {
        let mut span = self.span.clone();
        span.bytes = self.cur_byte..self.cur_byte;
        span
    }
}

impl Lexer {
    fn active_context(&mut self) -> Option<&mut Context> {
        // Any contexts that we are at the end of are only kept to prevent some infinite expansion cases,
        // but they should be ignored..
        self.contexts
            .iter_mut()
            .rev()
            .find(|ctx| !ctx.is_empty() || !ctx.span.kind.ends_implicitly())
    }

    fn push_context(
        &mut self,
        span: NormalSpan,
        loop_info: LoopInfo,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        debug_assert!(self.contexts.len() <= options.runtime_opts.recursion_depth);

        if self.contexts.len() == options.runtime_opts.recursion_depth {
            let span = match span.parent {
                Some(trigger_span) => Span::Normal(trigger_span.as_ref().clone()),
                None => Span::CommandLine,
            };
            Self::report_depth_overflow(self.contexts.len(), &span, nb_errors_left, options);
        } else {
            self.contexts.push(Context {
                cur_byte: span.bytes.start,
                // Expansions that end implicitly cannot contain other expansions.
                ofs_scanned_for_expansion: if span.kind.ends_implicitly() {
                    span.bytes.end
                } else {
                    span.bytes.start
                },
                span,
                loop_state: loop_info,
                cond_stack_depth: self.cond_stack.len(),
            })
        }
    }

    pub fn set_recursion_depth(
        &self,
        new_depth: usize,
        opt_span: &Span,
        nb_errors_left: &Cell<usize>,
        options: &mut Options,
    ) {
        if self.contexts.len() > new_depth {
            diagnostics::error(
                opt_span,
                |error| {
                    error.set_message("Recursion is deeper than new limit");
                    error.add_label(diagnostics::error_label(opt_span).with_message(format!(
                        "Depth is {} contexts at this point",
                        self.contexts.len()
                    )))
                },
                nb_errors_left,
                options,
            );
        } else {
            options.runtime_opts.recursion_depth = new_depth;
        }
    }

    pub fn top_context(&mut self) -> &mut Context {
        let ctx = self.active_context().expect("No active lexer context");
        debug_assert!(!ctx.span.kind.ends_implicitly());
        ctx
    }

    pub fn reset_loop_context(&mut self, nb_errors_left: &Cell<usize>, options: &Options) {
        let ctx = loop {
            let ctx = self
                .contexts
                .last_mut()
                .expect("Attempting to reset a lexer context when there are no more!?");
            // Pop any remaining implicitly-ending contexts.
            // (e.g. if the current top-level “hard“ context ends with an expansion).
            if !ctx.span.kind.ends_implicitly() {
                break ctx;
            }
            debug_assert_eq!(
                ctx.span.bytes.start, ctx.span.bytes.end,
                "Implicitly-ending context not empty when resetting!?"
            );
            self.contexts.pop();
        };

        debug_assert!(matches!(ctx.span.kind, SpanKind::Loop(..)));

        Lexer::report_unterminated_conditionals(&mut self.cond_stack, ctx, nb_errors_left, options);

        ctx.cur_byte = ctx.span.bytes.start;
        ctx.ofs_scanned_for_expansion = ctx.span.bytes.start;
    }

    /// Returns whether there are still more contexts after popping the top (active) one.
    #[must_use]
    pub fn pop_context(&mut self, nb_errors_left: &Cell<usize>, options: &Options) -> bool {
        let ctx = loop {
            let ctx = self
                .contexts
                .pop()
                .expect("Attempting to pop a lexer context when there are no more!?");
            // Pop any remaining implicitly-ending contexts.
            // (e.g. if the current top-level “hard“ context ends with an expansion).
            if !ctx.span.kind.ends_implicitly() {
                break ctx;
            }
            debug_assert_eq!(
                ctx.span.bytes.start, ctx.span.bytes.end,
                "Implicitly-ending context not empty when popping!?"
            );
        };

        Self::report_unterminated_conditionals(&mut self.cond_stack, &ctx, nb_errors_left, options);

        !self.contexts.is_empty()
    }

    fn report_unterminated_conditionals(
        cond_stack: &mut Vec<Condition>,
        ctx: &Context,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        debug_assert!(
            cond_stack.len() >= ctx.cond_stack_depth,
            "Fewer conditionals active than when the context was created!?",
        );

        while cond_stack.len() > ctx.cond_stack_depth {
            let condition = cond_stack.pop().unwrap();
            diagnostics::error(
                &condition.opening_span,
                |error| {
                    error.set_message("Unterminated conditional block");
                    error.add_label(
                        diagnostics::error_label(&condition.opening_span)
                            .with_message("This `IF` is missing a corresponding `ENDC`"),
                    );
                },
                nb_errors_left,
                options,
            );
        }
    }

    pub fn debug_check_done(&self) {
        debug_assert!(
            self.contexts.is_empty(),
            "Ending parse with lexer contexts active!?"
        );
    }

    fn peek(&mut self, params: &mut LexerParams) -> Option<char> {
        while let (depth, Some(ctx)) = (self.contexts.len(), self.active_context()) {
            let text = ctx.remaining_text();
            let mut chars = text.char_indices().peekable();
            let (zero, ch) = chars.next()?; // We might be at EOF in the current context.
            debug_assert_eq!(zero, 0);
            match ch {
                _ if ctx.cur_byte < ctx.ofs_scanned_for_expansion => {} // Characters that have been “painted blue” are “inert”.

                '\\' => {
                    let macro_arg_id = Self::read_macro_arg(
                        &mut chars,
                        text,
                        params.identifiers,
                        params.symbols,
                        &mut params.macro_args,
                        params.unique_id,
                        params.sections,
                    );
                    // `chars.offset()` is not stable yet.
                    let macro_arg_len = match chars.next() {
                        Some((ofs, _ch)) => ofs,
                        None => text.len(),
                    };
                    if let Some(res) = macro_arg_id {
                        let trigger_span = ctx
                            .span
                            .sub_span(ctx.cur_byte..ctx.cur_byte + macro_arg_len);
                        match res {
                            Ok((span_kind, source)) => {
                                // Consume all the characters implicated in the macro arg.
                                ctx.cur_byte += macro_arg_len;

                                self.push_context(
                                    NormalSpan::new(
                                        Rc::clone(source),
                                        span_kind,
                                        Some(Rc::new(trigger_span)),
                                    ),
                                    Default::default(),
                                    params.nb_errors_left,
                                    params.options,
                                );
                                continue; // Try again, reading from the new expansion context.
                            }
                            Err(err) => {
                                // Consuming the characters would cause them to be missed by any active spans, so we have to have them considered normally.
                                // TODO: it *might* be possible to work around this by pushing a dummy context that ends immediately and adjusts the span?
                                let span = Span::Normal(trigger_span);
                                diagnostics::error(
                                    &span,
                                    |error| {
                                        error.set_message(&err);
                                        error.add_label(
                                            diagnostics::error_label(&span)
                                                .with_message(err.label_msg()),
                                        );
                                        if let Some(help) = err.help_msg() {
                                            error.set_help(help);
                                        }
                                    },
                                    params.nb_errors_left,
                                    params.options,
                                );
                                // TODO: push a dummy context to cause `consume` to adjust the span

                                // Consume all the characters implicated in the macro arg.
                                ctx.cur_byte += macro_arg_len;
                                continue; // Try again, reading after the macro arg.
                            }
                        }
                    } else {
                        // Ensure that the backslash *and* everything it escapes (at least one character) are not scanned for expansion again.
                        // This is important, so that e.g. `\\1` isn't processed as a backslash then macro arg #1
                        // (which would happen if the second backslash doesn't get that “blue paint”).
                        ctx.ofs_scanned_for_expansion = ctx.cur_byte + macro_arg_len;
                    }
                }

                '{' => {
                    let mut contents = CompactString::default();
                    let res = Self::read_interpolation(
                        &mut chars,
                        ctx,
                        &mut contents,
                        params.identifiers,
                        params.symbols,
                        &params.macro_args.as_deref(),
                        params.sections,
                        depth,
                        params.nb_errors_left,
                        params.options,
                    );

                    // `chars.offset()` is not stable yet.
                    let interpolation_len = match chars.next() {
                        Some((ofs, _ch)) => ofs,
                        None => text.len(),
                    };
                    let mut trigger_span = ctx.new_span();
                    trigger_span.bytes.end += interpolation_len;

                    // Consume all the characters implicated in the interpolation.
                    ctx.cur_byte += interpolation_len;

                    let (span_kind, name) = match res {
                        Ok(ident) => (
                            SpanKind::Expansion(ident),
                            params.identifiers.resolve(ident).unwrap(),
                        ),
                        Err(()) => (SpanKind::Invalid, "<invalid>"),
                    };
                    self.push_context(
                        NormalSpan::new(
                            Rc::new(Source {
                                name: name.into(),
                                contents: contents.into(),
                            }),
                            span_kind,
                            Some(Rc::new(trigger_span)),
                        ),
                        Default::default(),
                        params.nb_errors_left,
                        params.options,
                    );
                    continue; // Try again, reading from the new expansion.
                }

                _ => {} // Other chars do not get any special treatment.
            }
            return Some(ch);
        }
        // TODO: is his ever reached?
        None
    }

    fn consume(&mut self, span: &mut NormalSpan) {
        // This check is nice, but breaks down in the presence of empty expansions.
        #[cfg(never)]
        if Rc::ptr_eq(&span.src, &self.contexts.last().unwrap().span.src) {
            debug_assert_eq!(span.bytes.end, self.active_context().unwrap().cur_byte);
        }

        // First, remove any “empty” contexts.
        let ctx = loop {
            let ctx = self
                .contexts
                .last_mut()
                .expect("Lexer attempting to consume a char with no more contexts!?");

            if !ctx.is_empty() {
                break ctx;
            }
            debug_assert!(ctx.span.kind.ends_implicitly());

            let ctx = self.contexts.pop().unwrap();

            // Only repoint the span if it is currently pointing to the context we are exiting.
            if Rc::ptr_eq(&span.src, &ctx.span.src) {
                let span_is_empty = span.bytes.is_empty();
                // Since we're popping a context, this means that the span is straddling two buffers.
                // Thus, we will mark the span as spanning the entirety of the buffer's “expansion”.
                // (Plus the character that's about to be consumed.)
                *span = ctx.span.parent.unwrap().deref().clone();
                // Exception: if the span is empty, we'll keep it empty (pointing at the end of the trigger),
                //            but we still need to point it to the new active buffer.
                if span_is_empty {
                    span.bytes.start = span.bytes.end;
                }
            } else {
                debug_assert!(
                    Rc::ptr_eq(&span.src, &ctx.span.parent.as_ref().unwrap().src),
                    "Span is pointing neither at current buffer ({}) nor its parent ({}), but at {}",
                    &ctx.span.src.name,
                    &ctx.span.parent.as_ref().unwrap().src.name,
                    &span.src.name,
                );
            }
        };

        // Advancing the span and context requires knowing how many bytes the character being consumed is.
        let text = ctx.remaining_text();
        let c = text
            .chars()
            .next()
            .expect("Lexer attempting to consume a char at EOF!?");

        if Rc::ptr_eq(&span.src, &ctx.span.src) {
            // The span is pointing at the active buffer.

            // This check works in most cases, but breaks down in the presence of expansions.
            // debug_assert_eq!(span.bytes.end, ctx.cur_byte); // The span should be pointing at the char before this one.

            span.bytes.end = ctx.cur_byte + c.len_utf8(); // Advance the span by as much, so it encompasses the character we just shifted.
        } else {
            debug_assert!(
                ctx.span.kind.ends_implicitly(),
                "Tokens can only straddle implicitly-ending contexts"
            );
            // The span is not pointing at the active buffer, so it should be pointing at its parent instead.
            let Some(parent) = ctx.span.parent.as_ref() else {
                panic!(
                    "Span not pointing at the current top-level buffer ({}), but at {} instead",
                    &ctx.span.src.name, &span.src.name,
                );
            };
            debug_assert!(
                Rc::ptr_eq(&span.src, &parent.src),
                "Span is pointing neither at {} nor its parent {}, but at {}",
                &ctx.span.src.name,
                &parent.src.name,
                &span.src.name,
            );
            // This modification is idempotent, so only do it at the beginning of the expansion, as a performance optimisation.
            if ctx.cur_byte == ctx.span.bytes.start {
                if !span.bytes.is_empty() {
                    // Move the right edge of the span to encompass the entirety of the “trigger”.
                    // Note that the parent will already have been advanced to start at the end of the trigger.
                    debug_assert_eq!(span.bytes.end, parent.bytes.start);
                    span.bytes.end = parent.bytes.end;
                } else {
                    // The span is empty, so move it to the beginning of the current buffer instead.
                    *span = ctx.new_span();
                    debug_assert!(span.bytes.is_empty());
                    span.bytes.end = ctx.cur_byte + c.len_utf8();
                }
            }
        }
        debug_assert!(!span.bytes.is_empty());

        // Advance the context's offset by that one character.
        ctx.cur_byte += c.len_utf8();
    }

    /// Tries to read a macro argument (the part after the backslash, which is expected to have already been consumed)
    /// and returns the macro arg index if a macro arg was found.
    ///
    /// Returns `None` if the backslash isn't part of a macro arg.
    ///
    /// Advances the iterator by however many characters are scanned; those characters must not be scanned again.
    fn read_macro_arg<'ret, 'mac_args: 'ret, 'text>(
        chars: &mut Peekable<CharIndices>,
        text: &'text str,
        identifiers: &Identifiers,
        symbols: &Symbols,
        macro_args: &'ret mut Option<&'mac_args mut MacroArgs>,
        unique_id: &'ret mut UniqueId,
        sections: &Sections,
    ) -> Option<Result<(SpanKind, &'ret Rc<Source>), MacroArgError<'text>>> {
        fn args<'a>(
            macro_args: &'a mut Option<&mut MacroArgs>,
        ) -> Result<&'a mut MacroArgs, MacroArgError<'static>> {
            macro_args
                .as_deref_mut()
                .ok_or(MacroArgError::MacroArgOutsideMacro)
        }

        let idx = match chars.next() {
            Some((_ofs, ch @ '1'..='9')) => (ch as u32 - '0' as u32) as usize,

            Some((_ofs, '<')) => {
                fn digit(opt: Option<&(usize, char)>) -> Result<usize, Option<char>> {
                    match opt {
                        None => Err(None),
                        Some(&(_ofs, ch)) => match ch.to_digit(10) {
                            Some(digit) => Ok(digit as usize),
                            None => Err(Some(ch)),
                        },
                    }
                }

                match digit(chars.peek()) {
                    Ok(mut idx) => loop {
                        chars.next();
                        match digit(chars.peek()) {
                            Ok(digit) => idx = idx * 10 + digit,
                            Err(Some('>')) => {
                                chars.next();
                                break idx;
                            }
                            _ => return Some(Err(MacroArgError::Unterminated)),
                        }
                    },

                    Err(Some(chars!(ident_start) | '@')) => {
                        let (ofs, _ch) = chars.next().unwrap();
                        let end_ofs = loop {
                            match chars.peek() {
                                Some((_ofs, chars!(ident))) => {
                                    chars.next();
                                }
                                Some(&(ofs, '>')) => {
                                    chars.next();
                                    break ofs;
                                }
                                _ => return Some(Err(MacroArgError::Unterminated)),
                            }
                        };
                        let name = &text[ofs..end_ofs];
                        match identifiers.get(name).and_then(|ident| symbols.find(&ident)) {
                            None => return Some(Err(MacroArgError::NoSuchSym(name))),
                            Some(sym) => match sym.get_number(macro_args.as_deref(), sections) {
                                None => return Some(Err(MacroArgError::SymNotConst(name))),
                                Some(idx) => idx as usize,
                            },
                        }
                    }

                    Err(Some('>')) => {
                        chars.next();
                        return Some(Err(MacroArgError::EmptyBracketed));
                    }
                    Err(Some(';' | chars!(newline))) => {
                        return Some(Err(MacroArgError::Unterminated))
                    }
                    _ => return Some(Err(MacroArgError::InvalidBracketedChar)),
                }
            }

            Some((_ofs, '@')) => {
                return Some(match unique_id.unique_id() {
                    Some(src) => Ok((SpanKind::UniqueId, src)),
                    None => Err(MacroArgError::NoUniqueId),
                })
            }

            Some((_ofs, '#')) => {
                return Some(
                    args(macro_args)
                        .map(|args| (SpanKind::CombinedMacroArgs, args.combined_args())),
                )
            }

            _ => return None,
        };

        if idx == 0 {
            Some(Err(MacroArgError::NoArg0))
        } else {
            Some(args(macro_args).and_then(|args| match args.arg(idx) {
                Some(arg) => Ok((SpanKind::MacroArg(idx), arg)),
                None => Err(MacroArgError::NoSuchArg {
                    idx,
                    max_valid: args.max_valid(),
                }),
            }))
        }
    }

    fn read_interpolation(
        chars: &mut Peekable<CharIndices>,
        ctx: &Context,
        output: &mut CompactString,
        identifiers: &Identifiers,
        symbols: &Symbols,
        macro_args: &Option<&MacroArgs>,
        sections: &Sections,
        cur_depth: usize,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) -> Result<Identifier, ()> {
        let mut name = CompactString::default();
        let mut fmt = None;
        let mut first_ofs = chars.peek().map(|(ofs, _ch)| *ofs);

        while let Some((ofs, ch)) = chars.next() {
            match ch {
                chars!(newline) => break,

                '{' => {
                    if cur_depth == options.runtime_opts.recursion_depth {
                        let mut span = ctx.new_span();
                        span.bytes.start += ofs;
                        span.bytes.end = span.bytes.start + ch.len_utf8();
                        Self::report_depth_overflow(
                            cur_depth,
                            &Span::Normal(span),
                            nb_errors_left,
                            options,
                        );
                        return Err(());
                    } else {
                        Self::read_interpolation(
                            chars,
                            ctx,
                            &mut name,
                            identifiers,
                            symbols,
                            macro_args,
                            sections,
                            cur_depth + 1,
                            nb_errors_left,
                            options,
                        )?;
                    }
                }
                ':' => {
                    if fmt.is_some() {
                        let mut span = ctx.new_span();
                        span.bytes.start += ofs;
                        span.bytes.end = span.bytes.start + ch.len_utf8();
                        let span = Span::Normal(span);
                        diagnostics::error(
                            &span,
                            |error| {
                                error.set_message("Multiple ':' characters found in interpolation");
                                error.add_label(
                                    diagnostics::error_label(&span)
                                        .with_message("This ':' is invalid"),
                                );
                            },
                            nb_errors_left,
                            options,
                        );
                    } else {
                        match FormatSpec::parse(&name, options.runtime_opts.q_precision) {
                            Ok(spec) => fmt = Some(spec),
                            Err(err) => {
                                let mut span = ctx.new_span();
                                span.bytes.start += first_ofs.unwrap();
                                span.bytes.end += ofs;
                                let span = Span::Normal(span);
                                diagnostics::error(
                                    &span,
                                    |error| {
                                        error.set_message(&err);
                                        error.add_label(
                                            diagnostics::error_label(&span).with_message(
                                                "Error parsing this format specification",
                                            ),
                                        );
                                    },
                                    nb_errors_left,
                                    options,
                                );
                                // Still, continue parsing the interpolation.
                            }
                        }
                    }
                    name.clear();
                    first_ofs = Some(ofs);
                }
                '}' => {
                    let ident = identifiers.get(&name);
                    if let Err(err) = symbols.format_as(
                        ident,
                        &name,
                        &fmt.unwrap_or_default(),
                        output,
                        macro_args.as_deref(),
                        sections,
                    ) {
                        let mut span = ctx.new_span();
                        span.bytes.start += first_ofs.unwrap();
                        span.bytes.end += ofs;
                        let span = Span::Normal(span);
                        diagnostics::error(
                            &span,
                            |error| {
                                error.set_message(&err);
                                error.add_label(
                                    diagnostics::error_label(&span)
                                        .with_message("This interpolation is invalid"),
                                );
                            },
                            nb_errors_left,
                            options,
                        );
                    }
                    return ident.ok_or(());
                }

                // Accept any and all chars, since this could be a formatting specifier.
                _ => name.push(ch),
            }
        }

        Err(())
    }

    fn report_depth_overflow(
        depth: usize,
        span: &Span,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        diagnostics::error(
            span,
            |error| {
                error.set_message("Maximum recursion depth exceeded");
                error.add_label(diagnostics::error_label(span).with_message(format!(
                    "Depth is {depth} contexts here, cannot enter a new one"
                )))
            },
            nb_errors_left,
            options,
        )
    }

    fn read_line_comment(chars: &mut Peekable<CharIndices>) {
        while chars
            .next_if(|&(_ofs, ch)| !matches!(ch, chars!(newline)))
            .is_some()
        {}
    }

    fn read_block_comment(chars: &mut Peekable<CharIndices>) -> Result<(), BlockCommentErr> {
        let mut nesting_depth = 0usize;
        while let Some((_ofs, ch)) = chars.next() {
            match ch {
                '*' => {
                    if chars.next_if(|&(_ofs, ch)| ch == '/').is_some() {
                        match nesting_depth.checked_sub(1) {
                            Some(new_depth) => nesting_depth = new_depth,
                            None => return Ok(()),
                        }
                    }
                }
                '/' => {
                    if chars.next_if(|&(_ofs, ch)| ch == '*').is_some() {
                        nesting_depth += 1;
                    }
                }
                _ => {}
            }
        }
        Err(BlockCommentErr::Unterminated) // TODO: unterminated
    }

    fn read_line_continuation(
        chars: &mut Peekable<CharIndices>,
    ) -> Result<(), LineContinuationErr> {
        while let Some(&(_ofs, ch)) = chars.peek() {
            match ch {
                chars!(newline) => {
                    chars.next();
                    if ch == '\r' {
                        chars.next_if(|&(_ofs, ch)| ch == '\n');
                    }
                    return Ok(());
                }

                chars!(whitespace) => {
                    chars.next();
                }
                ';' => Self::read_line_comment(chars),
                // Block comments are intentionally not allowed:
                // they can simply be placed before the continuation,
                // and they can also be used as line continuations themselves.
                _ => return Err(LineContinuationErr::NotAtEol),
            }
        }
        Err(LineContinuationErr::Eof)
    }

    fn with_active_context_raw<F: FnOnce(&Context, &str) -> usize>(
        &mut self,
        span: &mut NormalSpan,
        callback: F,
    ) {
        debug_assert!(
            span.bytes.is_empty(),
            "Do not consume a char before calling `with_active_context_raw`"
        );
        let ctx = self.active_context().unwrap();
        let text = ctx.remaining_text();

        *span = ctx.new_span();
        let nb_bytes_consumed = callback(ctx, text);
        debug_assert!(
            nb_bytes_consumed <= text.len(),
            "Consumed {nb_bytes_consumed} bytes out of a {}-byte string!?",
            text.len()
        );

        span.bytes.end += nb_bytes_consumed;
        ctx.cur_byte += nb_bytes_consumed;
    }
}

impl Lexer {
    fn handle_crlf(&mut self, ch: char, span: &mut NormalSpan, params: &mut LexerParams) {
        if ch == '\r' && self.peek(params) == Some('\n') {
            self.consume(span);
        }
    }

    pub fn next_token(
        &mut self,
        identifiers: &mut Identifiers,
        symbols: &Symbols,
        macro_args: Option<&mut MacroArgs>,
        unique_id: &mut UniqueId,
        sections: &Sections,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) -> Token {
        let mut params = LexerParams {
            identifiers,
            symbols,
            macro_args,
            unique_id,
            sections,
            nb_errors_left,
            options,
        };

        // Create a 1-char span pointing at the character right after the previous token,
        // which will be used for newlines, to provide better-looking syntax error messages.
        // Create it *after* the first `peek` call, so that it points inside of any expansions that would create.
        let first_char = self.peek(&mut params);
        let ctx = self
            .active_context() // Do this after `peek`, in case it triggered an expansion.
            .expect("Attempting to lex a token without an active context!?");
        // TODO: this span is also before any invalid chars, so a newline after invalid chars is reported in a weird location.
        let mut span_before_whitespace = ctx.new_span();
        let Some(ch) = first_char else {
            return Token {
                // Empty span pointing at EOF.
                span: Span::Normal(span_before_whitespace),
                payload: tok!("end of input"),
            };
        };
        span_before_whitespace.bytes.end += ch.len_utf8();

        let mut span = self.active_context().unwrap().new_span();
        macro_rules! token {
            ($what:tt $(($($params:tt)+))?) => {
                token!($what $(($($params)+))?, span)
            };
            ($what:tt $(($($params:tt)+))?, $span:expr) => {
                Token {
                    payload: tok!($what $(($($params)+))?),
                    span: Span::Normal($span),
                }
            };
        }
        loop {
            match self.peek(&mut params) {
                None => {
                    break token!("end of input", span_before_whitespace);
                }

                Some(ch @ chars!(newline)) => {
                    self.consume(&mut span);
                    self.handle_crlf(ch, &mut span, &mut params);
                    break token!("end of line", span_before_whitespace);
                }

                // All the stuff that gets ignored.
                Some(chars!(whitespace)) => {
                    debug_assert!(span.bytes.is_empty());
                    self.consume(&mut span);
                    // Move the start of the span as well.
                    span.bytes.start = span.bytes.end;
                }

                Some(';') => {
                    self.with_active_context_raw(&mut span, |_ctx, text| {
                        let mut chars = text.char_indices().peekable();
                        Self::read_line_comment(&mut chars);
                        match chars.next() {
                            Some((ofs, _newline)) => ofs,
                            None => text.len(),
                        }
                    });
                    // Ignore the comment.
                    span.bytes.start = span.bytes.end;
                }

                Some('\\') => {
                    // Macro args are implicitly handled by `peek`, and the only other thing that
                    // a backslash can be part of outside of a string, is a line continuation.
                    self.with_active_context_raw(&mut span, |ctx, text| {
                        let mut chars = text.char_indices().peekable();
                        let backslash = chars.next();
                        debug_assert_eq!(backslash, Some((0, '\\')));

                        let res = Self::read_line_continuation(&mut chars);
                        let cont_len = match chars.next() {
                            Some((ofs, _ch)) => ofs,
                            None => text.len(),
                        };
                        match res {
                            Ok(()) => {}
                            Err(err) => {
                                let mut span = ctx.new_span();
                                span.bytes.end += cont_len;
                                let span = Span::Normal(span.clone());
                                params.error(&span, |error| {
                                    error.set_message(&err);
                                    error.add_label(
                                        diagnostics::error_label(&span)
                                            .with_message(err.label_msg()),
                                    );
                                })
                            }
                        }
                        cont_len
                    });

                    // Ignore the line continuation.
                    span.bytes.start = span.bytes.end;
                }

                // Unambiguous single-char tokens.
                Some('~') => {
                    self.consume(&mut span);
                    break token!("~");
                }
                Some('@') => {
                    self.consume(&mut span);
                    // TODO(perf): this could be `.get("@").unwrap()`
                    break token!("identifier"(identifiers.get_or_intern_static("@"), false));
                }
                Some('[') => {
                    self.consume(&mut span);
                    break token!("[");
                }
                Some(']') => {
                    self.consume(&mut span);
                    break token!("]");
                }
                Some('(') => {
                    self.consume(&mut span);
                    break token!("(");
                }
                Some(')') => {
                    self.consume(&mut span);
                    break token!(")");
                }
                Some(',') => {
                    self.consume(&mut span);
                    break token!(",");
                }

                // 1- or 2-char tokens.
                Some('+') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => {
                            self.consume(&mut span);
                            break token!("+=");
                        }
                        _ => break token!("+"),
                    }
                }
                Some('-') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => {
                            self.consume(&mut span);
                            break token!("-=");
                        }
                        _ => break token!("-"),
                    }
                }
                Some('*') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => {
                            self.consume(&mut span);
                            break token!("*=");
                        }
                        Some('*') => {
                            self.consume(&mut span);
                            break token!("**");
                        }
                        _ => break token!("*"),
                    }
                }
                Some('/') => {
                    self.with_active_context_raw(&mut span, |ctx, text| {
                        let mut chars = text.char_indices().peekable();

                        let Some((zero, slash)) = chars.next() else {
                            return 0;
                        };
                        debug_assert_eq!((zero, slash), (0, '/'));
                        let Some((_ofs, '*')) = chars.next() else {
                            return 0;
                        };

                        if let Err(err) = Self::read_block_comment(&mut chars) {
                            let mut span = ctx.new_span();
                            span.bytes.end = span.bytes.start + "/*".len();
                            let span = Span::Normal(span);
                            params.error(&span, |error| {
                                error.set_message(&err);
                                error.add_label(
                                    diagnostics::error_label(&span).with_message(err.label_msg()),
                                )
                            });
                        }

                        match chars.next() {
                            Some((ofs, _ch)) => ofs,
                            None => text.len(),
                        }
                    });

                    if span.bytes.is_empty() {
                        self.consume(&mut span);
                        match self.peek(&mut params) {
                            Some('=') => {
                                self.consume(&mut span);
                                break token!("/=");
                            }
                            _ => break token!("/"),
                        }
                    }
                }
                Some('|') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => break token!("|="),
                        Some('|') => break token!("||"),
                        _ => break token!("|"),
                    }
                }
                Some('^') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => {
                            self.consume(&mut span);
                            break token!("^=");
                        }
                        _ => break token!("^"),
                    }
                }
                Some('=') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => {
                            self.consume(&mut span);
                            break token!("==");
                        }
                        _ => break token!("="),
                    }
                }
                Some('!') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => {
                            self.consume(&mut span);
                            break token!("!=");
                        }
                        _ => break token!("!"),
                    }
                }
                Some('<') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => break token!("<="),
                        Some('<') => {
                            self.consume(&mut span);
                            match self.peek(&mut params) {
                                Some('=') => {
                                    self.consume(&mut span);
                                    break token!("<<=");
                                }
                                _ => {
                                    break token!("<<");
                                }
                            }
                        }
                        _ => break token!("<"),
                    }
                }
                Some('>') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => break token!(">="),
                        Some('>') => {
                            self.consume(&mut span);
                            match self.peek(&mut params) {
                                Some('=') => {
                                    self.consume(&mut span);
                                    break token!(">>=");
                                }
                                Some('>') => {
                                    self.consume(&mut span);
                                    match self.peek(&mut params) {
                                        Some('=') => {
                                            self.consume(&mut span);
                                            break token!(">>>=");
                                        }
                                        _ => break token!(">>>"),
                                    }
                                }
                                _ => {
                                    break token!(">>");
                                }
                            }
                        }
                        _ => break token!(">"),
                    }
                }
                Some(':') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some(':') => {
                            self.consume(&mut span);
                            break token!("::");
                        }
                        Some(ch @ ('+' | '-')) => {
                            self.consume(&mut span);
                            break Token {
                                payload: self.read_anon_label_ref(ch, &mut span, &mut params),
                                span: Span::Normal(span),
                            };
                        }
                        _ => break token!(":"),
                    }
                }

                Some('0') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('x' | 'X') => {
                            self.consume(&mut span);
                            if let Some(first_digit) =
                                self.peek(&mut params).and_then(|ch| ch.to_digit(16))
                            {
                                let value =
                                    self.read_number(16, first_digit, &mut span, &mut params);
                                break token!("number"(value));
                            } else {
                                let err_span = Span::Normal(span.clone());
                                params.error(&err_span, |error| {
                                    error.set_message(
                                        "Missing hexadecimal digit(s) after `0x` prefix",
                                    );
                                    error.add_label(
                                        diagnostics::error_label(&err_span).with_message(
                                            "Expected at least one hex digit after this",
                                        ),
                                    );
                                });
                                span.bytes.end = span.bytes.start;
                            }
                        }
                        Some('o' | 'O') => {
                            self.consume(&mut span);
                            if let Some(first_digit) =
                                self.peek(&mut params).and_then(|ch| ch.to_digit(8))
                            {
                                let value =
                                    self.read_number(8, first_digit, &mut span, &mut params);
                                break token!("number"(value));
                            } else {
                                let err_span = Span::Normal(span.clone());
                                params.error(&err_span, |error| {
                                    error.set_message("Missing octal digit(s) after `0o` prefix");
                                    error.add_label(
                                        diagnostics::error_label(&err_span).with_message(
                                            "Expected at least one octal digit after this",
                                        ),
                                    );
                                });
                                span.bytes.end = span.bytes.start;
                            }
                        }
                        Some('b' | 'B') => {
                            self.consume(&mut span);
                            if let Some(first_digit) = self.peek(&mut params).and_then(|ch| {
                                params
                                    .options
                                    .runtime_opts
                                    .binary_digits
                                    .iter()
                                    .position(|&digit| digit == ch)
                            }) {
                                let value = self.read_bin_number(
                                    first_digit as u32,
                                    &mut span,
                                    &mut params,
                                );
                                break token!("number"(value));
                            } else {
                                let err_span = Span::Normal(span.clone());
                                params.error(&err_span, |error| {
                                    error.set_message("Missing octal digit(s) after `0o` prefix");
                                    error.add_label(
                                        diagnostics::error_label(&err_span).with_message(
                                            "Expected at least one octal digit after this",
                                        ),
                                    );
                                });
                                span.bytes.end = span.bytes.start;
                            }
                        }
                        Some('0'..='9') => {
                            let value = self.read_number(10, 0, &mut span, &mut params);
                            if let Some('.') = self.peek(&mut params) {
                                todo!();
                            } else {
                                break token!("number"(value));
                            }
                        }
                        _ => break token!("number"(0)),
                    }
                }
                Some(ch @ '1'..='9') => {
                    self.consume(&mut span);
                    let value = ch.to_digit(10).unwrap();
                    let value = self.read_number(10, value, &mut span, &mut params);
                    match self.peek(&mut params) {
                        Some('.') => {
                            todo!();
                        }
                        _ => break token!("number"(value)),
                    }
                }
                Some('&') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => {
                            self.consume(&mut span);
                            break token!("&=");
                        }
                        Some('&') => {
                            self.consume(&mut span);
                            break token!("&&");
                        }
                        Some(ch @ '0'..='7') => {
                            self.consume(&mut span);
                            let value = self.read_number(
                                8,
                                ch.to_digit(8).unwrap(),
                                &mut span,
                                &mut params,
                            );
                            break Token {
                                payload: tok!("number"(value)),
                                span: Span::Normal(span),
                            };
                        }
                        _ => break token!("&"),
                    }
                }
                Some('%') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => {
                            self.consume(&mut span);
                            break token!("%=");
                        }
                        Some(ch) if params.options.runtime_opts.binary_digits.contains(&ch) => {
                            self.consume(&mut span);
                            let first_digit = params
                                .options
                                .runtime_opts
                                .binary_digits
                                .iter()
                                .position(|&digit| digit == ch)
                                .unwrap();
                            let value =
                                self.read_bin_number(first_digit as u32, &mut span, &mut params);
                            break token!("number"(value));
                        }
                        _ => break token!("%"),
                    }
                }
                Some('$') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some(ch @ ('0'..='9' | 'A'..='F' | 'a'..='f')) => {
                            self.consume(&mut span);
                            let value = self.read_number(
                                16,
                                ch.to_digit(16).unwrap(),
                                &mut span,
                                &mut params,
                            );
                            break Token {
                                payload: tok!("number"(value)),
                                span: Span::Normal(span),
                            };
                        }
                        _ => {
                            let err_span = Span::Normal(span.clone());
                            params.error(&err_span, |error| {
                                error.set_message("Lone '$'");
                                error.add_label(
                                    diagnostics::error_label(&err_span)
                                        .with_message("Expected a hex digit after this"),
                                );
                                error.set_help(
                                    "In rgbasm, the current address is notated `@`, not `$`",
                                );
                            });
                            span.bytes.start = span.bytes.end;
                        }
                    }
                }
                // TODO: `

                // String.
                Some('"') => {
                    // Guaranteed to succeed, since we've already read the opening quote.
                    let payload = self.read_string_maybe(&mut span, &mut params).unwrap();
                    break Token {
                        payload,
                        span: Span::Normal(span),
                    };
                }

                // Raw string or raw identifier.
                Some('#') => {
                    let payload = if let Some(payload) =
                        self.read_string_maybe(&mut span, &mut params)
                    {
                        payload
                    } else {
                        self.consume(&mut span);
                        match self.peek(&mut params) {
                            Some(first_char @ chars!(ident_start)) => {
                                self.consume(&mut span);
                                self.read_identifier(first_char, &mut span, false, &mut params)
                            }
                            _ => {
                                let span = Span::Normal(span.clone());
                                params.error(&span, |error| {
                                    error.set_message("Invalid '#'");
                                    error.add_label(diagnostics::error_label(&span).with_message(
                                        "This doesn't start a raw string or raw identifier",
                                    ));
                                });
                                continue;
                            }
                        }
                    };
                    break Token {
                        payload,
                        span: Span::Normal(span),
                    };
                }

                // Identifier.
                Some(ch @ chars!(ident_start)) => {
                    self.consume(&mut span);

                    let payload = self.read_identifier(ch, &mut span, true, &mut params);
                    // TODO: `elif` hack (lexer.cpp:1937)
                    break Token {
                        payload,
                        span: Span::Normal(span),
                    };
                }

                // Default case.
                Some(ch) => {
                    debug_assert!(span.bytes.is_empty());
                    self.consume(&mut span);

                    let err_span = Span::Normal(span);
                    params.error(&err_span, |error| {
                        error
                            .set_message(format!("Unexpected character '{}'", ch.escape_default()));
                        error.add_label(
                            diagnostics::error_label(&err_span)
                                .with_message("This character was not expected at this point"),
                        );
                    });

                    // Borrowck is not happy otherwise, but this should hopefully compile to nothing.
                    let Span::Normal(moved_span) = err_span else {
                        unreachable!();
                    };
                    span = moved_span;
                    // Make the span empty, as we ignore the character that's just been consumed.
                    span.bytes.start = span.bytes.end;
                    span_before_whitespace = span.clone();
                }
            }
        }
    }

    fn read_anon_label_ref(
        &mut self,
        ch: char,
        span: &mut NormalSpan,
        params: &mut LexerParams,
    ) -> TokenPayload {
        let (mut offset, increment) = if ch == '+' { (0, 1) } else { (-1, -1) };
        while self.peek(params) == Some(ch) {
            self.consume(span);
            offset += increment;
        }
        tok!("anonymous label reference"(offset))
    }

    fn read_number(
        &mut self,
        radix: u32,
        mut value: u32,
        span: &mut NormalSpan,
        params: &mut LexerParams,
    ) -> i32 {
        let mut overflowed = false;

        loop {
            match self.peek(params) {
                Some('_') => self.consume(span), // TODO: disallow trailing underscores
                Some(ch) if ch.is_digit(radix) => {
                    self.consume(span);

                    let digit = ch.to_digit(radix).unwrap();
                    let (new_val, overflow) = value.overflowing_mul(radix);
                    overflowed |= overflow;
                    let (new_val, overflow) = new_val.overflowing_add(digit);
                    overflowed |= overflow;
                    value = new_val;
                }
                _ => break,
            }
        }

        if overflowed {
            let span = Span::Normal(span.clone());
            params.warn(warning!("large-constant"), &span, |warning| {
                warning.set_message(format!("Integer constant is larger than {}", u32::MAX));
                warning.add_label(
                    diagnostics::warning_label(&span)
                        .with_message(format!("This was truncated to {value}")),
                )
            })
        }

        value as i32
    }

    fn read_bin_number(
        &mut self,
        mut value: u32,
        span: &mut NormalSpan,
        params: &mut LexerParams,
    ) -> i32 {
        let mut overflowed = false;

        loop {
            let next_up = self.peek(params);
            let digits = params.options.runtime_opts.binary_digits;
            match next_up {
                Some('_') => self.consume(span), // TODO: disallow trailing underscores
                Some(ch) if digits.contains(&ch) => {
                    self.consume(span);

                    let digit = digits.iter().position(|&digit| digit == ch).unwrap() as u32;
                    let (new_val, overflow) = value.overflowing_mul(2);
                    overflowed |= overflow;
                    let (new_val, overflow) = new_val.overflowing_add(digit);
                    overflowed |= overflow;
                    value = new_val;
                }
                _ => break,
            }
        }

        if overflowed {
            let span = Span::Normal(span.clone());
            params.warn(warning!("large-constant"), &span, |warning| {
                warning.set_message(format!("Integer constant is larger than {}", u32::MAX));
                warning.add_label(
                    diagnostics::warning_label(&span)
                        .with_message(format!("This was truncated to {value}")),
                )
            })
        }

        value as i32
    }

    fn read_identifier(
        &mut self,
        first_char: char,
        span: &mut NormalSpan,
        can_be_keyword: bool,
        params: &mut LexerParams,
    ) -> TokenPayload {
        debug_assert!(
            !span.bytes.is_empty(),
            "Consume the first char before calling `read_identifier`"
        );
        debug_assert!(matches!(first_char, chars!(ident_start)));

        let mut name = CompactString::default();
        name.push(first_char);
        let followed_by_colon = loop {
            match self.peek(params) {
                Some(ch @ chars!(ident)) => {
                    self.consume(span);
                    name.push(ch);
                }
                Some(':') => break true,
                _ => break false,
            }
        };

        if can_be_keyword {
            if let Some(keyword) = KEYWORDS.get(&UniCase::ascii(name.as_str())) {
                return keyword.clone();
            }
        }
        let identifier = params.identifiers.get_or_intern(&name);
        tok!("identifier"(identifier, followed_by_colon))
    }

    fn read_string_maybe(
        &mut self,
        span: &mut NormalSpan,
        params: &mut LexerParams,
    ) -> Option<TokenPayload> {
        let mut string = CompactString::default();
        self.with_active_context_raw(span, |ctx, text| {
            fn is_quote(&(_, ch): &(usize, char)) -> bool {
                ch == '"'
            }

            let mut chars = text.char_indices().peekable();
            let raw = chars.next_if(|&(_zero, ch)| ch == '#').is_some();
            if chars.next_if(is_quote).is_none() {
                return 0; // Not a string.
            }
            Self::read_string_inner(&mut string, raw, &mut chars, ctx, text, params)
        });

        (!span.bytes.is_empty()).then_some(tok!("string"(string)))
    }

    fn read_string_inner(
        string: &mut CompactString,
        raw: bool,
        chars: &mut Peekable<CharIndices>,
        ctx: &Context,
        text: &str,
        params: &mut LexerParams,
    ) -> usize {
        fn is_quote(&(_, ch): &(usize, char)) -> bool {
            ch == '"'
        }

        let multiline = if let Some((ofs, quote)) = chars.next_if(is_quote) {
            // We have two consecutive quotes: if there are only two, then we have an empty string;
            //                                 if there are three, we have a multi-line string.
            if chars.next_if(is_quote).is_none() {
                return ofs + quote.len_utf8();
            }
            true
        } else {
            false
        };

        let mut end_ofs = text.len();
        while let Some((ofs, ch)) = chars.next() {
            match ch {
                '"' => {
                    if multiline {
                        // We need three consecutive quotes to close the string, not just one.
                        if let Some((_ofs, _quote)) = chars.next_if(is_quote) {
                            // Two in a row...
                            if let Some((ofs, _quote)) = chars.next_if(is_quote) {
                                // Three in a row! Winner winner chicken dinner
                                return ofs + ch.len_utf8();
                            }
                            string.push('"');
                        }
                        string.push('"');
                    } else {
                        return ofs + ch.len_utf8();
                    }
                }
                chars!(newline) => {
                    if !multiline {
                        end_ofs = ofs;
                        break;
                    } else if ch == '\r' {
                        // Handle CRLF, normalised as a single LF.
                        chars.next_if(|&(_, ch)| ch == '\n');
                    }
                    string.push('\n');
                }

                // Strings manually manage the normally implicit expansions.
                '\\' if !raw => {
                    // Check for a macro arg, and else for an escape.
                    let mut macro_chars = chars.clone();
                    if let Some(res) = Self::read_macro_arg(
                        &mut macro_chars,
                        text,
                        params.identifiers,
                        params.symbols,
                        &mut params.macro_args,
                        params.unique_id,
                        params.sections,
                    ) {
                        match res {
                            Ok((_kind, src)) => string.push_str(src.contents.text()),
                            Err(err) => {
                                let macro_arg_len = match macro_chars.peek() {
                                    Some(&(ofs, _ch)) => ofs,
                                    None => text.len(),
                                };
                                let mut span = ctx.new_span();
                                span.bytes = ctx.cur_byte + ofs..ctx.cur_byte + macro_arg_len;
                                let span = Span::Normal(span);
                                params.error(&span, |error| {
                                    error.set_message(&err);
                                    error.add_label(
                                        diagnostics::error_label(&span)
                                            .with_message(err.label_msg()),
                                    );
                                    if let Some(help) = err.help_msg() {
                                        error.set_help(help);
                                    }
                                });
                            }
                        }

                        *chars = macro_chars; // Consume all characters implicated in the macro arg.
                    } else if let Some(value) =
                        Self::get_char_escape(chars.next(), ofs, ctx, text, chars, params)
                    {
                        string.push(value);
                    }
                }
                '{' if !raw => {
                    todo!();
                }

                ch => string.push(ch),
            }
        }

        // The string wasn't terminated properly.
        let mut err_span = ctx.new_span();
        err_span.bytes.end = ctx.cur_byte + end_ofs;
        let err_span = Span::Normal(err_span);
        params.error(&err_span, |error| {
            error.set_message("Unterminated string literal");
            error.add_label(diagnostics::error_label(&err_span).with_message(format!(
                "No closing quote before the end of {}",
                if multiline { "input" } else { "the line" }
            )))
        });

        end_ofs
    }

    fn get_char_escape(
        escaped: Option<(usize, char)>,
        backslash_ofs: usize,
        ctx: &Context,
        text: &str,
        chars: &mut Peekable<CharIndices>,
        params: &LexerParams,
    ) -> Option<char> {
        match escaped {
            Some((_ofs, ch @ ('\\' | '"' | '{' | '}'))) => Some(ch),
            Some((_ofs, 'n')) => Some('\n'),
            Some((_ofs, 'r')) => Some('\r'),
            Some((_ofs, 't')) => Some('\t'),
            Some((_ofs, '0')) => Some('\0'),

            Some((_ofs, chars!(line_cont))) => {
                if let Err(err) = Self::read_line_continuation(chars) {
                    let mut span = ctx.new_span();
                    span.bytes.start += backslash_ofs;
                    span.bytes.end = span.bytes.start + '\\'.len_utf8();
                    let span = Span::Normal(span);
                    params.error(&span, |error| {
                        error.set_message(&err);
                        error.add_label(
                            diagnostics::error_label(&span).with_message(err.label_msg()),
                        );
                    });
                }

                None
            }

            Some((escaped_ofs, ch)) => {
                let mut span = ctx.new_span();
                span.bytes =
                    ctx.cur_byte + backslash_ofs..ctx.cur_byte + escaped_ofs + ch.len_utf8();
                let span = Span::Normal(span);
                params.error(&span, |error| {
                    error.set_message("Invalid character escape");
                    error.add_label(
                        diagnostics::error_label(&span)
                            .with_message("Cannot escape this character"),
                    );
                });

                Some(ch)
            }
            None => {
                let mut span = ctx.new_span();
                span.bytes = ctx.cur_byte + backslash_ofs..ctx.cur_byte + text.len();
                let span = Span::Normal(span);
                params.error(&span, |error| {
                    error.set_message("Invalid character escape");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("No character to escape"),
                    );
                });

                None
            }
        }
    }
}

impl Lexer {
    pub fn next_token_raw(
        &mut self,
        identifiers: &mut Identifiers,
        symbols: &Symbols,
        macro_args: Option<&mut MacroArgs>,
        unique_id: &mut UniqueId,
        sections: &Sections,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) -> Option<(CompactString, Span)> {
        let mut params = LexerParams {
            identifiers,
            symbols,
            macro_args,
            unique_id,
            sections,
            nb_errors_left,
            options,
        };

        let mut span = self
            .active_context()
            .expect("Cannot lex a raw string without a context active")
            .new_span();
        let mut string = CompactString::default();
        let mut last_char = None;

        self.with_active_context_raw(&mut span, |ctx, text| {
            let mut chars = text.char_indices().peekable();

            // Trim leading whitespace (but stop at a block comment).
            while let Some(&(ofs, ch)) = chars.peek() {
                match ch {
                    chars!(whitespace) => {
                        chars.next();
                    }
                    '\\' => {
                        let backup = chars.clone();
                        // If the backslash isn't a line continuation, we'll want to process it normally.
                        if chars
                            .next_if(|&(_ofs, ch)| {
                                matches!(ch, chars!(whitespace) | chars!(newline))
                            })
                            .is_none()
                        {
                            chars = backup;
                            break;
                        }

                        // Line continuations count as “whitespace”.
                        if let Err(err) = Lexer::read_line_continuation(&mut chars) {
                            let mut span = ctx.new_span();
                            span.bytes.start += ofs;
                            span.bytes.end += match chars.peek() {
                                Some(&(ofs, _ch)) => ofs,
                                None => text.len(),
                            };
                            let span = Span::Normal(span);
                            params.error(&span, |error| {
                                error.set_message(&err);
                                error.add_label(
                                    diagnostics::error_label(&span).with_message(err.label_msg()),
                                );
                            });
                        }
                    }
                    _ => break,
                }
            }

            let mut parens_depth = 0usize;
            while let Some((ofs, ch)) = chars.next() {
                last_char = Some(ch);

                match ch {
                    '"' => {
                        string.push('"');
                        let _len = Self::read_string_inner(
                            &mut string,
                            false,
                            &mut chars,
                            ctx,
                            text,
                            &mut params,
                        );
                        string.push('"');
                        last_char = Some('"'); // The terminating quote.
                    }
                    '#' => {
                        string.push('#');
                        if chars.next_if(|&(_ofs, ch)| ch == '"').is_some() {
                            string.push('"');
                            let _len = Self::read_string_inner(
                                &mut string,
                                true,
                                &mut chars,
                                ctx,
                                text,
                                &mut params,
                            );
                            string.push('"');
                            last_char = Some('"'); // The terminating quote.
                        }
                    }

                    chars!(newline) => return ofs,
                    ';' => {
                        Self::read_line_comment(&mut chars);
                        break;
                    }
                    '/' => {
                        if chars.next_if(|&(_ofs, ch)| ch == '*').is_some() {
                            if let Err(err) = Self::read_block_comment(&mut chars) {
                                let mut span = ctx.new_span();
                                span.bytes.start += ofs;
                                span.bytes.end = span.bytes.start + "/*".len();
                                let span = Span::Normal(span);
                                params.error(&span, |error| {
                                    error.set_message(&err);
                                    error.add_label(
                                        diagnostics::error_label(&span)
                                            .with_message(err.label_msg()),
                                    )
                                });
                            }
                        } else {
                            string.push('/');
                        }
                    }

                    ',' if parens_depth == 0 => return ofs,

                    '(' => {
                        parens_depth += 1;
                        string.push('(');
                    }
                    ')' if parens_depth != 0 => {
                        parens_depth -= 1;
                        string.push(')');
                    }

                    '\\' => {
                        let ch = chars.peek().copied();
                        if let Some(res) = Self::read_macro_arg(
                            &mut chars,
                            text,
                            params.identifiers,
                            params.symbols,
                            &mut params.macro_args,
                            params.unique_id,
                            params.sections,
                        ) {
                            match res {
                                Ok((_kind, source)) => string.push_str(source.contents.text()),
                                Err(err) => {
                                    let mut span = ctx.new_span();
                                    span.bytes.start += ofs;
                                    span.bytes.end = span.bytes.start + '\\'.len_utf8();
                                    let span = Span::Normal(span);
                                    params.error(&span, |error| {
                                        error.set_message(&err);
                                        error.add_label(
                                            diagnostics::error_label(&span)
                                                .with_message(err.label_msg()),
                                        )
                                    });
                                }
                            }
                        } else if let Some(value) =
                            Self::get_char_escape(ch, ofs, ctx, text, &mut chars, &params)
                        {
                            string.push(value);
                        }
                    }

                    _ => string.push(ch),
                }
            }
            text.len()
        });

        // Trim right whitespace.
        let trimmed_len = string.trim_end_matches(is_whitespace).len();
        string.truncate(trimmed_len);

        // Returning COMMAs to the parser would mean that two consecutive commas
        // (i.e. an empty argument) need to return two different tokens (string then comma)
        // without consuming any chars.
        // To avoid this, commas in raw mode end the current macro argument,
        // but are not tokenized themselves.
        if last_char == Some(',') {
            let mut dummy_span = span.clone();
            self.consume(&mut dummy_span);
            return Some((string, Span::Normal(span)));
        }

        // The last argument may end in a trailing comma, newline, or EOF.
        // To allow trailing commas, what would be the last argument is not emitted if empty.
        // To pass an empty last argument, use a second trailing comma.
        if !string.is_empty() {
            return Some((string, Span::Normal(span)));
        }

        None
    }
}

impl Lexer {
    pub fn capture_until_keyword(
        &mut self,
        end_keyword: &str,
        nesting_keywords: &[&str],
        kind: &'static str,
    ) -> (NormalSpan, Result<(), CaptureBlockErr>) {
        let mut span = self
            .active_context()
            .expect("Cannot capture a block without a context active")
            .new_span();

        let mut capture_len = 0;
        let mut res = Ok(());
        self.with_active_context_raw(&mut span, |ctx, text| {
            debug_assert!(
                matches!(
                    ctx.span.src.contents.text()[ctx.cur_byte - 1..ctx.cur_byte]
                        .chars()
                        .next(),
                    Some(chars!(newline))
                ),
                "Block capture not started at beginning of line",
            );

            let mut capture = text;
            let mut nesting_depth = 0usize;
            loop {
                let (line, remainder) = match capture.split_once(is_newline) {
                    Some((line, remainder)) => (line, Some(remainder)),
                    None => (capture, None),
                };

                // Capture everything before the start of this line.
                // SAFETY: `line` is derived from `text` via offsetting.
                capture_len = unsafe { line.as_ptr().offset_from(text.as_ptr()) } as usize;
                debug_assert!(capture_len as isize >= 0); // `line` comes after `text`.

                let trimmed = line.trim_start_matches(is_whitespace);
                let nb_trimmed = line.len() - trimmed.len();
                if let Some(first_word) = trimmed.get(..end_keyword.len()) {
                    if unicase::eq_ascii(first_word, end_keyword) {
                        // Found the ending keyword!
                        match nesting_depth.checked_sub(1) {
                            Some(new_depth) => nesting_depth = new_depth,
                            None => break capture_len + nb_trimmed + end_keyword.len(),
                        }
                    }
                } else {
                    for keyword in nesting_keywords {
                        if let Some(first_word) = trimmed.get(..keyword.len()) {
                            if unicase::eq_ascii(first_word, keyword) {
                                nesting_depth += 1;
                                break; // As an optimisation.
                            }
                        }
                    }
                }

                // Try again with the next line.
                let Some(remainder) = remainder else {
                    res = Err(CaptureBlockErr::Unterminated { name: kind });
                    capture_len = text.len();
                    break text.len();
                };
                capture = remainder;
            }
        });
        // Don't capture the closing keyword.
        span.bytes.end = span.bytes.start + capture_len;

        (span, res)
    }

    pub fn skip_to_eol(&mut self) {
        let mut span = self
            .active_context()
            .expect("Cannot skip to EOL without a context active")
            .new_span();

        self.with_active_context_raw(&mut span, |ctx, text| {
            match text.char_indices().find(|(_ofs, ch)| is_newline(*ch)) {
                Some((ofs, _ch)) => ofs,
                None => text.len(),
            }
        });
    }

    pub fn skip_conditional_block(&mut self) -> Result<(), CaptureBlockErr> {
        let mut span = self
            .active_context()
            .expect("Cannot skip a block without a context active")
            .new_span();

        let mut capture_len = 0;
        let mut res = Ok(());
        self.with_active_context_raw(&mut span, |ctx, text| {
            debug_assert!(
                matches!(
                    ctx.span.src.contents.text()[ctx.cur_byte - 1..ctx.cur_byte]
                        .chars()
                        .next(),
                    Some(chars!(newline))
                ),
                "Conditional block not started at beginning of line",
            );

            const END_KEYWORD: &str = "ENDC";
            const NESTING_KEYWORD: &str = "IF";
            let mut block = text;
            let mut nesting_depth = 0usize;
            loop {
                let (line, remainder) = match block.split_once(is_newline) {
                    Some((line, remainder)) => (line, Some(remainder)),
                    None => (text, None),
                };

                let trimmed = line.trim_start_matches(is_whitespace);
                // SAFETY: `trimmed` is derived from `text`.
                let block_len = unsafe { trimmed.as_ptr().offset_from(text.as_ptr()) } as usize;

                if let Some(first_word) = trimmed.get(..END_KEYWORD.len()) {
                    if unicase::eq_ascii(first_word, END_KEYWORD) {
                        // Found the ending keyword!
                        match nesting_depth.checked_sub(1) {
                            Some(new_depth) => nesting_depth = new_depth,
                            None => break block_len,
                        }
                    } else if nesting_depth == 0
                        && (unicase::eq_ascii(first_word, "ELIF")
                            || unicase::eq_ascii(first_word, "ELSE"))
                    {
                        break block_len;
                    }
                } else if let Some(first_word) = trimmed.get(..NESTING_KEYWORD.len()) {
                    if unicase::eq_ascii(first_word, NESTING_KEYWORD) {
                        nesting_depth += 1;
                    }
                }

                // Try again with the next line.
                let Some(remainder) = remainder else {
                    res = Err(CaptureBlockErr::Unterminated {
                        name: "conditional block",
                    });
                    capture_len = text.len();
                    break text.len();
                };
                block = remainder;
            }
        });

        res
    }
}

#[derive(displaydoc::Display)]
enum MacroArgError<'text> {
    /// unterminated macro argument
    // (For `\<`.))
    Unterminated,
    /// macro argument #{idx} doesn't exist
    NoSuchArg { idx: usize, max_valid: usize },
    /// a macro argument was used outside of a macro
    MacroArgOutsideMacro,
    /// a unique identifier is not available in this context
    NoUniqueId,
    /// macro argument #0 doesn't exist
    NoArg0,
    /// empty bracketed macro argument
    EmptyBracketed,
    /// invalid character in bracketed macro argument
    InvalidBracketedChar,
    /// no such symbol `{0}`
    NoSuchSym(&'text str),
    /// the symbol `{0}` is not constant
    SymNotConst(&'text str),
}
impl MacroArgError<'_> {
    fn label_msg(&self) -> String {
        match self {
            Self::Unterminated => "missing '>' after this".into(),
            Self::NoSuchArg { max_valid, .. } => {
                format!("{max_valid} macro arguments are available")
            }
            Self::MacroArgOutsideMacro => "cannot use a macro argument here".into(),
            Self::NoUniqueId => "cannot use `\\@` here".into(),
            Self::NoArg0 => "macro argument 0 doesn't exist".into(),
            Self::EmptyBracketed => {
                "expected a number or symbol name between the angle brackets".into()
            }
            Self::InvalidBracketedChar => "invalid character for bracketed macro argument".into(),
            Self::NoSuchSym(_name) => "no symbol by this name is defined at this point".into(),
            Self::SymNotConst(_name) => {
                "a symbol by this name exists, but is not a constant".into()
            }
        }
    }
    fn help_msg(&self) -> Option<String> {
        match self {
            Self::NoUniqueId => {
                Some("unique identifiers are available inside of macros and loops".into())
            }
            Self::NoArg0 => {
                Some("macro arguments are 1-indexed: the first macro argument is `\\1`".into())
            }
            Self::EmptyBracketed => Some("it kind of looks like a fish, doesn't it?".into()),
            _ => None,
        }
    }
}

#[derive(displaydoc::Display)]
enum LineContinuationErr {
    /// line continuation at end of input
    Eof,
    /// unexpected character after line continuation
    NotAtEol,
}
impl LineContinuationErr {
    fn label_msg(&self) -> String {
        match self {
            Self::Eof => "this should be followed by a newline, or removed".into(),
            Self::NotAtEol => {
                "there can only be whitespace between this backslash and the end of the line".into()
            }
        }
    }
}

#[derive(Debug, displaydoc::Display)]
enum BlockCommentErr {
    /// unterminated block comment
    Unterminated,
}
impl BlockCommentErr {
    fn label_msg(&self) -> String {
        match self {
            Self::Unterminated => "block comment starting here".into(),
        }
    }
}

impl LexerParams<'_, '_, '_, '_, '_, '_, '_> {
    pub fn error<'span, F: FnOnce(&mut ReportBuilder<'span>)>(&self, span: &'span Span, build: F) {
        diagnostics::error(span, build, self.nb_errors_left, self.options);
    }

    pub fn warn<'span, F: FnOnce(&mut ReportBuilder<'span>)>(
        &self,
        id: diagnostics::WarningKind,
        span: &'span Span,
        build: F,
    ) {
        diagnostics::warn(id, span, build, self.nb_errors_left, self.options);
    }
}

#[derive(Debug, displaydoc::Display)]
pub enum CaptureBlockErr {
    /// unterminated {name}
    Unterminated { name: &'static str },
}
