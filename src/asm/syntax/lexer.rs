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
//! The core of the lexer is the [`peek`][Lexer::peek] and [`consume`][Lexer::consume] functions.
//! The former seeks the next character, accounting for expansions
//! (since it is the function responsible for “opening” expansions, that's why it takes `&mut LexerParams`);
//! the latter makes said character be part of the current token.
//!
//! These two functions are however not part of the lexer's public API: this honor goes mainly
//! to [`next_token`][Lexer::next_token] and [`next_token_raw`][Lexer::next_token_raw].
//! The former is the main token generation function, the latter permitting special cases
//! to process the syntax more directly (macro calls passing the arguments through, which defers
//! actual tokenization to expansion time; `opt` effectively doing its own lexing).
//!
//! You may notice that there are no functions to “rewind” input: this is intentional,
//! as rewinding the effects of a `consume` is difficult due to the expansions.
//! Therefore, this lexer is strictly a 1-char lookahead non-backtracking lexer.
//!
//! [#63]: https://github.com/gbdev/rgbds/issues/63
//! [#362]: https://github.com/gbdev/rgbds/issues/362
//! [#531]: https://github.com/gbdev/rgbds/issues/531

use std::{
    cell::Cell,
    iter::Peekable,
    ops::{Deref, Range},
    rc::Rc,
    str::CharIndices,
};

use compact_str::CompactString;
use unicase::UniCase;

use crate::{
    common::S,
    cond::Condition,
    diagnostics::{self, warning, ReportBuilder},
    format::FormatSpec,
    macro_args::{MacroArgs, UniqueId},
    section::Sections,
    sources::{FileNode, NormalSpan, Source, Span, SpanKind},
    symbols::{SymbolData, SymbolError, Symbols},
    Identifier, Identifiers, Options,
};

use super::tokens::{tok, Token, TokenPayload, KEYWORDS};

#[derive(Debug)]
pub struct Lexer {
    contexts: Vec<Context>,
    pub cond_stack: Vec<Condition>,
    pub last_ident_was_raw: bool,
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
            last_ident_was_raw: false,
        }
    }

    pub fn push_file(
        &mut self,
        file: Rc<Source>,
        parent: Option<Rc<NormalSpan>>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) -> Result<(), ()> {
        debug_assert_eq!(parent.is_none(), self.contexts.is_empty());

        self.push_context(
            NormalSpan::new(file, SpanKind::File, parent),
            Default::default(),
            nb_errors_left,
            options,
        )
    }

    pub fn push_macro(
        &mut self,
        macro_name: Identifier,
        mut contents: NormalSpan,
        parent: Rc<NormalSpan>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) -> Result<(), ()> {
        contents.node.parent = Some(parent);
        contents.node.kind = SpanKind::Macro(macro_name);
        self.push_context(contents, Default::default(), nb_errors_left, options)
    }

    pub fn push_loop(
        &mut self,
        loop_info: LoopInfo,
        mut contents: NormalSpan,
        parent: Rc<NormalSpan>,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) -> Result<(), ()> {
        contents.node.parent = Some(parent);
        contents.node.kind = SpanKind::Loop(0);
        self.push_context(contents, loop_info, nb_errors_left, options)
    }

    pub fn break_loop(
        &mut self,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) -> Result<(), bool> {
        let (i, context) = self
            .contexts
            .iter_mut()
            .enumerate()
            .rfind(|(_i, ctx)| !ctx.span.node.kind.ends_implicitly())
            .unwrap();

        if matches!(context.span.node.kind, SpanKind::Loop(..)) {
            let expected_cond_depth = context.cond_stack_depth;
            let mut span = context.new_span(); // Dummy.

            // We are intentionally stopping at the end of the buffer.
            let _ = context.skimming_lines(
                &mut span,
                "",
                |trimmed_line, ofs_of_line, nb_trimmed, ctx| {
                    if Self::starts_with_keyword(trimmed_line, "endc") {
                        let span =
                            Span::Normal(ctx.new_span_len(ofs_of_line + nb_trimmed, "endc".len()));
                        crate::cond::exit_conditional(
                            &mut self.cond_stack,
                            expected_cond_depth,
                            &span,
                            nb_errors_left,
                            options,
                        );
                    } else if Self::starts_with_keyword(trimmed_line, "if") {
                        let opening_span =
                            Span::Normal(ctx.new_span_len(ofs_of_line + nb_trimmed, "if".len()));
                        self.cond_stack.push(Condition {
                            opening_span,
                            entered_block: false,
                            else_span: None,
                        });
                    }
                    None
                },
            );
            Self::report_unterminated_conditionals(
                &mut self.cond_stack,
                expected_cond_depth,
                nb_errors_left,
                options,
            );
            self.contexts.truncate(i);
            Ok(())
        } else {
            Err(self
                .contexts
                .iter()
                .any(|ctx| matches!(ctx.span.node.kind, SpanKind::Loop(..))))
        }
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
    (whitespace) => {' ' | '\t'};
    (newline) => {'\n' | '\r'};
    // Characters which, after a backslash, signify that it has to be a line continuation.
    (line_cont) => {chars!(whitespace) | chars!(newline) | ';'};
    (ident_start) => {'a'..='z' | 'A'..='Z' | '_'};
    (ident) => {chars!(ident_start) | '0'..='9' | '$' | '@' | '#'};
    // Characters that yield themselves when escaped.
    (self_escape) => {'\\' | '"' | '\'' | '{' | '}'};
}
pub fn is_whitespace(ch: char) -> bool {
    matches!(ch, chars!(whitespace))
}
pub fn is_newline(ch: char) -> bool {
    matches!(ch, chars!(newline))
}

impl Context {
    pub fn is_empty(&self) -> bool {
        debug_assert!(self.span.is_offset_valid(self.cur_byte));
        self.cur_byte == self.span.bytes.end
    }

    fn remaining_text(&self) -> &str {
        &self.span.node.src.contents.text()[self.cur_byte..self.span.bytes.end]
    }
    fn with_raw_text<F: FnOnce(&Context, &str) -> usize>(
        &mut self,
        span: &mut NormalSpan,
        callback: F,
    ) {
        debug_assert!(
            span.bytes.is_empty(),
            "Do not consume a char before calling `with_active_context_raw/with_raw_text`: spans may point to different buffers!",
        );
        if Rc::ptr_eq(&span.node.src, &self.span.node.src) {
            debug_assert_eq!(
                span.bytes.start, self.cur_byte,
                "Create the span with `ctx.new_span()`",
            );
        } else {
            *span = self.new_span();
        }
        let text = self.remaining_text();

        let nb_bytes_consumed = callback(self, text);
        debug_assert!(
            nb_bytes_consumed <= text.len(),
            "Consumed {nb_bytes_consumed} bytes out of a {}-byte string!?",
            text.len(),
        );

        debug_assert_eq!(span.bytes.end, self.cur_byte);
        self.cur_byte += nb_bytes_consumed;
        span.bytes.end = self.cur_byte;
    }
    fn skimming_lines<F: FnMut(&str, usize, usize, &Context) -> Option<usize>>(
        &mut self,
        span: &mut NormalSpan,
        name: &'static str,
        mut callback: F,
    ) -> Result<(), CaptureBlockErr> {
        let mut res = Ok(());
        self.with_raw_text(span, |ctx, text| {
            debug_assert!(
                matches!(
                    ctx.span.node.src.contents.text()[ctx.cur_byte - 1..ctx.cur_byte]
                        .chars()
                        .next(),
                    Some(chars!(newline))
                ),
                "Block capture not started at beginning of line",
            );

            let mut block = text;
            loop {
                let (line, remainder) = match block.split_once(is_newline) {
                    Some((line, remainder)) => (line, Some(remainder)),
                    None => (block, None),
                };
                let trimmed_line = line.trim_start_matches(is_whitespace);

                // SAFETY: `line` is derived from `text` via offsetting.
                let ofs_of_line = unsafe { line.as_ptr().offset_from(text.as_ptr()) } as usize;
                debug_assert!(ofs_of_line as isize >= 0); // `line` comes after `text`.

                let nb_trimmed = line.len() - trimmed_line.len();
                if let Some(offset) = callback(trimmed_line, ofs_of_line, nb_trimmed, ctx) {
                    break ofs_of_line + nb_trimmed + offset;
                }

                // Try again with the next line.
                let Some(remainder) = remainder else {
                    res = Err(CaptureBlockErr::Unterminated { name });
                    break text.len();
                };
                block = remainder;
            }
        });
        res
    }

    fn new_span(&self) -> NormalSpan {
        self.span.sub_span(self.cur_byte..self.cur_byte)
    }
    // Convenience functions for creating spans.
    fn new_span_len(&self, start_ofs: usize, len: usize) -> NormalSpan {
        let start = self.cur_byte + start_ofs;
        self.span.sub_span(start..start + len)
    }
    fn new_span_ofs(&self, bytes: Range<usize>) -> NormalSpan {
        self.span
            .sub_span(self.cur_byte + bytes.start..self.cur_byte + bytes.end)
    }
}

impl Lexer {
    /// Returns the topmost character from which characters should be read.
    /// If this returns an empty context, then it should be popped with [`pop_context`][Self::pop_context].
    fn active_context(&mut self) -> Option<&mut Context> {
        // Any contexts that we are at the end of are only kept to prevent some infinite expansion cases,
        // but they should be ignored.
        self.contexts
            .iter_mut()
            .rev()
            .find(|ctx| !ctx.is_empty() || !ctx.span.node.kind.ends_implicitly())
    }

    pub fn top_context_mut(&mut self) -> &mut Context {
        self.contexts
            .iter_mut()
            .rev()
            .find(|ctx| !ctx.span.node.kind.ends_implicitly())
            .expect("No active lexer context")
    }

    pub fn expand_equs_symbol(
        &mut self,
        ident: Identifier,
        src: CompactString,
        trigger_span: NormalSpan,
        identifiers: &Identifiers,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        let nb_bytes = src.len();
        let src = Rc::new(Source {
            name: identifiers.resolve(ident).unwrap().into(),
            contents: src.into(),
        });
        // It's fine if this fails; the function will have reported the error.
        let _ = self.push_context(
            NormalSpan {
                bytes: 0..nb_bytes,
                node: FileNode {
                    src,
                    kind: SpanKind::Expansion(ident),
                    parent: Some(Rc::new(trigger_span)),
                },
            },
            LoopInfo::default(),
            nb_errors_left,
            options,
        );
    }

    fn push_context(
        &mut self,
        span: NormalSpan,
        loop_info: LoopInfo,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) -> Result<(), ()> {
        debug_assert!(self.contexts.len() <= options.runtime_opts.recursion_depth);

        if self.contexts.len() == options.runtime_opts.recursion_depth {
            let err_span = match span.node.parent {
                Some(trigger_span) => Span::Normal(trigger_span.as_ref().clone()),
                None => Span::CommandLine,
            };
            Self::report_depth_overflow(self.contexts.len(), &err_span, nb_errors_left, options);
            Err(())
        } else {
            self.contexts.push(Context {
                cur_byte: span.bytes.start,
                // Expansions that end implicitly cannot contain other expansions.
                ofs_scanned_for_expansion: if span.node.kind.ends_implicitly() {
                    span.bytes.end
                } else {
                    span.bytes.start
                },
                span,
                loop_state: loop_info,
                cond_stack_depth: self.cond_stack.len(),
            });
            Ok(())
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
                    error.set_message("context stack is deeper than new limit");
                    error.add_label(diagnostics::error_label(opt_span).with_message(format!(
                        "depth is {} context{} at this point",
                        self.contexts.len(),
                        S::from(self.contexts.len()),
                    )))
                },
                nb_errors_left,
                options,
            );
        } else {
            options.runtime_opts.recursion_depth = new_depth;
        }
    }

    pub fn reset_loop_context(&mut self, nb_errors_left: &Cell<usize>, options: &Options) {
        let ctx = loop {
            let ctx = self
                .contexts
                .last_mut()
                .expect("Attempting to reset a lexer context when there are no more!?");
            // Pop any remaining implicitly-ending contexts.
            // (e.g. if the current top-level “hard“ context ends with an expansion).
            if !ctx.span.node.kind.ends_implicitly() {
                break ctx;
            }
            debug_assert_eq!(
                ctx.span.bytes.start, ctx.span.bytes.end,
                "Implicitly-ending context not empty when resetting!?"
            );
            self.contexts.pop();
        };

        debug_assert!(matches!(ctx.span.node.kind, SpanKind::Loop(..)));

        Lexer::report_unterminated_conditionals(
            &mut self.cond_stack,
            ctx.cond_stack_depth,
            nb_errors_left,
            options,
        );

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
            if !ctx.span.node.kind.ends_implicitly() {
                break ctx;
            }
            debug_assert_eq!(
                ctx.span.bytes.start, ctx.span.bytes.end,
                "Implicitly-ending context not empty when popping!?"
            );
        };

        Self::report_unterminated_conditionals(
            &mut self.cond_stack,
            ctx.cond_stack_depth,
            nb_errors_left,
            options,
        );

        !self.contexts.is_empty()
    }

    fn report_unterminated_conditionals(
        cond_stack: &mut Vec<Condition>,
        expected_depth: usize,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) {
        debug_assert!(
            cond_stack.len() >= expected_depth,
            "Fewer conditionals active than when the context was created!?",
        );

        while cond_stack.len() > expected_depth {
            let condition = cond_stack.pop().unwrap();
            diagnostics::error(
                &condition.opening_span,
                |error| {
                    error.set_message("unterminated conditional block");
                    error.add_label(
                        diagnostics::error_label(&condition.opening_span)
                            .with_message("this `if` is missing a corresponding `endc`"),
                    );
                },
                nb_errors_left,
                options,
            );
        }
    }

    pub fn active_condition_mut(&mut self) -> Option<&mut Condition> {
        let min_depth = self
            .active_context()
            .expect("No active lexer context")
            .cond_stack_depth;
        self.cond_stack[min_depth..].last_mut()
    }

    pub fn debug_check_done(&self) {
        debug_assert!(
            self.contexts.is_empty(),
            "Ending parse with lexer contexts active!?"
        );
    }

    pub fn next_char_is_a_colon(&mut self) -> bool {
        // This is only meant to be used after lexing an identifier, which will have `peek`ed the next character;
        // so we can rely on expansions having been triggered.
        self.active_context()
            .expect("No active lexer context")
            .remaining_text()
            .starts_with(':')
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
                        let trigger_span = ctx.new_span_len(0, macro_arg_len);
                        match res {
                            Ok((span_kind, source)) => {
                                // Consume all the characters implicated in the macro arg.
                                ctx.cur_byte += macro_arg_len;

                                // If this fails to push, we'll just read more characters.
                                let _ = self.push_context(
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
                        text,
                        ctx,
                        &mut contents,
                        depth,
                        params,
                    );

                    // `chars.offset()` is not stable yet.
                    let interpolation_len = match chars.next() {
                        Some((ofs, _ch)) => ofs,
                        None => text.len(),
                    };
                    let trigger_span = ctx.new_span_len(0, interpolation_len);

                    // Consume all the characters implicated in the interpolation.
                    ctx.cur_byte += interpolation_len;

                    let (span_kind, name) = Self::interpolation_span_kind(res, params.identifiers);
                    // If this fails to push, we'll just read more characters.
                    let _ = self.push_context(
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
        #[cfg(any())]
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
            debug_assert!(ctx.span.node.kind.ends_implicitly());

            let ctx = self.contexts.pop().unwrap();

            // Only repoint the span if it is currently pointing to the context we are exiting.
            if Rc::ptr_eq(&span.node.src, &ctx.span.node.src) {
                let span_was_empty = span.bytes.is_empty();
                // Since we're popping a context, this means that the span is straddling two buffers.
                // Thus, we will mark the span as spanning the entirety of the buffer's “expansion”.
                // (Plus the character that's about to be consumed.)
                *span = ctx.span.node.parent.unwrap().deref().clone();
                // Exception: if the span is empty, we'll keep it empty (pointing at the end of the trigger),
                //            but we still need to point it to the new active buffer.
                if span_was_empty {
                    span.make_empty();
                }
            }
        };

        // Advancing the span and context requires knowing how many bytes the character being consumed is.
        let text = ctx.remaining_text();
        let c = text
            .chars()
            .next()
            .expect("Lexer attempting to consume a char at EOF!?");

        if Rc::ptr_eq(&span.node.src, &ctx.span.node.src) {
            // The span is pointing at the active buffer.

            // This check works in most cases, but breaks down in the presence of expansions.
            #[cfg(any())]
            debug_assert_eq!(span.bytes.end, ctx.cur_byte); // The span should be pointing at the char before this one.

            span.bytes.end = ctx.cur_byte + c.len_utf8(); // Advance the span by as much, so it encompasses the character we just shifted.
        } else {
            debug_assert!(
                ctx.span.node.kind.ends_implicitly(),
                "Tokens can only straddle implicitly-ending contexts"
            );
            // Only perform a merge if at the beginning of the expansion.
            // Otherwise, the merge has already been performed,
            // which is redundant but also trips the ordering assertion.
            if ctx.cur_byte == ctx.span.bytes.start {
                if span.bytes.is_empty() {
                    // The span just happened to point to another context,
                    // but it really belongs to the context its first char is read from.
                    *span = ctx.new_span_len(0, c.len_utf8());
                } else {
                    // Use the standard merging logic to find the lowest common ancestor.
                    *span = span.merged_with(&ctx.new_span_len(0, c.len_utf8()));
                }
            }
        }
        debug_assert!(!span.bytes.is_empty());

        // Advance the context's offset by that one character.
        ctx.cur_byte += c.len_utf8();
    }

    fn ignore_thus_far(&mut self, span: &mut NormalSpan) {
        span.make_empty();
        debug_assert!(span.bytes.is_empty());

        let ctx = self.active_context().unwrap();
        if !Rc::ptr_eq(&span.node.src, &ctx.span.node.src) {
            // We exited the previously active cotext; repoint the span to the new active context.
            *span = ctx.new_span();
        }
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
        symbols: &'text Symbols,
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
            Some((_ofs, ch @ '1'..='9')) => (ch as u32 - '0' as u32) as isize,

            Some((_ofs, '<')) => {
                fn digit(opt: Option<&(usize, char)>) -> Result<isize, Option<char>> {
                    match opt {
                        None => Err(None),
                        Some(&(_ofs, ch)) => match ch.to_digit(10) {
                            Some(digit) => Ok(digit as isize),
                            None => Err(Some(ch)),
                        },
                    }
                }

                match chars.peek() {
                    Some((_ofs, ch @ ('0'..='9' | '-'))) => {
                        let (mut idx, sign) = if *ch == '-' {
                            chars.next();
                            let Ok(first_digit) = digit(chars.peek()) else {
                                return Some(Err(MacroArgError::NoDigitAfterMinus));
                            };
                            (first_digit, true)
                        } else {
                            (ch.to_digit(10).unwrap() as isize, false)
                        };
                        let mut underscore_allowed = true;
                        let mut consecutive_underscores = false;
                        loop {
                            chars.next();
                            match digit(chars.peek()) {
                                Ok(digit) => {
                                    idx = idx * 10 + digit;
                                    underscore_allowed = true;
                                }
                                Err(Some('_')) => {
                                    consecutive_underscores |= !underscore_allowed;
                                    underscore_allowed = false;
                                }
                                Err(Some('>')) => {
                                    chars.next();
                                    if consecutive_underscores {
                                        return Some(Err(MacroArgError::ConsecutiveUnderscores));
                                    }
                                    if !underscore_allowed {
                                        return Some(Err(MacroArgError::TrailingUnderscore));
                                    }
                                    break if sign { idx.wrapping_neg() } else { idx };
                                }
                                _ => return Some(Err(MacroArgError::Unterminated)),
                            }
                        }
                    }

                    Some((_ofs, chars!(ident_start) | '@' | '#')) => {
                        let (ofs, first_char) = chars.next().unwrap();
                        let ofs = if first_char == '#' {
                            ofs + '#'.len_utf8()
                        } else {
                            ofs
                        };
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
                        if name.is_empty() {
                            // This is possible if `first_char` is '#'.
                            return Some(Err(MacroArgError::EmptyBracketed));
                        }
                        match identifiers.get(name).and_then(|ident| symbols.find(&ident)) {
                            None => return Some(Err(SymbolError::NotFound(name).into())),
                            Some(SymbolData::Deleted(span)) => {
                                return Some(Err(SymbolError::Deleted(name, span).into()))
                            }
                            Some(sym) => match sym.get_number(macro_args.as_deref(), sections) {
                                None => return Some(Err(SymbolError::NotNumeric(name).into())),
                                Some(Err(err)) => return Some(Err(err.into())),
                                Some(Ok(None)) => {
                                    return Some(Err(SymbolError::NotConst(name).into()))
                                }
                                Some(Ok(Some(idx))) => idx as isize,
                            },
                        }
                    }

                    Some((_ofs, '>')) => {
                        chars.next();
                        return Some(Err(MacroArgError::EmptyBracketed));
                    }
                    Some((_ofs, ';' | chars!(newline))) => {
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
            Some(args(macro_args).and_then(|args| {
                let physical_idx = if idx > 0 {
                    idx as usize
                } else {
                    (args.max_valid() + 1).wrapping_add_signed(idx)
                };
                match args.arg(physical_idx) {
                    Some(arg) => Ok((SpanKind::MacroArg(physical_idx), arg)),
                    None => Err(MacroArgError::NoSuchArg {
                        idx,
                        max_valid: args.max_valid(),
                    }),
                }
            }))
        }
    }

    fn interpolation_span_kind(
        res: Result<Identifier, ()>,
        identifiers: &Identifiers,
    ) -> (SpanKind, &str) {
        match res {
            Ok(ident) => (
                SpanKind::Expansion(ident),
                identifiers.resolve(ident).unwrap(),
            ),
            Err(()) => (SpanKind::Invalid, "<invalid>"),
        }
    }

    fn read_interpolation(
        chars: &mut Peekable<CharIndices>,
        text: &str,
        ctx: &Context,
        output: &mut CompactString,
        cur_depth: usize,
        params: &mut LexerParams,
    ) -> Result<Identifier, ()> {
        let mut name = CompactString::default();
        let mut fmt = None;
        let mut first_ofs = chars.peek().map(|(ofs, _ch)| *ofs);

        while let Some((ofs, ch)) =
            chars.next_if(|&(_ofs, ch)| !matches!(ch, chars!(newline) | '"'))
        {
            fn consume_rest_of_interpolation(chars: &mut Peekable<CharIndices>) {
                for (_ofs, ch) in chars {
                    if ch == '}' {
                        break;
                    }
                }
            }

            match ch {
                '\\' => {
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
                        *chars = macro_chars; // Consume the macro's contents.

                        if cur_depth == params.options.runtime_opts.recursion_depth {
                            let span = ctx.new_span_len(ofs, ch.len_utf8());
                            Self::report_depth_overflow(
                                cur_depth,
                                &Span::Normal(span),
                                params.nb_errors_left,
                                params.options,
                            );
                            consume_rest_of_interpolation(chars);
                            return Err(());
                        } else {
                            match res {
                                Err(err) => {
                                    let span = Span::Normal(ctx.new_span_ofs(
                                        ofs..chars.peek().map_or(text.len(), |(ofs, _ch)| *ofs),
                                    ));
                                    diagnostics::error(
                                        &span,
                                        |error| {
                                            error.set_message(&err);
                                            error.add_label(
                                                diagnostics::error_label(&span)
                                                    .with_message(err.label_msg()),
                                            );
                                            if let Some(help_msg) = err.help_msg() {
                                                error.set_help(help_msg);
                                            }
                                        },
                                        params.nb_errors_left,
                                        params.options,
                                    );
                                    consume_rest_of_interpolation(chars);
                                    return Err(());
                                }
                                Ok((_kind, contents)) => {
                                    name.push_str(contents.contents.text());

                                    // Process any colons added by the macro arg.
                                    let end_ofs_in_src =
                                        chars.peek().map_or(text.len(), |(ofs, _ch)| *ofs);
                                    let mut to_process = name.as_str();
                                    while let Some((spec, rest)) = to_process.split_once(':') {
                                        process_fmt_spec(
                                            spec,
                                            &mut fmt,
                                            ctx,
                                            || ctx.new_span_ofs(ofs..end_ofs_in_src),
                                            end_ofs_in_src,
                                            first_ofs,
                                            params,
                                        );
                                        to_process = rest;
                                    }
                                    if to_process.as_ptr() != name.as_ptr() {
                                        // SAFETY: `to_process` is derived from `name`.
                                        let ofs_processed = unsafe {
                                            to_process.as_ptr().offset_from(name.as_ptr())
                                        };
                                        drop(name.drain(..ofs_processed as usize));
                                    }
                                }
                            }
                        }
                    }
                }
                '{' => {
                    if cur_depth == params.options.runtime_opts.recursion_depth {
                        let span = ctx.new_span_len(ofs, ch.len_utf8());
                        Self::report_depth_overflow(
                            cur_depth,
                            &Span::Normal(span),
                            params.nb_errors_left,
                            params.options,
                        );
                        consume_rest_of_interpolation(chars);
                        return Err(());
                    } else {
                        if let Err(()) = Self::read_interpolation(
                            chars,
                            text,
                            ctx,
                            &mut name,
                            cur_depth + 1,
                            params,
                        ) {
                            consume_rest_of_interpolation(chars);
                            return Err(());
                        }

                        // Process any colons added by the interpolation.
                        let end_ofs_in_src = chars.peek().map_or(text.len(), |(ofs, _ch)| *ofs);
                        let mut to_process = name.as_str();
                        while let Some((spec, rest)) = to_process.split_once(':') {
                            process_fmt_spec(
                                spec,
                                &mut fmt,
                                ctx,
                                || ctx.new_span_ofs(ofs..end_ofs_in_src),
                                end_ofs_in_src,
                                first_ofs,
                                params,
                            );
                            to_process = rest;
                        }
                        if to_process.as_ptr() != name.as_ptr() {
                            // SAFETY: `to_process` is derived from `name`.
                            let ofs_processed =
                                unsafe { to_process.as_ptr().offset_from(name.as_ptr()) };
                            drop(name.drain(..ofs_processed as usize));
                        }
                    }
                }
                ':' => {
                    process_fmt_spec(
                        &name,
                        &mut fmt,
                        ctx,
                        || ctx.new_span_len(ofs, ch.len_utf8()),
                        ofs,
                        first_ofs,
                        params,
                    );
                    name.clear();
                    first_ofs = Some(ofs);
                }
                '}' => {
                    let ident = if let Some(raw_name) = name.strip_prefix('#') {
                        params.identifiers.get(raw_name)
                    } else if KEYWORDS.contains_key(&UniCase::ascii(&name)) {
                        let span = Span::Normal(ctx.new_span_ofs(first_ofs.unwrap()..ofs));
                        diagnostics::error(
                            &span,
                            |error| {
                                error.set_message("interpolated symbol is a reserved keyword");
                                error.add_label(diagnostics::error_label(&span).with_message(
                                    format!("attempting to interpolate `{name}` here"),
                                ));
                                error.set_help(
                                    "add a `#` prefix to use the name as a symbol anyway",
                                );
                            },
                            params.nb_errors_left,
                            params.options,
                        );
                        return Err(());
                    } else {
                        params.identifiers.get(&name)
                    };
                    if let Err(err) = params.symbols.format_as(
                        ident,
                        &name,
                        &fmt.unwrap_or_default(),
                        output,
                        params.macro_args.as_deref(),
                        params.identifiers,
                        params.sections,
                    ) {
                        let span = Span::Normal(ctx.new_span_ofs(first_ofs.unwrap()..ofs));
                        diagnostics::error(
                            &span,
                            |error| {
                                error.set_message(&err);
                                error.add_label(
                                    diagnostics::error_label(&span)
                                        .with_message("this interpolation is invalid"),
                                );
                            },
                            params.nb_errors_left,
                            params.options,
                        );
                    }
                    return ident.ok_or(());
                }

                // Accept any and all chars, since this could be a formatting specifier.
                _ => name.push(ch),
            }
        }

        fn process_fmt_spec(
            string: &str,
            fmt: &mut Option<FormatSpec>,
            ctx: &Context,
            colon_span: impl FnOnce() -> NormalSpan,
            end_ofs: usize,
            first_ofs: Option<usize>,
            params: &mut LexerParams,
        ) {
            if fmt.is_some() {
                let span = Span::Normal(colon_span());
                diagnostics::error(
                    &span,
                    |error| {
                        error.set_message("multiple ':' characters found in interpolation");
                        error.add_label(
                            diagnostics::error_label(&span).with_message("this ':' is invalid"),
                        );
                    },
                    params.nb_errors_left,
                    params.options,
                );
            } else {
                match FormatSpec::parse(string, params.options.runtime_opts.q_precision)
                    .and_then(FormatSpec::require_full_parse)
                {
                    Ok(spec) => *fmt = Some(spec),
                    Err(err) => {
                        let span = Span::Normal(ctx.new_span_ofs(first_ofs.unwrap()..end_ofs));
                        diagnostics::error(
                            &span,
                            |error| {
                                error.set_message(&err);
                                error.add_label(
                                    diagnostics::error_label(&span)
                                        .with_message("error parsing this format specification"),
                                );
                            },
                            params.nb_errors_left,
                            params.options,
                        );
                        // Still, continue parsing the interpolation.
                    }
                }
            }
        }

        let end = first_ofs.unwrap_or(text.len());
        let span = Span::Normal(ctx.new_span_ofs(end - '{'.len_utf8()..end));
        diagnostics::error(
            &span,
            |error| {
                error.set_message("unterminated interpolation");
                error.add_label(
                    diagnostics::error_label(&span)
                        .with_message("no closing brace matches this one"),
                );
            },
            params.nb_errors_left,
            params.options,
        );
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
                error.set_message("maximum recursion depth exceeded");
                error.add_label(diagnostics::error_label(span).with_message(format!(
                    "depth is {depth} context{} here, cannot enter a new one",
                    S::from(depth),
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

    fn read_block_comment(
        chars: &mut Peekable<CharIndices>,
        ctx: &Context,
        nb_errors_left: &Cell<usize>,
        options: &Options,
    ) -> Result<(), BlockCommentErr> {
        // We don't do nested block comments, for reasons outlined in this post:
        // https://futhark-lang.org/blog/2017-10-10-block-comments-are-a-bad-idea.html
        while let Some((ofs, ch)) = chars.next() {
            match ch {
                '*' => {
                    if chars.next_if(|&(_ofs, ch)| ch == '/').is_some() {
                        return Ok(());
                    }
                }
                '/' => {
                    if matches!(chars.peek(), Some((_, '*'))) {
                        let span = Span::Normal(ctx.new_span_len(ofs, 2));
                        diagnostics::warn(
                            warning!("nested-comment"),
                            &span,
                            |warning| {
                                warning.set_message("`/*` in block comment");
                                warning.add_label(
                                    diagnostics::warning_label(&span)
                                        .with_message("is a previous block comment unclosed?"),
                                );
                            },
                            nb_errors_left,
                            options,
                        )
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

    #[track_caller]
    fn with_active_context_raw<F: FnOnce(&Context, &str) -> usize>(
        &mut self,
        span: &mut NormalSpan,
        callback: F,
    ) {
        self.active_context().unwrap().with_raw_text(span, callback)
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

        let ctx = self
            .active_context()
            .expect("Attempting to lex a token without an active context!?");
        let mut span = ctx.new_span();

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
                None => break token!("end of line"),

                Some(ch @ chars!(newline)) => {
                    self.consume(&mut span);
                    self.handle_crlf(ch, &mut span, &mut params);
                    break token!("end of line");
                }

                // All the stuff that gets ignored.
                Some(chars!(whitespace)) => {
                    debug_assert!(span.bytes.is_empty());
                    self.consume(&mut span);
                    // Ignore the character.
                    self.ignore_thus_far(&mut span);
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
                    self.ignore_thus_far(&mut span);
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
                                let span = Span::Normal(ctx.new_span_len(0, cont_len));
                                params.error(&span, |error| {
                                    error.set_message(&err);
                                    error.add_label(
                                        diagnostics::error_label(&span)
                                            .with_message(err.label_msg()),
                                    );
                                    if ctx.span.node.kind.ends_implicitly() {
                                        error.set_help("characters inside of interpolations and macro args cannot start one themselves");
                                    }
                                })
                            }
                        }
                        cont_len
                    });

                    // Ignore the line continuation.
                    self.ignore_thus_far(&mut span);
                }

                // Unambiguous single-char tokens.
                Some('~') => {
                    self.consume(&mut span);
                    break token!("~");
                }
                Some('@') => {
                    self.consume(&mut span);
                    // TODO(perf): this could be `.get("@").unwrap()`
                    break token!("identifier"(identifiers.get_or_intern_static("@")));
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
                        Some('+') => {
                            self.consume(&mut span);
                            break token!("++");
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

                        if let Err(err) = Self::read_block_comment(
                            &mut chars,
                            ctx,
                            params.nb_errors_left,
                            params.options,
                        ) {
                            let span = Span::Normal(ctx.new_span_len(0, "/*".len()));
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
                    } else {
                        // Ignore the block comment.
                        self.ignore_thus_far(&mut span);
                    }
                }
                Some('|') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => {
                            self.consume(&mut span);
                            break token!("|=");
                        }
                        Some('|') => {
                            self.consume(&mut span);
                            break token!("||");
                        }
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

                // 1-, 2-, or 3-char tokens.
                Some('=') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => {
                            self.consume(&mut span);
                            match self.peek(&mut params) {
                                Some('=') => {
                                    self.consume(&mut span);
                                    break token!("===");
                                }
                                _ => break token!("=="),
                            }
                        }
                        _ => break token!("="),
                    }
                }
                Some('!') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => {
                            self.consume(&mut span);
                            match self.peek(&mut params) {
                                Some('=') => {
                                    self.consume(&mut span);
                                    break token!("!==");
                                }
                                _ => break token!("!="),
                            }
                        }
                        _ => break token!("!"),
                    }
                }
                Some('<') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => {
                            self.consume(&mut span);
                            break token!("<=");
                        }
                        Some('<') => {
                            self.consume(&mut span);
                            match self.peek(&mut params) {
                                Some('=') => {
                                    self.consume(&mut span);
                                    break token!("<<=");
                                }
                                _ => break token!("<<"),
                            }
                        }
                        _ => break token!("<"),
                    }
                }
                Some('>') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('=') => {
                            self.consume(&mut span);
                            break token!(">=");
                        }
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
                                _ => break token!(">>"),
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
                            if matches!(self.peek(&mut params), Some('_')) {
                                self.consume(&mut span);
                            }
                            if let Some(first_digit) =
                                self.peek(&mut params).and_then(|ch| ch.to_digit(16))
                            {
                                self.consume(&mut span);
                                let value = self.read_number(
                                    16,
                                    first_digit,
                                    false,
                                    &mut span,
                                    &mut params,
                                );
                                break token!("number"(value));
                            } else {
                                let err_span = Span::Normal(span.clone());
                                params.error(&err_span, |error| {
                                    error.set_message(
                                        "missing hexadecimal digit(s) after `0x` prefix",
                                    );
                                    error.add_label(
                                        diagnostics::error_label(&err_span).with_message(
                                            "expected at least one hex digit after this",
                                        ),
                                    );
                                });
                                break token!("number"(0));
                            }
                        }
                        Some('o' | 'O') => {
                            self.consume(&mut span);
                            if matches!(self.peek(&mut params), Some('_')) {
                                self.consume(&mut span);
                            }
                            if let Some(first_digit) =
                                self.peek(&mut params).and_then(|ch| ch.to_digit(8))
                            {
                                self.consume(&mut span);
                                let value =
                                    self.read_number(8, first_digit, false, &mut span, &mut params);
                                break token!("number"(value));
                            } else {
                                let err_span = Span::Normal(span.clone());
                                params.error(&err_span, |error| {
                                    error.set_message("missing octal digit(s) after `0o` prefix");
                                    error.add_label(
                                        diagnostics::error_label(&err_span).with_message(
                                            "expected at least one octal digit after this",
                                        ),
                                    );
                                });
                                break token!("number"(0));
                            }
                        }
                        Some('b' | 'B') => {
                            self.consume(&mut span);
                            if matches!(self.peek(&mut params), Some('_')) {
                                self.consume(&mut span);
                            }
                            if let Some(first_digit) = self.peek(&mut params).and_then(|ch| {
                                params
                                    .options
                                    .runtime_opts
                                    .binary_digits
                                    .iter()
                                    .position(|&digit| digit == ch)
                            }) {
                                self.consume(&mut span);
                                let value = self.read_bin_number(
                                    first_digit as u32,
                                    false,
                                    &mut span,
                                    &mut params,
                                );
                                break token!("number"(value));
                            } else {
                                let err_span = Span::Normal(span.clone());
                                params.error(&err_span, |error| {
                                    error.set_message("missing binary digit(s) after `0b` prefix");
                                    error.add_label(
                                        diagnostics::error_label(&err_span).with_message(
                                            "expected at least one binary digit after this",
                                        ),
                                    );
                                });
                                break token!("number"(0));
                            }
                        }
                        Some('0'..='9' | '.' | '_') => {
                            let mut value = self.read_number(10, 0, false, &mut span, &mut params);
                            if let Some('.') = self.peek(&mut params) {
                                self.consume(&mut span);
                                value = self.read_fractional_part(value, &mut span, &mut params);
                            }
                            break token!("number"(value));
                        }
                        _ => break token!("number"(0)),
                    }
                }
                Some(ch @ '1'..='9') => {
                    self.consume(&mut span);
                    let value = ch.to_digit(10).unwrap();
                    let mut value = self.read_number(10, value, false, &mut span, &mut params);
                    if let Some('.') = self.peek(&mut params) {
                        self.consume(&mut span);
                        value = self.read_fractional_part(value, &mut span, &mut params);
                    }
                    break token!("number"(value));
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
                        Some('_') => {
                            self.consume(&mut span);
                            let value = self.read_number(8, 0, true, &mut span, &mut params);
                            break token!("number"(value));
                        }
                        Some(ch @ '0'..='7') => {
                            self.consume(&mut span);
                            let value = self.read_number(
                                8,
                                ch.to_digit(8).unwrap(),
                                false,
                                &mut span,
                                &mut params,
                            );
                            break token!("number"(value));
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
                        Some('_') => {
                            self.consume(&mut span);
                            let value = self.read_bin_number(0, true, &mut span, &mut params);
                            break token!("number"(value));
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
                            let value = self.read_bin_number(
                                first_digit as u32,
                                false,
                                &mut span,
                                &mut params,
                            );
                            break token!("number"(value));
                        }
                        _ => break token!("%"),
                    }
                }
                Some('$') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('_') => {
                            self.consume(&mut span);
                            let value = self.read_number(16, 0, true, &mut span, &mut params);
                            break token!("number"(value));
                        }
                        Some(ch @ ('0'..='9' | 'A'..='F' | 'a'..='f')) => {
                            self.consume(&mut span);
                            let value = self.read_number(
                                16,
                                ch.to_digit(16).unwrap(),
                                false,
                                &mut span,
                                &mut params,
                            );
                            break token!("number"(value));
                        }
                        _ => {
                            let err_span = Span::Normal(span.clone());
                            params.error(&err_span, |error| {
                                error.set_message("lone '$'");
                                error.add_label(
                                    diagnostics::error_label(&err_span)
                                        .with_message("expected a hex digit after this"),
                                );
                                error.set_help(
                                    "in rgbasm, the current address is notated `@`, not `$`",
                                );
                            });
                            break token!("number"(0));
                        }
                    }
                }
                Some('`') => {
                    self.consume(&mut span);
                    match self.peek(&mut params) {
                        Some('_') => {
                            self.consume(&mut span);
                            let value = self.read_gfx_constant(0, true, &mut span, &mut params);
                            break token!("number"(value));
                        }
                        Some(ch) if params.options.runtime_opts.gfx_chars.contains(&ch) => {
                            self.consume(&mut span);
                            let first_digit = params
                                .options
                                .runtime_opts
                                .gfx_chars
                                .iter()
                                .position(|&digit| digit == ch)
                                .unwrap() as u8;
                            let value =
                                self.read_gfx_constant(first_digit, false, &mut span, &mut params);
                            break token!("number"(value));
                        }
                        _ => {
                            let err_span = Span::Normal(span.clone());
                            params.error(&err_span, |error| {
                                error.set_message("lone '`'");
                                error.add_label(
                                    diagnostics::error_label(&err_span)
                                        .with_message("expected a graphics character after this"),
                                );
                                let gfx_char = |i: usize| params.options.runtime_opts.gfx_chars[i].escape_default();
                                error.set_help(format!(
                                    "the graphics characters at this point are '{}', '{}', '{}', and '{}'",
                                    gfx_char(0),
                                    gfx_char(1),
                                    gfx_char(2),
                                    gfx_char(3),
                                ));
                            });
                            break token!("number"(0));
                        }
                    }
                }

                // Character literal (very similar to `read_string_maybe`).
                Some('\'') => {
                    let payload = self.read_char_literal(&mut span, &mut params);
                    break Token {
                        payload,
                        span: Span::Normal(span),
                    };
                }

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
                    let payload =
                        if let Some(payload) = self.read_string_maybe(&mut span, &mut params) {
                            payload
                        } else {
                            self.consume(&mut span);
                            match self.peek(&mut params) {
                                Some(first_char @ (chars!(ident_start) | '.')) => {
                                    self.last_ident_was_raw = true;
                                    self.consume(&mut span);
                                    self.read_identifier(first_char, &mut span, false, &mut params)
                                }
                                _ => {
                                    let err_span = Span::Normal(span.clone());
                                    params.error(&err_span, |error| {
                                        error.set_message("invalid '#'");
                                        error.add_label(
                                            diagnostics::error_label(&err_span).with_message(
                                                "this doesn't start a raw string or raw identifier",
                                            ),
                                        );
                                    });
                                    self.ignore_thus_far(&mut span);
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
                Some(ch @ (chars!(ident_start) | '.')) => {
                    self.consume(&mut span);

                    let payload = self.read_identifier(ch, &mut span, true, &mut params);
                    self.last_ident_was_raw = false;
                    break Token {
                        payload,
                        span: Span::Normal(span),
                    };
                }

                // Default case.
                Some(ch) => {
                    debug_assert!(span.bytes.is_empty());
                    let was_blue_painted = self
                        .active_context()
                        .unwrap()
                        .span
                        .node
                        .kind
                        .ends_implicitly();
                    self.consume(&mut span);

                    let err_span = Span::Normal(span);
                    params.error(&err_span, |error| {
                        error
                            .set_message(format!("unexpected character '{}'", ch.escape_default()));
                        error.add_label(
                            diagnostics::error_label(&err_span)
                                .with_message("this character was not expected at this point"),
                        );
                        if was_blue_painted && matches!(ch, '{' | '}' | '\\') {
                            error.set_help("characters inside of macro args cannot start an expansion themselves")
                        }
                    });

                    // Borrowck is not happy without this, but this should hopefully compile to nothing.
                    span = err_span.extract_normal();
                    // Make the span empty, as we ignore the character that's just been consumed.
                    self.ignore_thus_far(&mut span);
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
        mut prev_underscore: bool,
        span: &mut NormalSpan,
        params: &mut LexerParams,
    ) -> u32 {
        let mut overflowed = false;

        let mut consecutive_underscores = false;
        loop {
            match self.peek(params) {
                Some('_') => {
                    self.consume(span);
                    consecutive_underscores |= prev_underscore;
                    prev_underscore = true;
                }
                Some(ch) if ch.is_digit(radix) => {
                    self.consume(span);
                    prev_underscore = false;

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

        let span = Span::Normal(span.clone());
        if overflowed {
            params.warn(warning!("large-constant"), &span, |warning| {
                warning.set_message(format!("integer constant is larger than {}", u32::MAX));
                warning.add_label(
                    diagnostics::warning_label(&span)
                        .with_message(format!("this was truncated to {value}")),
                )
            });
        }
        if consecutive_underscores {
            params.error(&span, |error| {
                error.set_message("consecutive underscores are not allowed");
                error.add_label(
                    diagnostics::error_label(&span).with_message("this number is invalid"),
                );
            });
        }
        if prev_underscore {
            params.error(&span, |error| {
                error.set_message("trailing underscores are not allowed");
                error.add_label(
                    diagnostics::error_label(&span).with_message("this number is invalid"),
                );
            });
        }

        value
    }

    fn read_fractional_part(
        &mut self,
        integer_part: u32,
        span: &mut NormalSpan,
        params: &mut LexerParams,
    ) -> u32 {
        let mut value = 0;
        let mut divisor: u32 = 1;
        let mut underscore_after_dot = false;
        let mut too_many_digits = false;
        let mut prev_underscore = false;
        let mut consecutive_underscores = false;
        let first_nondigit = 'digits: loop {
            match self.peek(params) {
                Some('_') => {
                    self.consume(span);
                    consecutive_underscores |= prev_underscore;
                    prev_underscore = true;
                    underscore_after_dot |= divisor == 1;
                }
                Some(digit @ '0'..='9') => {
                    self.consume(span);
                    prev_underscore = false;
                    match divisor.checked_mul(10) {
                        Some(new_divisor) => divisor = new_divisor,
                        None => {
                            too_many_digits = true;
                            // Discard additional digits, since we can't account for them.
                            loop {
                                match self.peek(params) {
                                    Some('0'..='9' | '_') => {}
                                    ch => break 'digits ch,
                                }
                            }
                        }
                    }
                    value = value * 10 + digit.to_digit(10).unwrap();
                }
                ch => break ch,
            }
        };

        let mut empty_precision = false;
        let mut underscore_after_q = false;
        let precision_raw = if matches!(first_nondigit, Some('q' | 'Q')) {
            prev_underscore = false; // An underscore before the `q` is not actually trailing.
            self.consume(span);
            // Allow `q1`, but also `q.1`.
            let mut first_digit = match self.peek(params) {
                Some('.') => {
                    self.consume(span);
                    self.peek(params)
                }
                ch => ch,
            };
            underscore_after_q = if matches!(first_digit, Some('_')) {
                while matches!(first_digit, Some('_')) {
                    self.consume(span);
                    first_digit = self.peek(params);
                }
                true
            } else {
                false
            };
            if let Some(first_digit) = first_digit.and_then(|ch| ch.to_digit(10)) {
                let mut precision = first_digit as i32;
                self.consume(span);
                while let Some(digit) = self.peek(params).and_then(|ch| ch.to_digit(10)) {
                    self.consume(span);
                    match precision.checked_mul(10) {
                        Some(new_precision) => precision = new_precision + digit as i32,
                        None => precision = i32::MAX, // This will trip the range check, and further digits will just take this code path again.
                    }
                }
                precision
            } else {
                empty_precision = true;
                params.options.runtime_opts.q_precision.into()
            }
        } else {
            params.options.runtime_opts.q_precision.into()
        };

        // There we go, we finally have lexed the whole thing!
        // This means the span is complete, and we can report any issues.
        let span = Span::Normal(span.clone());
        if underscore_after_dot {
            params.error(&span, |error| {
                error.set_message("expected a digit after '.', not an underscore");
                error.add_label(
                    diagnostics::error_label(&span)
                        .with_message("this fixed-point constant is invalid"),
                );
            });
        }
        if underscore_after_q {
            params.error(&span, |error| {
                error.set_message("expected a digit after 'q', not an underscore");
                error.add_label(
                    diagnostics::error_label(&span)
                        .with_message("this fixed-point constant is invalid"),
                );
            });
        }
        if prev_underscore {
            params.error(&span, |error| {
                error.set_message("trailing underscores are not allowed");
                error.add_label(
                    diagnostics::error_label(&span)
                        .with_message("this fixed-point constant is invalid"),
                );
            });
        }
        if consecutive_underscores {
            params.error(&span, |error| {
                error.set_message("consecutive underscores are not allowed");
                error.add_label(
                    diagnostics::error_label(&span)
                        .with_message("this fixed-point constant is invalid"),
                );
            });
        }
        if too_many_digits {
            params.warn(warning!("large-constant"), &span, |warning| {
                warning.set_message("too many digits after the dot");
                warning.add_label(
                    diagnostics::warning_label(&span)
                        .with_message("some digits of this fixed-point constant were ignored"),
                );
            });
        }
        if empty_precision {
            params.error(&span, |error| {
                error.set_message("missing digits after 'q'");
                error.add_label(
                    diagnostics::error_label(&span)
                        .with_message("this fixed-point constant is invalid"),
                );
            });
        }
        let precision = super::semantics::fixed_point::clamp_fixpoint_precision(
            precision_raw,
            &span,
            params.nb_errors_left,
            params.options,
        )
        .unwrap_or(params.options.runtime_opts.q_precision);
        if integer_part >= 1u32 << (32 - precision) {
            params.warn(warning!("large-constant"), &span, |warning| {
                warning.set_message("magnitude of fixed-point constant is too large");
                warning.add_label(diagnostics::warning_label(&span).with_message(format!(
                    "the integer part of this was truncated to {}",
                    integer_part % (1 << (32 - precision)),
                )))
            });
        }
        integer_part << precision
            | (value as f64 / divisor as f64 * 2.0f64.powi(precision.into())).round() as u32
    }

    fn read_bin_number(
        &mut self,
        mut value: u32,
        mut was_underscore: bool,
        span: &mut NormalSpan,
        params: &mut LexerParams,
    ) -> u32 {
        let mut overflowed = false;
        let mut consecutive_underscores = false;

        loop {
            let next_up = self.peek(params);
            let digits = params.options.runtime_opts.binary_digits;
            match next_up {
                Some('_') => {
                    self.consume(span);
                    consecutive_underscores |= was_underscore;
                    was_underscore = true;
                }
                Some(ch) if digits.contains(&ch) => {
                    self.consume(span);
                    was_underscore = false;

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
                warning.set_message(format!("integer constant is larger than {}", u32::MAX));
                warning.add_label(
                    diagnostics::warning_label(&span)
                        .with_message(format!("this was truncated to {value}")),
                )
            });
        }
        if was_underscore {
            let span = Span::Normal(span.clone());
            params.error(&span, |error| {
                error.set_message("trailing underscores are not allowed");
                error.add_label(
                    diagnostics::error_label(&span).with_message("this number is invalid"),
                );
            });
        }
        if consecutive_underscores {
            let span = Span::Normal(span.clone());
            params.error(&span, |error| {
                error.set_message("consecutive underscores are not allowed");
                error.add_label(
                    diagnostics::error_label(&span).with_message("this number is invalid"),
                );
            });
        }

        value
    }

    fn read_gfx_constant(
        &mut self,
        value: u8,
        mut was_underscore: bool,
        span: &mut NormalSpan,
        params: &mut LexerParams,
    ) -> u32 {
        fn value_of(ch: char, params: &LexerParams) -> u8 {
            params
                .options
                .runtime_opts
                .gfx_chars
                .iter()
                .position(|&digit| digit == ch)
                .unwrap() as u8
        }
        let mut low_bitplane = value & 1;
        let mut high_bitplane = value >> 1;

        let mut overflowed = false;
        let mut consecutive_underscores = false;
        loop {
            let next_up = self.peek(params);
            let digits = params.options.runtime_opts.gfx_chars;
            match next_up {
                Some('_') => {
                    self.consume(span);
                    consecutive_underscores |= was_underscore;
                    was_underscore = true;
                }
                Some(ch) if digits.contains(&ch) => {
                    self.consume(span);
                    was_underscore = false;

                    let value = value_of(ch, params);
                    overflowed |= low_bitplane & 0x80 != 0 || high_bitplane & 0x80 != 0;
                    low_bitplane = (low_bitplane << 1) + (value & 1);
                    high_bitplane = (high_bitplane << 1) + (value >> 1);
                }
                _ => break,
            }
        }
        let value = (high_bitplane as u32) << 8 | low_bitplane as u32;

        if overflowed {
            let span = Span::Normal(span.clone());
            params.warn(warning!("large-constant"), &span, |warning| {
                warning.set_message("graphics constant contains more than 8 pixels");
                warning.add_label(
                    diagnostics::warning_label(&span)
                        .with_message(format!("this was truncated to ${value:04X}")),
                )
            });
        }
        if was_underscore {
            let span = Span::Normal(span.clone());
            params.error(&span, |error| {
                error.set_message("trailing underscores are not allowed");
                error.add_label(
                    diagnostics::error_label(&span).with_message("this number is invalid"),
                );
            });
        }
        if consecutive_underscores {
            let span = Span::Normal(span.clone());
            params.error(&span, |error| {
                error.set_message("consecutive underscores are not allowed");
                error.add_label(
                    diagnostics::error_label(&span).with_message("this number is invalid"),
                );
            });
        }

        value
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
        debug_assert!(matches!(first_char, chars!(ident_start) | '.'));

        let mut name = CompactString::default();
        let mut is_local = (first_char == '.') as u8;
        let mut not_just_dots = first_char != '.';

        name.push(first_char);
        loop {
            match self.peek(params) {
                Some(ch @ chars!(ident)) => {
                    self.consume(span);
                    name.push(ch);
                    not_just_dots = true;
                }
                Some('.') => {
                    self.consume(span);
                    name.push('.');
                    is_local = is_local.saturating_add(1);
                }
                _ => break,
            }
        }

        if is_local > 1 && not_just_dots {
            let span = Span::Normal(span.clone());
            params.error(&span, |error| {
                error.set_message("nested local labels are not supported");
                error.add_label(
                    diagnostics::error_label(&span)
                        .with_message("there can only be up to 1 dot per identifier"),
                );
            });
        }

        if first_char == '.' && not_just_dots {
            return tok!("local identifier"(name));
        }

        if can_be_keyword && is_local == 0 {
            if let Some(keyword) = KEYWORDS.get(&UniCase::ascii(name.as_str())) {
                return keyword.clone();
            }
        }
        let identifier = params.identifiers.get_or_intern(&name);
        // `.` and `..` are not local.
        if is_local != 0 && not_just_dots {
            let mut chars = name.chars();
            while let Some(ch) = chars.next() {
                if ch == '.' && !chars.next().is_some_and(|next| next != '.') {
                    let span = Span::Normal(span.clone());
                    params.error(&span, |error| {
                        error.set_message("a component of this identifier is empty");
                        error.add_label(
                            diagnostics::error_label(&span)
                                .with_message("dots cannot be trailing or consecutive"),
                        );
                    });
                }
            }
            tok!("scoped identifier"(identifier))
        } else {
            tok!("identifier"(identifier))
        }
    }

    fn read_string_maybe(
        &mut self,
        span: &mut NormalSpan,
        params: &mut LexerParams,
    ) -> Option<TokenPayload> {
        let mut string = CompactString::default();
        let depth = self.contexts.len();

        let ctx = self.active_context().unwrap();
        debug_assert!(span.bytes.is_empty());
        *span = ctx.new_span();
        ctx.with_raw_text(span, |ctx, text| {
            fn is_quote(&(_, ch): &(usize, char)) -> bool {
                ch == '"'
            }

            let mut chars = text.char_indices().peekable();
            let raw = chars.next_if(|&(_zero, ch)| ch == '#').is_some();
            if chars.next_if(is_quote).is_none() {
                return 0; // Not a string.
            }
            Self::read_string_inner(
                &mut string,
                raw,
                false, // Not passthrough.
                '"',
                &mut chars,
                ctx,
                depth,
                text,
                params,
            )
        });

        (!span.bytes.is_empty()).then_some(tok!("string"(string)))
    }

    fn read_char_literal(
        &mut self,
        span: &mut NormalSpan,
        params: &mut LexerParams,
    ) -> TokenPayload {
        let mut string = CompactString::default();
        let depth = self.contexts.len();

        let ctx = self
            .active_context()
            .expect("No active context to read char from");
        ctx.with_raw_text(span, |ctx, text| {
            let mut chars = text.char_indices().peekable();
            let opt = chars.next();
            debug_assert_eq!(opt, Some((0, '\'')));
            Self::read_string_inner(
                &mut string,
                false, // Not raw.
                false, // Not passthrough.
                '\'',
                &mut chars,
                ctx,
                depth,
                text,
                params,
            )
        });

        tok!("character literal"(string))
    }

    fn read_string_inner(
        string: &mut CompactString,
        raw: bool,
        passthrough: bool,
        delim_char: char,
        chars: &mut Peekable<CharIndices>,
        ctx: &Context,
        ctx_depth: usize,
        text: &str,
        params: &mut LexerParams,
    ) -> usize {
        let is_delim = |&(_, ch): &(usize, char)| ch == delim_char;

        // If passthrough, make sure to add the string's opening quotes.
        if passthrough {
            if raw {
                string.push('#');
            }
            string.push(delim_char);
        }
        let multiline = if delim_char != '"' {
            false // Only strings can be multiline, not character literals.
        } else if let Some((ofs, quote)) = chars.next_if(is_delim) {
            if passthrough {
                string.push(delim_char);
            }
            // We have two consecutive quotes: if there are only two, then we have an empty string;
            //                                 if there are three, we have a multi-line string.
            if chars.next_if(is_delim).is_none() {
                return ofs + quote.len_utf8();
            }
            if passthrough {
                string.push(delim_char);
            }
            true
        } else {
            false
        };

        let mut end_ofs = text.len();
        while let Some((ofs, ch)) = chars.next() {
            let append_expansion = |dest: &mut CompactString, contents| {
                if !passthrough || raw {
                    dest.push_str(contents);
                } else {
                    dest.reserve(contents.len()); // TODO(perf): is this helping?
                    for ch in contents.chars() {
                        push_char_passthrough(dest, ch);
                    }
                }
            };
            fn push_char_passthrough(string: &mut CompactString, ch: char) {
                match ch {
                    '\n' => string.push_str("\\n"),
                    '\r' => string.push_str("\\r"),
                    '\t' => string.push_str("\\t"),
                    '\0' => string.push_str("\\0"),
                    chars!(self_escape) => {
                        string.push('\\');
                        string.push(ch);
                    }
                    _ => string.push(ch),
                }
            }

            match ch {
                ch if ch == delim_char => {
                    if multiline {
                        // We need three consecutive quotes to close the string, not just one.
                        if let Some((_ofs, _quote)) = chars.next_if(is_delim) {
                            // Two in a row...
                            if let Some((ofs, _quote)) = chars.next_if(is_delim) {
                                if passthrough {
                                    string.push(delim_char);
                                    string.push(delim_char);
                                    string.push(delim_char);
                                }
                                // Three in a row! Winner winner chicken dinner
                                return ofs + ch.len_utf8();
                            }
                            string.push(delim_char);
                        }
                        string.push(delim_char);
                    } else {
                        if passthrough {
                            string.push(delim_char);
                        }
                        return ofs + ch.len_utf8();
                    }
                }
                chars!(newline) => {
                    if !multiline {
                        // Unterminated string, but that will be reported at expansion time
                        // (if it is expanded at all).
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
                            Ok((_kind, src)) => append_expansion(string, src.contents.text()),
                            Err(err) => {
                                let macro_arg_len = match macro_chars.peek() {
                                    Some(&(ofs, _ch)) => ofs,
                                    None => text.len(),
                                };
                                let span = Span::Normal(ctx.new_span_ofs(ofs..macro_arg_len));
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
                    } else {
                        let escaped = chars.next();
                        if let Some(value) =
                            Self::get_char_escape(escaped, ofs, ctx, text, chars, params)
                        {
                            // If passthrough, pass the escaped character as-is, but only if it is escapable.
                            if passthrough {
                                string.push('\\');
                                string.push(escaped.unwrap().1);
                            } else {
                                string.push(value);
                            }
                        }
                    }
                }
                '{' if !raw => {
                    let mut expanded = CompactString::default();
                    // We don't care for the ident.
                    let _ = Self::read_interpolation(
                        chars,
                        text,
                        ctx,
                        &mut expanded,
                        ctx_depth,
                        params,
                    );
                    append_expansion(string, &expanded);
                }
                '}' if !raw => {
                    let span = Span::Normal(ctx.new_span_len(ofs, '}'.len_utf8()));
                    params.warn(warning!("unescaped-brace"), &span, |warning| {
                        warning.set_message("unescaped closing brace");
                        warning.add_label(
                            diagnostics::warning_label(&span)
                                .with_message("this isn't part of an interpolation"),
                        );
                    });
                    string.push('}');
                }

                ch => string.push(ch),
            }
        }

        // The string wasn't terminated properly.
        let err_span = Span::Normal(ctx.new_span_ofs(0..end_ofs));
        params.error(&err_span, |error| {
            error.set_message(if delim_char == '"' {
                "unterminated string literal"
            } else {
                "unterminated character literal"
            });
            error.add_label(diagnostics::error_label(&err_span).with_message(format!(
                "no closing quote before the end of {}",
                if multiline { "input" } else { "the line" }
            )))
        });

        // If passthrough, close the string, so that the error doesn't occur again at expansion time.
        if passthrough {
            string.push(delim_char);
            if multiline {
                string.push(delim_char);
                string.push(delim_char);
            }
        }

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
            Some((_ofs, ch @ chars!(self_escape))) => Some(ch),
            Some((_ofs, 'n')) => Some('\n'),
            Some((_ofs, 'r')) => Some('\r'),
            Some((_ofs, 't')) => Some('\t'),
            Some((_ofs, '0')) => Some('\0'),

            Some((_ofs, '\r')) => {
                chars.next_if(|(_ofs, ch)| *ch == '\n'); // Handle CRLF.
                None
            }
            Some((_ofs, '\n')) => None, // The newline has already been consumed, the line continuation is already complete.
            // `line_cont` contains `newline`, which is intentionally handled above.
            #[allow(unreachable_patterns)]
            Some((_ofs, chars!(line_cont))) => {
                if let Err(err) = Self::read_line_continuation(chars) {
                    let span = Span::Normal(ctx.new_span_len(backslash_ofs, '\\'.len_utf8()));
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
                let span =
                    Span::Normal(ctx.new_span_ofs(backslash_ofs..escaped_ofs + ch.len_utf8()));
                params.error(&span, |error| {
                    error.set_message("invalid character escape");
                    error.add_label(
                        diagnostics::error_label(&span)
                            .with_message("cannot escape this character"),
                    );
                });

                None // Avoid returning a character, so passthrough strings don't report the bad escape twice.
            }
            None => {
                let span = Span::Normal(ctx.new_span_ofs(backslash_ofs..text.len()));
                params.error(&span, |error| {
                    error.set_message("invalid character escape");
                    error.add_label(
                        diagnostics::error_label(&span).with_message("no character to escape"),
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

        let ctx = self
            .active_context()
            .expect("Cannot lex a raw string without a context active");
        let mut span = ctx.new_span();

        let mut span_before_whitespace = None;
        let mut string = CompactString::default();
        let mut parens_depth = 0usize;
        let ends_with_comma = loop {
            match self.peek(&mut params) {
                Some('\'') => {
                    let depth = self.contexts.len();
                    let ctx = self.active_context().unwrap();
                    let mut string_span = ctx.new_span();
                    ctx.with_raw_text(&mut string_span, |ctx, text| {
                        let mut chars = text.char_indices().peekable();
                        chars.next(); // Opening quote.
                        Self::read_string_inner(
                            &mut string,
                            false, // Not raw.
                            true,  // Passthrough.
                            '\'',
                            &mut chars,
                            ctx,
                            depth,
                            text,
                            &mut params,
                        )
                    });
                    if Rc::ptr_eq(&string_span.node.src, &span.node.src) {
                        debug_assert_eq!(span.bytes.end, string_span.bytes.start);
                        span.bytes.end = string_span.bytes.end;
                    }
                    span_before_whitespace = None;
                }
                Some('"') => {
                    let depth = self.contexts.len();
                    let ctx = self.active_context().unwrap();
                    let mut string_span = ctx.new_span();
                    ctx.with_raw_text(&mut string_span, |ctx, text| {
                        let mut chars = text.char_indices().peekable();
                        chars.next(); // Opening quote.
                        Self::read_string_inner(
                            &mut string,
                            false, // Not raw.
                            true,  // Passthrough.
                            '"',
                            &mut chars,
                            ctx,
                            depth,
                            text,
                            &mut params,
                        )
                    });
                    if Rc::ptr_eq(&string_span.node.src, &span.node.src) {
                        debug_assert_eq!(span.bytes.end, string_span.bytes.start);
                        span.bytes.end = string_span.bytes.end;
                    }
                    span_before_whitespace = None;
                }
                Some('#') => {
                    let depth = self.contexts.len();
                    let ctx = self.active_context().unwrap();
                    let mut string_span = ctx.new_span();
                    ctx.with_raw_text(&mut string_span, |ctx, text| {
                        let mut chars = text.char_indices().peekable();
                        chars.next(); // Hash.
                        if chars.next_if(|(_ofs, ch)| *ch == '"').is_none() {
                            string.push('#');
                            return '#'.len_utf8(); // Not a string, just the hash.
                        }
                        Self::read_string_inner(
                            &mut string,
                            true, // Raw.
                            true, // Passthrough.
                            '"',
                            &mut chars,
                            ctx,
                            depth,
                            text,
                            &mut params,
                        )
                    });
                    if Rc::ptr_eq(&string_span.node.src, &span.node.src) {
                        debug_assert_eq!(span.bytes.end, string_span.bytes.start);
                        span.bytes.end = string_span.bytes.end;
                    }
                    span_before_whitespace = None;
                }

                Some(chars!(newline)) | None => break false,
                Some(';') => {
                    // Intentionally not consuming the semicolon! We don't want it to be part of `span`.
                    let ctx = self.active_context().unwrap();
                    let mut command_span = ctx.new_span();
                    ctx.with_raw_text(&mut command_span, |_ctx, text| {
                        let mut chars = text.char_indices().peekable();
                        Self::read_line_comment(&mut chars);
                        chars.next().map_or(text.len(), |(ofs, _ch)| ofs)
                    });
                    break false;
                }
                Some('/') => {
                    let ctx = self.active_context().unwrap();
                    let mut comment_span = ctx.new_span();
                    ctx.with_raw_text(&mut comment_span, |ctx, text| {
                        let mut chars = text.char_indices().peekable();
                        chars.next(); // The slash.
                        let Some((_ofs, '*')) = chars.next() else {
                            return 0; // Not a block comment.
                        };

                        if let Err(err) = Self::read_block_comment(
                            &mut chars,
                            ctx,
                            params.nb_errors_left,
                            params.options,
                        ) {
                            let span = Span::Normal(ctx.new_span_len(0, "/*".len()));
                            params.error(&span, |error| {
                                error.set_message(&err);
                                error.add_label(
                                    diagnostics::error_label(&span).with_message(err.label_msg()),
                                );
                            });
                        }
                        chars.next().map_or(text.len(), |(ofs, _ch)| ofs)
                    });
                    if comment_span.bytes.is_empty() {
                        // We didn't read a block comment.
                        self.consume(&mut span);
                        string.push('/');
                        span_before_whitespace = None;
                    } else if span.bytes.is_empty() {
                        // Leading block comments are treated as whitespace for span tracking purposes.
                        span = comment_span;
                        span.make_empty();
                    } else {
                        // Non-leading block comment.
                        if span_before_whitespace.is_none() {
                            // Trailing block comments are treated as whitespace for span tracking purposes.
                            span_before_whitespace = Some(span.clone());
                        }
                        if Rc::ptr_eq(&span.node.src, &comment_span.node.src) {
                            debug_assert_eq!(span.bytes.end, comment_span.bytes.start);
                            span.bytes.end = comment_span.bytes.end;
                        }
                    }
                }

                Some(',') if parens_depth == 0 => {
                    let mut dummy_span = span.clone(); // TODO: maybe instead, a version of `consume` that doesn't take a span?
                    self.consume(&mut dummy_span);
                    break true;
                }

                Some('(') => {
                    self.consume(&mut span);
                    string.push('(');
                    span_before_whitespace = None;
                    parens_depth += 1;
                }
                Some(')') if parens_depth != 0 => {
                    self.consume(&mut span);
                    string.push(')');
                    span_before_whitespace = None;
                    parens_depth -= 1;
                }

                Some('\\') => {
                    let ctx = self.active_context().unwrap();
                    let mut escape_span = ctx.new_span();
                    ctx.with_raw_text(&mut escape_span, |ctx, text| {
                        let mut chars = text.char_indices().peekable();
                        chars.next(); // The backslash.
                        let ch = chars.next();
                        if let Some((_ofs, escapee @ (',' | '(' | ')'))) = ch {
                            // This escape is only valid in raw contexts.
                            string.push(escapee);
                            2 // The backslash plus the escapee.
                        } else {
                            if let Some(value) =
                                Self::get_char_escape(ch, 0, ctx, text, &mut chars, &params)
                            {
                                string.push(value);
                            }
                            chars.next().map_or(text.len(), |(ofs, _ch)| ofs)
                        }
                    });
                    span = span.merged_with(&escape_span);
                    span_before_whitespace = None;
                }

                Some(ch @ chars!(whitespace)) => {
                    if span.bytes.is_empty() {
                        // Leading whitespace.
                        self.consume(&mut span);
                        span.make_empty();
                    } else {
                        if span_before_whitespace.is_none() {
                            span_before_whitespace = Some(span.clone());
                        }
                        self.consume(&mut span);
                        string.push(ch);
                    }
                }
                Some(ch) => {
                    self.consume(&mut span);
                    string.push(ch);
                    span_before_whitespace = None;
                }
            }
        };

        // Trim right whitespace.
        let trimmed_len = string.trim_end_matches(is_whitespace).len();
        string.truncate(trimmed_len);
        // Trim left whitespace. TODO: it can only have been added by expansions, maybe perform the check only there?
        let trimmed = string.trim_start_matches(is_whitespace);
        // SAFETY: `trimmed` is derived from `string`.
        drop(string.drain(..unsafe { trimmed.as_ptr().offset_from(string.as_ptr()) } as usize));

        if let Some(trimmed_span) = span_before_whitespace {
            // The argument ended with some (literal) whitespace, “rewind” the span to before that.
            span = trimmed_span;
        }

        // Returning COMMAs to the parser would mean that two consecutive commas (i.e. an empty argument)
        // need to return two different tokens (string then comma) without consuming any chars.
        // To avoid this, commas in raw mode end the current macro argument, but are not tokenized themselves.
        if ends_with_comma {
            return Some((string, Span::Normal(span)));
        }

        // The last argument may end in a trailing comma, newline, or EOF.
        // To allow trailing commas, what would be the last argument is not emitted if empty.
        // (To pass an empty last argument, use a second trailing comma.)
        if !string.is_empty() {
            debug_assert!(!span.bytes.is_empty());
            Some((string, Span::Normal(span)))
        } else {
            // The span may not be empty, e.g. if it contained only whitespace.
            None
        }
    }
}

impl Lexer {
    fn starts_with_keyword(string: &str, keyword: &str) -> bool {
        // The line begins with a word the size of the keyword...
        string.get(..keyword.len()).is_some_and(|first_word| {
            // ...which matches the keyword...
            unicase::eq_ascii(first_word, keyword)
            // ...and the keyword doesn't happen to just be a prefix.
            && !matches!(
                string[keyword.len()..].chars().next(),
                Some(chars!(ident))
            )
        })
    }

    pub fn capture_until_keyword(
        &mut self,
        end_keyword: &str,
        nesting_keywords: &[&str],
        kind: &'static str,
    ) -> (NormalSpan, Result<(), CaptureBlockErr>) {
        let ctx = self
            .active_context()
            .expect("Cannot capture a block without a context active");
        let mut span = ctx.new_span();

        let mut capture_len = 0;
        let mut nesting_depth = 0usize;
        let res = ctx.skimming_lines(
            &mut span,
            kind,
            |trimmed_line, ofs_of_line, _nb_trimmed, _ctx| {
                // Capture everything before the start of this line.
                capture_len = ofs_of_line;

                if Self::starts_with_keyword(trimmed_line, end_keyword) {
                    // Found the ending keyword!
                    match nesting_depth.checked_sub(1) {
                        Some(new_depth) => nesting_depth = new_depth,
                        None => return Some(end_keyword.len()),
                    }
                } else {
                    for keyword in nesting_keywords {
                        if Self::starts_with_keyword(trimmed_line, keyword) {
                            nesting_depth += 1;
                            break; // As an optimisation.
                        }
                    }
                }
                None
            },
        );

        if res.is_ok() {
            // Don't capture the closing keyword, if one was found.
            span.bytes.end = span.bytes.start + capture_len;
        }

        (span, res)
    }

    pub fn skip_to_eol(&mut self) {
        let ctx = self
            .active_context()
            .expect("Cannot skip to EOL without a context active");
        let mut span = ctx.new_span();

        ctx.with_raw_text(&mut span, |_ctx, text| {
            match text.char_indices().find(|(_ofs, ch)| is_newline(*ch)) {
                Some((ofs, _ch)) => ofs,
                None => text.len(),
            }
        });
    }

    pub fn skip_conditional_block(&mut self) -> Result<(), CaptureBlockErr> {
        let ctx = self
            .active_context()
            .expect("Cannot skip a block without a context active");
        let mut span = ctx.new_span();

        let mut nesting_depth = 0usize;
        ctx.skimming_lines(
            &mut span,
            "conditional block",
            |trimmed_line, _ofs_of_line, _nb_trimmed, _ctx| {
                if Self::starts_with_keyword(trimmed_line, "endc") {
                    // Found the ending keyword!
                    match nesting_depth.checked_sub(1) {
                        Some(new_depth) => nesting_depth = new_depth,
                        None => return Some(0),
                    }
                } else if nesting_depth == 0
                    && (Self::starts_with_keyword(trimmed_line, "elif")
                        || Self::starts_with_keyword(trimmed_line, "else"))
                {
                    return Some(0);
                } else if Self::starts_with_keyword(trimmed_line, "if") {
                    nesting_depth += 1;
                }
                None
            },
        )
    }
}

#[derive(displaydoc::Display)]
enum MacroArgError<'text> {
    /// unterminated macro argument
    // (For `\<`.))
    Unterminated,
    /// macro argument #{idx} doesn't exist
    NoSuchArg { idx: isize, max_valid: usize },
    /// a macro argument was used outside of a macro
    MacroArgOutsideMacro,
    /// a unique identifier is not available in this context
    NoUniqueId,
    /// macro argument #0 doesn't exist
    NoArg0,
    /// empty bracketed macro argument
    EmptyBracketed,
    /// no digit after minus sign
    NoDigitAfterMinus,
    /// invalid character in bracketed macro argument
    InvalidBracketedChar,
    /// consecutive underscores are not allowed
    ConsecutiveUnderscores,
    /// trailing underscore
    TrailingUnderscore,
    /// {0}
    SymErr(SymbolError<'text, 'text>),
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
            Self::NoArg0 => "this reference is invalid".into(),
            Self::EmptyBracketed => {
                "expected a number or symbol name between the angle brackets".into()
            }
            Self::NoDigitAfterMinus => "no digits in this".into(),
            Self::InvalidBracketedChar => "this bracketed macro argument is invalid".into(),
            Self::ConsecutiveUnderscores => "this bracketed macro argument is invalid".into(),
            Self::TrailingUnderscore => "this bracketed macro argument is invalid".into(),
            // TODO: more specific label messages
            Self::SymErr(_err) => "this symbol is invalid".into(),
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
impl<'text> From<SymbolError<'text, 'text>> for MacroArgError<'text> {
    fn from(value: SymbolError<'text, 'text>) -> Self {
        Self::SymErr(value)
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
