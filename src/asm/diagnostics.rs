use std::{cell::Cell, ops::Range};

use ariadne::{Config, IndexType, Label, ReportKind};
use yansi::Color;

use crate::{
    context_stack::{SourceNode, Span},
    source_store::{RawSpan, Report, ReportBuilder, SourceStore},
    Options,
};

const ERROR_COLOR: Color = Color::Red;
pub const ERROR_KIND: ReportKind = ReportKind::Custom("error", ERROR_COLOR);

const WARNING_COLOR: Color = Color::Yellow;
pub const WARNING_KIND: ReportKind = ReportKind::Custom("warning", WARNING_COLOR);

const NOTE_COLOR: Color = Color::Fixed(115); // ariadne's default `note_color`.

pub fn error<F: FnOnce(&mut ReportBuilder)>(
    span: &Span<'_>,
    build: F,
    sources: &SourceStore,
    nb_errors_left: &Cell<usize>,
    options: &Options,
) {
    if nb_errors_left.get() == 0 {
        return;
    }

    let mut error = Report::build(ERROR_KIND, span.resolve())
        .with_config(Config::default().with_index_type(IndexType::Byte));
    build(&mut error);
    error
        .finish()
        .eprint(sources)
        .expect("Failed to print diagnostic");
    decrement_error_count(sources, nb_errors_left, options);
}

pub fn lex_error<F: FnOnce(ReportBuilder, RawSpan) -> ReportBuilder>(
    src_node: &SourceNode,
    byte_range: &Range<usize>,
    build: F,
    sources: &SourceStore,
    nb_errors_left: &Cell<usize>,
    options: &Options,
) {
    if nb_errors_left.get() == 0 {
        return;
    }

    let span = src_node.resolve(byte_range);
    build(
        Report::build(ERROR_KIND, span.clone())
            .with_config(Config::default().with_index_type(IndexType::Byte)),
        span,
    )
    .finish()
    .eprint(sources)
    .expect("Failed to print diagnostic");
    decrement_error_count(sources, nb_errors_left, options);
}

fn decrement_error_count(sources: &SourceStore, nb_errors_left: &Cell<usize>, options: &Options) {
    nb_errors_left.set(nb_errors_left.get() - 1);

    if nb_errors_left.get() == 0 {
        let span = Span::BUILTIN.resolve();
        Report::build(WARNING_KIND, span)
            .with_message(format!(
                "Reached {} errors, any subsequent will not be printed",
                options.max_errors,
            ))
            .finish()
            .eprint(sources)
            .expect("Failed to print diagnostic");
    }
}

pub fn warn<F: FnOnce(&mut ReportBuilder)>(
    id: WarningKind,
    span: &Span<'_>,
    build: F,
    sources: &SourceStore,
    nb_errors_left: &Cell<usize>,
    options: &Options,
) {
    if options.inhibit_warnings {
        return;
    }

    let warning = &options.runtime_opts.warnings[id.0];
    // The state of the meta warning that this specific warning is a part of.
    let meta_warning = &options.runtime_opts.meta_warnings[DEFAULT_WARNINGS[id.0].0];
    let enabled_kind = if options.runtime_opts.warnings_are_errors
        && warning.error != WarningLevel::Disabled
        && meta_warning.error != WarningLevel::Disabled
    {
        ERROR_KIND // Error promotion is enabled globally, and not suppressed for this particular warning.
    } else {
        WARNING_KIND
    };
    let kind = match warning {
        WarningState {
            state: WarningLevel::Disabled,
            ..
        } => return, // `-Wno-this`
        WarningState {
            error: WarningLevel::Enabled,
            ..
        } => ERROR_KIND, // `-Werror=this`
        WarningState {
            state: WarningLevel::Enabled,
            ..
        } => enabled_kind, // `-Wthis`
        WarningState {
            state: WarningLevel::Default,
            error: WarningLevel::Default | WarningLevel::Disabled,
        } => {
            // Defer to the meta warning this particular one is part of.
            match meta_warning {
                WarningState {
                    state: WarningLevel::Disabled,
                    ..
                } => return, // `-Wno-meta`
                WarningState {
                    error: WarningLevel::Enabled,
                    ..
                } => ERROR_KIND, // `-Werror=meta`
                WarningState {
                    state: WarningLevel::Enabled,
                    ..
                } => enabled_kind, // `-Wmeta`
                WarningState {
                    state: WarningLevel::Default,
                    error: WarningLevel::Default | WarningLevel::Disabled,
                } => {
                    // Absolutely no flag related to this warning has been given,
                    // so either this warning is part of the default group, or it's disabled.
                    if DEFAULT_WARNINGS[id.0].0 == 0 {
                        enabled_kind
                    } else {
                        return;
                    }
                }
            }
        }
    };
    if kind == ERROR_KIND && nb_errors_left.get() == 0 {
        return;
    }

    let span = span.resolve();
    let mut warning = Report::build(kind, span)
        .with_config(Config::default().with_index_type(IndexType::Byte))
        .with_code(id);
    build(&mut warning);
    warning
        .finish()
        .eprint(sources)
        .expect("Failed to print diagnostic");
    if kind == ERROR_KIND {
        decrement_error_count(sources, nb_errors_left, options);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct WarningState {
    pub state: WarningLevel,
    pub error: WarningLevel,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum WarningLevel {
    Disabled,
    Enabled,
    #[default]
    Default, // "Meta" warnings will only override this level.
}
impl WarningLevel {
    fn update(&mut self, other: WarningLevel) {
        if other != WarningLevel::Default {
            *self = other;
        }
    }
}
impl WarningState {
    pub fn update(&mut self, other: WarningState) {
        self.state.update(other.state);
        self.error.update(other.error);
    }
}

// This file is generated by the build script (`build.rs`).
include!(concat!(env!("OUT_DIR"), "/warnings.rs"));

pub fn error_label<S: Into<RawSpan>>(span: S) -> Label<RawSpan> {
    Label::new(span.into()).with_color(ERROR_COLOR)
}

pub fn warning_label<S: Into<RawSpan>>(span: S) -> Label<RawSpan> {
    Label::new(span.into()).with_color(WARNING_COLOR)
}

pub fn note_label<S: Into<RawSpan>>(span: S) -> Label<RawSpan> {
    Label::new(span.into()).with_color(NOTE_COLOR)
}
