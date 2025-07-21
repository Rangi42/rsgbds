use std::{
    cell::Cell,
    num::NonZeroUsize,
    ops::{Deref, Index, Range},
    sync::OnceLock,
};

use ariadne::{Config, IndexType, Label, ReportKind};
use compact_str::CompactString;
use yansi::Color;

use crate::{
    common::S,
    sources::{NormalSpan, Span, SpanKind},
    Options,
};

const ERROR_COLOR: Color = Color::Red;
pub const ERROR_KIND: ReportKind = ReportKind::Custom("error", ERROR_COLOR);

const WARNING_COLOR: Color = Color::Yellow;
pub const WARNING_KIND: ReportKind = ReportKind::Custom("warning", WARNING_COLOR);

const NOTE_COLOR: Color = Color::Fixed(115); // ariadne's default `note_color`.

pub type ReportBuilder<'span> = ariadne::ReportBuilder<'static, &'span Span>;
pub type Report<'span> = ariadne::Report<'static, &'span Span>;

pub fn error<'span, F: FnOnce(&mut ReportBuilder<'span>)>(
    span: &'span Span,
    build: F,
    nb_errors_left: &Cell<usize>,
    options: &Options,
) {
    if nb_errors_left.get() == 0 {
        return;
    }

    let mut error = Report::build(ERROR_KIND, span)
        .with_config(Config::default().with_index_type(IndexType::Byte));
    build(&mut error);
    add_backtrace_labels_and_print(error, span, options);
    decrement_error_count(nb_errors_left, options);
}

fn decrement_error_count(nb_errors_left: &Cell<usize>, options: &Options) {
    nb_errors_left.set(nb_errors_left.get() - 1);

    if nb_errors_left.get() == 0 {
        let span = Span::Builtin;
        Report::build(WARNING_KIND, &span)
            .with_message(format!(
                "Reached {} errors, any subsequent will not be printed",
                options.max_errors,
            ))
            .finish()
            .eprint(())
            .expect("Failed to print diagnostic");
    }
}

pub fn warn<'span, F: FnOnce(&mut ReportBuilder<'span>)>(
    id: WarningKind,
    span: &'span Span,
    build: F,
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
            state: WarningLevel::Default | WarningLevel::Enabled,
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
                    state: WarningLevel::Default | WarningLevel::Enabled,
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

    let mut warning = Report::build(kind, span)
        .with_config(Config::default().with_index_type(IndexType::Byte))
        .with_code(id);
    build(&mut warning);
    add_backtrace_labels_and_print(warning, span, options);
    if kind == ERROR_KIND {
        decrement_error_count(nb_errors_left, options);
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

fn add_backtrace_labels_and_print(diag: ReportBuilder<'_>, span: &Span, options: &Options) {
    let Span::Normal(span) = span else {
        print_diag(diag);
        return;
    };

    fn print_diag(diag: ReportBuilder<'_>) {
        diag.finish()
            .eprint(())
            .expect("Failed to print diagnostic");
    }
    fn add_level(
        diag: ReportBuilder<'_>,
        mut span: &NormalSpan,
        allowed_depth: usize,
        options: &Options,
    ) {
        let mut diag = diag; // Required for rustc to figure out a new lifetime for that binding of `diag`.

        if let Some(parent) = &span.parent {
            match allowed_depth.checked_sub(1) {
                Some(depth) => {
                    let diag_span = Span::Normal((**parent).clone());
                    diag.add_label(note_label(&diag_span).with_message(match span.kind {
                        SpanKind::File => "file included from here",
                        SpanKind::Macro(_) => "macro called here",
                        SpanKind::Loop(_) => "loop beginning here",
                        SpanKind::Expansion(_) => "interpolated here",
                        SpanKind::MacroArg(_) => "expanded here",
                    }));
                    add_level(diag, parent, depth, options);
                }
                None => {
                    let mut remaining_depth = 1usize;
                    while let Some(parent) = &span.parent {
                        remaining_depth += 1;
                        span = &**parent;
                    }
                    diag.add_note(format!(
                        "... {remaining_depth} more level{} of nesting omitted",
                        S::from(remaining_depth)
                    ));
                    print_diag(diag);
                }
            }
        } else {
            print_diag(diag);
        }
    }
    add_level(diag, span, options.backtrace_depth, options);
}

pub fn error_label(span: &Span) -> Label<&Span> {
    Label::new(span).with_color(ERROR_COLOR)
}

pub fn warning_label(span: &Span) -> Label<&Span> {
    Label::new(span).with_color(WARNING_COLOR)
}

pub fn note_label(span: &Span) -> Label<&Span> {
    Label::new(span).with_color(NOTE_COLOR)
}

impl<'span> ariadne::Span for &'span Span {
    type SourceId = Span;

    fn source(&self) -> &Self::SourceId {
        self
    }

    fn start(&self) -> usize {
        match self {
            Span::CommandLine | Span::Builtin => 0,
            Span::Normal(span) => span.bytes.start,
        }
    }
    fn end(&self) -> usize {
        match self {
            Span::CommandLine | Span::Builtin => 0,
            Span::Normal(span) => span.bytes.end,
        }
    }
}

impl ariadne::Cache<Span> for () {
    type Storage = CompactString;

    fn fetch<'ret, 'cache: 'ret, 'id: 'ret>(
        &'cache mut self,
        id: &'id Span,
    ) -> Result<&'ret ariadne::Source<Self::Storage>, Box<dyn std::fmt::Debug + 'cache>> {
        static EMPTY_SOURCE: OnceLock<ariadne::Source<CompactString>> = OnceLock::new();
        match id {
            Span::CommandLine | Span::Builtin => {
                Ok(EMPTY_SOURCE.get_or_init(|| CompactString::default().into()))
            }
            Span::Normal(span) => Ok(&span.src.contents),
        }
    }

    fn display<'a>(&self, id: &'a Span) -> Option<Box<dyn std::fmt::Display + 'a>> {
        match id {
            Span::CommandLine => Some(Box::new("<command line>")),
            Span::Builtin => Some(Box::new("<built-in>")),
            Span::Normal(span) => Some(Box::new(&span.src.name)),
        }
    }
}
