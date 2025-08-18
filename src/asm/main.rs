use std::{cell::Cell, path::PathBuf};

use rustc_hash::FxBuildHasher;
use sysexits::ExitCode;

mod charmap;
use charmap::Charmaps;
mod cli;
use cli::Cli;
#[path = "../common/mod.rs"]
mod common;
mod cond;
mod diagnostics;
use diagnostics::{warning, WarningState, NB_META_WARNINGS, NB_WARNINGS};
mod expr;
mod format;
mod instructions;
mod macro_args;
mod obj_file;
mod section;
use section::Sections;
mod sources;
mod symbols;
use symbols::Symbols;
mod syntax;

pub type Identifier = string_interner::symbol::SymbolU32;
// TODO(perf): evaluate other backends
pub type Identifiers = string_interner::StringInterner<
    string_interner::backend::StringBackend<Identifier>,
    FxBuildHasher,
>;

#[derive(Debug, Clone)]
pub struct Options {
    export_all: bool,
    inc_paths: Vec<PathBuf>,
    dependfile: Option<PathBuf>,
    // TODO: `MG`, `MP`, `MT`, `MQ`
    output: Option<PathBuf>,
    preinclude: Option<PathBuf>,
    inhibit_warnings: bool,
    backtrace_depth: usize,
    max_errors: usize,
    runtime_opts: RuntimeOptions,
    // TODO(perf): maybe a `smallvec` instead? This never has more than one entry in practice.
    runtime_opt_stack: Vec<RuntimeOptions>,
}
#[derive(Debug, Clone)]
pub struct RuntimeOptions {
    binary_digits: [char; 2],
    gfx_chars: [char; 4],
    pad_byte: u8,
    q_precision: usize,
    recursion_depth: usize,
    // TODO(perf): use some bitfield(s) instead?
    warnings: [WarningState; NB_WARNINGS],
    meta_warnings: [WarningState; NB_META_WARNINGS + 1],
    warnings_are_errors: bool,
}

fn main() -> ExitCode {
    let Ok((mut options, main_path, defines)) =
        crate::common::cli::setup_and_parse_args().and_then(Cli::finish)
    else {
        return ExitCode::Usage;
    };
    let nb_errors_left = Cell::new(options.max_errors);

    let mut identifiers = Identifiers::new();
    let mut charmaps = Charmaps::new();
    let mut sections = Sections::new();
    let mut symbols = Symbols::new(&mut identifiers, defines, &nb_errors_left, &options);

    if let Some(preinclude_path) = &options.preinclude {
        syntax::parser::parse_file(
            &preinclude_path.clone(),
            &mut identifiers,
            &mut sections,
            &mut charmaps,
            &mut symbols,
            &nb_errors_left,
            &mut options,
        );
    }

    syntax::parser::parse_file(
        &main_path,
        &mut identifiers,
        &mut sections,
        &mut charmaps,
        &mut symbols,
        &nb_errors_left,
        &mut options,
    );

    // TODO: unclosed UNION
    sections.warn_if_unclosed_load_block(&nb_errors_left, &options);
    sections.check_section_sizes(&nb_errors_left, &options);
    warn_if_opt_stack_not_empty(&nb_errors_left, &options);
    charmaps.warn_if_stack_not_empty(&nb_errors_left, &options);
    sections.warn_if_stack_not_empty(&nb_errors_left, &options);

    let nb_errors = options.max_errors - nb_errors_left.get();
    if nb_errors != 0 {
        eprintln!("{nb_errors} error{} generated.", common::S::from(nb_errors));
        return ExitCode::DataErr;
    }

    if let Some(obj_path) = &options.output {
        if let Err(code) = obj_file::emit(
            obj_path,
            &identifiers,
            &sections,
            &symbols,
            &nb_errors_left,
            &options,
        ) {
            let nb_errors = options.max_errors - nb_errors_left.get();
            debug_assert_ne!(nb_errors, 0);
            eprintln!("{nb_errors} error{} generated.", common::S::from(nb_errors));
            return code;
        }
    }

    ExitCode::Ok
}

fn warn_if_opt_stack_not_empty(nb_errors_left: &Cell<usize>, options: &Options) {
    if !options.runtime_opt_stack.is_empty() {
        diagnostics::warn(
            warning!("unmatched-directive"),
            &sources::Span::TopLevel,
            |warning| {
                warning.set_message("`pusho` without corresponding `popo`");
                if options.runtime_opt_stack.len() != 1 {
                    warning.set_note(format!(
                        "{} unclosed `pusho`s",
                        options.runtime_opt_stack.len()
                    ));
                }
            },
            nb_errors_left,
            options,
        );
    }
}
