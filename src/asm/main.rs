use std::{cell::Cell, path::PathBuf};

use rustc_hash::FxBuildHasher;
use sysexits::ExitCode;

mod charmap;
use charmap::Charmaps;
mod cli;
use cli::Cli;
#[path = "../common/mod.rs"]
mod common;
mod diagnostics;
use diagnostics::{WarningState, NB_META_WARNINGS, NB_WARNINGS};
mod expr;
mod format;
mod macro_args;
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
    // TODO: maybe a `smallvec` instead? This never has more than one entry in practice.
    runtime_opt_stack: Vec<RuntimeOptions>,
}
#[derive(Debug, Clone)]
pub struct RuntimeOptions {
    binary_digits: [char; 2],
    gfx_chars: [char; 4],
    pad_byte: u8,
    q_precision: usize,
    recursion_depth: usize,
    // TODO: use some bitfield(s) instead?
    warnings: [WarningState; NB_WARNINGS],
    meta_warnings: [WarningState; NB_META_WARNINGS + 1],
    warnings_are_errors: bool,
}

fn main() -> ExitCode {
    let Ok((mut options, main_path, defines, warnings)) =
        crate::common::cli::setup_and_parse_args().and_then(Cli::finish)
    else {
        return ExitCode::Usage;
    };
    let nb_errors_left = Cell::new(options.max_errors);

    let mut identifiers = Identifiers::new();
    let mut charmaps = Charmaps::new();
    let mut sections = Sections::new();
    let mut symbols = Symbols::new(&mut identifiers);

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

    if nb_errors_left.get() == options.max_errors {
        ExitCode::Ok
    } else {
        ExitCode::DataErr
    }
}
