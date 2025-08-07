/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use std::{fmt::Display, path::PathBuf};

use clap::{ColorChoice, CommandFactory, Parser};
use derive_more::From;
use displaydoc::Display;

use crate::diagnostics::{WarningKind, WarningLevel, META_WARNINGS, SIMPLE_WARNINGS};

use super::*;

#[deny(missing_docs)]
/// The command-line interface.
#[derive(Debug, Parser)]
#[clap(color = crate::common::cli::clap_color_choice())]
#[command(
    name = "rgbasm",
    version = crate::common::build::PKG_VERSION,
    long_version = crate::common::build::CLAP_LONG_VERSION,
    about = "Game Boy assembler",
    long_about = "Assembles some Game Boy assembly into an object file, which can then be linked into a ROM.",
    after_help = "For comprehensive help, run `man rgbasm`, or go to http://rgbds.gbdev.io/docs/",
    arg_required_else_help = true,
    help_expected = true
)]
pub struct Cli {
    /// The two characters to use for binary constants
    #[arg(short, long, value_name = "chars")]
    binary_digits: Option<String>,
    /// How many levels deep should errors report their “backtraces”
    #[arg(long, default_value_t = 10)]
    backtrace_depth: usize,
    /// Controls when to use color
    #[arg(long, default_value_t = ColorChoice::Auto)]
    color: ColorChoice,
    /// Define a string symbol before assembling the source code
    #[arg(short = 'D', long, value_name = "definition")]
    defines: Vec<String>,
    /// Export all labels, even unreferenced and local ones
    #[arg(short, long)]
    export_all: bool,
    /// The four characters to use for character constants
    #[arg(short, long, value_name = "chars")]
    gfx_chars: Option<String>,
    /// Add a new include path
    #[arg(short = 'I', long, value_name = "path")]
    inc_paths: Vec<PathBuf>,
    /// Print Make-style dependencies to this file
    #[arg(short = 'M', long, value_name = "path")]
    dependfile: Option<PathBuf>,
    // TODO: `MG`, `MP`, `MT`, `MQ`
    /// Write an object file to this path
    #[arg(short, long, value_name = "path")]
    output: Option<PathBuf>,
    /// Include this file before starting to read the input
    #[arg(short = 'P', long, value_name = "path")]
    preinclude: Option<PathBuf>,
    /// Use this as the default byte for `ds`
    #[arg(short, long, value_name = "byte")]
    pad_value: Option<String>,
    /// Use this as the default precision of fixed-point numbers
    #[arg(short = 'Q', long, value_name = "precision")]
    q_precision: Option<String>,
    /// Recursion depth past which rgbasm will assume being in an infinite loop
    #[arg(short, long, value_name = "max depth")]
    recursion_depth: Option<String>,
    /// Enable or disable a warning
    #[arg(short = 'W', long, value_name = "flag")]
    warning: Vec<String>,
    /// Inhibit all warnings, even when turned into errors
    #[arg(short = 'w')]
    inhibit_warnings: bool,
    /// Abort if more than this many errors are generated
    #[arg(short = 'X', long, default_value_t = 64, value_name = "max")]
    max_errors: usize,

    /// Path to the file to assemble
    input: PathBuf,
}

impl Cli {
    pub fn finish(self) -> Result<(Options, PathBuf, Vec<String>), ()> {
        crate::common::cli::apply_color_choice(self.color);
        let mut fail = false;

        let mut runtime_opts = RuntimeOptions {
            binary_digits: ['0', '1'],
            gfx_chars: ['0', '1', '2', '3'],
            pad_byte: 0,
            q_precision: 16,
            recursion_depth: 64,
            warnings: Default::default(),
            meta_warnings: Default::default(),
            warnings_are_errors: false,
        };
        runtime_opts.meta_warnings[0].state = WarningLevel::Enabled; // Level 0 is "default".
        if let Some(digits) = self.binary_digits {
            handle_error(runtime_opts.parse_b(&digits), &mut fail);
        }
        if let Some(chars) = self.gfx_chars {
            handle_error(runtime_opts.parse_g(&chars), &mut fail);
        }
        if let Some(value) = self.pad_value {
            handle_error(runtime_opts.parse_p(&value), &mut fail);
        }
        if let Some(precision) = self.q_precision {
            handle_error(runtime_opts.parse_q(&precision), &mut fail);
        }
        if let Some(depth) = self.recursion_depth {
            handle_error(
                runtime_opts
                    .parse_r(&depth)
                    .map(|new_depth| runtime_opts.recursion_depth = new_depth),
                &mut fail,
            );
        }
        for flag in &self.warning {
            handle_error(runtime_opts.parse_w(flag), &mut fail);
        }
        fn handle_error<E: Display>(r: Result<(), E>, fail: &mut bool) {
            if let Err(error) = r {
                Cli::command()
                    .error(clap::error::ErrorKind::InvalidValue, error)
                    .print()
                    .unwrap();
                *fail = true;
            }
        }

        if fail {
            Err(())
        } else {
            Ok((
                Options {
                    export_all: self.export_all,
                    inc_paths: self.inc_paths,
                    dependfile: self.dependfile,
                    output: self.output,
                    preinclude: self.preinclude,
                    inhibit_warnings: self.inhibit_warnings,
                    backtrace_depth: self.backtrace_depth,
                    max_errors: self.max_errors,
                    runtime_opts,
                    runtime_opt_stack: vec![],
                },
                self.input,
                self.defines,
            ))
        }
    }
}

impl RuntimeOptions {
    fn parse_chars<E: From<DynCharParseErr>, const N: usize>(
        arg: &str,
        target: &mut [char; N],
    ) -> Result<(), E> {
        let mut chars = arg.chars();
        let mut digits = [Default::default(); N];
        for (i, slot) in digits.iter_mut().enumerate() {
            *slot = chars.next().ok_or(DynCharParseErr::WrongCharCount(i))?;
            // Only allow printable ASCII characters, to avoid surprising behaviours.
            if !slot.is_ascii_graphic() {
                return Err(DynCharParseErr::BadChar(*slot).into());
            }
        }
        match chars.count() {
            0 => {}
            n => return Err(DynCharParseErr::WrongCharCount(N + n).into()),
        }
        *target = digits;
        Ok(())
    }
    pub fn parse_b(&mut self, arg: &str) -> Result<(), BinDigitsParseErr> {
        Self::parse_chars(arg, &mut self.binary_digits)
    }
    pub fn parse_g(&mut self, arg: &str) -> Result<(), GfxCharsParseErr> {
        Self::parse_chars(arg, &mut self.gfx_chars)
    }
}
#[derive(Debug)]
enum DynCharParseErr {
    WrongCharCount(usize),
    BadChar(char),
}
#[derive(Debug, Display)]
pub enum BinDigitsParseErr {
    /// The argument must be exactly two characters long, not {0}
    WrongCharCount(usize),
    /// Only printable ASCII characters are allowed, not '{0}'
    BadChar(char),
}
#[derive(Debug, Display)]
pub enum GfxCharsParseErr {
    /// The argument must be exactly four characters long, not {0}
    WrongCharCount(usize),
    /// Only printable ASCII characters are allowed, not '{0}'
    BadChar(char),
}
impl From<DynCharParseErr> for BinDigitsParseErr {
    fn from(value: DynCharParseErr) -> Self {
        match value {
            DynCharParseErr::WrongCharCount(n) => Self::WrongCharCount(n),
            DynCharParseErr::BadChar(c) => Self::BadChar(c),
        }
    }
}
impl From<DynCharParseErr> for GfxCharsParseErr {
    fn from(value: DynCharParseErr) -> Self {
        match value {
            DynCharParseErr::WrongCharCount(n) => Self::WrongCharCount(n),
            DynCharParseErr::BadChar(c) => Self::BadChar(c),
        }
    }
}

impl RuntimeOptions {
    pub fn parse_p(&mut self, arg: &str) -> Result<(), RecDepthParseErr> {
        self.pad_byte = crate::common::cli::parse_number(arg)?;
        Ok(())
    }
}

impl RuntimeOptions {
    pub fn parse_q(&mut self, arg: &str) -> Result<(), FixPrecParseErr> {
        match arg.strip_prefix('.').unwrap_or(arg).parse()? {
            precision @ 1..=31 => {
                self.q_precision = precision;
                Ok(())
            }
            n => Err(FixPrecParseErr::OutOfRange(n)),
        }
    }
}
#[derive(Debug, Display, From)]
pub enum FixPrecParseErr {
    /// {0}
    BadNum(std::num::ParseIntError),
    /// Fixed-point precision must be between 1 and 31, not {0}
    #[from(ignore)]
    OutOfRange(usize),
}

impl RuntimeOptions {
    pub fn parse_r(&mut self, arg: &str) -> Result<usize, RecDepthParseErr> {
        let new_depth = arg.parse()?;
        Ok(new_depth)
    }
}
#[derive(Debug, Display, From)]
pub enum RecDepthParseErr {
    /// {0}
    BadNum(std::num::ParseIntError),
}

impl RuntimeOptions {
    pub fn parse_w<'arg>(&mut self, arg: &'arg str) -> Result<(), WarningParseErr<'arg>> {
        if arg == "error" {
            // `-Werror` promotes warnings to errors.
            self.warnings_are_errors = true;
            return Ok(());
        } else if arg == "no-error" {
            // `-Wno-error` cancels the above.
            self.warnings_are_errors = false;
            return Ok(());
        }

        let (state, mut flag) = if let Some(suffix) = arg.strip_prefix("error=") {
            // `-Werror=<flag>` enables the flag as an error.
            (
                WarningState {
                    state: WarningLevel::Enabled,
                    error: WarningLevel::Enabled,
                },
                suffix,
            )
        } else if let Some(suffix) = arg.strip_prefix("no-error=") {
            // `-Wno-error=<flag>` prevents the flag from being an error,
            // without affecting whether it is enabled.
            (
                WarningState {
                    state: WarningLevel::Default,
                    error: WarningLevel::Disabled,
                },
                suffix,
            )
        } else if let Some(suffix) = arg.strip_prefix("no-") {
            // `-Wno-<flag>` disables the flag.
            (
                WarningState {
                    state: WarningLevel::Disabled,
                    error: WarningLevel::Default,
                },
                suffix,
            )
        } else {
            // `-W<flag>` enables the flag.
            (
                WarningState {
                    state: WarningLevel::Enabled,
                    error: WarningLevel::Default,
                },
                arg,
            )
        };

        // Check for an `=` parameter to process as a parametric warning.
        // `-Wno-<flag>` and `-Wno-error=<flag>` negation cannot have an `=` parameter, but without a
        // parameter, the 0 value will apply to all levels of a parametric warning.
        let param = if let Some((root_flag, param)) = (state.state == WarningLevel::Enabled)
            .then(|| flag.split_once('='))
            .flatten()
        {
            flag = root_flag;
            Some(param.parse()?)
        } else {
            None
        };

        // Try to match the flag against a parametric warning.
        for &(name, WarningKind(id), nb_levels) in &diagnostics::PARAMETRIC_WARNINGS {
            if flag == name {
                let level = match param {
                    None => 1, // TODO: allow specifying other defaults
                    Some(level) => {
                        if level > nb_levels {
                            return Err(WarningParseErr::ParamOutOfRange(name, level, nb_levels));
                        }
                        level
                    }
                };

                // Set the first <level> to enabled/error, and disable the rest.
                for ofs in 0..nb_levels {
                    let warning = &mut self.warnings[id + ofs];
                    if ofs < level {
                        warning.update(state);
                    } else {
                        warning.state = WarningLevel::Disabled;
                    }
                }
                return Ok(());
            }
        }

        // Try to match against a non-parametric warning, unless a parameter was given.
        if param.is_none() {
            // Try to match against a "meta" warning.
            for (i, &meta_warning) in META_WARNINGS.iter().enumerate() {
                if flag == meta_warning {
                    self.meta_warnings[i + 1].update(state);
                    return Ok(());
                }
            }

            // Try to match against a "simple" warning.
            for &(simple_warning, id) in &SIMPLE_WARNINGS {
                if flag == simple_warning {
                    self.warnings[id.0].update(state);
                    return Ok(());
                }
            }
        }

        Err(WarningParseErr::Unknown(arg))
    }
}
#[derive(Debug, Display, From)]
pub enum WarningParseErr<'arg> {
    /// Invalid warning level: {0}
    BadLevel(std::num::ParseIntError),
    /// {1} is too large a parameter for `-W{0}`, the maximum is {2}
    #[from(ignore)]
    ParamOutOfRange(&'static str, usize, usize),
    /// Unknown warning name `{0}`
    #[from(ignore)]
    Unknown(&'arg str),
}
