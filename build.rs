/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use std::{
    fs::File,
    io::{BufRead, BufReader, Write},
    path::{Path, PathBuf},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    #[cfg(any(
        feature = "rgbasm",
        feature = "rgblink",
        feature = "rgbfix",
        feature = "rgbgfx"
    ))]
    shadow_rs::ShadowBuilder::builder().build()?;

    #[cfg(feature = "rgbasm")]
    generate_warnings_mod();

    #[cfg(feature = "rgbasm")]
    lalrpop::Configuration::new()
        .emit_rerun_directives(true)
        .use_cargo_dir_conventions()
        //.emit_whitespace(false)
        .process_file("src/asm/syntax/parser.lalrpop")?;

    Ok(())
}

#[derive(Debug)]
struct Warning {
    name: String,
    kind: WarningKind,
}

#[derive(Debug)]
enum WarningKind {
    Boolean {
        meta_level: u8,
    },
    Parametric {
        meta_levels: Vec<u8>,
        default_level: u8,
    },
}

const META_WARNINGS: [&str; 3] = ["all", "extra", "everything"];
fn meta_warning_level(name: &str) -> u8 {
    match name {
        "all" => 1,
        "extra" => 2,
        "everything" => panic!("Please use `default = false` instead of `meta = everything`"),
        _ => panic!("Invalid meta warning name \"{name}\""),
    }
}

fn generate_warnings_mod() {
    let warnings = parse_all_warnings();

    let mut out_path = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    out_path.push("warnings.rs");
    let mut file = File::create(out_path).expect("Failed to create `warnings.rs`");

    writeln!(
        &mut file,
        "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WarningKind(#[doc(hidden)] pub usize);
pub const NB_WARNINGS: usize = {};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MetaWarningKind(#[doc(hidden)] pub usize);

macro_rules! warning {{",
        warnings.iter().fold(0, |acc, warning| acc
            + match &warning.kind {
                WarningKind::Boolean { .. } => 1,
                WarningKind::Parametric { meta_levels, .. } => meta_levels.len(),
            }),
    )
    .unwrap();
    let mut i = 0;
    for warning in &warnings {
        match &warning.kind {
            WarningKind::Boolean { .. } => {
                writeln!(
                    &mut file,
                    "    (\"{}\") => {{ $crate::diagnostics::WarningKind({i}) }};",
                    &warning.name
                )
                .unwrap();
                i += 1;
            }
            WarningKind::Parametric { meta_levels, .. } => {
                for level in 0..meta_levels.len() {
                    writeln!(
                        &mut file,
                        "    (\"{}={}\") => {{ $crate::diagnostics::WarningKind({i}) }};",
                        &warning.name,
                        level + 1,
                    )
                    .unwrap();
                    i += 1;
                }
            }
        }
    }
    writeln!(
        &mut file,
        "}}
pub(crate) use warning;

const DEFAULT_WARNINGS: [MetaWarningKind; NB_WARNINGS] = ["
    )
    .unwrap();
    for warning in &warnings {
        match &warning.kind {
            WarningKind::Boolean { meta_level } => {
                writeln!(
                    &mut file,
                    "    MetaWarningKind({meta_level}), // {}",
                    warning.name
                )
                .unwrap();
            }
            WarningKind::Parametric { meta_levels, .. } => {
                for (level, meta_level) in meta_levels.iter().enumerate() {
                    writeln!(
                        &mut file,
                        "    MetaWarningKind({meta_level}), // {}={}",
                        warning.name,
                        level + 1
                    )
                    .unwrap();
                }
            }
        }
    }
    writeln!(
        &mut file,
        "];

pub(crate) const SIMPLE_WARNINGS: [(&str, WarningKind); NB_SIMPLE_WARNINGS] = ["
    )
    .unwrap();
    let mut nb_simple_warnings = 0;
    for warning in &warnings {
        if let WarningKind::Boolean { .. } = &warning.kind {
            writeln!(
                &mut file,
                "\t(\"{}\", warning!(\"{}\")),",
                warning.name, warning.name,
            )
            .unwrap();
            nb_simple_warnings += 1;
        }
    }

    writeln!(
        &mut file,
        "];
const NB_SIMPLE_WARNINGS: usize = {nb_simple_warnings};

pub(crate) const PARAMETRIC_WARNINGS: [(&str, WarningKind, usize, usize); NB_PARAMETRIC_WARNINGS] = ["
    )
    .unwrap();
    let mut nb_parametric_warnings = 0;
    for warning in &warnings {
        if let WarningKind::Parametric {
            meta_levels,
            default_level,
        } = &warning.kind
        {
            writeln!(
                &mut file,
                "\t(\"{}\", warning!(\"{}=1\"), {}, {default_level}),",
                warning.name,
                warning.name,
                meta_levels.len(),
            )
            .unwrap();
            nb_parametric_warnings += 1;
        }
    }

    writeln!(
        &mut file,
        "];
const NB_PARAMETRIC_WARNINGS: usize = {nb_parametric_warnings};

pub(crate) const NB_META_WARNINGS: usize = {};
pub(crate) const META_WARNINGS: [&str; NB_META_WARNINGS] = [",
        META_WARNINGS.len(),
    )
    .unwrap();
    for &name in &META_WARNINGS {
        writeln!(&mut file, "\t\"{name}\",").unwrap();
    }
    writeln!(
        &mut file,
        "];

impl std::fmt::Display for WarningKind {{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
        f.write_str(match *self {{"
    )
    .unwrap();
    let mut i = 0;
    for warning in &warnings {
        match &warning.kind {
            WarningKind::Boolean { .. } => {
                writeln!(
                    &mut file,
                    "            WarningKind({i}) => \"-W{}\",",
                    &warning.name
                )
                .unwrap();
                i += 1;
            }
            WarningKind::Parametric { meta_levels, .. } => {
                for level in 0..meta_levels.len() {
                    writeln!(
                        &mut file,
                        "            WarningKind({i}) => \"-W{}={}\",",
                        &warning.name,
                        level + 1
                    )
                    .unwrap();
                    i += 1;
                }
            }
        }
    }
    writeln!(
        &mut file,
        "            WarningKind(_) => unreachable!(),
        }})
    }}
}}"
    )
    .unwrap();
}

fn parse_all_warnings() -> Vec<Warning> {
    let mut warnings = Vec::new();

    let mut line_buf = String::new();
    fn read_first_line<'line>(path: &Path, buf: &'line mut String) -> &'line str {
        buf.clear();
        BufReader::new(
            File::open(path)
                .unwrap_or_else(|err| panic!("Failed to open {}: {err}", path.display())),
        )
        .read_line(buf)
        .unwrap_or_else(|err| panic!("Failed to read the first line of {}: {err}", path.display()));
        buf.strip_prefix(".\\\"").unwrap_or_else(|| {
            panic!(
                "`{}` must start with a mdoc comment line (`.\\\"`)",
                path.display()
            )
        })
    }

    fn parse_directive(directive: &str) -> (&str, &str) {
        let (name, value) = directive.split_once('=').unwrap_or_else(|| {
            panic!("Directive `{directive}` should be formatted as `<name> = <value>`")
        });
        (
            name.trim_matches(|c: char| c.is_ascii_whitespace()),
            value.trim_matches(|c: char| c.is_ascii_whitespace()),
        )
    }

    println!("cargo:rerun-if-changed=src/asm/warnings");
    for warning in std::fs::read_dir("src/asm/warnings").expect("Failed to list `src/asm/warnings`")
    {
        let warning = warning.expect("Error while listing `src/asm/warnings`");
        let path = warning.path();

        let (name, kind) = if warning
            .file_type()
            .expect("Failed to get file type")
            .is_dir()
        {
            // This is a parametric warning.
            let mut meta_levels = vec![];
            let mut default_level = None;

            for f in std::fs::read_dir(&path).expect("Failed to list parametric warning directory")
            {
                let f = f.expect("Error while listing parametric warning directory");
                let name = f.file_name();

                if name == "descr.mdoc" {
                    let line = read_first_line(&f.path(), &mut line_buf);

                    let (name, value) = parse_directive(line);
                    match name {
                        "default-level" => {
                            default_level =
                                Some(value.parse().expect("Bad value for `default-level`"))
                        }
                        _ => panic!("Unknown descr.mdoc directive `{name}`"),
                    }
                } else {
                    // Parse the file name; it should be `<level>.mdoc`.
                    let level = name
                        .to_str()
                        .and_then(|s| s.strip_suffix(".mdoc"))
                        .unwrap_or_else(|| panic!(
                            "Files in parametric warning directories should be named e.g. `1.mdoc`; `{}` is not",
                            f.path().display(),
                        ))
                        .parse()
                        .unwrap_or_else(|err| panic!("Invalid file name for {}: {err}", f.path().display()));
                    if level == 0 {
                        panic!(
                            "Files in parametric warning directories cannot be named `0.mdoc` ({})",
                            f.path().display()
                        );
                    } else {
                        let line = read_first_line(&f.path(), &mut line_buf);

                        if meta_levels.len() < level {
                            meta_levels.resize(level, u8::MAX);
                        }
                        for directive in line.split(';') {
                            let (name, value) = parse_directive(directive);
                            match name {
                                "default" => {
                                    if meta_levels[level - 1] != u8::MAX {
                                        panic!(
                                            "{}: default status specified twice",
                                            f.path().display()
                                        );
                                    }
                                    meta_levels[level - 1] =
                                        if value.parse().unwrap_or_else(|err| {
                                            panic!(
                                                "{}: Bad value for `default` directive: {err}",
                                                f.path().display()
                                            )
                                        }) {
                                            0
                                        } else {
                                            META_WARNINGS.len() as u8
                                        }
                                }
                                "meta" => {
                                    if meta_levels[level - 1] != u8::MAX {
                                        panic!(
                                            "{}: default status specified twice",
                                            f.path().display()
                                        );
                                    }
                                    meta_levels[level - 1] = meta_warning_level(value);
                                }
                                _ => panic!("Unknown directive `{name}`"),
                            }
                        }
                    }
                }
            }

            if meta_levels.len() < 2 {
                panic!(
                    "Please create at least `1.mdoc` and `2.mdoc` in `{}`",
                    path.display()
                );
            }
            for (i, slot) in meta_levels.iter().enumerate() {
                if *slot == u8::MAX {
                    panic!("{}/{i}.mdoc is missing", path.display());
                }
            }
            // TODO: sanity checks, like all enabled levels being contiguous (and, generally, the specificity increasing)

            (
                warning.file_name().to_string_lossy().into(),
                WarningKind::Parametric {
                    meta_levels,
                    default_level: default_level.expect("Missing descr.mdoc"),
                },
            )
        } else {
            // This is a simple warning.
            let name = warning
                .file_name()
                .to_string_lossy()
                .strip_suffix(".mdoc")
                .expect("Warning files must have the `.mdoc` extension")
                .into();

            let line = read_first_line(&path, &mut line_buf);
            let mut meta_level = None;

            for directive in line.split(';') {
                let (name, value) = parse_directive(directive);
                match name {
                    "default" => {
                        if meta_level
                            .replace(
                                if value.parse().unwrap_or_else(|err| {
                                    panic!("Bad value for `default` directive: {err}")
                                }) {
                                    0
                                } else {
                                    META_WARNINGS.len() as u8
                                },
                            )
                            .is_some()
                        {
                            panic!("`default` directive specified twice");
                        }
                    }
                    "meta" => {
                        if meta_level.replace(meta_warning_level(value)).is_some() {
                            panic!("`meta` directive specified twice");
                        }
                    }
                    _ => panic!("Unknown directive `{name}`"),
                }
            }

            let meta_level = meta_level.expect("Missing `default` or `meta` directive");
            (name, WarningKind::Boolean { meta_level })
        };

        warnings.push(Warning { name, kind });
    }

    warnings
}
