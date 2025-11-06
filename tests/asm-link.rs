use std::{ffi::OsStr, path::Path};

use anyhow::{anyhow, Context};
use datatest_stable::Utf8Path;
use snapbox::{cmd::Command, data::DataFormat, Assert, Data};
use tempfile::NamedTempFile;

datatest_stable::harness! {
    { test = rgbasm_rgblink, root = "tests/asm-link", pattern = r"/test\.asm$" },
    // Intentionally testing opening a file that doesn't exist.
    { test = rgbasm_notexist, root = "tests/asm-link", pattern = r"notexist/rgbasm\.err$" },
    { test = multi, root = "tests/asm-link/_multi", pattern = r"rgblink\.err" },
    { test = consistency, root = "tests/asm-link/_consistency", pattern = r"rgblink.err" },
    // TODO: `version.asm`, other special tests in `test.sh`
}

const RGBASM_PATH: &str = env!("CARGO_BIN_EXE_rgbasm");
const RGBLINK_PATH: &str = "/home/issotm/rgbds/rgblink"; // TODO: use rsgblink when it's complete

const ACTION_ENV_VAR_NAME: &str = "SNAPSHOTS_ASM_LINK";

// Game Boy release date, 1989-04-21T12:34:56Z (for reproducible test results)
const TIMESTAMP: &str = "609165296";

fn rgbasm_notexist(err_path: &Utf8Path) -> datatest_stable::Result<()> {
    rgbasm_rgblink(&err_path.with_file_name("test.asm"))
}

fn rgbasm_rgblink(asm_path: &Utf8Path) -> datatest_stable::Result<()> {
    // We need to pass the path to `rgbasm`, but we don't really care for security, so `NamedTempFile` is fine.
    let obj_file = NamedTempFile::new().context("Unable to create temp obj file")?;

    let working_directory = asm_path.parent().unwrap();

    let mut asm_cmd = command(RGBASM_PATH, working_directory, obj_file.path())?
        .arg(asm_path.file_name().unwrap());

    let flags_file_path = asm_path.with_file_name("rgbasm.flags");
    if flags_file_path.exists() {
        asm_cmd = asm_cmd.arg("@rgbasm.flags");
    }

    let result = asm_cmd.assert().with_assert(
        assert_cfg()
            // On its face, path normalisation is desirable;
            // however, it causes `snapbox` to replace ALL backslashes with slashes...
            // including in source code listings (e.g. macro args).
            // `datatest-stable` passes relative paths with forward slashes always, anyway.
            .normalize_paths(false),
    );

    let err_file_path = asm_path.with_file_name("rgbasm.err");
    let result = if err_file_path.exists() {
        // Note that the presence of a stderr log does not indicate failure is expected;
        // possibly the log contains only warnings.
        result.stderr_eq(
            Data::try_read_from(err_file_path.as_std_path(), None) // TODO: shouldn't this always be text?
                .context("Error reading rgbasm errput")?,
        )
    } else {
        result.success().stderr_eq("")
    };

    let out_file_path = asm_path.with_file_name("rgbasm.out");
    if out_file_path.exists() {
        result.stdout_eq(
            Data::try_read_from(out_file_path.as_std_path(), None)
                .context("Error reading rgbasm output")?,
        );
    } else {
        result.stdout_eq("");
    }

    let bin_file_path = asm_path.with_file_name("output.bin");
    if bin_file_path.exists() {
        let bin_file = NamedTempFile::new().context("Unable to create temp bin file")?;

        let mut cmd = command(RGBLINK_PATH, working_directory, bin_file.path())?
            .arg(obj_file.path())
            .arg("--nopad");

        let flags_file_path = asm_path.with_file_name("rgblink.flags");
        if flags_file_path.exists() {
            cmd = cmd.arg("@rgblink.flags");
        }

        let result = cmd.assert().with_assert(assert_cfg()).success(); // If failure is expected, remove `output.bin`.
        let err_file_path = asm_path.with_file_name("rgblink.err");
        let result = if err_file_path.exists() {
            // Note that the presence of a stderr log does not indicate failure is expected;
            // possibly the log contains only warnings.
            result.stderr_eq(
                Data::try_read_from(err_file_path.as_std_path(), None)
                    .context("Error reading rgblink errput")?,
            )
        } else {
            result.stderr_eq("")
        };

        let out_file_path = asm_path.with_file_name("rgblink.out");
        if out_file_path.exists() {
            result.stdout_eq(
                Data::try_read_from(out_file_path.as_std_path(), None)
                    .context("Error reading rgblink output")?,
            );
        } else {
            result.stdout_eq("");
        }

        let generated = std::fs::read(bin_file).context("Unable to read temp bin file")?;
        let reference = Data::try_read_from(bin_file_path.as_std_path(), Some(DataFormat::Binary))
            .context("Error reading binary")?;
        assert_cfg()
            .try_eq(None, Data::binary(generated.as_slice()), reference.clone())
            .map_err(|_err| BinDiff {
                expected: reference.to_bytes().unwrap(),
                actual: generated,
                path: bin_file_path,
            })?;
    } else {
        let err_file_path = asm_path.with_file_name("rgblink.err");
        if err_file_path.exists() {
            let bin_file = NamedTempFile::new().context("Unable to create temp bin file")?;
            command(RGBLINK_PATH, working_directory, bin_file.path())?
                .arg(obj_file.path())
                .assert()
                .with_assert(assert_cfg())
                .stdout_eq("")
                .stderr_eq(
                    Data::try_read_from(err_file_path.as_std_path(), Some(DataFormat::Text))
                        .context("Unable to read default linker errput")?,
                );
        }
    }

    for flag in ["-d", "-t"] {
        let mut path = asm_path.with_file_name(flag);
        path.set_extension("err");

        if path.exists() {
            let bin_file = NamedTempFile::new().context("Unable to create temp bin file")?;
            command(RGBLINK_PATH, working_directory, bin_file.path())?
                .arg(obj_file.path())
                .arg(flag)
                .assert()
                .with_assert(assert_cfg())
                .stdout_eq("")
                .stderr_eq(
                    Data::try_read_from(path.as_std_path(), Some(DataFormat::Text))
                        .context("Error reading expected errput")?,
                );
        }
    }

    for entry in std::fs::read_dir(working_directory).context("Unable to scan working directory")? {
        let entry = entry.context("Error while scanning working directory")?;
        let file_name = entry.file_name();

        if Path::new(&file_name).extension() == Some(OsStr::new("link")) {
            let bin_file = NamedTempFile::new().context("Unable to create temp bin file")?;
            let err_file_path = Path::new(&entry.path()).with_extension("err");
            command(RGBLINK_PATH, working_directory, bin_file.path())?
                .arg(obj_file.path())
                .arg("--linkerscript")
                .arg(file_name)
                .assert()
                .with_assert(assert_cfg())
                .stdout_eq("")
                .stderr_eq(
                    Data::try_read_from(&err_file_path, Some(DataFormat::Text))
                        .context("Error reading linkerscript errput")?,
                );
        }
    }

    Ok(())
}

fn multi(err_path: &Utf8Path) -> datatest_stable::Result<()> {
    let dir = err_path.parent().unwrap();

    let obj_files = std::fs::read_dir(dir)
        .context("Unable to scan directory")?
        .filter_map(|entry| {
            let entry = match entry {
                Ok(entry) => entry,
                Err(err) => return Some(Err(err.into())),
            };
            let file_name = entry.file_name();
            if Path::new(&file_name).extension() != Some(OsStr::new("asm")){
                None
            } else if file_name == OsStr::new("test.asm") {
                Some(Err(anyhow!("`test.asm` is not allowed in `_multi` tests, as this'd overlap with regular tests")))
            } else {
                Some(NamedTempFile::new().context("Error creating temp obj file").and_then(|obj_file| {
                    let mut cmd = command(RGBASM_PATH, dir, obj_file.path())?
                        .arg(&file_name);
                    let asm_flags_path = err_path.with_file_name("rgbasm.flags");
                    if asm_flags_path.exists() {
                        cmd = cmd.arg("@rgbasm.flags");
                    }
                    cmd
                        .assert()
                        .with_assert(assert_cfg())
                        .success()
                        .stdout_eq("")
                        .stderr_eq("");
                    Ok(obj_file)
                }))
            }
        })
        .collect::<Result<Vec<_>, _>>()?;

    let bin_file = NamedTempFile::new().context("Error creating temp bin file")?;
    let sym_file = NamedTempFile::new().context("Error creating temp sym file")?;
    let map_file = NamedTempFile::new().context("Error creating temp map file")?;
    command(RGBLINK_PATH, dir, bin_file.path())?
        .args(obj_files.iter().map(|obj| obj.path()))
        .arg("--map")
        .arg(map_file.path())
        .arg("--sym")
        .arg(sym_file.path())
        .arg("--nopad")
        .assert()
        .with_assert(assert_cfg())
        .stdout_eq("")
        .stderr_eq(
            Data::try_read_from(err_path.as_std_path(), Some(DataFormat::Text))
                .context("Error reading expected linker errput")?,
        );

    let mut path = err_path.with_file_name("out.bin");
    let generated = std::fs::read(bin_file).context("Error reading generated bin file")?;
    let reference = Data::try_read_from(path.as_std_path(), Some(DataFormat::Binary))
        .context("Error reading expected bin data")?;
    if assert_cfg()
        .try_eq(None, Data::binary(generated.as_slice()), reference.clone())
        .is_err()
    {
        return Err(BinDiff {
            expected: reference.to_bytes().unwrap(),
            actual: generated,
            path,
        }
        .into());
    }
    for (extension, output) in [("sym", &sym_file), ("map", &map_file)] {
        path.set_extension(extension);

        if path.exists() {
            let expected = Data::try_read_from(path.as_std_path(), Some(DataFormat::Text))
                .context("Error reading expected data")?;
            let actual =
                std::fs::read_to_string(output.path()).context("Error reading generated file")?;
            assert_cfg().eq(Data::text(actual), expected)
        }
    }

    Ok(())
}

fn consistency(link_err_path: &Utf8Path) -> datatest_stable::Result<()> {
    let dir = link_err_path.parent().unwrap();
    let tmp1 = NamedTempFile::new().context("Error creating temp file")?;
    let tmp2 = NamedTempFile::new().context("Error creating temp file")?;

    command(RGBASM_PATH, dir, tmp1.path())?
        .arg("one-two.asm")
        .assert()
        .with_assert(assert_cfg())
        .success()
        .stdout_eq("")
        .stderr_eq("");
    command(RGBASM_PATH, dir, tmp2.path())?
        .arg("one-two.asm")
        .arg("-DSECOND")
        .assert()
        .with_assert(assert_cfg())
        .success()
        .stdout_eq("")
        .stderr_eq("");
    command(RGBLINK_PATH, dir, "".as_ref())?
        .args([tmp1.path(), tmp2.path()])
        .assert()
        .with_assert(assert_cfg())
        .failure()
        .stdout_eq("")
        .stderr_eq(
            Data::try_read_from(link_err_path.as_std_path(), Some(DataFormat::Text))
                .context("Error reading expected linker errput")?,
        );

    let asm_err_path = link_err_path.with_file_name("rgbasm.err");
    std::fs::write(tmp1.path(), "def SECOND equs \"1\"").context("Failed to write SECOND stub")?;
    command(RGBASM_PATH, dir, "".as_ref())?
        .args(["--preinclude", "one-two.asm", "--preinclude"])
        .arg(tmp1.path()) // Define `SECOND` for the second pass.
        .arg("one-two.asm")
        .assert()
        .with_assert(assert_cfg())
        .failure()
        .stdout_eq("")
        .stderr_eq(
            Data::try_read_from(asm_err_path.as_std_path(), Some(DataFormat::Text))
                .context("Error reading expected assembler errput")?,
        );

    Ok(())
}

/* Utilities. */

fn command(
    cmd_path: &str,
    cwd: &Utf8Path,
    output_path: &Path,
) -> anyhow::Result<snapbox::cmd::Command> {
    Ok(Command::new(cmd_path)
        .current_dir(
            cwd.canonicalize()
                .context("Unable to canonicalise working directory")?,
        )
        .env("SOURCE_DATE_EPOCH", TIMESTAMP)
        .arg("-o")
        .arg(output_path)
        .arg("-Weverything"))
}
fn assert_cfg() -> Assert {
    Assert::new().action_env(ACTION_ENV_VAR_NAME)
}

struct BinDiff<D> {
    expected: Vec<u8>,
    actual: Vec<u8>,
    path: D,
}
impl<D: std::fmt::Display> std::fmt::Debug for BinDiff<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let palette = snapbox::report::Palette::color();

        const NB_BYTES_PER_LINE: usize = 8;

        writeln!(
            f,
            "{}                  | {} ({})",
            palette.actual("actual"),
            palette.expected("expected"),
            self.path,
        )?;

        let mut expected = self.expected.iter();
        let mut actual = self.actual.iter();
        while !expected.as_slice().is_empty() || !actual.as_slice().is_empty() {
            let matching: [_; NB_BYTES_PER_LINE] =
                std::array::from_fn(|i| expected.as_slice().get(i) == actual.as_slice().get(i));
            for matches in matching {
                match actual.next() {
                    Some(&byte) => {
                        if matches {
                            write!(f, "{byte:02x} ")?;
                        } else {
                            write!(f, "{} ", palette.actual(format_args!("{byte:02x}")))?;
                        }
                    }
                    None => write!(f, "   ")?,
                }
            }
            write!(f, "|")?;
            for (&byte, matches) in expected.by_ref().take(NB_BYTES_PER_LINE).zip(matching) {
                if matches {
                    write!(f, " {byte:02x}")?;
                } else {
                    write!(f, " {}", palette.expected(format_args!("{byte:02x}")))?;
                }
            }
            writeln!(f)?;
        }

        writeln!(
            f,
            "\n{}",
            palette.hint(format_args!("Update with {ACTION_ENV_VAR_NAME}=overwrite")),
        )
    }
}

impl<D: std::fmt::Display> std::fmt::Display for BinDiff<D> {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl<D: std::fmt::Display> std::error::Error for BinDiff<D> {}
