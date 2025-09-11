use anyhow::Context;
use datatest_stable::Utf8Path;
use snapbox::{cmd::Command, data::DataFormat, Assert, Data};
use tempfile::NamedTempFile;

datatest_stable::harness! {
    { test = rgbasm_rgblink, root = "tests/asm-link", pattern = r"/test\.asm$" },
    // Intentionally testing opening a file that doesn't exist.
    { test = rgbasm_notexist, root = "tests/asm-link", pattern = r"notexist/stderr\.log$" },
    // TODO: `version.asm`, other special tests in `test.sh`
}

const RGBASM_PATH: &str = env!("CARGO_BIN_EXE_rgbasm");
const RGBLINK_PATH: &str = "../../../../rgbds/rgblink"; // TODO: use rsgblink when it's complete

const ACTION_ENV_VAR_NAME: &str = "SNAPSHOTS_ASM_LINK";

// Game Boy release date, 1989-04-21T12:34:56Z (for reproducible test results)
const TIMESTAMP: &str = "609165296";

fn rgbasm_notexist(err_path: &Utf8Path) -> datatest_stable::Result<()> {
    rgbasm_rgblink(&err_path.with_file_name("test.asm"))
}

fn rgbasm_rgblink(asm_path: &Utf8Path) -> datatest_stable::Result<()> {
    // We need to pass the path to `rgbasm`, but we don't really care for security.
    let obj_file = NamedTempFile::new().context("Unable to create temp obj file")?;

    let mut cmd = Command::new(RGBASM_PATH)
        .current_dir(
            asm_path
                .parent()
                .unwrap()
                .canonicalize()
                .context("Unable to canonicalise working directory")?,
        )
        .env("SOURCE_DATE_EPOCH", TIMESTAMP)
        .arg(asm_path.file_name().unwrap())
        .arg("-o")
        .arg(obj_file.path())
        .arg("-Weverything");

    let flags_file_path = asm_path.with_file_name("flags.txt");
    if flags_file_path.exists() {
        cmd = cmd.arg("@flags.txt");
    }

    let result = cmd.assert().with_assert(
        Assert::new()
            .action_env(ACTION_ENV_VAR_NAME)
            // On its face, path normalisation is desirable;
            // however, it causes `snapbox` to replace ALL backslashes with slashes...
            // including in source code listings (e.g. macro args).
            // `datatest-stable` passes relative path with forward slashes always, anyway.
            .normalize_paths(false),
    );

    let err_file_path = asm_path.with_file_name("stderr.log");
    let result = if err_file_path.exists() {
        // Note that the presence of a stderr log does not indicate failure is expected;
        // possibly the log contains only warnings.
        result.stderr_eq(
            Data::try_read_from(err_file_path.as_std_path(), None)
                .context("Error reading errput")?,
        )
    } else {
        result.success().stderr_eq([].as_slice())
    };

    let out_file_path = asm_path.with_file_name("stdout.log");
    if out_file_path.exists() {
        result.stdout_eq(
            Data::try_read_from(out_file_path.as_std_path(), None)
                .context("Error reading output")?,
        );
    } else {
        result.stdout_eq([].as_slice());
    }

    let bin_file_path = asm_path.with_file_name("output.bin");
    if bin_file_path.exists() {
        let bin_file = NamedTempFile::new().context("Unable to create temp bin file")?;

        Command::new(RGBLINK_PATH)
            .current_dir(
                asm_path
                    .parent()
                    .unwrap()
                    .canonicalize()
                    .context("Unable to canonicalise working directory")?,
            )
            .env("SOURCE_DATE_EPOCH", TIMESTAMP)
            .arg(obj_file.path())
            .arg("-o")
            .arg(bin_file.path())
            .arg("-x")
            //.arg("-Weverything") TODO
            .assert()
            .success()
            .stdout_eq([].as_slice())
            .stderr_eq([].as_slice());

        let generated = std::fs::read(bin_file).context("Unable to read temp bin file")?;
        let reference = Data::try_read_from(bin_file_path.as_std_path(), Some(DataFormat::Binary))
            .context("Error reading binary")?;
        if Assert::new()
            .action_env("SNAPSHOTS_ASM")
            .try_eq(None, Data::binary(generated.as_slice()), reference.clone())
            .is_err()
        {
            return Err(BinDiff {
                expected: reference.to_bytes().unwrap(),
                actual: generated,
                path: bin_file_path,
            }
            .into());
        }
    }

    Ok(())
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
            for _ in 0..NB_BYTES_PER_LINE {
                match actual.next() {
                    Some(&byte) => write!(f, "{byte:02x} ")?,
                    None => write!(f, "   ")?,
                }
            }
            write!(f, "|")?;
            for &byte in expected.by_ref().take(NB_BYTES_PER_LINE) {
                write!(f, " {byte:02x}")?;
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
