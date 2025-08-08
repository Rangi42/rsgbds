use std::fmt::Display;

use anyhow::Context;
use datatest_stable::Utf8Path;
use snapbox::{cmd::Command, data::DataFormat, Assert, Data};
use tempfile::NamedTempFile;

datatest_stable::harness! {
    { test = run_test, root = "tests/rgbasm", pattern = r"\.asm$" },
    // TODO: `version.asm`, `notexist.asn`, other special tests in `test.sh`
}

const RGBASM_PATH: &str = env!("CARGO_BIN_EXE_rgbasm");
const RGBLINK_PATH: &str = "rgblink";

const ACTION_ENV_VAR_NAME: &str = "SNAPSHOTS_ASM";

fn run_test(asm_path: &Utf8Path) -> datatest_stable::Result<()> {
    // We need to pass the path to `rgbasm`, but we don't really care for security.
    let obj_file = NamedTempFile::new().context("Unable to create temp obj file")?;

    let mut cmd = Command::new(RGBASM_PATH)
        .arg(asm_path)
        .arg("-o")
        .arg(obj_file.path())
        .arg("-Weverything");

    let flags_file_path = asm_path.with_file_name("test.flags");
    if flags_file_path.exists() {
        cmd = cmd.arg(format!("@{flags_file_path}"));
    }

    let result = cmd
        .assert()
        .with_assert(Assert::new().action_env(ACTION_ENV_VAR_NAME));

    let err_file_path = asm_path.with_file_name("stderr.log");
    let result = if err_file_path.exists() {
        result.failure().stderr_eq(
            Data::try_read_from(err_file_path.as_std_path(), Some(DataFormat::Text))
                .context("Error reading errput")?,
        )
    } else {
        result.success()
    };

    let out_file_path = asm_path.with_file_name("stdout.log");
    if out_file_path.exists() {
        result.stdout_eq(
            Data::try_read_from(out_file_path.as_std_path(), Some(DataFormat::Text))
                .context("Error reading output")?,
        );
    } else {
        result.stdout_eq([].as_slice());
    }

    let bin_file_path = asm_path.with_file_name("output.bin");
    if bin_file_path.exists() {
        let bin_file = NamedTempFile::new().context("Unable to create temp bin file")?;

        Command::new(RGBLINK_PATH)
            .arg(obj_file.path())
            .arg("-o")
            .arg(bin_file.path())
            .arg("-x")
            //.arg("-Weverything")
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
            return Err(BinDiff(reference.to_bytes().unwrap(), generated).into());
        }
    }

    Ok(())
}

#[derive(Debug)]
struct BinDiff(Vec<u8>, Vec<u8>);

impl Display for BinDiff {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl std::error::Error for BinDiff {}
