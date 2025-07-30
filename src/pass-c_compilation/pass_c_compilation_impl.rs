use crate::pass_c_lowering::pass_yir_to_c::CSourceCode;
use miette::{IntoDiagnostic, Result};
use std::fs;
use std::path::Path;
use std::process::Command;
use tempfile::NamedTempFile;

pub struct CCompilation;

impl Default for CCompilation {
    fn default() -> Self {
        Self
    }
}

impl CCompilation {
    pub fn new() -> Self {
        Self
    }
}

pub struct CExecutable {
    pub path: String,
    pub temp_file: Option<NamedTempFile>,
}

impl CExecutable {
    pub fn execute(&self, args: &[&str]) -> Result<std::process::Output> {
        Command::new(&self.path)
            .args(args)
            .output()
            .into_diagnostic()
    }
}

impl Drop for CExecutable {
    fn drop(&mut self) {
        // Clean up the executable file
        if let Err(e) = fs::remove_file(&self.path) {
            eprintln!(
                "Warning: Failed to clean up executable {}: {}",
                self.path, e
            );
        }
    }
}

impl CCompilation {
    pub fn compile_c_code(&self, c_code: &CSourceCode) -> Result<CExecutable> {
        // Create temporary C source file
        let c_file = NamedTempFile::with_suffix(".c").into_diagnostic()?;
        fs::write(c_file.path(), &c_code.0).into_diagnostic()?;

        // Create temporary executable file
        let exe_file = NamedTempFile::new().into_diagnostic()?;
        let exe_path = exe_file.path().to_string_lossy().to_string();

        // Try clang first, then gcc
        let compile_result = self
            .try_compile_with_clang(c_file.path(), &exe_path)
            .or_else(|_| self.try_compile_with_gcc(c_file.path(), &exe_path));

        match compile_result {
            Ok(_) => Ok(CExecutable {
                path: exe_path,
                temp_file: Some(exe_file),
            }),
            Err(e) => Err(e),
        }
    }

    fn try_compile_with_clang(&self, c_file: &Path, exe_path: &str) -> Result<()> {
        let output = Command::new("clang")
            .arg("-o")
            .arg(exe_path)
            .arg(c_file)
            .arg("-lm") // Link math library
            .output()
            .into_diagnostic()?;

        if !output.status.success() {
            return Err(miette::miette!(
                "Clang compilation failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ));
        }

        Ok(())
    }

    fn try_compile_with_gcc(&self, c_file: &Path, exe_path: &str) -> Result<()> {
        let output = Command::new("gcc")
            .arg("-o")
            .arg(exe_path)
            .arg(c_file)
            .arg("-lm") // Link math library
            .output()
            .into_diagnostic()?;

        if !output.status.success() {
            return Err(miette::miette!(
                "GCC compilation failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ));
        }

        Ok(())
    }
}
