use crate::pass_c_lowering::pass_yir_to_c::CSourceCode;
use miette::{IntoDiagnostic, Result};
use std::fs;
use std::path::Path;
use std::process::Command;

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
}

impl CExecutable {
    pub fn execute(&self, args: &[&str]) -> Result<std::process::Output> {
        Command::new(&self.path)
            .args(args)
            .output()
            .into_diagnostic()
    }
}

impl CCompilation {
    pub fn compile_c_code(
        &self,
        c_code: &CSourceCode,
        base_path: impl AsRef<Path>,
        filename: &str,
    ) -> Result<CExecutable> {
        let base = base_path.as_ref();
        // build is the single build directory; files inside are named
        // <filename>.c and <filename>(.exe)
        let build_dir = base.join("build").join("C");
        fs::create_dir_all(&build_dir).into_diagnostic()?;

        // write C source to build/C/<filename>.c (overwrite if exists)
        let c_path = build_dir.join(format!("{}.c", filename));
        fs::write(&c_path, &c_code.0).into_diagnostic()?;

        // executable path: build/C/<filename> (with .exe on Windows)
        let mut exe_path_buf = build_dir.join(filename);
        if cfg!(windows) {
            exe_path_buf.set_extension("exe");
        }
        let exe_path = exe_path_buf.to_string_lossy().to_string();

        // Try clang first, then gcc
        let compile_result = self
            .try_compile_with_clang(&c_path, &exe_path)
            .or_else(|_| self.try_compile_with_gcc(&c_path, &exe_path));

        match compile_result {
            Ok(_) => Ok(CExecutable { path: exe_path }),
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
                "Clang compilation failed",
                //String::from_utf8_lossy(&output.stderr)
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
                "GCC compilation failed",
                //String::from_utf8_lossy(&output.stderr)
            ));
        }

        Ok(())
    }
}
