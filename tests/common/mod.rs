// Common utilities for integration tests

use yuu::pass_c_compilation::pass_c_compilation_impl::CExecutable;
use yuu::pass_parse::AST;
use yuu::utils::pipeline::*;

/// Helper function to run the full YIR pipeline and return YIR output
pub fn run_to_yir(source: &str, filename: &str) -> Result<String, Box<dyn std::error::Error>> {
    let yir_output =
        Pipeline::new(source.to_string(), filename.to_string())?.print_yir_colored()?;
    Ok(yir_output.0)
}

/// Helper function to run the full C compilation pipeline
pub fn run_to_c(source: &str, filename: &str) -> Result<String, Box<dyn std::error::Error>> {
    let mut pipeline = Pipeline::new(source.to_string(), filename.to_string())?;
    Ok(pipeline.get_c_code()?.0.clone())
}

/// Helper function to run the full compilation pipeline to executable
pub fn run_to_executable(
    source: &str,
    filename: &str,
) -> Result<CExecutable, Box<dyn std::error::Error>> {
    let mut pipeline = Pipeline::new(source.to_string(), filename.to_string())?;
    Ok(pipeline.compile_executable()?)
}

/// Helper function to run just parsing (without diagnostics)
pub fn run_parse_only(source: &str, filename: &str) -> Result<AST, Box<dyn std::error::Error>> {
    let out = Pipeline::new(source.to_string(), filename.to_string())?;

    Ok(out.ast.unwrap())
}

/// Helper function to run executable and capture output
pub fn run_executable_with_output(executable: &CExecutable, args: &[&str]) -> miette::Result<i32> {
    let output = executable.execute(args)?;
    Ok(output.status.code().unwrap())
}
