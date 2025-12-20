// Common utilities for integration tests

use yuu::pass_c_compilation::pass_c_compilation_impl::CExecutable;
use yuu::pass_parse::AST;
use yuu::utils::pipeline::*;

/// Helper function to run the full YIR pipeline and return YIR output
pub fn run_to_yir(source: &str, filename: &str) -> Result<String, Box<dyn std::error::Error>> {
    let yir_output = Pipeline::new(source.to_string(), filename.to_string()).calc_yir_colored()?;
    Ok(yir_output.0)
}

/// Helper function to run the full C compilation pipeline
pub fn run_to_c(source: &str, filename: &str) -> Result<String, Box<dyn std::error::Error>> {
    let mut pipeline = Pipeline::new(source.to_string(), filename.to_string());
    Ok(pipeline.calc_c()?.0.clone())
}

/// Helper function to run the full compilation pipeline to executable
pub fn run_to_executable(
    source: &str,
    filename: &str,
) -> Result<CExecutable, Box<dyn std::error::Error>> {
    let mut pipeline = Pipeline::new(source.to_string(), filename.to_string());
    Ok(pipeline.calc_executable()?)
}

/// Helper function to run just parsing (without diagnostics)
pub fn run_parse_only(source: &str, filename: &str) -> Result<AST, Box<dyn std::error::Error>> {
    let mut pipeline = Pipeline::new(source.to_string(), filename.to_string());
    pipeline.calc_ast()?;

    Ok(pipeline.ast.unwrap())
}

/// Helper function to run executable and capture output
pub fn run_executable_with_output(executable: &CExecutable, args: &[&str]) -> miette::Result<i32> {
    let output = executable.execute(args)?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let value: i32 = stdout.trim().parse().unwrap_or(0);
    Ok(value)
}
