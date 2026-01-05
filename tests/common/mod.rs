// Common utilities for integration tests

use yuu::pass_parse::AST;
use yuu::utils::pipeline::*;

/// Helper function to run the full YIR pipeline and return YIR output
pub fn run_to_yir(source: &str, filename: &str) -> Result<String, Box<dyn std::error::Error>> {
    let yir_output = Pipeline::new(source.to_string(), filename.to_string()).calc_yir_colored()?;
    Ok(yir_output.0)
}

/// Helper function to run the full compilation pipeline using JIT execution
/// Returns the exit code from the main function
pub fn run_to_executable(source: &str, filename: &str) -> Result<i32, Box<dyn std::error::Error>> {
    let mut pipeline = Pipeline::new(source.to_string(), filename.to_string());
    Ok(pipeline.jit_execute()?)
}

/// Helper function to run just parsing (without diagnostics)
pub fn run_parse_only(source: &str, filename: &str) -> Result<AST, Box<dyn std::error::Error>> {
    let mut pipeline = Pipeline::new(source.to_string(), filename.to_string());
    pipeline.calc_ast()?;

    Ok(pipeline.ast.unwrap())
}

/// Helper function for compatibility - JIT execution returns the exit code directly
/// This function now simply returns the exit code that was passed in
pub fn run_executable_with_output(exit_code: i32, _args: &[&str]) -> miette::Result<i32> {
    Ok(exit_code)
}
