// Common utilities for integration tests

use yuu::utils::pipeline::*;

/// Helper function to run the full YIR pipeline and return YIR output
pub fn run_to_yir(source: &str, filename: &str) -> Result<String, Box<dyn std::error::Error>> {
    let yir_output = Pipeline::parse(source.to_string(), filename.to_string())?
        .print_yir_colored()?;
    Ok(yir_output.0)
}

/// Helper function to run the full C compilation pipeline
pub fn run_to_c(source: &str, filename: &str) -> Result<String, Box<dyn std::error::Error>> {
    let mut pipeline = Pipeline::parse(source.to_string(), filename.to_string())?;
    Ok(pipeline.get_c_code()?.0.clone())
}

/// Helper function to run just parsing and diagnostics
pub fn run_parse_only(source: &str, filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    Pipeline::parse(source.to_string(), filename.to_string())?
        .diagnostics()?;
    Ok(())
}