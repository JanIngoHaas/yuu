pub mod scheduling;

// Pass modules
#[path = "pass-c_lowering/mod.rs"]
pub mod pass_c_lowering;
#[path = "pass-diagnostics/mod.rs"]
pub mod pass_diagnostics;
#[path = "pass-parse/mod.rs"]
pub mod pass_parse;
#[path = "pass-print_yir/mod.rs"]
pub mod pass_print_yir;
#[path = "pass-type_inference/mod.rs"]
pub mod pass_type_inference;
#[path = "pass-yir_lowering/mod.rs"]
pub mod pass_yir_lowering;

// Re-export commonly used types from scheduling
pub use scheduling::{Context, Pass, ResourceId, ResourceName, Schedule, Scheduler};

#[cfg(test)]
mod tests {
    use super::*;
    use miette::{Result, SourceSpan};
    use std::sync::Arc;

    #[test]
    fn test_syntax_highlighting() -> Result<()> {
        // Setup error formatter with syntax highlighting (default theme)
        pass_diagnostics::setup_error_formatter(None, false)?;

        // Create a simple Rust source code with an error
        let source_code = r#"
fn main() {
    let x = 5;
    let y = "hello";
    let z = x + y; // Type error: cannot add integer and string
}
"#;

        // Create an error with syntax highlighting
        let err = pass_diagnostics::YuuError::builder()
            .kind(pass_diagnostics::ErrorKind::InvalidExpression)
            .message("Cannot add integer and string")
            .source(Arc::from(source_code), Arc::from("example.rs"))
            .span((76, 5), "type mismatch in addition")
            .label((59, 1), "this is an integer")
            .label((71, 1), "this is a string")
            .help("Try converting the string to a number first")
            .build();

        // Print the error - this will use syntax highlighting
        eprintln!("{:?}", err);

        Ok(())
    }
}
