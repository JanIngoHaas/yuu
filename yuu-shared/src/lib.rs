pub mod ast;
pub mod binding_info;
pub mod graphviz_output;
pub mod token;
pub mod type_info;
pub mod yir;
pub type Span = logos::Span;
pub type Range = Span;
pub mod block;
pub mod context;
pub mod error;
pub mod scheduler;

#[cfg(test)]
mod tests {
    use super::*;
    use miette::{Result, SourceSpan};
    use std::sync::Arc;

    #[test]
    fn test_syntax_highlighting() -> Result<()> {
        // Setup error formatter with syntax highlighting (default theme)
        error::setup_error_formatter(None, false)?;

        // Create a simple Rust source code with an error
        let source_code = r#"
fn main() {
    let x = 5;
    let y = "hello";
    let z = x + y; // Type error: cannot add integer and string
}
"#;

        // Create an error with syntax highlighting
        let err = error::YuuError::builder()
            .kind(error::ErrorKind::InvalidExpression)
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

    #[test]
    fn test_different_themes() -> Result<()> {
        // List available themes
        let themes = error::list_syntax_highlighting_themes();
        println!("Available themes: {:?}", themes);

        // Try a different theme
        error::setup_error_formatter(Some("InspiredGitHub"), false)?;

        // Sample Rust code with syntax highlighting
        let source_code = r#"
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn new(x: f64, y: f64) -> Self {
        Self { x, y }
    }
    
    fn distance(&self, other: &Point) -> f64 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        (dx * dx + dy * dy).sqrt()
    }
}
"#;

        // Create an error highlighting a specific part of the code
        let err = error::YuuError::builder()
            .kind(error::ErrorKind::InvalidSyntax)
            .message("Example of syntax highlighting with a different theme")
            .source(Arc::from(source_code), Arc::from("point.rs"))
            .span((208, 57), "this calculation is highlighted")
            .help("This is just a demonstration of syntax highlighting")
            .build();

        // Print the error with the custom theme
        eprintln!("{:?}", err);

        Ok(())
    }
}
