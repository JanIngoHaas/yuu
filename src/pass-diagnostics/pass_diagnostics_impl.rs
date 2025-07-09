use crate::pass_diagnostics::{YuuError, setup_error_formatter};
use crate::pass_parse::pass_parse_impl::SyntaxErrors;
use miette::{bail, IntoDiagnostic};
use colored::*;

use crate::pass_type_inference::TypeInferenceErrors;
pub struct Diagnostics;

impl Default for Diagnostics {
    fn default() -> Self {
        Self::new()
    }
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {}
    }

    // Helper method to print colored headers
    fn print_colored_header(&self, text: &str) -> std::io::Result<()> {
        println!("\n{}", text.bold().underline().red());
        Ok(())
    }

    fn print_colored_subheader(&self, text: &str) -> std::io::Result<()> {
        // TODO: Make this a bit less red, maybe italicize it
        println!("{}", text.italic().red());
        Ok(())
    }

    // Helper method to print error count with red indicator
    fn print_colored_count(&self, count: usize, label: &str) -> std::io::Result<()> {
        if count == 0 {
            return Ok(());
        }

        // Print count with red indicator
        println!(
            "  {} {}{}",
            format!("{}", count).red().bold(),
            label,
            if count == 1 { "" } else { "s" }
        );

        Ok(())
    }

    // Helper to print a summary of errors
    fn print_error_summary(
        &self,
        syntax_errors: &[YuuError],
        type_errors: &[YuuError],
    ) -> std::io::Result<()> {
        let total_errors = syntax_errors.len() + type_errors.len();

        let summary = if !syntax_errors.is_empty() || !type_errors.is_empty() {
            "Summary: Compilation failed!".bold().red()
        } else {
            "Summary: Semantic Analysis successful!".bold().green()
        };

        println!("\n{}", summary);

        // Print the overall count with red indicator
        println!(
            "{} {}:",
            format!("{}", total_errors).red().bold(),
            format!("error{}", if total_errors == 1 { "" } else { "s" }).bold()
        );

        self.print_colored_count(syntax_errors.len(), "syntax error")?;
        self.print_colored_count(type_errors.len(), "type error")?;

        println!();
        Ok(())
    }
}

impl Diagnostics {
    pub fn run(&self, syntax_errors: &SyntaxErrors, type_inference_errors: &TypeInferenceErrors) -> miette::Result<()> {
        // Set up proper error formatting with syntax highlighting
        setup_error_formatter(Some("WarmEmber"), true)?;

        let syntax_errors = &syntax_errors.0;
        let type_inference_errors = &type_inference_errors.0;

        let total_errors = syntax_errors.len() + type_inference_errors.len();

        if total_errors == 0 {
            // No errors found, exit without printing anything
            return Ok(());
        }

        // Print summary
        self.print_error_summary(syntax_errors, type_inference_errors).into_diagnostic()?;

        // Print syntax errors
        if !syntax_errors.is_empty() {
            self.print_colored_header("Syntax Errors\n").into_diagnostic()?;
            for (idx, error) in syntax_errors.iter().enumerate() {
                let error: miette::Report = error.clone().into();
                self.print_colored_subheader(format!("Syntax Error {}:", idx + 1).as_str()).into_diagnostic()?;
                println!("{:?}", error);
            }
        }

        // Print type inference errors
        if !type_inference_errors.is_empty() {
            self.print_colored_header("Type Errors\n").into_diagnostic()?;
            for (idx, error) in type_inference_errors.iter().enumerate() {
                let error: miette::Report = error.clone().into();
                self.print_colored_subheader(format!("Type Error {}:", idx + 1).as_str()).into_diagnostic()?;
                println!("{:?}", error);
            }
        }

        // Return an error to indicate compilation failure
        if !type_inference_errors.is_empty() || !syntax_errors.is_empty() {
            bail!("Compilation failed due to errors");
        } else {
            Ok(())
        }
    }
}
