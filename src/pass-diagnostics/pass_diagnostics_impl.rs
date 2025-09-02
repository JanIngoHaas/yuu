use crate::pass_diagnostics::{YuuError, setup_error_formatter};
use colored::*;
use miette::{IntoDiagnostic, bail};

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
        sema_errors: &[YuuError],
    ) -> u64 {
        let total_errors = syntax_errors.len() + type_errors.len() + sema_errors.len();

        let summary = if total_errors > 0 {
            "Summary: Compilation failed!".bold().red()
        } else {
            return total_errors as u64;
        };

        println!("\n{}", summary);

        // Print the overall count with red indicator
        println!(
            "{} {}:",
            format!("{}", total_errors).red().bold(),
            format!("error{}", if total_errors == 1 { "" } else { "s" }).bold()
        );

        self.print_colored_count(syntax_errors.len(), "syntax error")
            .unwrap();
        self.print_colored_count(type_errors.len(), "type error")
            .unwrap();
        self.print_colored_count(sema_errors.len(), "semantic error")
            .unwrap();

        println!();
        total_errors as u64
    }
}

impl Diagnostics {
    pub fn run(
        &self,
        syntax_errors: &[YuuError],
        type_inference_errors: &[YuuError],
        sema_errors: &[YuuError],
        //break_semantic_errors: &BreakSemanticAnalysisErrors,
        //mutability_errors: &MutabilityAnalysisErrors,
    ) -> miette::Result<()> {
        // Set up proper error formatting with syntax highlighting
        setup_error_formatter(Some("WarmEmber"), true)?;

        // Print summary
        let total_errors = self.print_error_summary(
            syntax_errors,
            type_inference_errors,
            sema_errors,
            //break_semantic_errors,
            //mutability_errors,
        );
        if total_errors == 0 {
            return Ok(());
        }

        // Print syntax errors
        if !syntax_errors.is_empty() {
            self.print_colored_header("Syntax Errors\n")
                .into_diagnostic()?;
            for (idx, error) in syntax_errors.iter().enumerate() {
                let error: miette::Report = error.clone().into();
                self.print_colored_subheader(format!("Syntax Error {}:", idx + 1).as_str())
                    .into_diagnostic()?;
                println!("{:?}", error);
            }
        }

        // Print type inference errors
        if !type_inference_errors.is_empty() {
            self.print_colored_header("Type Errors\n")
                .into_diagnostic()?;
            for (idx, error) in type_inference_errors.iter().enumerate() {
                let error: miette::Report = error.clone().into();
                self.print_colored_subheader(format!("Type Error {}:", idx + 1).as_str())
                    .into_diagnostic()?;
                println!("{:?}", error);
            }
        }

        // Print control flow errors (including break statement errors)
        if !sema_errors.is_empty() {
            self.print_colored_header("Semantic Analysis Errors\n")
                .into_diagnostic()?;
            for (idx, error) in sema_errors.iter().enumerate() {
                let error: miette::Report = (*error).clone().into();
                self.print_colored_subheader(format!("Semantic Error {}:", idx + 1).as_str())
                    .into_diagnostic()?;
                println!("{:?}", error);
            }
        }

        // Print mutability errors
        // if !mutability_errors.is_empty() {
        //     self.print_colored_header("Mutability Errors\n")
        //         .into_diagnostic()?;
        //     for (idx, error) in mutability_errors.iter().enumerate() {
        //         let error: miette::Report = error.clone().into();
        //         self.print_colored_subheader(format!("Mutability Error {}:", idx + 1).as_str())
        //             .into_diagnostic()?;
        //         println!("{:?}", error);
        //     }
        // }

        // Return an error to indicate compilation failure
        if total_errors > 0 {
            bail!("Compilation failed with {} errors", total_errors);
        } else {
            Ok(())
        }
    }
}
