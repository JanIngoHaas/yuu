use crate::pass_control_flow_analysis::ControlFlowAnalysisErrors;
use crate::pass_diagnostics::{YuuError, setup_error_formatter};
use crate::pass_parse::pass_parse_impl::SyntaxErrors;
use colored::*;
use miette::{IntoDiagnostic, bail};

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
        control_flow_errors: &[YuuError],
    ) -> u64 {
        let total_errors = syntax_errors.len() + type_errors.len() + control_flow_errors.len();

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
        self.print_colored_count(control_flow_errors.len(), "control flow error")
            .unwrap();

        println!();
        total_errors as u64
    }
}

impl Diagnostics {
    pub fn run(
        &self,
        syntax_errors: &SyntaxErrors,
        type_inference_errors: &TypeInferenceErrors,
        control_flow_errors: &ControlFlowAnalysisErrors,
        //break_semantic_errors: &BreakSemanticAnalysisErrors,
        //mutability_errors: &MutabilityAnalysisErrors,
    ) -> miette::Result<()> {
        // Set up proper error formatting with syntax highlighting
        setup_error_formatter(Some("WarmEmber"), true)?;

        // Print summary
        let total_errors = self.print_error_summary(
            &syntax_errors.0,
            &type_inference_errors.0,
            &control_flow_errors.0,
            //break_semantic_errors,
            //mutability_errors,
        );
        if total_errors == 0 {
            return Ok(());
        }

        // Print syntax errors
        if !syntax_errors.0.is_empty() {
            self.print_colored_header("Syntax Errors\n")
                .into_diagnostic()?;
            for (idx, error) in syntax_errors.0.iter().enumerate() {
                let error: miette::Report = error.clone().into();
                self.print_colored_subheader(format!("Syntax Error {}:", idx + 1).as_str())
                    .into_diagnostic()?;
                println!("{:?}", error);
            }
        }

        // Print type inference errors
        if !type_inference_errors.0.is_empty() {
            self.print_colored_header("Type Errors\n")
                .into_diagnostic()?;
            for (idx, error) in type_inference_errors.0.iter().enumerate() {
                let error: miette::Report = error.clone().into();
                self.print_colored_subheader(format!("Type Error {}:", idx + 1).as_str())
                    .into_diagnostic()?;
                println!("{:?}", error);
            }
        }

        // Print control flow errors (including break statement errors)
        let all_control_flow_errors: Vec<_> = control_flow_errors.0.iter().collect(); // TODO: Add Break stmts checking here as well.
        if !all_control_flow_errors.is_empty() {
            self.print_colored_header("Control Flow Errors\n")
                .into_diagnostic()?;
            for (idx, error) in all_control_flow_errors.iter().enumerate() {
                let error: miette::Report = (*error).clone().into();
                self.print_colored_subheader(format!("Control Flow Error {}:", idx + 1).as_str())
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
