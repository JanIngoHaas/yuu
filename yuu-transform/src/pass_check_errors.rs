use anyhow::bail;
use colored::*;
use yuu_parse::pass_parse::SyntaxErrors;

use yuu_shared::{
    context::Context,
    error::{setup_error_formatter, YuuError},
    scheduler::Pass,
};

use crate::type_inference::TypeInferenceErrors;
pub struct PassPrintErrors;

impl Default for PassPrintErrors {
    fn default() -> Self {
        Self::new()
    }
}

impl PassPrintErrors {
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

impl Pass for PassPrintErrors {
    fn run(&self, context: &mut Context) -> anyhow::Result<()> {
        // Set up proper error formatting with syntax highlighting
        setup_error_formatter(Some("WarmEmber"), true).unwrap();

        // Gather errors from different passes
        let syntax_errors = context.get_resource::<SyntaxErrors>(self);
        let type_inference_errors = context.get_resource::<TypeInferenceErrors>(self);

        let syntax_errors = &mut syntax_errors.lock().unwrap().0;

        let type_inference_errors = &mut type_inference_errors.lock().unwrap().0;

        let total_errors = syntax_errors.len() + type_inference_errors.len();

        if total_errors == 0 {
            // No errors found, exit without printing anything
            return Ok(());
        }

        // Print summary
        self.print_error_summary(syntax_errors, type_inference_errors)?;

        // Print syntax errors
        if !syntax_errors.is_empty() {
            self.print_colored_header("Syntax Errors\n")?;
            for (idx, error) in syntax_errors.iter().enumerate() {
                let error: miette::Report = error.clone().into();
                self.print_colored_subheader(format!("Syntax Error {}:", idx + 1).as_str())?;
                println!("{:?}", error);
            }
        }

        // Print type inference errors
        if !type_inference_errors.is_empty() {
            self.print_colored_header("Type Errors\n")?;
            for (idx, error) in type_inference_errors.iter().enumerate() {
                let error: miette::Report = error.clone().into();
                self.print_colored_subheader(format!("Type Error {}:", idx + 1).as_str())?;
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

    fn install(self, schedule: &mut yuu_shared::scheduler::Schedule)
    where
        Self: Sized,
    {
        schedule.requires_resource_write::<SyntaxErrors>(&self);
        schedule.requires_resource_write::<TypeInferenceErrors>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "PassPrintErrors"
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::{pass_check_errors::PassPrintErrors, type_inference::PassTypeInference};
    use yuu_parse::pass_parse::PassParse;
    use yuu_shared::{
        ast::SourceInfo,
        context::Context,
        scheduler::{Pass, Schedule, Scheduler},
    };

    #[test]
    fn test_compilation_with_errors() {
        // Create a sample source with multiple errors
        let source = r#"

    fn test(x: i64, y: f32) -> i64 {
        let z = x + y; // Type error
    
        if z {                     // Type error: z is not a boolean
            return 5;               // Break outside of loop
        } else {
            return 3.0; // Type error
        };
    
        3.0!
    }

    fn main() {
        let result = test(42, 5);
        
        // Syntax error: missing closing parenthesis
        test(34, result);
        test(46, 45.0);
    }

"#;

        // Create a new context and add the code info
        let mut context = Context::new();
        context.add_pass_data(SourceInfo {
            source: Arc::from(source),
            file_name: Arc::from("test_errors.yu"),
        });

        // Create and configure the schedule
        let mut schedule = Schedule::new();

        // Add passes
        PassParse.install(&mut schedule);
        PassTypeInference.install(&mut schedule);
        PassPrintErrors.install(&mut schedule);

        // schedule.print_dot();
        // Run the schedule - should fail due to errors
        let scheduler = Scheduler::new();
        let result = scheduler.run(schedule, context);

        // We expect it to fail and print the errors
        assert!(result.is_err(), "Expected compilation to fail with errors");
    }
}
