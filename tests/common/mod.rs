// Common utilities for integration tests

use std::sync::Arc;
use yuu::Pass;
use yuu::pass_diagnostics::pass_diagnostics_impl::PassDiagnostics;
use yuu::pass_parse::ast::SourceInfo;
use yuu::pass_parse::pass_parse_impl::PassParse;
use yuu::pass_print_yir::pass_print_yir_impl::{
    PassYirToColoredString, PassYirToString, YirTextualRepresentation,
};
use yuu::pass_type_inference::PassTypeInference;
use yuu::pass_yir_lowering::PassAstToYir;
use yuu::scheduling::context::Context;
use yuu::scheduling::scheduler::{Schedule, Scheduler};

/// Helper function to create a basic context with source code
pub fn create_context_with_source(source: &str, filename: &str) -> Context {
    let mut context = Context::new();
    context.add_pass_data(SourceInfo {
        source: Arc::from(source),
        file_name: Arc::from(filename),
    });
    context
}

/// Helper function to create a schedule with common passes for YIR generation
pub fn create_yir_schedule() -> Schedule {
    let mut schedule = Schedule::new();
    PassParse.install(&mut schedule);
    PassTypeInference.install(&mut schedule);
    PassDiagnostics.install(&mut schedule);
    PassAstToYir.install(&mut schedule);
    PassYirToColoredString.install(&mut schedule);
    schedule
}

/// Helper function to create a schedule with parsing and diagnostics
pub fn create_parse_schedule() -> Schedule {
    let mut schedule = Schedule::new();
    PassParse.install(&mut schedule);
    PassTypeInference.install(&mut schedule);
    PassDiagnostics.install(&mut schedule);
    schedule
}

/// Helper function to run a schedule and get YIR output
pub fn run_schedule_and_get_yir(
    context: Context,
    schedule: Schedule,
) -> Result<String, Box<dyn std::error::Error>> {
    let scheduler = Scheduler::new();
    let context = scheduler.run(schedule, context)?;

    let yir_output = context.get_resource::<YirTextualRepresentation>(&PassYirToString);
    let yir_output = yir_output.lock().unwrap();
    Ok(yir_output.0.clone())
}
