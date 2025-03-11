use yuu_shared::{
    context::Context,
    scheduler::{Pass, ResourceId, ResourceName},
    yir::Module,
};

pub struct PassYirToString;

impl Default for PassYirToString {
    fn default() -> Self {
        Self::new()
    }
}

impl PassYirToString {
    pub fn new() -> Self {
        Self
    }
}

pub struct PassYirToColoredString;

impl Default for PassYirToColoredString {
    fn default() -> Self {
        Self
    }
}

impl Pass for PassYirToColoredString {
    fn run(&self, context: &mut Context) -> anyhow::Result<()> {
        let module = context.get_resource::<Module>(self);
        let module = module.lock().unwrap();
        let mut f = String::new();
        module.format_yir(true, &mut f)?;
        context.add_pass_data(YirTextualRepresentation(f));
        Ok(())
    }

    fn install(self, schedule: &mut yuu_shared::scheduler::Schedule)
    where
        Self: Sized,
    {
        schedule.requires_resource_read::<Module>(&self);
        schedule.produces_resource::<YirTextualRepresentation>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "YirToColoredString"
    }
}

pub struct YirTextualRepresentation(pub String);

impl ResourceId for YirTextualRepresentation {
    fn resource_name() -> ResourceName {
        "YirTextualRepresentation"
    }
}

impl Pass for PassYirToString {
    fn run(&self, context: &mut Context) -> anyhow::Result<()> {
        let module = context.get_resource::<Module>(self);
        let module = module.lock().unwrap();
        let ir_string = format!("{}", module);
        context.add_pass_data(YirTextualRepresentation(ir_string));
        Ok(())
    }

    fn install(self, schedule: &mut yuu_shared::scheduler::Schedule) {
        schedule.requires_resource_read::<Module>(&self);
        schedule.produces_resource::<YirTextualRepresentation>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "YirToString"
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::{
        pass_ast_to_yir::PassAstToYir, pass_collect_decls::PassCollectDecls,
        type_inference::PassTypeInference,
    };
    use yuu_parse::pass_parse::PassParse;
    use yuu_shared::{
        ast::SourceInfo,
        scheduler::{Schedule, Scheduler},
    };

    #[test]
    fn test_fac_yir() {
        // Create the factorial function code
        let code_info = SourceInfo {
            source: Arc::from(
                r#"fn fac(n: i64) -> i64 {
                if n == 0 {
                    1!
                }
                else {
                    let n_out = n * fac(n - 1);
                    return n_out;
                }!
            }"#,
            ),
            file_name: Arc::from("test.yuu"),
        };

        // Create a new context and add the code info
        let mut context = Context::new();
        context.add_pass_data(code_info);

        // Create and configure the schedule
        let mut schedule = Schedule::new();

        // Add passes
        PassParse.install(&mut schedule);
        PassCollectDecls.install(&mut schedule);
        PassTypeInference.install(&mut schedule);
        PassAstToYir.install(&mut schedule);
        PassYirToColoredString.install(&mut schedule);

        // // Print the pass dependency graph in DOT format for debugging
        // println!("Pass Dependencies:");
        // schedule.print_dot();

        // Run the schedule
        let scheduler = Scheduler::new();
        let context = scheduler
            .run(schedule, context)
            .expect("Failed to run schedule");

        // Get the YIR output
        let yir_output = context.get_resource::<YirTextualRepresentation>(&PassYirToString);
        let yir_output = yir_output.lock().unwrap();
        println!("Generated YIR:\n{}", yir_output.0);
    }
}
