use crate::{
    pass_yir_lowering::yir::Module,
    utils::context::Context,
    utils::scheduler::{Pass, ResourceId, ResourceName},
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
    fn install(self, schedule: &mut crate::utils::scheduler::Schedule)
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
    fn install(self, schedule: &mut crate::utils::scheduler::Schedule) {
        schedule.requires_resource_read::<Module>(&self);
        schedule.produces_resource::<YirTextualRepresentation>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "YirToString"
    }
}
