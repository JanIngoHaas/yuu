use yuu_shared::{
    context::Context,
    scheduler::{Pass, ResourceId, ResourceName},
    yir::Module,
};

pub struct YirToStringPass;

impl Default for YirToStringPass {
    fn default() -> Self {
        Self::new()
    }
}

impl YirToStringPass {
    pub fn new() -> Self {
        Self
    }
}

pub struct YirTextualRepresentation(pub String);

impl ResourceId for YirTextualRepresentation {
    fn resource_name() -> ResourceName {
        "YirTextualRepresentation"
    }
}

impl Pass for YirToStringPass {
    fn run(&self, context: &mut Context) -> anyhow::Result<()> {
        let module = context.require_pass_data::<Module>(self);
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
