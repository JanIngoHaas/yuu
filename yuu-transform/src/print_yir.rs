use yuu_shared::{yir::Module, Pass};

pub struct PrintYirPass {
    pub output: Option<String>,
}

impl PrintYirPass {
    pub fn new() -> Self {
        Self { output: None }
    }
}

impl Pass for PrintYirPass {
    fn run(&mut self, context: &mut yuu_shared::Context) -> bool {
        let module = context.require_pass_data::<Module>("PrintYir");
        let module = module.as_ref().borrow();
        self.output = Some(format!("{}", module));
        true
    }

    fn install(self, pipeline: &mut yuu_shared::Pipeline)
    where
        Self: Sized,
    {
        pipeline.add_pass(self);
    }
}
