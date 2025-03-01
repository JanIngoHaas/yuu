use yuu_shared::{ast::AST, context::Context, scheduler::Pass};

use crate::{lexer::UnprocessedCodeInfo, parser::Parser};

pub struct ParsePass;

impl Pass for ParsePass {
    fn run(&self, context: &mut Context) -> anyhow::Result<()> {
        let code_info = context.get_resource::<UnprocessedCodeInfo>(self);
        let code_info = code_info.lock().unwrap();
        let code_info = &*code_info;
        let mut parser = Parser::new(code_info);
        let ast = parser.parse_and_add_ids();
        context.add_pass_data(ast);
        Ok(())
    }

    fn install(self, schedule: &mut yuu_shared::scheduler::Schedule)
    where
        Self: Sized,
    {
        schedule.requires_resource_read::<UnprocessedCodeInfo>(&self);
        schedule.produces_resource::<AST>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "Parse"
    }
}
