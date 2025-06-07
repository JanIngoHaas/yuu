use crate::scheduling::{
    context::Context,
    scheduler::{Pass, ResourceId},
};
use crate::pass_parse::{
    ast::{AST, SourceInfo},
    parser::Parser,
};
use crate::pass_diagnostics::error::YuuError;

pub struct SyntaxErrors(pub Vec<YuuError>);

impl ResourceId for SyntaxErrors {
    fn resource_name() -> &'static str {
        "SyntaxErrors"
    }
}

pub struct PassParse;

impl Pass for PassParse {
    fn run(&self, context: &mut Context) -> anyhow::Result<()> {
        let code_info = context.get_resource::<SourceInfo>(self);
        let code_info = code_info.lock().unwrap();
        let code_info = &*code_info;
        let mut parser = Parser::new(code_info);
        let ast = parser.parse_and_add_ids();
        context.add_pass_data(ast);

        let (syntax_errors, _) = parser.dismantle();

        context.add_pass_data(SyntaxErrors(syntax_errors));

        Ok(())
    }
    fn install(self, schedule: &mut crate::scheduling::scheduler::Schedule)
    where
        Self: Sized,
    {
        schedule.requires_resource_read::<SourceInfo>(&self);
        schedule.produces_resource::<AST>(&self);
        schedule.produces_resource::<SyntaxErrors>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "ParsePass"
    }
}
