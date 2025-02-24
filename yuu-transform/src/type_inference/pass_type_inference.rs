use yuu_shared::{
    ast::AST, block::RootBlock, context::Context, scheduler::Pass, type_info::TypeInfoTable,
};

use super::{infer_structural, TransientData};

pub struct PassTypeInference {}

impl Default for PassTypeInference {
    fn default() -> Self {
        Self::new()
    }
}

impl PassTypeInference {
    pub fn new() -> Self {
        Self {}
    }
}

impl Pass for PassTypeInference {
    fn run(&self, context: &mut Context) -> anyhow::Result<()> {
        let root_block = context.require_pass_data::<Box<RootBlock>>(self);
        let ast = context.require_pass_data::<AST>(self);
        let type_info_table = context.require_pass_data::<TypeInfoTable>(self);

        let mut root_block = root_block.lock().unwrap();
        let ast = ast.lock().unwrap();
        let ast = &*ast;

        let mut type_info_table = type_info_table.lock().unwrap();
        let type_info_table = &mut *type_info_table;

        let mut data = TransientData {
            ast,
            type_info_table,
        };

        for node in &ast.structurals {
            let _ = infer_structural(node, root_block.root_mut(), &mut data);
        }

        Ok(())
    }

    fn install(self, schedule: &mut yuu_shared::scheduler::Schedule)
    where
        Self: Sized,
    {
        schedule.requires_resource_read::<AST>(&self);
        schedule.requires_resource_write::<Box<RootBlock>>(&self);
        schedule.requires_resource_write::<TypeInfoTable>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "TypeInference"
    }
}
