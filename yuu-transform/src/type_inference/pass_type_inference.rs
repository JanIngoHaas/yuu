use yuu_shared::{
    ast::AST,
    block::{BindingTable, RootBlock},
    context::Context,
    scheduler::Pass,
    type_info::TypeInfoTable,
};

use super::infer_structural;

pub struct TransientData<'a> {
    pub type_info_table: &'a mut TypeInfoTable,
    pub ast: &'a AST,
    pub binding_table: BindingTable, // Maps reference IDs to their target binding IDs
}

impl<'a> TransientData<'a> {
    pub fn new(
        type_info_table: &'a mut TypeInfoTable,
        ast: &'a AST,
        binding_table: BindingTable,
    ) -> Self {
        Self {
            type_info_table,
            ast,
            binding_table,
        }
    }
}

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
        let root_block = context.get_resource::<Box<RootBlock>>(self);
        let ast = context.get_resource::<AST>(self);
        let type_info_table = context.get_resource::<TypeInfoTable>(self);

        let mut root_block = root_block.lock().unwrap();
        let ast = ast.lock().unwrap();
        let ast = &*ast;

        let mut type_info_table = type_info_table.lock().unwrap();
        let type_info_table = &mut *type_info_table;

        let mut data = TransientData {
            ast,
            type_info_table,
            binding_table: Default::default(),
        };

        for node in &ast.structurals {
            let _ = infer_structural(node, root_block.root_mut(), &mut data);
        }

        context.add_pass_data(data.binding_table);

        Ok(())
    }

    fn install(self, schedule: &mut yuu_shared::scheduler::Schedule)
    where
        Self: Sized,
    {
        schedule.requires_resource_read::<AST>(&self);
        schedule.requires_resource_write::<Box<RootBlock>>(&self);
        schedule.requires_resource_write::<TypeInfoTable>(&self);
        schedule.produces_resource::<BindingTable>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "TypeInference"
    }
}
