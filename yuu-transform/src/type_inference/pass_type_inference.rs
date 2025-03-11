use yuu_shared::{
    ast::{SourceInfo, AST},
    block::{BindingTable, RootBlock},
    context::Context,
    error::YuuError,
    scheduler::{Pass, ResourceId},
    type_info::TypeInfoTable,
};

use super::infer_structural;

pub struct TransientData<'a> {
    pub type_info_table: &'a mut TypeInfoTable,
    pub ast: &'a AST,
    pub binding_table: BindingTable, // Maps reference IDs to their target binding IDs
    pub errors: Vec<YuuError>,
    pub src_code: SourceInfo,
}

pub struct TypeInferenceErrors(pub Vec<YuuError>);

impl ResourceId for TypeInferenceErrors {
    fn resource_name() -> &'static str {
        "TypeInferenceErrors"
    }
}

impl<'a> TransientData<'a> {
    pub fn new(type_info_table: &'a mut TypeInfoTable, ast: &'a AST, src_code: SourceInfo) -> Self {
        Self {
            type_info_table,
            ast,
            binding_table: BindingTable::default(),
            errors: Vec::default(),
            src_code,
        }
    }
}

pub struct PassTypeInference;

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

        let src_code = context.get_resource::<SourceInfo>(self);
        let src_code = src_code.lock().unwrap();

        let mut data = TransientData::new(type_info_table, ast, src_code.clone());

        for node in &ast.structurals {
            let _ = infer_structural(node, root_block.root_mut(), &mut data);
        }

        context.add_pass_data(data.binding_table);

        context.add_pass_data(TypeInferenceErrors(data.errors));

        Ok(())
    }

    fn install(self, schedule: &mut yuu_shared::scheduler::Schedule)
    where
        Self: Sized,
    {
        schedule.requires_resource_read::<AST>(&self);
        schedule.requires_resource_write::<Box<RootBlock>>(&self);
        schedule.requires_resource_write::<TypeInfoTable>(&self);
        schedule.requires_resource_read::<SourceInfo>(&self);
        schedule.produces_resource::<BindingTable>(&self);
        schedule.produces_resource::<TypeInferenceErrors>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "TypeInference"
    }
}
