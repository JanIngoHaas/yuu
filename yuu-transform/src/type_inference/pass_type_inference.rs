use indexmap::IndexMap;
use yuu_shared::{
    ast::{SourceInfo, StructuralNode, AST},
    binding_info::BindingInfo,
    block::{BindingTable, Block, RootBlock},
    context::Context,
    error::YuuError,
    scheduler::{Pass, ResourceId},
    type_info::TypeInfoTable,
    type_registry::{FieldsMap, StructFieldInfo, TypeRegistry},
};

use super::{declare_function, infer_structural, infer_type};

pub struct TransientData<'a> {
    pub type_registry: &'a mut TypeRegistry,
    pub ast: &'a AST,
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
    pub fn new(type_registry: &'a mut TypeRegistry, ast: &'a AST, src_code: SourceInfo) -> Self {
        Self {
            type_registry,
            ast,
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

fn collect_structural(structural: &StructuralNode, data: &mut TransientData, block: &mut Block) {
    match structural {
        StructuralNode::FuncDecl(decl) => {
            declare_function(
                decl.name,
                &decl.args,
                &decl.ret_ty,
                decl.id,
                decl.span.clone(),
                block,
                data,
            );
        }
        StructuralNode::FuncDef(def) => {
            let ret = declare_function(
                def.decl.name,
                &def.decl.args,
                &def.decl.ret_ty,
                def.id,
                def.decl.span.clone(),
                block,
                data,
            );
            data.type_registry.type_info_table.insert(def.body.id, ret);
        }
        StructuralNode::Error(_) => (),
        StructuralNode::StructDecl(_struct_decl) => {
            unimplemented!("StructDecls are not supported");
        }
        StructuralNode::StructDef(struct_def) => {
            let mut struct_defs = FieldsMap::default();

            for field in &struct_def.fields {
                let ty = infer_type(&field.ty, data);
                let sfi = StructFieldInfo {
                    name: field.name,
                    ty,
                    binding_info: BindingInfo {
                        id: field.id,
                        src_location: Some(field.span.clone()),
                    },
                };
                struct_defs.insert(field.name, sfi);
            }

            data.type_registry.add_struct(
                struct_defs,
                struct_def.decl.name,
                BindingInfo {
                    id: struct_def.id,
                    src_location: Some(struct_def.span.clone()),
                },
            );
        }
    };
}

impl Pass for PassTypeInference {
    fn run(&self, context: &mut Context) -> anyhow::Result<()> {
        let mut root_block = RootBlock::new();
        let ast = context.get_resource::<AST>(self);
        let mut type_registry = TypeRegistry::new();

        let ast = ast.lock().unwrap();
        let ast = &*ast;

        let src_code = context.get_resource::<SourceInfo>(self);
        let src_code = src_code.lock().unwrap();

        let mut data = TransientData::new(&mut type_registry, ast, src_code.clone());

        for node in &ast.structurals {
            collect_structural(node, &mut data, root_block.root_mut());
        }

        for node in &ast.structurals {
            infer_structural(node, root_block.root_mut(), &mut data);
        }

        context.add_pass_data(TypeInferenceErrors(data.errors));
        context.add_pass_data(type_registry);
        Ok(())
    }

    fn install(self, schedule: &mut yuu_shared::scheduler::Schedule)
    where
        Self: Sized,
    {
        schedule.produces_resource::<Box<RootBlock>>(&self);
        schedule.produces_resource::<TypeInferenceErrors>(&self);
        schedule.produces_resource::<TypeRegistry>(&self);
        schedule.requires_resource_read::<AST>(&self);
        schedule.requires_resource_read::<SourceInfo>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "TypeInference"
    }
}
