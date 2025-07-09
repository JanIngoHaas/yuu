use crate::{
    pass_diagnostics::YuuError,
    pass_parse::{AST, SourceInfo, StructuralNode},
    pass_type_inference::{
        binding_info::BindingInfo,
        type_registry::{FieldsMap, StructFieldInfo, TypeRegistry},
    },
    pass_yir_lowering::block::{Block, RootBlock},
};

use super::{declare_function, infer_structural, infer_type};

pub struct TransientData<'a> {
    pub type_registry: &'a mut TypeRegistry,
    pub ast: &'a AST,
    pub errors: Vec<YuuError>,
    pub src_code: SourceInfo,
}

pub struct TypeInferenceErrors(pub Vec<YuuError>);

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

pub struct TypeInference;

impl Default for TypeInference {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeInference {
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

impl TypeInference {
    pub fn run(&self, ast: &AST, src_code: SourceInfo) -> miette::Result<(TypeRegistry, Box<RootBlock>, TypeInferenceErrors)> {
        let mut root_block = RootBlock::new();
        let mut type_registry = TypeRegistry::new();
        let errors = {
            let mut data = TransientData::new(&mut type_registry, ast, src_code);

            for node in &ast.structurals {
                collect_structural(node, &mut data, root_block.root_mut());
            }

            for node in &ast.structurals {
                infer_structural(node, root_block.root_mut(), &mut data);
            }

            data.errors
        };

        Ok((type_registry, root_block, TypeInferenceErrors(errors)))
    }
}
