use crate::{
    pass_diagnostics::YuuError,
    pass_parse::{AST, SourceInfo, StructuralNode, StructuralMetadata},
    utils::{
        BindingInfo, BindingTable, BlockTree, EnumVariantInfo, StructFieldInfo, TypeRegistry,
        collections::UstrHashMap,
        type_info_table::{TypeInfo, TypeInfoTable, error_type},
    },
};
use rayon::prelude::*;

use super::{infer_structural, infer_type};

pub struct TransientData<'a> {
    pub type_registry: &'a TypeRegistry,
    pub type_info_table: &'a mut TypeInfoTable,
    pub block_tree: &'a mut BlockTree,
    pub bindings: &'a mut BindingTable,
    pub errors: &'a mut Vec<YuuError>,
    pub src_code: &'a SourceInfo,
    pub current_function_return_type: &'static TypeInfo,
}

pub struct TypeInferenceErrors(pub Vec<YuuError>);

impl<'a> TransientData<'a> {
    pub fn new(
        type_registry: &'a TypeRegistry,
        type_info_table: &'a mut TypeInfoTable,
        block_tree: &'a mut BlockTree,
        bindings: &'a mut BindingTable,
        errors: &'a mut Vec<YuuError>,
        src_code: &'a SourceInfo,
    ) -> Self {
        Self {
            type_registry,
            type_info_table,
            block_tree,
            bindings,
            errors,
            src_code,
            current_function_return_type: error_type(),
        }
    }

    pub fn set_current_function_return_type(&mut self, return_type: &'static TypeInfo) {
        self.current_function_return_type = return_type;
    }
}

pub struct TypeInference;

impl Default for TypeInference {
    fn default() -> Self {
        Self
    }
}

impl TypeInference {
    pub fn run(
        &self,
        ast: &mut AST,
        type_registry: &TypeRegistry,
        src_code: SourceInfo,
    ) -> miette::Result<TypeInferenceErrors> {
        // Process structural elements in parallel with isolated metadata
        let all_errors: Vec<YuuError> = ast.structurals
            .par_iter_mut()
            .flat_map(|structural_node| {
                // Create isolated metadata for this structural element
                let mut local_type_info_table = TypeInfoTable::new();
                let mut local_block_tree = BlockTree::new();
                let mut local_bindings = BindingTable::default();
                let mut local_errors = Vec::new();
                
                // Create local TransientData with read-only type registry access
                let root_id = local_block_tree.root_id();
                let mut local_data = TransientData::new(
                    type_registry,
                    &mut local_type_info_table,
                    &mut local_block_tree,
                    &mut local_bindings,
                    &mut local_errors,
                    &src_code,
                );
                
                // Process this structural element
                infer_structural(structural_node, root_id, &mut local_data);
                
                // Store computed metadata in the AST node (only for non-Error nodes)
                if !matches!(structural_node.as_ref(), StructuralNode::Error(_)) {
                    let metadata = StructuralMetadata {
                        type_info_table: local_type_info_table,
                        binding_table: local_bindings,
                        block_tree: local_block_tree,
                    };
                    
                    match structural_node.as_mut() {
                        StructuralNode::FuncDecl(node) => node.metadata = Some(metadata),
                        StructuralNode::FuncDef(node) => node.metadata = Some(metadata),
                        StructuralNode::StructDecl(node) => node.metadata = Some(metadata),
                        StructuralNode::StructDef(node) => node.metadata = Some(metadata),
                        StructuralNode::EnumDef(node) => node.metadata = Some(metadata),
                        StructuralNode::LuaMeta(node) => node.metadata = Some(metadata),
                        StructuralNode::Error(_) => unreachable!(),
                    }
                }
                
                local_errors
            })
            .collect();
            
        Ok(TypeInferenceErrors(all_errors))
    }
}
