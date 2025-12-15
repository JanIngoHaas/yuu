use crate::{
    pass_diagnostics::YuuError,
    pass_parse::{AST, SourceInfo, StructuralNode},
    utils::{
        BindingInfo, BindingTable, BlockTree, EnumVariantInfo, StructFieldInfo, TypeRegistry,
        collections::UstrIndexMap,
        type_info_table::{TypeInfo, TypeInfoTable, error_type},
    },
};

use super::{declare_function, infer_structural, infer_type};

pub struct TransientData<'a> {
    pub type_registry: &'a mut TypeRegistry,
    pub type_info_table: &'a mut TypeInfoTable,
    pub ast: &'a AST,
    pub errors: &'a mut Vec<YuuError>,
    pub src_code: SourceInfo,
    pub current_function_return_type: &'static TypeInfo,
}

pub struct TransientDataStructural<'a> {
    pub type_registry: &'a TypeRegistry,
    pub type_info_table: &'a mut TypeInfoTable,
    pub block_tree: &'a mut BlockTree,
    pub bindings: &'a mut BindingTable,
    pub ast: &'a AST,
    pub errors: &'a mut Vec<YuuError>,
    pub src_code: SourceInfo,
    pub current_function_return_type: &'static TypeInfo,
}

pub struct TypeInferenceErrors(pub Vec<YuuError>);

impl<'a> TransientData<'a> {
    pub fn new(
        type_registry: &'a mut TypeRegistry,
        type_info_table: &'a mut TypeInfoTable,
        ast: &'a AST,
        errors: &'a mut Vec<YuuError>,
        src_code: SourceInfo,
    ) -> Self {
        Self {
            type_registry,
            type_info_table,
            ast,
            errors,
            src_code,
            current_function_return_type: error_type(),
        }
    }

    pub fn set_current_function_return_type(&mut self, return_type: &'static TypeInfo) {
        self.current_function_return_type = return_type;
    }
}

impl<'a> TransientDataStructural<'a> {
    pub fn new(
        type_registry: &'a TypeRegistry,
        type_info_table: &'a mut TypeInfoTable,
        block_tree: &'a mut BlockTree,
        bindings: &'a mut BindingTable,
        ast: &'a AST,
        errors: &'a mut Vec<YuuError>,
        src_code: SourceInfo,
    ) -> Self {
        Self {
            type_registry,
            type_info_table,
            block_tree,
            bindings,
            ast,
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
        Self::new()
    }
}

impl TypeInference {
    pub fn new() -> Self {
        Self {}
    }
}

fn declare_user_def_types(structural: &StructuralNode, data: &mut TransientData) {
    match structural {
        StructuralNode::StructDef(struct_def) => {
            let mut struct_defs = UstrIndexMap::default();

            for field in &struct_def.fields {
                let ty = error_type(); //later: infer_type(&field.ty, &data.type_registry, &mut data.errors, &data.src_code); See below for reason
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
        StructuralNode::EnumDef(ed) => {
            let mut enum_variant_defs = UstrIndexMap::default();

            for (idx, variant) in ed.variants.iter().enumerate() {
                let evi = EnumVariantInfo {
                    variant_name: variant.name,
                    variant_idx: idx as u64,
                    binding_info: BindingInfo {
                        id: variant.id,
                        src_location: Some(variant.span.clone()),
                    },
                    variant: variant.data_type.as_ref().map(|_x| error_type()), // For collecting, we have to first declare everything as error_type, as we don't have all type info right now - later then: variant.data_type.as_ref().map(|x| infer_type(x, &data.type_registry, &mut data.errors, &data.src_code)),
                };
                enum_variant_defs.insert(variant.name, evi);
            }

            data.type_registry.add_enum(
                ed.decl.name,
                enum_variant_defs,
                BindingInfo {
                    id: ed.id,
                    src_location: Some(ed.span.clone()),
                },
            );
        }
        _ => (),
    };
}

fn declare_and_define_functions(
    structural: &StructuralNode,
    data: &mut TransientData,
    tree: &mut BlockTree,
    current_id: usize,
) {
    let block = tree.get_block_mut(current_id);
    match structural {
        StructuralNode::FuncDecl(decl) => {
            declare_function(
                decl.name,
                &decl.args,
                &decl.ret_ty,
                decl.id,
                decl.span.clone(),
                data,
            );
        }
        StructuralNode::FuncDef(def) => {
            let function_type = declare_function(
                def.decl.name,
                &def.decl.args,
                &def.decl.ret_ty,
                def.id,
                def.decl.span.clone(),
                data,
            );

            // Store the function type for later access
            data.type_info_table.insert(def.id, function_type);
        }
        _ => (),
    };
}

fn define_user_def_types(
    structural: &StructuralNode,
    data: &mut TransientData,
    helper_vec: &mut Vec<&'static TypeInfo>,
) {
    helper_vec.clear();
    match structural {
        StructuralNode::StructDef(struct_def_structural) => {
            for field in &struct_def_structural.fields {
                let ty = infer_type(
                    &field.ty,
                    &data.type_registry,
                    &mut data.errors,
                    &data.src_code,
                );
                helper_vec.push(ty);
            }

            // Then, get the mutable reference and update the types
            let sfi = data
                .type_registry
                .resolve_struct_mut(struct_def_structural.decl.name)
                .unwrap();

            for (ty, (_sfi_field_name, sfi_info)) in helper_vec.iter().zip(sfi.fields.iter_mut()) {
                sfi_info.ty = *ty;
            }
        }
        StructuralNode::EnumDef(enum_def_structural) => {
            for variant in &enum_def_structural.variants {
                let ty = variant.data_type.as_ref().map(|ty| {
                    infer_type(ty, &data.type_registry, &mut data.errors, &data.src_code)
                });
                helper_vec.push(ty.unwrap_or_else(|| error_type())); // Use error_type as placeholder for None
            }

            // Then, get the mutable reference and update the types
            let evi = data
                .type_registry
                .resolve_enum_mut(enum_def_structural.decl.name)
                .unwrap();

            for ((variant, ty), (_evi_variant_name, evi_info)) in enum_def_structural
                .variants
                .iter()
                .zip(helper_vec.iter())
                .zip(evi.variants.iter_mut())
            {
                if variant.data_type.is_some() {
                    evi_info.variant = Some(*ty);
                }
            }
        }
        _ => (),
    }
}

use rayon::prelude::*;

impl TypeInference {
    pub fn run(
        &self,
        ast: &AST,
        expr_count: usize,
        src_code: SourceInfo,
    ) -> miette::Result<(TypeRegistry, BlockTree, BindingTable, TypeInfoTable, TypeInferenceErrors)> {
        let mut root_block = BlockTree::new();
        let mut type_registry = TypeRegistry::new();
        let mut type_info_table = TypeInfoTable::with_size(expr_count);
        let mut errors = Vec::new();
        let mut data = TransientData::new(
            &mut type_registry,
            &mut type_info_table,
            ast,
            &mut errors,
            src_code.clone(),
        );
        let mut helper_vec = Vec::<&'static TypeInfo>::new();

        // First pass: declare user-defined types (structs and enums)
        for node in &ast.structurals {
            declare_user_def_types(node, &mut data);
        }

        // Second pass: define user-defined types right after declaration
        for node in &ast.structurals {
            define_user_def_types(node, &mut data, &mut helper_vec);
        }

        // Third pass: declare and define functions (can be done together since functions signatures don't reference each other)
        let root_id = root_block.root_id();
        for node in &ast.structurals {
            declare_and_define_functions(node, &mut data, &mut root_block, root_id);
        }

        // Fourth pass: infer the structural elements (SYNC)
        let mut bindings = BindingTable::default();
        let mut data = TransientDataStructural::new(
            &type_registry,
            &mut type_info_table,
            &mut root_block,
            &mut bindings,
            ast,
            &mut errors,
            src_code.clone(),
        );
        for node in &ast.structurals {
            infer_structural(node, root_id, &mut data);
        }

        // Fourth pass: infer the structural elements (PARALLEL)
        // let errors_pass4: Vec<YuuError> = ast.structurals.par_iter().flat_map(|node| {
        //     // Create local BlockTree for this function/structural
        //     let mut local_tree = BlockTree::new();
        //     let root_id = local_tree.root_id();
        //     let mut local_type_info_table = TypeInfoTable::new();
        //     let mut local_bindings = BindingTable::new();

        //     // Create local TransientData with IMMUTABLE type registry reference
        //     let mut local_data = TransientDataStructural::new(&type_registry, &mut local_type_info_table, &mut local_tree, &mut local_bindings, ast, src_code.clone());

        //     infer_structural(node, root_id, &mut local_data);

        //     local_data.errors
        // }).collect();

        // let mut all_errors = errors;
        // all_errors.extend(errors_pass4);
        Ok((
            type_registry,
            root_block,
            bindings,
            type_info_table,
            TypeInferenceErrors(errors),
        ))
    }
}
