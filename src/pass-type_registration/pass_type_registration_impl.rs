use logos::Span;
use ustr::Ustr;

use crate::{
    pass_diagnostics::YuuError, pass_parse::{SourceInfo, ast::*}, pass_type_inference::infer_type, utils::{
        binding_info::BindingInfo, collections::{UstrHashMap, UstrIndexMap}, type_info_table::{TypeInfo, error_type, function_type, primitive_nil, unknown_type}, type_registry::{EnumVariantInfo, StructFieldInfo, TypeRegistry}
    }
};

/// Errors from type registration analysis
pub struct TypeRegistrationErrors(pub Vec<YuuError>);

impl TypeRegistrationErrors {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn add_error(&mut self, error: YuuError) {
        self.0.push(error);
    }
}

/// Pass that registers user-defined types (structs and enums) in the TypeRegistry
pub struct TypeRegistration;

impl TypeRegistration {

    /// Run the type registration pass on the AST
    pub fn run(
        &self,
        ast: &AST,
        src_code: &SourceInfo,
    ) -> (TypeRegistry, TypeRegistrationErrors) {
        let mut type_registry = TypeRegistry::new();
        let mut errors = Vec::new();

        let mut data = TransientData::new(&mut type_registry, &mut errors, src_code);

        // Process all structural nodes to register types
        for structural in &ast.structurals {
            declare_user_def_types(structural.as_ref(), &mut data);
        }

        let mut helper_vec = Vec::new();
        for structural in &ast.structurals {
            define_user_def_types(structural.as_ref(), &mut data, &mut helper_vec);
        }

        // Register function declarations and definitions
        for structural in &ast.structurals {
            declare_and_define_functions(structural.as_ref(), &mut data);
        }

        (type_registry, TypeRegistrationErrors(errors))
    }
}

impl Default for TypeRegistration {
    fn default() -> Self {
        Self
    }
}

fn declare_and_define_functions(
    structural: &StructuralNode,
    data: &mut TransientData,
) {
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
        }
        _ => (),
    };
}

pub fn declare_function(
    name: Ustr,
    args: &[Arg],
    ret_ty: &Option<Box<TypeNode>>,
    id: NodeId,
    span: Span,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let func_arg_types = args
        .iter()
        .map(|arg| {
            let semantic_arg_type = infer_type(
                &arg.ty,
                &data.type_registry,
                &mut data.errors,
                &data.src_code,
            );
            semantic_arg_type
        })
        .collect::<Vec<_>>();

    let ret_type = if let Some(ty) = ret_ty {
        infer_type(ty, &data.type_registry, &mut data.errors, &data.src_code)
    } else {
        primitive_nil()
    };

    data.type_registry.add_function(
        &func_arg_types,
        ret_type,
        name,
        BindingInfo {
            id,
            src_location: Some(span),
        },
    );

    // Create and return the full function type
    let (_, full_function_type) = function_type(&func_arg_types, ret_type);
    full_function_type
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

            for (field, ty) in struct_def_structural.fields.iter().zip(helper_vec.iter()) {
                let sfi_info = sfi.fields.get_mut(&field.name).unwrap();
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

            for (variant, ty) in enum_def_structural
                .variants
                .iter()
                .zip(helper_vec.iter())
            {
                if variant.data_type.is_some() {
                    let evi_info = evi.variants.get_mut(&variant.name).unwrap();
                    evi_info.variant = Some(*ty);
                }
            }
        }
        _ => (),
    }
}

struct TransientData<'a> {
    type_registry: &'a mut TypeRegistry,
    errors: &'a mut Vec<YuuError>,
    src_code: &'a SourceInfo,
}

impl<'a> TransientData<'a> {
    fn new(
        type_registry: &'a mut TypeRegistry,
        errors: &'a mut Vec<YuuError>,
        src_code: &'a SourceInfo,
    ) -> Self {
        Self {
            type_registry,
            errors,
            src_code,
        }
    }
}

fn declare_user_def_types(structural: &StructuralNode, data: &mut TransientData) {
    match structural {
        StructuralNode::StructDef(struct_def) => {
            let mut struct_defs = UstrIndexMap::default();

            for field in &struct_def.fields {
                let sfi = StructFieldInfo {
                    name: field.name,
                    ty: unknown_type(),
                    binding_info: BindingInfo {
                        id: field.id,
                        src_location: Some(field.span.clone()),
                    },
                };
                struct_defs.insert(field.name, sfi);
            }

            let is_new = data.type_registry.add_struct(
                struct_defs,
                struct_def.decl.name,
                BindingInfo {
                    id: struct_def.id,
                    src_location: Some(struct_def.span.clone()),
                },
            );
            
            if !is_new {
                let error = YuuError::builder()
                    .kind(crate::pass_diagnostics::error::ErrorKind::InvalidSyntax)
                    .message(format!("Duplicate struct definition: '{}'", struct_def.decl.name))
                    .source(data.src_code.source.clone(), data.src_code.file_name.clone())
                    .span(struct_def.span.clone(), "duplicate struct definition here")
                    .help("Struct names must be unique within a module. Consider renaming one of the structs.".to_string())
                    .build();
                data.errors.push(error);
            }
        }
        StructuralNode::EnumDef(ed) => {
            let mut enum_variant_defs = UstrHashMap::default();

            for (idx, variant) in ed.variants.iter().enumerate() {
                let evi = EnumVariantInfo {
                    variant_name: variant.name,
                    variant_idx: idx as u64,
                    binding_info: BindingInfo {
                        id: variant.id,
                        src_location: Some(variant.span.clone()),
                    },
                    variant: variant.data_type.as_ref().map(|_x| unknown_type()),
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
        _ => {
            // Ignore other structural nodes (functions, etc.) - they're handled in other passes
        }
    }
}