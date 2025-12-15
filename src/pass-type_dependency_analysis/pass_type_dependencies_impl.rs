use colored::Colorize;
use ustr::Ustr;

use super::TypeDependencyAnalysisErrors;

use crate::{
    pass_diagnostics::error::{ErrorKind, YuuError},
    pass_parse::SourceInfo,
    pass_type_dependency_analysis::TypeDependencyGraph,
    utils::{StructOrEnumInfo, TypeRegistry, collections::{UstrIndexMap, UstrIndexSet}, type_info_table::TypeInfo},
};

pub struct TypeDependencyAnalysis;

impl TypeDependencyAnalysis {
    pub fn new() -> Self {
        Self
    }

    fn build_dependency_graph(data: &mut TransientData, info: StructOrEnumInfo<'_>) {
        if data.dependency_graph.contains_key(&info.name()) {
            // Already processed
            return;
        }

        data.dependency_graph.entry(info.name()).or_default();

        for (field_name, field_ty, field_binding_info, field_discr) in info.iterate() {
            let field_ty_name = match field_ty {
                TypeInfo::Struct(si) => si.name,
                TypeInfo::Enum(ei) => ei.name,
                _ => continue,
            };

            // Add dependency: current type depends on field type
            data.dependency_graph
                .entry(info.name())
                .or_default()
                .push(field_ty_name);

            // Check for immediate cycle
            if data.current_path.contains(&field_ty_name) {
                let (type_kind_label, type_binding_info_opt) = match info {
                    StructOrEnumInfo::Struct(si) => ("struct", Some(&si.binding_info)),
                    StructOrEnumInfo::Enum(ei) => ("enum", Some(&ei.binding_info)),
                };

                let type_binding_info = type_binding_info_opt.unwrap();
                let type_span = type_binding_info.src_location.as_ref().unwrap();

                // Build cycle string
                let cycle_start = data
                    .current_path
                    .iter()
                    .position(|&n| n == field_ty_name)
                    .unwrap_or(0);
                let cycle_path: Vec<_> = data
                    .current_path
                    .iter()
                    .skip(cycle_start)
                    .cloned()
                    .collect();
                let mut cycle_str_parts: Vec<String> =
                    cycle_path.iter().map(|s| s.as_str().to_string()).collect();
                cycle_str_parts.push(field_ty_name.as_str().to_string());
                let cycle_str = cycle_str_parts.join(" -> ");

                // Build an explanatory error: recursive type without indirection
                let err = YuuError::builder()
                    .kind(ErrorKind::InfinitelySizedType)
                    .message(format!(
                        "Recursive {type_kind_label} '{}' would be infinitely sized",
                        info.name()
                    ))
                    .source(data.source_info.source.clone(), data.source_info.file_name.clone())
                    .span(type_span.clone(), format!("definition of {} here", type_kind_label))
                    .label(field_binding_info.src_location.unwrap().clone(), format!("problematic member '{}' of type '{} {}' here", field_name, field_discr.as_str(), field_ty_name))
                    .help(format!("Type cycle: {}\nA type that directly or indirectly contains itself would have infinite size: each instance would embed another instance forever. Use indirection (for example a pointer) for the recursive field so the field stores a fixed-size reference instead of embedding the whole type.", cycle_str.yellow()))
                    .build();

                data.errors.add_error(err);
                continue;
            }

            // Recurse if we haven't visited this type yet
            if !data.dependency_graph.contains_key(&field_ty_name) {
                match field_ty {
                    TypeInfo::Struct(_) => {
                        let si = data.type_registry.resolve_struct(field_ty_name).expect(
                            "Compiler Bug: Struct not found, but should be present at this point",
                        );
                        data.current_path.insert(field_ty_name);
                        Self::build_dependency_graph(data, StructOrEnumInfo::Struct(si));
                        data.current_path.pop();
                    }
                    TypeInfo::Enum(_) => {
                        let ei = data.type_registry.resolve_enum(field_ty_name).expect(
                            "Compiler Bug: Enum not found, but should be present at this point",
                        );
                        data.current_path.insert(field_ty_name);
                        Self::build_dependency_graph(data, StructOrEnumInfo::Enum(ei));
                        data.current_path.pop();
                    }
                    _ => continue,
                };
            }
        }
    }

    pub fn run(
        &self,
        type_registry: &TypeRegistry,
        source_info: &SourceInfo,
    ) -> (TypeDependencyGraph, TypeDependencyAnalysisErrors) {
        let mut data = TransientData::new(source_info, type_registry);

        // Build dependency graph for all types
        for (name, info) in type_registry.all_structs() {
            data.current_path.clear();
            data.current_path.insert(*name);
            Self::build_dependency_graph(&mut data, StructOrEnumInfo::Struct(info));
        }

        for (name, info) in type_registry.all_enums() {
            data.current_path.clear();
            data.current_path.insert(*name);
            Self::build_dependency_graph(&mut data, StructOrEnumInfo::Enum(info));
        }

        (TypeDependencyGraph(data.dependency_graph), data.errors)
    }
}

impl Default for TypeDependencyAnalysis {
    fn default() -> Self {
        Self::new()
    }
}

struct TransientData<'a> {
    dependency_graph: UstrIndexMap<Vec<Ustr>>,
    errors: TypeDependencyAnalysisErrors,
    source_info: &'a SourceInfo,
    type_registry: &'a TypeRegistry,
    current_path: UstrIndexSet,
}

impl<'a> TransientData<'a> {
    fn new(source_info: &'a SourceInfo, type_registry: &'a TypeRegistry) -> Self {
        Self {
            dependency_graph: UstrIndexMap::default(),
            errors: TypeDependencyAnalysisErrors::new(),
            source_info,
            type_registry,
            current_path: UstrIndexSet::default(),
        }
    }
}
