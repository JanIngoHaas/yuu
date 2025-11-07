use crate::pass_type_inference::{TypeInfo, PrimitiveType};
use crate::pass_type_inference::{StructInfo, EnumInfo, TypeRegistry};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LayoutInfo {
    pub size: usize,
    pub alignment: usize,
}

impl LayoutInfo {
    pub fn new(size: usize, alignment: usize) -> Self {
        Self { size, alignment }
    }
}

#[derive(Debug, Clone)]
pub struct FieldLayout {
    pub offset: usize,
    pub layout: LayoutInfo,
}

#[derive(Debug, Clone)]
pub struct StructLayout {
    pub fields: Vec<FieldLayout>,
    pub total_size: usize,
    pub total_alignment: usize,
}

#[derive(Debug, Clone)]
pub struct EnumLayout {
    pub discriminant_size: usize,
    pub discriminant_alignment: usize,
    pub payload_offset: usize,
    pub max_variant_size: usize,
    pub total_size: usize,
    pub total_alignment: usize,
}

pub fn calculate_primitive_layout(prim: PrimitiveType) -> LayoutInfo {
    match prim {
        PrimitiveType::I64 | PrimitiveType::U64 => LayoutInfo::new(8, 8),
        PrimitiveType::F32 => LayoutInfo::new(4, 4),
        PrimitiveType::F64 => LayoutInfo::new(8, 8),
        PrimitiveType::Bool => LayoutInfo::new(1, 1),
        PrimitiveType::Nil => LayoutInfo::new(0, 1),
    }
}

pub fn calculate_type_layout(ty: &TypeInfo, type_registry: &TypeRegistry) -> LayoutInfo {
    match ty {
        TypeInfo::BuiltInPrimitive(prim) => calculate_primitive_layout(*prim),
        TypeInfo::Pointer(_) => LayoutInfo::new(8, 8),
        TypeInfo::Function(_) => LayoutInfo::new(8, 8),
        TypeInfo::Struct(struct_type) => {
            let struct_info = type_registry.resolve_struct(struct_type.name)
                .unwrap_or_else(|| panic!("Compiler Error: Unknown struct type: {}", struct_type.name));
            let layout = calculate_struct_layout(struct_info, type_registry);
            LayoutInfo::new(layout.total_size, layout.total_alignment)
        },
        TypeInfo::Enum(enum_type) => {
            let enum_info = type_registry.resolve_enum(enum_type.name)
                .unwrap_or_else(|| panic!("Compiler Error: Unknown enum type: {}", enum_type.name));
            let layout = calculate_enum_layout(enum_info, type_registry);
            LayoutInfo::new(layout.total_size, layout.total_alignment)
        },
        TypeInfo::Inactive | TypeInfo::Error => LayoutInfo::new(0, 1),
    }
}

pub fn calculate_struct_layout(struct_info: &StructInfo, type_registry: &TypeRegistry) -> StructLayout {
    let mut fields = Vec::new();
    let mut current_offset = 0;
    let mut struct_alignment = 1;

    for field_info in struct_info.fields.values() {
        let field_layout = calculate_type_layout(field_info.ty, type_registry);

        struct_alignment = struct_alignment.max(field_layout.alignment);
        current_offset = align_to(current_offset, field_layout.alignment);

        fields.push(FieldLayout {
            offset: current_offset,
            layout: field_layout,
        });

        current_offset += field_layout.size;
    }

    let total_size = align_to(current_offset, struct_alignment);

    StructLayout {
        fields,
        total_size,
        total_alignment: struct_alignment,
    }
}

pub fn calculate_enum_layout(enum_info: &EnumInfo, type_registry: &TypeRegistry) -> EnumLayout {
    let discriminant_size = 8;
    let discriminant_alignment = 8;

    let mut max_variant_size = 0;
    let mut max_variant_alignment = 1;

    for variant_info in enum_info.variants.values() {
        if let Some(variant_type) = variant_info.variant {
            let layout = calculate_type_layout(variant_type, type_registry);
            max_variant_size = max_variant_size.max(layout.size);
            max_variant_alignment = max_variant_alignment.max(layout.alignment);
        }
    }

    let payload_offset = align_to(discriminant_size, max_variant_alignment);
    let total_alignment = discriminant_alignment.max(max_variant_alignment);
    let total_size = align_to(payload_offset + max_variant_size, total_alignment);

    EnumLayout {
        discriminant_size,
        discriminant_alignment,
        payload_offset,
        max_variant_size,
        total_size,
        total_alignment,
    }
}

fn align_to(offset: usize, alignment: usize) -> usize {
    (offset + alignment - 1) & !(alignment - 1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_layouts() {
        assert_eq!(calculate_primitive_layout(PrimitiveType::I64), LayoutInfo::new(8, 8));
        assert_eq!(calculate_primitive_layout(PrimitiveType::F32), LayoutInfo::new(4, 4));
        assert_eq!(calculate_primitive_layout(PrimitiveType::Bool), LayoutInfo::new(1, 1));
    }

    #[test]
    fn test_align_to() {
        assert_eq!(align_to(0, 4), 0);
        assert_eq!(align_to(1, 4), 4);
        assert_eq!(align_to(3, 4), 4);
        assert_eq!(align_to(4, 4), 4);
        assert_eq!(align_to(5, 8), 8);
    }
}