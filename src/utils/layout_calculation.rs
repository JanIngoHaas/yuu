use crate::pass_type_inference::{PrimitiveType, StructInfo, TypeInfo};
use crate::pass_yir_lowering::Function;
use indexmap::IndexMap;
use std::collections::HashMap;
use std::hash::BuildHasherDefault;
use ustr::{IdentityHasher, Ustr};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetPlatform {
    Wasm,
}

#[derive(Debug, Clone)]
pub struct LayoutInfo {
    pub size: usize,
    pub alignment: usize,
}

#[derive(Debug, Clone)]
pub struct StructLayoutInfo {
    pub size: usize,
    pub alignment: usize,
    pub field_offsets: IndexMap<Ustr, usize, BuildHasherDefault<IdentityHasher>>, // field_name -> offset
}

#[derive(Debug, Clone)]
pub struct CallingConvention {
    pub name: &'static str,
    pub max_register_args: usize,
    pub stack_alignment: usize,
    pub return_in_memory_threshold: usize, // Structs larger than this are returned via pointer
}

impl TargetPlatform {
    pub fn current() -> Self {
        Self::Wasm
    }

    pub fn calling_convention(&self) -> CallingConvention {
        match self {
            Self::Wasm => CallingConvention {
                name: "WASM Stack",
                max_register_args: 0, // WASM uses stack for all arguments
                stack_alignment: 8,   // 8-byte alignment for better performance
                return_in_memory_threshold: 0, // WASM returns values directly on stack
            },
        }
    }
}

// Primitive type layout calculation
pub fn calc_i64_size() -> usize {
    8
}
pub fn calc_i64_alignment(_platform: TargetPlatform) -> usize {
    8
}

pub fn calc_f64_size() -> usize {
    8
}
pub fn calc_f64_alignment(_platform: TargetPlatform) -> usize {
    8
}

pub fn calc_f32_size() -> usize {
    4
}
pub fn calc_f32_alignment(_platform: TargetPlatform) -> usize {
    4
}

pub fn calc_bool_size() -> usize {
    1
}
pub fn calc_bool_alignment(_platform: TargetPlatform) -> usize {
    1
}

pub fn calc_pointer_size(_platform: TargetPlatform) -> usize {
    8
} // WASM Memory64 uses 64-bit addresses
pub fn calc_pointer_alignment(_platform: TargetPlatform) -> usize {
    8
}

pub fn calc_usize_size(_platform: TargetPlatform) -> usize {
    8 // Same as pointer size on 64-bit targets
}
pub fn calc_usize_alignment(_platform: TargetPlatform) -> usize {
    8 // Same as pointer alignment on 64-bit targets
}

pub fn calc_primitive_layout(primitive: PrimitiveType, platform: TargetPlatform) -> LayoutInfo {
    match primitive {
        PrimitiveType::I64 => LayoutInfo {
            size: calc_i64_size(),
            alignment: calc_i64_alignment(platform),
        },
        PrimitiveType::U64 => LayoutInfo {
            size: calc_i64_size(),                   // Same size as i64
            alignment: calc_i64_alignment(platform), // Same alignment as i64
        },
        PrimitiveType::F64 => LayoutInfo {
            size: calc_f64_size(),
            alignment: calc_f64_alignment(platform),
        },
        PrimitiveType::F32 => LayoutInfo {
            size: calc_f32_size(),
            alignment: calc_f32_alignment(platform),
        },
        PrimitiveType::Bool => LayoutInfo {
            size: calc_bool_size(),
            alignment: calc_bool_alignment(platform),
        },
        PrimitiveType::Nil => LayoutInfo {
            size: 0,
            alignment: 1,
        },
    }
}

pub fn calc_type_layout(ty: &TypeInfo, platform: TargetPlatform) -> LayoutInfo {
    match ty {
        TypeInfo::BuiltInPrimitive(prim) => calc_primitive_layout(*prim, platform),
        TypeInfo::Pointer(_) => LayoutInfo {
            size: calc_pointer_size(platform),
            alignment: calc_pointer_alignment(platform),
        },
        TypeInfo::Function(_) => LayoutInfo {
            size: calc_pointer_size(platform), // Function pointers are just pointers
            alignment: calc_pointer_alignment(platform),
        },
        TypeInfo::Struct(_struct_type) => {
            // This requires the struct to be fully resolved
            // For now, return a placeholder - will be implemented with calc_struct_layout
            LayoutInfo {
                size: 0,
                alignment: 1,
            }
        }
        TypeInfo::Enum(_e) => {
            todo!("Later...")
        }
        TypeInfo::Inactive => panic!("Cannot calculate layout for inactive type"),
        TypeInfo::Error => panic!("Cannot calculate layout for error type"),
    }
}

pub fn calc_struct_layout(struct_info: &StructInfo, platform: TargetPlatform) -> StructLayoutInfo {
    let mut current_offset = 0;
    let mut max_alignment = 1;
    let mut field_offsets = IndexMap::default();

    // Calculate layout for each field
    for (field_name, field_info) in &struct_info.fields {
        let field_layout = calc_type_layout(field_info.ty, platform);

        // Align the current offset to the field's alignment
        current_offset = align_up(current_offset, field_layout.alignment);

        // Record the field offset
        field_offsets.insert(*field_name, current_offset);

        // Update maximum alignment
        max_alignment = max_alignment.max(field_layout.alignment);

        // Advance offset by field size
        current_offset += field_layout.size;
    }

    // Align the final size to the struct's alignment
    let final_size = align_up(current_offset, max_alignment);

    StructLayoutInfo {
        size: final_size,
        alignment: max_alignment,
        field_offsets,
    }
}

// Helper function to align a value up to the nearest multiple of alignment
fn align_up(value: usize, alignment: usize) -> usize {
    (value + alignment - 1) & !(alignment - 1)
}

// Convenience functions for getting sizes without platform parameter (uses current platform)
pub fn calc_i64_size_current() -> usize {
    calc_i64_size()
}
pub fn calc_i64_alignment_current() -> usize {
    calc_i64_alignment(TargetPlatform::current())
}

pub fn calc_f64_size_current() -> usize {
    calc_f64_size()
}
pub fn calc_f64_alignment_current() -> usize {
    calc_f64_alignment(TargetPlatform::current())
}

pub fn calc_f32_size_current() -> usize {
    calc_f32_size()
}
pub fn calc_f32_alignment_current() -> usize {
    calc_f32_alignment(TargetPlatform::current())
}

pub fn calc_bool_size_current() -> usize {
    calc_bool_size()
}
pub fn calc_bool_alignment_current() -> usize {
    calc_bool_alignment(TargetPlatform::current())
}

pub fn calc_pointer_size_current() -> usize {
    calc_pointer_size(TargetPlatform::current())
}
pub fn calc_pointer_alignment_current() -> usize {
    calc_pointer_alignment(TargetPlatform::current())
}

pub fn calc_type_layout_current(ty: &TypeInfo) -> LayoutInfo {
    calc_type_layout(ty, TargetPlatform::current())
}

pub fn calc_struct_layout_current(struct_info: &StructInfo) -> StructLayoutInfo {
    calc_struct_layout(struct_info, TargetPlatform::current())
}

/// Calculate memory layout for all variables in a function
/// Returns a mapping from variable ID to memory offset (using 64-bit offsets for Memory64)
pub fn calculate_variable_memory_layout(func: &Function) -> HashMap<i64, u64> {
    let platform = TargetPlatform::current();
    let mut layout = HashMap::new();
    let mut offset = 0u64;

    // Allocate space for each variable declared in the function
    for var in func.calculate_var_decls() {
        layout.insert(var.id(), offset);
        let size = calc_type_layout(var.ty(), platform).size as u64;
        offset += size;
        // Align to platform alignment for better performance
        let alignment = platform.calling_convention().stack_alignment as u64;
        offset = align_up_u64(offset, alignment);
    }

    // Add parameters (they also need stack space in WASM)
    for param in &func.params {
        if let std::collections::hash_map::Entry::Vacant(e) = layout.entry(param.id()) {
            e.insert(offset);
            let size = calc_type_layout(param.ty(), platform).size as u64;
            offset += size;
            let alignment = platform.calling_convention().stack_alignment as u64;
            offset = align_up_u64(offset, alignment);
        }
    }

    layout
}

/// Get the size in bytes for a type (convenience function for WASM)
pub fn get_type_size_bytes(ty: &TypeInfo) -> u64 {
    calc_type_layout(ty, TargetPlatform::current()).size as u64
}

/// Helper function to align a u64 value up to the nearest multiple of alignment
fn align_up_u64(value: u64, alignment: u64) -> u64 {
    (value + alignment - 1) & !(alignment - 1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_sizes() {
        assert_eq!(calc_i64_size(), 8);
        assert_eq!(calc_f64_size(), 8);
        assert_eq!(calc_f32_size(), 4);
        assert_eq!(calc_bool_size(), 1);
    }

    #[test]
    fn test_alignments() {
        let wasm = TargetPlatform::Wasm;

        assert_eq!(calc_i64_alignment(wasm), 8);
        assert_eq!(calc_f64_alignment(wasm), 8);
        assert_eq!(calc_f32_alignment(wasm), 4);
        assert_eq!(calc_bool_alignment(wasm), 1);
        assert_eq!(calc_pointer_alignment(wasm), 8); // 64-bit pointers in WASM Memory64
    }

    #[test]
    fn test_align_up() {
        assert_eq!(align_up(0, 4), 0);
        assert_eq!(align_up(1, 4), 4);
        assert_eq!(align_up(4, 4), 4);
        assert_eq!(align_up(5, 4), 8);
        assert_eq!(align_up(7, 8), 8);
        assert_eq!(align_up(9, 8), 16);
    }

    #[test]
    fn test_calling_conventions() {
        let wasm = TargetPlatform::Wasm;
        let wasm_cc = wasm.calling_convention();

        assert_eq!(wasm_cc.max_register_args, 0); // WASM uses stack for all arguments
        assert_eq!(wasm_cc.stack_alignment, 8);
        assert_eq!(wasm_cc.return_in_memory_threshold, 0); // WASM returns values directly
        assert_eq!(wasm_cc.name, "WASM Stack");
    }

    #[test]
    fn test_wasm_pointer_size() {
        let wasm = TargetPlatform::Wasm;
        assert_eq!(calc_pointer_size(wasm), 8); // 64-bit addresses in WASM Memory64
        assert_eq!(calc_pointer_alignment(wasm), 8);
    }

    #[test]
    fn test_align_up_u64() {
        assert_eq!(align_up_u64(0, 4), 0);
        assert_eq!(align_up_u64(1, 4), 4);
        assert_eq!(align_up_u64(4, 4), 4);
        assert_eq!(align_up_u64(5, 4), 8);
        assert_eq!(align_up_u64(7, 8), 8);
        assert_eq!(align_up_u64(9, 8), 16);
    }
}
