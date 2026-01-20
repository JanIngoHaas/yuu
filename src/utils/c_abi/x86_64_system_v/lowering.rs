use super::super::core::{CAbiLowerer, ParamLowering, ReturnLowering};
use crate::utils::type_info_table::TypeInfo;
use crate::utils::{TypeRegistry, c_packing};

// TODO: Check this again
/// System V x86_64 ABI lowerer
pub struct X86_64SystemVAbiLowerer;

impl CAbiLowerer for X86_64SystemVAbiLowerer {
    fn classify_param(&self, ty: &'static TypeInfo, type_registry: &TypeRegistry) -> ParamLowering {
        match ty {
            // Primitives and pointers pass directly
            TypeInfo::BuiltInPrimitive(_) | TypeInfo::Pointer(_) | TypeInfo::Function(_) => {
                ParamLowering::Direct
            }

            // Structs need special handling
            TypeInfo::Struct(s) => {
                let struct_info = type_registry
                    .resolve_struct(s.name)
                    .expect("Unknown struct type");
                let layout = c_packing::calculate_struct_layout(struct_info, type_registry);

                classify_aggregate_param(layout.total_size)
            }

            // Unions
            TypeInfo::Union(u) => {
                let union_info = type_registry
                    .resolve_union(u.name)
                    .expect("Unknown union type");
                let layout = c_packing::calculate_union_layout(union_info, type_registry);

                // Unions follow same rules as structs
                match layout.total_size {
                    0 => ParamLowering::Direct,
                    1..=8 => ParamLowering::CoerceToInt {
                        bits: (layout.total_size * 8) as u32,
                    },
                    9..=16 => ParamLowering::Split {
                        lo_bits: 64,
                        hi_bits: ((layout.total_size - 8) * 8) as u32,
                    },
                    _ => ParamLowering::ByVal,
                }
            }

            TypeInfo::Error | TypeInfo::Unknown => {
                panic!("Cannot lower error/unknown type")
            }
        }
    }

    fn classify_return(
        &self,
        ty: &'static TypeInfo,
        type_registry: &TypeRegistry,
    ) -> ReturnLowering {
        match ty {
            TypeInfo::BuiltInPrimitive(_) | TypeInfo::Pointer(_) | TypeInfo::Function(_) => {
                ReturnLowering::Direct
            }

            TypeInfo::Struct(s) => {
                let struct_info = type_registry
                    .resolve_struct(s.name)
                    .expect("Unknown struct type");
                let layout = c_packing::calculate_struct_layout(struct_info, type_registry);

                match layout.total_size {
                    0 => ReturnLowering::Direct,
                    1..=8 => ReturnLowering::CoerceToInt {
                        bits: (layout.total_size * 8) as u32,
                    },
                    9..=16 => ReturnLowering::Pair {
                        lo_bits: 64,
                        hi_bits: ((layout.total_size - 8) * 8) as u32,
                    },
                    _ => ReturnLowering::Sret,
                }
            }

            TypeInfo::Union(u) => {
                let union_info = type_registry
                    .resolve_union(u.name)
                    .expect("Unknown union type");
                let layout = c_packing::calculate_union_layout(union_info, type_registry);

                match layout.total_size {
                    0 => ReturnLowering::Direct,
                    1..=8 => ReturnLowering::CoerceToInt {
                        bits: (layout.total_size * 8) as u32,
                    },
                    9..=16 => ReturnLowering::Pair {
                        lo_bits: 64,
                        hi_bits: ((layout.total_size - 8) * 8) as u32,
                    },
                    _ => ReturnLowering::Sret,
                }
            }

            TypeInfo::Error | TypeInfo::Unknown => {
                panic!("Cannot lower error/unknown type")
            }
        }
    }
}

fn classify_aggregate_param(size: usize) -> ParamLowering {
    match size {
        0 => ParamLowering::Direct,
        1..=8 => ParamLowering::CoerceToInt {
            bits: (size * 8) as u32,
        },
        9..=16 => ParamLowering::Split {
            lo_bits: 64,
            hi_bits: ((size - 8) * 8) as u32,
        },
        _ => ParamLowering::ByVal,
    }
}
