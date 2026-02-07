use super::super::core::{CAbiLowerer, ParamLowering, ReturnLowering};
use crate::utils::type_info_table::TypeInfo;
use crate::utils::{TypeRegistry, c_packing};

// TODO: Check this again
/// System V x86_64 ABI lowerer
pub struct X86_64SystemVAbiLowerer;

impl CAbiLowerer for X86_64SystemVAbiLowerer {
    // partially implemented by @gpt-5.3-codex
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
                classify_struct_param(struct_info, type_registry)
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

    // partially implemented by @gpt-5.3-codex
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
                classify_struct_return(struct_info, type_registry)
            }

            TypeInfo::Union(u) => {
                let union_info = type_registry
                    .resolve_union(u.name)
                    .expect("Unknown union type");
                let layout = c_packing::calculate_union_layout(union_info, type_registry);

                classify_aggregate_return(layout.total_size)
            }

            TypeInfo::Error | TypeInfo::Unknown => {
                panic!("Cannot lower error/unknown type")
            }
        }
    }
}

// partially implemented by @gpt-5.3-codex
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

// implemented by @gpt-5.3-codex
fn classify_aggregate_return(size: usize) -> ReturnLowering {
    match size {
        0 => ReturnLowering::Direct,
        1..=8 => ReturnLowering::CoerceToInt {
            bits: (size * 8) as u32,
        },
        9..=16 => ReturnLowering::Pair {
            lo_bits: 64,
            hi_bits: ((size - 8) * 8) as u32,
        },
        _ => ReturnLowering::Sret,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum EightbyteClass {
    Integer,
    Sse,
}

#[derive(Clone, Copy, Debug)]
struct EightbyteInfo {
    class: EightbyteClass,
    float_bits: Option<u32>,
    float_count: u8,
    multiple_floats: bool,
}

impl EightbyteInfo {
    fn new(class: EightbyteClass, float_bits: Option<u32>) -> Self {
        Self {
            class,
            float_bits,
            float_count: u8::from(float_bits.is_some()),
            multiple_floats: false,
        }
    }

    fn merge(&mut self, class: EightbyteClass, float_bits: Option<u32>) {
        // SysV merge rule: INTEGER dominates SSE.
        if matches!(self.class, EightbyteClass::Integer) || matches!(class, EightbyteClass::Integer)
        {
            self.class = EightbyteClass::Integer;
        } else {
            self.class = EightbyteClass::Sse;
        }

        if let Some(bits) = float_bits {
            self.float_count = self.float_count.saturating_add(1);
            match self.float_bits {
                None => self.float_bits = Some(bits),
                Some(existing) if existing == bits => {}
                Some(_) => self.multiple_floats = true,
            }
        }
    }
}

// implemented by @gpt-5.3-codex
fn classify_struct_single_eightbyte(
    struct_info: &crate::utils::StructInfo,
    type_registry: &TypeRegistry,
) -> Option<EightbyteInfo> {
    let layout = c_packing::calculate_struct_layout(struct_info, type_registry);
    if !(1..=8).contains(&layout.total_size) {
        return None;
    }

    let mut eb: Option<EightbyteInfo> = None;
    for ((_, field), field_layout) in struct_info.fields.iter().zip(layout.fields.iter()) {
        let size = field_layout.layout.size;
        if size == 0 {
            continue;
        }

        let (class, float_bits) = field_eightbyte_class(field.ty)?;
        let start_eb = field_layout.offset / 8;
        let end_eb = (field_layout.offset + size - 1) / 8;
        if start_eb != 0 || end_eb != 0 {
            return None;
        }

        match &mut eb {
            Some(info) => info.merge(class, float_bits),
            None => eb = Some(EightbyteInfo::new(class, float_bits)),
        }
    }

    eb
}

fn field_eightbyte_class(ty: &'static TypeInfo) -> Option<(EightbyteClass, Option<u32>)> {
    match ty {
        TypeInfo::BuiltInPrimitive(prim) => match prim {
            crate::utils::type_info_table::PrimitiveType::F32 => {
                Some((EightbyteClass::Sse, Some(32)))
            }
            crate::utils::type_info_table::PrimitiveType::F64 => {
                Some((EightbyteClass::Sse, Some(64)))
            }
            crate::utils::type_info_table::PrimitiveType::I8
            | crate::utils::type_info_table::PrimitiveType::U8
            | crate::utils::type_info_table::PrimitiveType::I64
            | crate::utils::type_info_table::PrimitiveType::U64
            | crate::utils::type_info_table::PrimitiveType::Bool
            | crate::utils::type_info_table::PrimitiveType::Nil => {
                Some((EightbyteClass::Integer, None))
            }
        },
        TypeInfo::Pointer(_) | TypeInfo::Function(_) => Some((EightbyteClass::Integer, None)),
        // Nested composable types need full recursive classifier; fall back for now.
        TypeInfo::Struct(_) | TypeInfo::Union(_) | TypeInfo::Error | TypeInfo::Unknown => None,
    }
}

fn classify_struct_two_eightbytes(
    struct_info: &crate::utils::StructInfo,
    type_registry: &TypeRegistry,
) -> Option<(EightbyteInfo, EightbyteInfo)> {
    let layout = c_packing::calculate_struct_layout(struct_info, type_registry);
    if !(9..=16).contains(&layout.total_size) {
        return None;
    }

    let mut eb0: Option<EightbyteInfo> = None;
    let mut eb1: Option<EightbyteInfo> = None;

    for ((_, field), field_layout) in struct_info.fields.iter().zip(layout.fields.iter()) {
        let size = field_layout.layout.size;
        if size == 0 {
            continue;
        }

        let (class, float_bits) = field_eightbyte_class(field.ty)?;
        let start_eb = field_layout.offset / 8;
        let end_eb = (field_layout.offset + size - 1) / 8;

        // Crossing eightbyte boundaries requires full SysV recursive handling.
        if start_eb != end_eb || end_eb > 1 {
            return None;
        }

        let slot = if start_eb == 0 { &mut eb0 } else { &mut eb1 };
        match slot {
            Some(info) => info.merge(class, float_bits),
            None => *slot = Some(EightbyteInfo::new(class, float_bits)),
        }
    }

    Some((eb0?, eb1?))
}

// partially implemented by @gpt-5.3-codex
fn classify_struct_param(
    struct_info: &crate::utils::StructInfo,
    type_registry: &TypeRegistry,
) -> ParamLowering {
    let layout = c_packing::calculate_struct_layout(struct_info, type_registry);
    if let Some(eb) = classify_struct_single_eightbyte(struct_info, type_registry)
        && eb.class == EightbyteClass::Sse
        && eb.float_bits == Some(32)
        && eb.float_count == 2
        && !eb.multiple_floats
        && layout.total_size == 8
    {
        return ParamLowering::CoerceToVec2Float;
    }

    if let Some((eb0, eb1)) = classify_struct_two_eightbytes(struct_info, type_registry) {
        match (eb0.class, eb1.class) {
            (EightbyteClass::Integer, EightbyteClass::Sse)
                if eb1.float_bits.is_some() && !eb1.multiple_floats =>
            {
                return ParamLowering::SplitIntFloat {
                    lo_bits: 64,
                    hi_float_bits: eb1.float_bits.unwrap(),
                };
            }
            (EightbyteClass::Sse, EightbyteClass::Integer)
                if eb0.float_bits.is_some() && !eb0.multiple_floats =>
            {
                return ParamLowering::SplitFloatInt {
                    lo_float_bits: eb0.float_bits.unwrap(),
                    hi_bits: 64,
                };
            }
            _ => {}
        }
    }

    classify_aggregate_param(layout.total_size)
}

// partially implemented by @gpt-5.3-codex
fn classify_struct_return(
    struct_info: &crate::utils::StructInfo,
    type_registry: &TypeRegistry,
) -> ReturnLowering {
    let layout = c_packing::calculate_struct_layout(struct_info, type_registry);
    if let Some(eb) = classify_struct_single_eightbyte(struct_info, type_registry)
        && eb.class == EightbyteClass::Sse
        && eb.float_bits == Some(32)
        && eb.float_count == 2
        && !eb.multiple_floats
        && layout.total_size == 8
    {
        return ReturnLowering::CoerceToVec2Float;
    }

    if let Some((eb0, eb1)) = classify_struct_two_eightbytes(struct_info, type_registry) {
        match (eb0.class, eb1.class) {
            (EightbyteClass::Integer, EightbyteClass::Sse)
                if eb1.float_bits.is_some() && !eb1.multiple_floats =>
            {
                return ReturnLowering::PairIntFloat {
                    lo_bits: 64,
                    hi_float_bits: eb1.float_bits.unwrap(),
                };
            }
            (EightbyteClass::Sse, EightbyteClass::Integer)
                if eb0.float_bits.is_some() && !eb0.multiple_floats =>
            {
                return ReturnLowering::PairFloatInt {
                    lo_float_bits: eb0.float_bits.unwrap(),
                    hi_bits: 64,
                };
            }
            _ => {}
        }
    }

    classify_aggregate_return(layout.total_size)
}
