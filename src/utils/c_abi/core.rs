use crate::utils::TypeRegistry;
use crate::utils::type_info_table::TypeInfo;
use inkwell::context::Context;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};

/// How to lower a struct parameter to LLVM
#[derive(Debug, Clone)]
pub enum ParamLowering {
    /// Pass directly (primitives, pointers)
    Direct,
    /// Coerce to a single integer (≤8 bytes)
    CoerceToInt { bits: u32 },
    /// Coerce to vector type (floats only)
    CoerceToVec2Float,
    /// Split into two parameters (9-16 bytes)
    Split { lo_bits: u32, hi_bits: u32 },
    /// Pass by pointer with byval (>16 bytes)
    ByVal,
}

/// How to lower a return value
#[derive(Debug, Clone)]
pub enum ReturnLowering {
    /// Return directly
    Direct,
    /// Coerce to integer
    CoerceToInt { bits: u32 },
    /// Return as pair { iN, iN }
    Pair { lo_bits: u32, hi_bits: u32 },
    /// Use sret (hidden first parameter)
    Sret,
}

/// Trait representing a C ABI lowering strategy for a specific platform/architecture
pub trait CAbiLowerer {
    fn classify_param(&self, ty: &'static TypeInfo, type_registry: &TypeRegistry) -> ParamLowering;
    fn classify_return(
        &self,
        ty: &'static TypeInfo,
        type_registry: &TypeRegistry,
    ) -> ReturnLowering;
}

/// Convert parameter lowering to LLVM types
pub fn param_lowering_to_llvm_types<'ctx>(
    lowering: &ParamLowering,
    context: &'ctx Context,
) -> Vec<BasicMetadataTypeEnum<'ctx>> {
    match lowering {
        ParamLowering::Direct => vec![],
        ParamLowering::CoerceToInt { bits } => vec![context.custom_width_int_type(*bits).into()],
        ParamLowering::CoerceToVec2Float => vec![context.f32_type().vec_type(2).into()],
        ParamLowering::Split { lo_bits, hi_bits } => vec![
            context.custom_width_int_type(*lo_bits).into(),
            context.custom_width_int_type(*hi_bits).into(),
        ],
        ParamLowering::ByVal => vec![],
    }
}

/// Convert return lowering to LLVM type
pub fn return_lowering_to_llvm_type<'ctx>(
    lowering: &ReturnLowering,
    context: &'ctx Context,
) -> Option<BasicTypeEnum<'ctx>> {
    match lowering {
        ReturnLowering::Direct => None,
        ReturnLowering::CoerceToInt { bits } => Some(context.custom_width_int_type(*bits).into()),
        ReturnLowering::Pair { lo_bits, hi_bits } => Some(
            context
                .struct_type(
                    &[
                        context.custom_width_int_type(*lo_bits).into(),
                        context.custom_width_int_type(*hi_bits).into(),
                    ],
                    false,
                )
                .into(),
        ),
        ReturnLowering::Sret => None,
    }
}
