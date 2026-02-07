use crate::utils::TypeRegistry;
use crate::utils::type_info_table::TypeInfo;

/// How to lower a struct parameter to LLVM
#[derive(Debug, Clone, Copy)]
pub enum ParamLowering {
    /// Pass directly (primitives, pointers)
    Direct,
    /// Coerce to a single integer (≤8 bytes)
    CoerceToInt { bits: u32 },
    /// Coerce to vector type (floats only)
    CoerceToVec2Float,
    /// Split into two parameters (9-16 bytes)
    Split { lo_bits: u32, hi_bits: u32 },
    /// Split into INTEGER + SSE scalar parameters (9-16 bytes)
    SplitIntFloat { lo_bits: u32, hi_float_bits: u32 },
    /// Split into SSE + INTEGER scalar parameters (9-16 bytes)
    SplitFloatInt { lo_float_bits: u32, hi_bits: u32 },
    /// Pass by pointer with byval (>16 bytes)
    ByVal,
}

/// How to lower a return value
#[derive(Debug, Clone, Copy)]
pub enum ReturnLowering {
    /// Return directly
    Direct,
    /// Coerce to integer
    CoerceToInt { bits: u32 },
    /// Coerce to vector type (floats only)
    CoerceToVec2Float,
    /// Return as pair { iN, iN }
    Pair { lo_bits: u32, hi_bits: u32 },
    /// Return as pair { iN, f32/f64 }
    PairIntFloat { lo_bits: u32, hi_float_bits: u32 },
    /// Return as pair { f32/f64, iN }
    PairFloatInt { lo_float_bits: u32, hi_bits: u32 },
    /// Use sret (hidden first parameter)
    Sret,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionAbiKind {
    Internal,
    C,
}

#[derive(Debug, Clone)]
pub struct AbiSignature {
    pub kind: FunctionAbiKind,
    pub return_type: &'static TypeInfo,
    pub return_lowering: ReturnLowering,
    pub param_lowerings: Vec<ParamLowering>,
}

impl AbiSignature {
    // implemented by @gpt-5.3-codex
    pub fn uses_sret(&self) -> bool {
        matches!(self.return_lowering, ReturnLowering::Sret)
    }
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
