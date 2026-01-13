mod binding;
mod expr;
pub mod pass_type_inference_impl;
mod pattern;
mod stmt;
mod structural;
mod types;

pub(crate) use binding::*;
pub(crate) use expr::*;
pub use pass_type_inference_impl::TypeInference;
pub use pass_type_inference_impl::TypeInferenceErrors;
pub(crate) use stmt::*;
pub(crate) use structural::*;
pub(crate) use types::*;

pub const MAX_SIMILAR_NAMES: u64 = 3;
pub const MIN_DST_SIMILAR_NAMES: u64 = 3;
