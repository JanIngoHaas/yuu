mod binding;
mod expr;
pub mod pass_type_inference_impl;
mod pattern;
mod stmt;
mod structural;
mod types;

pub use binding::*;
pub use expr::*;
pub use pass_type_inference_impl::TypeInference;
pub use pass_type_inference_impl::TypeInferenceErrors;
pub use pattern::*;
pub use stmt::*;
pub use structural::*;
pub use types::*;

pub const MAX_SIMILAR_NAMES: u64 = 3;
pub const MIN_DST_SIMILAR_NAMES: u64 = 3;
