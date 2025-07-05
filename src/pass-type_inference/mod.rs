mod binding;
mod binding_info;
mod expr;
pub mod pass_type_inference_impl;
mod stmt;
mod structural;
mod type_info;
mod type_registry;
mod types;

pub use binding::*;
pub use binding_info::*;
pub use expr::*;
pub use pass_type_inference_impl::TypeInference;
pub use pass_type_inference_impl::TypeInferenceErrors;
pub use stmt::*;
pub use structural::*;
pub use type_info::*;
pub use type_registry::*;
pub use types::*;

pub const MAX_SIMILAR_NAMES: u64 = 3;
pub const MIN_DST_SIMILAR_NAMES: u64 = 3;
