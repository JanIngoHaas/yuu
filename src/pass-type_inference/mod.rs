mod binding;
mod expr;
mod pass_type_inference;
mod stmt;
mod structural;
mod types;
mod type_info;
mod type_registry;
mod binding_info;

pub use binding::*;
pub use expr::*;
pub use pass_type_inference::PassTypeInference;
pub use pass_type_inference::TypeInferenceErrors;
pub use stmt::*;
pub use structural::*;
pub use types::*;
pub use type_info::*;
pub use type_registry::*;
pub use binding_info::*;

pub const MAX_SIMILAR_NAMES: u64 = 3;
pub const MIN_DST_SIMILAR_NAMES: u64 = 3;
