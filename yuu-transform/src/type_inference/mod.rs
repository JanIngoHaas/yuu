mod binding;
mod expr;
mod pass_type_inference;
mod stmt;
mod structural;
mod types;

pub use binding::*;
pub use expr::*;
pub use pass_type_inference::PassTypeInference;
pub use pass_type_inference::TypeInferenceErrors;
pub use stmt::*;
pub use structural::*;
pub use types::*;

pub const MAX_SIMILAR_NAMES: u64 = 3;
pub const MIN_DST_SIMILAR_NAMES: u64 = 3;
