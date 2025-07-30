pub mod utils;

// Pass modules
#[path = "pass-c_compilation/mod.rs"]
pub mod pass_c_compilation;
#[path = "pass-c_lowering/mod.rs"]
pub mod pass_c_lowering;
#[path = "pass-diagnostics/mod.rs"]
pub mod pass_diagnostics;
#[path = "pass-parse/mod.rs"]
pub mod pass_parse;
#[path = "pass-print_yir/mod.rs"]
pub mod pass_print_yir;
#[path = "pass-type_inference/mod.rs"]
pub mod pass_type_inference;
#[path = "pass-yir_lowering/mod.rs"]
pub mod pass_yir_lowering;
