pub mod core;
pub mod x86_64_system_v;

pub use core::*;
pub use x86_64_system_v::*;

/// Helper for C ABI selection
pub struct CAbi;

#[cfg(all(target_arch = "x86_64", any(target_os = "linux", target_os = "macos")))]
static HOST_ABI: X86_64SystemVAbiLowerer = X86_64SystemVAbiLowerer;

impl CAbi {
    /// Returns the C ABI lowerer for the current host platform
    pub fn get_host_abi() -> &'static dyn CAbiLowerer {
        #[cfg(all(target_arch = "x86_64", any(target_os = "linux", target_os = "macos")))]
        {
            &HOST_ABI
        }
        #[cfg(not(all(target_arch = "x86_64", any(target_os = "linux", target_os = "macos"))))]
        {
            panic!("Unsupported host platform for C ABI lowering")
        }
    }
}
