use crate::protocol::SandboxPolicy;
use std::collections::HashMap;
use std::path::Path;

/// Kill switch for the elevated sandbox NUX on Windows.
///
/// When false, revert to the previous sandbox NUX, which only
/// prompts users to enable the legacy sandbox feature.
pub const ELEVATED_SANDBOX_NUX_ENABLED: bool = true;

#[cfg(target_os = "windows")]
pub fn sandbox_setup_is_complete(hypercode_home: &Path) -> bool {
    hypercode_windows_sandbox::sandbox_setup_is_complete(hypercode_home)
}

#[cfg(not(target_os = "windows"))]
pub fn sandbox_setup_is_complete(_hypercode_home: &Path) -> bool {
    false
}

#[cfg(target_os = "windows")]
pub fn run_elevated_setup(
    policy: &SandboxPolicy,
    policy_cwd: &Path,
    command_cwd: &Path,
    env_map: &HashMap<String, String>,
    hypercode_home: &Path,
) -> anyhow::Result<()> {
    hypercode_windows_sandbox::run_elevated_setup(
        policy,
        policy_cwd,
        command_cwd,
        env_map,
        hypercode_home,
        None,
        None,
    )
}

#[cfg(not(target_os = "windows"))]
pub fn run_elevated_setup(
    _policy: &SandboxPolicy,
    _policy_cwd: &Path,
    _command_cwd: &Path,
    _env_map: &HashMap<String, String>,
    _hypercode_home: &Path,
) -> anyhow::Result<()> {
    anyhow::bail!("elevated Windows sandbox setup is only supported on Windows")
}
