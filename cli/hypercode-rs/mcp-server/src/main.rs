use hypercode_arg0::arg0_dispatch_or_else;
use hypercode_common::CliConfigOverrides;
use hypercode_mcp_server::run_main;

fn main() -> anyhow::Result<()> {
    arg0_dispatch_or_else(|hypercode_linux_sandbox_exe| async move {
        run_main(hypercode_linux_sandbox_exe, CliConfigOverrides::default()).await?;
        Ok(())
    })
}
