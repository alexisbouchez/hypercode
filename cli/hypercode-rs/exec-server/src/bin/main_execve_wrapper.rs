#[cfg(not(unix))]
fn main() {
    eprintln!("hypercode-execve-wrapper is only implemented for UNIX");
    std::process::exit(1);
}

#[cfg(unix)]
pub use hypercode_exec_server::main_execve_wrapper as main;
