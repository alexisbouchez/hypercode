# hypercode-core

This crate implements the business logic for Hypercode. It is designed to be used by the various Hypercode UIs written in Rust.

## Dependencies

Note that `hypercode-core` makes some assumptions about certain helper utilities being available in the environment. Currently, this support matrix is:

### macOS

Expects `/usr/bin/sandbox-exec` to be present.

When using the workspace-write sandbox policy, the Seatbelt profile allows
writes under the configured writable roots while keeping `.git` (directory or
pointer file), the resolved `gitdir:` target, and `.hypercode` read-only.

### Linux

Expects the binary containing `hypercode-core` to run the equivalent of `hypercode sandbox linux` (legacy alias: `hypercode debug landlock`) when `arg0` is `hypercode-linux-sandbox`. See the `hypercode-arg0` crate for details.

### All Platforms

Expects the binary containing `hypercode-core` to simulate the virtual `apply_patch` CLI when `arg1` is `--hypercode-run-as-apply-patch`. See the `hypercode-arg0` crate for details.
