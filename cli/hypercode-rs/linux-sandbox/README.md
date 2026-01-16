# hypercode-linux-sandbox

This crate is responsible for producing:

- a `hypercode-linux-sandbox` standalone executable for Linux that is bundled with the Node.js version of the Hypercode CLI
- a lib crate that exposes the business logic of the executable as `run_main()` so that
  - the `hypercode-exec` CLI can check if its arg0 is `hypercode-linux-sandbox` and, if so, execute as if it were `hypercode-linux-sandbox`
  - this should also be true of the `hypercode` multitool CLI

## Git safety mounts (Linux)

When the sandbox policy allows workspace writes, the Linux sandbox uses a user
namespace plus a mount namespace to bind-mount sensitive subpaths read-only
before applying Landlock rules. This keeps Git and Hypercode metadata immutable
while still allowing writes to other workspace files, including worktree setups
where `.git` is a pointer file.

Protected subpaths under each writable root include:

- `.git` (directory or pointer file)
- the resolved `gitdir:` target when `.git` is a pointer file
- `.hypercode` when present

### How this plays with Landlock

Mount permissions and Landlock intersect: if a bind mount is read-only, writes
are denied even if Landlock would allow them. For that reason, the sandbox sets
up the read-only mounts *before* calling `landlock_restrict_self()` and then
applies Landlock rules on top.

### Quick manual test

Run the sandbox directly with a workspace-write policy (from a Git repository
root):

```bash
hypercode-linux-sandbox \
  --sandbox-policy-cwd "$PWD" \
  --sandbox-policy '{"type":"workspace-write"}' \
  -- bash -lc '
set -euo pipefail

echo "should fail" > .git/config && exit 1 || true
echo "should fail" > .git/hooks/pre-commit && exit 1 || true
echo "should fail" > .git/index.lock && exit 1 || true
echo "should fail" > .hypercode/config.toml && exit 1 || true
echo "ok" > sandbox-write-test.txt
'
```

Expected behavior:

- Writes to `.git/config` fail with `Read-only file system`.
- Creating or modifying files under `.git/hooks/` fails.
- Writing `.git/index.lock` fails (since `.git` is read-only).
- Writes under `.hypercode/` fail when the directory exists.
- Writing a normal repo file succeeds.
