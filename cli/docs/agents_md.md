# AGENTS.md

For information about AGENTS.md, see [this documentation](https://developers.openai.com/hypercode/guides/agents-md).

## Hierarchical agents message

When the `child_agents_md` feature flag is enabled (via `[features]` in `config.toml`), Hypercode appends additional guidance about AGENTS.md scope and precedence to the user instructions message and emits that message even when no AGENTS.md is present.
