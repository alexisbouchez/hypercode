# Configuration

For basic configuration instructions, see [this documentation](https://developers.openai.com/hypercode/config-basic).

For advanced configuration instructions, see [this documentation](https://developers.openai.com/hypercode/config-advanced).

For a full configuration reference, see [this documentation](https://developers.openai.com/hypercode/config-reference).

## Connecting to MCP servers

Hypercode can connect to MCP servers configured in `~/.hypercode/config.toml`. See the configuration reference for the latest MCP server options:

- https://developers.openai.com/hypercode/config-reference

## Notify

Hypercode can run a notification hook when the agent finishes a turn. See the configuration reference for the latest notification settings:

- https://developers.openai.com/hypercode/config-reference

## JSON Schema

The generated JSON Schema for `config.toml` lives at `hypercode-rs/core/config.schema.json`.

## Notices

Hypercode stores "do not show again" flags for some UI prompts under the `[notice]` table.

Ctrl+C/Ctrl+D quitting uses a ~1 second double-press hint (`ctrl + c again to quit`).
