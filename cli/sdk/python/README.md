# Hypercode Python SDK

A Python SDK for the Hypercode AI coding assistant REST API.

## Installation

```bash
pip install hypercode-sdk
```

## Quick Start

### Synchronous Usage

```python
from hypercode import HypercodeClient, Thread

# Create a client
client = HypercodeClient(base_url="http://localhost:8080")

# Create a thread and run a turn
thread = Thread(client, working_directory="/path/to/project")
response = thread.run("Explain the main function in this codebase")

# Print the response
for item in response.output:
    if item.type == "message":
        print(item.content)
```

### Streaming

```python
from hypercode import HypercodeClient, Thread

client = HypercodeClient(base_url="http://localhost:8080")
thread = Thread(client)

for event in thread.run_stream("Write a hello world function"):
    if event.type == "message_delta":
        print(event.content, end="", flush=True)
    elif event.type == "turn_completed":
        print("\nDone!")
```

### Async Usage

```python
import asyncio
from hypercode import AsyncHypercodeClient

async def main():
    async with AsyncHypercodeClient(base_url="http://localhost:8080") as client:
        # Create a thread
        thread = await client.create_thread()

        # Start a streaming turn
        async for event in client.start_turn_stream(
            thread.thread_id,
            [{"type": "message", "content": "Hello!"}]
        ):
            print(event)

asyncio.run(main())
```

## API Reference

### HypercodeClient

The main client class for interacting with the REST API.

#### Methods

- `health()` - Check API health
- `create_thread(...)` - Create a new thread
- `list_threads()` - List all threads
- `get_thread(thread_id)` - Get a specific thread
- `start_turn(thread_id, input_items)` - Start a turn (non-streaming)
- `start_turn_stream(thread_id, input_items)` - Start a turn with streaming
- `interrupt_turn(thread_id)` - Interrupt the current turn
- `list_models()` - List available models
- `read_config()` - Read configuration
- `write_config(key, value)` - Write configuration
- `get_account()` - Get account info
- `get_rate_limits()` - Get rate limits
- `login(api_key, provider)` - Login with API key
- `logout()` - Logout

### Thread

High-level interface for conversation threads.

#### Methods

- `run(message, images)` - Send a message and get response
- `run_stream(message, images)` - Send a message and stream response
- `interrupt()` - Interrupt current turn
- `fork(turn_index)` - Fork this thread
- `rollback(turn_index)` - Rollback to a specific turn
- `archive()` - Archive (delete) this thread

## License

Apache-2.0
