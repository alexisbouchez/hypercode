"""
HTTP client for the Hypercode REST API.
"""

from __future__ import annotations

import json
from typing import Any, AsyncIterator, Iterator, Optional

import httpx
from httpx_sse import aconnect_sse, connect_sse

from hypercode.models import (
    AccountInfo,
    Config,
    HealthResponse,
    InputItem,
    ModelInfo,
    RateLimits,
    StreamEvent,
    ThreadInfo,
    TurnResponse,
)


class HypercodeClient:
    """
    Synchronous client for the Hypercode REST API.

    Args:
        base_url: Base URL for the Hypercode REST API server.
        api_key: Optional API key for authentication.
        timeout: Request timeout in seconds.
    """

    def __init__(
        self,
        base_url: str = "http://localhost:8080",
        api_key: Optional[str] = None,
        timeout: float = 30.0,
    ) -> None:
        self.base_url = base_url.rstrip("/")
        self.api_key = api_key
        self.timeout = timeout
        self._client: Optional[httpx.Client] = None

    def _get_client(self) -> httpx.Client:
        if self._client is None:
            headers = {"Content-Type": "application/json"}
            if self.api_key:
                headers["Authorization"] = f"Bearer {self.api_key}"
            self._client = httpx.Client(
                base_url=self.base_url,
                headers=headers,
                timeout=self.timeout,
            )
        return self._client

    def close(self) -> None:
        """Close the HTTP client."""
        if self._client:
            self._client.close()
            self._client = None

    def __enter__(self) -> "HypercodeClient":
        return self

    def __exit__(self, *args: Any) -> None:
        self.close()

    def _request(self, method: str, path: str, **kwargs: Any) -> Any:
        client = self._get_client()
        response = client.request(method, path, **kwargs)
        response.raise_for_status()
        return response.json()

    # Health
    def health(self) -> HealthResponse:
        """Check API health."""
        data = self._request("GET", "/health")
        return HealthResponse(**data)

    # Threads
    def create_thread(
        self,
        working_directory: Optional[str] = None,
        model: Optional[str] = None,
        instructions: Optional[str] = None,
    ) -> ThreadInfo:
        """Create a new thread."""
        payload: dict[str, Any] = {}
        if working_directory:
            payload["working_directory"] = working_directory
        if model:
            payload["model"] = model
        if instructions:
            payload["instructions"] = instructions
        data = self._request("POST", "/threads", json=payload)
        return ThreadInfo(thread_id=data["thread_id"], status="active", created_at=data.get("created_at"))

    def list_threads(self) -> list[ThreadInfo]:
        """List all threads."""
        data = self._request("GET", "/threads")
        return [ThreadInfo(**t) for t in data.get("threads", [])]

    def get_thread(self, thread_id: str) -> ThreadInfo:
        """Get a specific thread."""
        data = self._request("GET", f"/threads/{thread_id}")
        return ThreadInfo(**data)

    def resume_thread(self, thread_id: str, model: Optional[str] = None) -> ThreadInfo:
        """Resume a thread."""
        payload: dict[str, Any] = {}
        if model:
            payload["model"] = model
        data = self._request("POST", f"/threads/{thread_id}/resume", json=payload)
        return ThreadInfo(**data)

    def fork_thread(self, thread_id: str, turn_index: Optional[int] = None) -> ThreadInfo:
        """Fork a thread."""
        payload: dict[str, Any] = {}
        if turn_index is not None:
            payload["turn_index"] = turn_index
        data = self._request("POST", f"/threads/{thread_id}/fork", json=payload)
        return ThreadInfo(thread_id=data["thread_id"], status="active")

    def archive_thread(self, thread_id: str) -> bool:
        """Archive (delete) a thread."""
        data = self._request("DELETE", f"/threads/{thread_id}")
        return data.get("success", False)

    def rollback_thread(self, thread_id: str, turn_index: int) -> bool:
        """Rollback a thread to a specific turn."""
        data = self._request(
            "POST", f"/threads/{thread_id}/rollback", json={"turn_index": turn_index}
        )
        return data.get("success", False)

    # Turns
    def start_turn(
        self,
        thread_id: str,
        input_items: list[InputItem],
    ) -> TurnResponse:
        """Start a turn (non-streaming)."""
        payload = {
            "input": [item.model_dump(exclude_none=True) for item in input_items],
            "stream": False,
        }
        data = self._request("POST", f"/threads/{thread_id}/turns", json=payload)
        return TurnResponse(**data)

    def start_turn_stream(
        self,
        thread_id: str,
        input_items: list[InputItem],
    ) -> Iterator[StreamEvent]:
        """Start a turn with streaming (SSE)."""
        client = self._get_client()
        payload = {
            "input": [item.model_dump(exclude_none=True) for item in input_items],
            "stream": True,
        }
        with connect_sse(
            client, "POST", f"/threads/{thread_id}/turns/stream", json=payload
        ) as event_source:
            for sse in event_source.iter_sse():
                if sse.data and sse.data != "{}":
                    try:
                        event_data = json.loads(sse.data)
                        yield StreamEvent(**event_data)
                    except json.JSONDecodeError:
                        continue

    def interrupt_turn(self, thread_id: str) -> bool:
        """Interrupt the current turn."""
        data = self._request("POST", f"/threads/{thread_id}/interrupt")
        return data.get("success", False)

    # Models
    def list_models(self) -> list[ModelInfo]:
        """List available models."""
        data = self._request("GET", "/models")
        return [ModelInfo(**m) for m in data.get("models", [])]

    # Config
    def read_config(self) -> Config:
        """Read current configuration."""
        data = self._request("GET", "/config")
        return Config(**data)

    def write_config(self, key: str, value: Any) -> bool:
        """Write a configuration value."""
        data = self._request("POST", "/config", json={"key": key, "value": value})
        return data.get("success", False)

    def batch_write_config(self, values: dict[str, Any]) -> bool:
        """Write multiple configuration values."""
        payload = {"values": [{"key": k, "value": v} for k, v in values.items()]}
        data = self._request("POST", "/config/batch", json=payload)
        return data.get("success", False)

    # Account
    def get_account(self) -> AccountInfo:
        """Get account information."""
        data = self._request("GET", "/account")
        return AccountInfo(**data)

    def get_rate_limits(self) -> RateLimits:
        """Get rate limits."""
        data = self._request("GET", "/account/rate-limits")
        return RateLimits(**data)

    def login(self, api_key: str, provider: Optional[str] = None) -> bool:
        """Login with API key."""
        payload: dict[str, Any] = {"api_key": api_key}
        if provider:
            payload["provider"] = provider
        data = self._request("POST", "/account/login", json=payload)
        return data.get("success", False)

    def logout(self) -> bool:
        """Logout."""
        data = self._request("POST", "/account/logout")
        return data.get("success", False)


class AsyncHypercodeClient:
    """
    Asynchronous client for the Hypercode REST API.

    Args:
        base_url: Base URL for the Hypercode REST API server.
        api_key: Optional API key for authentication.
        timeout: Request timeout in seconds.
    """

    def __init__(
        self,
        base_url: str = "http://localhost:8080",
        api_key: Optional[str] = None,
        timeout: float = 30.0,
    ) -> None:
        self.base_url = base_url.rstrip("/")
        self.api_key = api_key
        self.timeout = timeout
        self._client: Optional[httpx.AsyncClient] = None

    async def _get_client(self) -> httpx.AsyncClient:
        if self._client is None:
            headers = {"Content-Type": "application/json"}
            if self.api_key:
                headers["Authorization"] = f"Bearer {self.api_key}"
            self._client = httpx.AsyncClient(
                base_url=self.base_url,
                headers=headers,
                timeout=self.timeout,
            )
        return self._client

    async def close(self) -> None:
        """Close the HTTP client."""
        if self._client:
            await self._client.aclose()
            self._client = None

    async def __aenter__(self) -> "AsyncHypercodeClient":
        return self

    async def __aexit__(self, *args: Any) -> None:
        await self.close()

    async def _request(self, method: str, path: str, **kwargs: Any) -> Any:
        client = await self._get_client()
        response = await client.request(method, path, **kwargs)
        response.raise_for_status()
        return response.json()

    # Health
    async def health(self) -> HealthResponse:
        """Check API health."""
        data = await self._request("GET", "/health")
        return HealthResponse(**data)

    # Threads
    async def create_thread(
        self,
        working_directory: Optional[str] = None,
        model: Optional[str] = None,
        instructions: Optional[str] = None,
    ) -> ThreadInfo:
        """Create a new thread."""
        payload: dict[str, Any] = {}
        if working_directory:
            payload["working_directory"] = working_directory
        if model:
            payload["model"] = model
        if instructions:
            payload["instructions"] = instructions
        data = await self._request("POST", "/threads", json=payload)
        return ThreadInfo(thread_id=data["thread_id"], status="active", created_at=data.get("created_at"))

    async def list_threads(self) -> list[ThreadInfo]:
        """List all threads."""
        data = await self._request("GET", "/threads")
        return [ThreadInfo(**t) for t in data.get("threads", [])]

    async def get_thread(self, thread_id: str) -> ThreadInfo:
        """Get a specific thread."""
        data = await self._request("GET", f"/threads/{thread_id}")
        return ThreadInfo(**data)

    # Turns
    async def start_turn(
        self,
        thread_id: str,
        input_items: list[InputItem],
    ) -> TurnResponse:
        """Start a turn (non-streaming)."""
        payload = {
            "input": [item.model_dump(exclude_none=True) for item in input_items],
            "stream": False,
        }
        data = await self._request("POST", f"/threads/{thread_id}/turns", json=payload)
        return TurnResponse(**data)

    async def start_turn_stream(
        self,
        thread_id: str,
        input_items: list[InputItem],
    ) -> AsyncIterator[StreamEvent]:
        """Start a turn with streaming (SSE)."""
        client = await self._get_client()
        payload = {
            "input": [item.model_dump(exclude_none=True) for item in input_items],
            "stream": True,
        }
        async with aconnect_sse(
            client, "POST", f"/threads/{thread_id}/turns/stream", json=payload
        ) as event_source:
            async for sse in event_source.aiter_sse():
                if sse.data and sse.data != "{}":
                    try:
                        event_data = json.loads(sse.data)
                        yield StreamEvent(**event_data)
                    except json.JSONDecodeError:
                        continue

    async def interrupt_turn(self, thread_id: str) -> bool:
        """Interrupt the current turn."""
        data = await self._request("POST", f"/threads/{thread_id}/interrupt")
        return data.get("success", False)

    # Models
    async def list_models(self) -> list[ModelInfo]:
        """List available models."""
        data = await self._request("GET", "/models")
        return [ModelInfo(**m) for m in data.get("models", [])]

    # Config
    async def read_config(self) -> Config:
        """Read current configuration."""
        data = await self._request("GET", "/config")
        return Config(**data)

    async def write_config(self, key: str, value: Any) -> bool:
        """Write a configuration value."""
        data = await self._request("POST", "/config", json={"key": key, "value": value})
        return data.get("success", False)

    # Account
    async def get_account(self) -> AccountInfo:
        """Get account information."""
        data = await self._request("GET", "/account")
        return AccountInfo(**data)

    async def get_rate_limits(self) -> RateLimits:
        """Get rate limits."""
        data = await self._request("GET", "/account/rate-limits")
        return RateLimits(**data)

    async def login(self, api_key: str, provider: Optional[str] = None) -> bool:
        """Login with API key."""
        payload: dict[str, Any] = {"api_key": api_key}
        if provider:
            payload["provider"] = provider
        data = await self._request("POST", "/account/login", json=payload)
        return data.get("success", False)

    async def logout(self) -> bool:
        """Logout."""
        data = await self._request("POST", "/account/logout")
        return data.get("success", False)
