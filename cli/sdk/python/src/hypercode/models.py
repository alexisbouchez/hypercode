"""
Data models for the Hypercode SDK.
"""

from __future__ import annotations

from typing import Any, Literal, Optional
from pydantic import BaseModel, Field


class ThreadInfo(BaseModel):
    """Information about a thread."""

    thread_id: str
    status: str
    created_at: Optional[str] = None


class InputItem(BaseModel):
    """Input item for a turn."""

    type: Literal["message", "local_image", "base64_image"]
    content: Optional[str] = None
    path: Optional[str] = None
    data: Optional[str] = None
    media_type: Optional[str] = None


class OutputItem(BaseModel):
    """Output item from a turn."""

    type: str
    content: Optional[str] = None
    id: Optional[str] = None
    name: Optional[str] = None
    arguments: Optional[Any] = None
    tool_call_id: Optional[str] = None
    output: Optional[str] = None
    path: Optional[str] = None
    operation: Optional[str] = None
    command: Optional[str] = None
    exit_code: Optional[int] = None
    stdout: Optional[str] = None
    stderr: Optional[str] = None


class TurnResponse(BaseModel):
    """Response from a turn execution."""

    turn_id: str
    output: list[OutputItem]
    status: str


class StreamEvent(BaseModel):
    """Event from a streaming turn."""

    type: str
    turn_id: Optional[str] = None
    content: Optional[str] = None
    id: Optional[str] = None
    name: Optional[str] = None
    arguments: Optional[Any] = None
    tool_call_id: Optional[str] = None
    output: Optional[str] = None
    path: Optional[str] = None
    operation: Optional[str] = None
    command: Optional[str] = None
    exit_code: Optional[int] = None
    status: Optional[str] = None
    code: Optional[str] = None
    message: Optional[str] = None


class ReasoningLevel(BaseModel):
    """Reasoning level configuration."""

    effort: str
    description: str


class ModelInfo(BaseModel):
    """Information about an available model."""

    id: str
    name: str
    description: Optional[str] = None
    default_reasoning_level: Optional[str] = None
    supported_reasoning_levels: list[ReasoningLevel] = Field(default_factory=list)
    context_window: Optional[int] = None


class AccountInfo(BaseModel):
    """Account information."""

    authenticated: bool
    auth_mode: Optional[str] = None
    email: Optional[str] = None


class RateLimits(BaseModel):
    """Rate limit information."""

    requests_per_minute: Optional[int] = None
    tokens_per_minute: Optional[int] = None
    tokens_per_day: Optional[int] = None
    remaining_requests: Optional[int] = None
    remaining_tokens: Optional[int] = None


class Config(BaseModel):
    """Configuration settings."""

    config: dict[str, Any]


class HealthResponse(BaseModel):
    """Health check response."""

    status: str
    version: str
