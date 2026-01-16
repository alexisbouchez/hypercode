"""
Thread class for high-level interaction with the Hypercode agent.
"""

from __future__ import annotations

from typing import Any, Iterator, Optional, Union

from hypercode.client import HypercodeClient
from hypercode.models import InputItem, OutputItem, StreamEvent, TurnResponse


class Thread:
    """
    A conversation thread with the Hypercode agent.

    This class provides a high-level interface for interacting with the agent,
    automatically managing thread creation and turn execution.

    Args:
        client: HypercodeClient instance.
        thread_id: Optional existing thread ID to resume.
        working_directory: Working directory for file operations.
        model: Model to use for this thread.
        instructions: System instructions for the agent.
    """

    def __init__(
        self,
        client: HypercodeClient,
        thread_id: Optional[str] = None,
        working_directory: Optional[str] = None,
        model: Optional[str] = None,
        instructions: Optional[str] = None,
    ) -> None:
        self.client = client
        self._thread_id = thread_id
        self.working_directory = working_directory
        self.model = model
        self.instructions = instructions

    @property
    def thread_id(self) -> Optional[str]:
        """Get the thread ID."""
        return self._thread_id

    def _ensure_thread(self) -> str:
        """Create thread if it doesn't exist."""
        if self._thread_id is None:
            info = self.client.create_thread(
                working_directory=self.working_directory,
                model=self.model,
                instructions=self.instructions,
            )
            self._thread_id = info.thread_id
        return self._thread_id

    def run(
        self,
        message: Union[str, list[str]],
        images: Optional[list[dict[str, Any]]] = None,
    ) -> TurnResponse:
        """
        Send a message and get a response.

        Args:
            message: User message(s) to send.
            images: Optional list of images to include.
                    Each image should be a dict with:
                    - type: "local_image" or "base64_image"
                    - path: Path for local_image
                    - data: Base64 data for base64_image
                    - media_type: MIME type for base64_image

        Returns:
            TurnResponse with the agent's output.
        """
        thread_id = self._ensure_thread()

        # Build input items
        input_items: list[InputItem] = []

        # Add messages
        messages = [message] if isinstance(message, str) else message
        for msg in messages:
            input_items.append(InputItem(type="message", content=msg))

        # Add images
        if images:
            for img in images:
                img_type = img.get("type", "local_image")
                if img_type == "local_image":
                    input_items.append(
                        InputItem(type="local_image", path=img.get("path"))
                    )
                elif img_type == "base64_image":
                    input_items.append(
                        InputItem(
                            type="base64_image",
                            data=img.get("data"),
                            media_type=img.get("media_type"),
                        )
                    )

        return self.client.start_turn(thread_id, input_items)

    def run_stream(
        self,
        message: Union[str, list[str]],
        images: Optional[list[dict[str, Any]]] = None,
    ) -> Iterator[StreamEvent]:
        """
        Send a message and stream the response.

        Args:
            message: User message(s) to send.
            images: Optional list of images to include.

        Yields:
            StreamEvent objects as the response is generated.
        """
        thread_id = self._ensure_thread()

        # Build input items
        input_items: list[InputItem] = []

        # Add messages
        messages = [message] if isinstance(message, str) else message
        for msg in messages:
            input_items.append(InputItem(type="message", content=msg))

        # Add images
        if images:
            for img in images:
                img_type = img.get("type", "local_image")
                if img_type == "local_image":
                    input_items.append(
                        InputItem(type="local_image", path=img.get("path"))
                    )
                elif img_type == "base64_image":
                    input_items.append(
                        InputItem(
                            type="base64_image",
                            data=img.get("data"),
                            media_type=img.get("media_type"),
                        )
                    )

        yield from self.client.start_turn_stream(thread_id, input_items)

    def interrupt(self) -> bool:
        """Interrupt the current turn."""
        if self._thread_id:
            return self.client.interrupt_turn(self._thread_id)
        return False

    def fork(self, turn_index: Optional[int] = None) -> "Thread":
        """
        Fork this thread.

        Args:
            turn_index: Optional turn index to fork from.

        Returns:
            New Thread instance for the forked thread.
        """
        if not self._thread_id:
            raise ValueError("Cannot fork a thread that hasn't been started")

        info = self.client.fork_thread(self._thread_id, turn_index)
        return Thread(
            client=self.client,
            thread_id=info.thread_id,
            working_directory=self.working_directory,
            model=self.model,
            instructions=self.instructions,
        )

    def rollback(self, turn_index: int) -> bool:
        """
        Rollback to a specific turn.

        Args:
            turn_index: Turn index to rollback to.

        Returns:
            True if successful.
        """
        if not self._thread_id:
            raise ValueError("Cannot rollback a thread that hasn't been started")

        return self.client.rollback_thread(self._thread_id, turn_index)

    def archive(self) -> bool:
        """
        Archive (delete) this thread.

        Returns:
            True if successful.
        """
        if self._thread_id:
            result = self.client.archive_thread(self._thread_id)
            if result:
                self._thread_id = None
            return result
        return False
