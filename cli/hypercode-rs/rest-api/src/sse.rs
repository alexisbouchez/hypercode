//! Server-Sent Events (SSE) support for streaming responses

use axum::response::sse::{Event, KeepAlive, Sse};
use futures::stream::Stream;
use serde::Serialize;
use std::convert::Infallible;
use std::time::Duration;

/// Create an SSE response from a stream of events
pub fn sse_response<S, T>(stream: S) -> Sse<impl Stream<Item = Result<Event, Infallible>>>
where
    S: Stream<Item = T> + Send + 'static,
    T: Serialize + Send + 'static,
{
    let event_stream = async_stream::stream! {
        tokio::pin!(stream);
        while let Some(item) = futures::StreamExt::next(&mut stream).await {
            if let Ok(data) = serde_json::to_string(&item) {
                yield Ok(Event::default().data(data));
            }
        }
        // Send done event
        yield Ok(Event::default().event("done").data("{}"));
    };

    Sse::new(event_stream).keep_alive(
        KeepAlive::new()
            .interval(Duration::from_secs(15))
            .text("keep-alive"),
    )
}

/// SSE event types for streaming
#[derive(Serialize, Clone, Debug)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum StreamEvent {
    /// Thread started
    ThreadStarted { thread_id: String },
    /// Turn started
    TurnStarted { turn_id: String },
    /// Agent message (streaming)
    AgentMessage { content: String, finished: bool },
    /// Tool call
    ToolCall {
        tool_name: String,
        arguments: serde_json::Value,
    },
    /// Tool result
    ToolResult {
        tool_name: String,
        result: serde_json::Value,
    },
    /// Turn completed
    TurnCompleted { turn_id: String },
    /// Error occurred
    Error { code: String, message: String },
    /// Stream done
    Done,
}
