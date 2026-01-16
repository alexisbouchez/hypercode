//! Turn (message) endpoints

use axum::{
    extract::{Path, State},
    response::sse::{Event, KeepAlive, Sse},
    Json,
};
use futures::stream::Stream;
use hypercode_protocol::config_types::ReasoningSummary;
use hypercode_protocol::protocol::{
    AskForApproval, EventMsg, Op, SandboxPolicy,
};
use hypercode_protocol::user_input::UserInput as CoreUserInput;
use serde::{Deserialize, Serialize};
use std::convert::Infallible;
use std::path::PathBuf;
use std::time::Duration;

use crate::error::ApiError;
use crate::state::AppState;

/// Input item for a turn
#[derive(Deserialize, Clone, Debug)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum InputItem {
    /// Text message from user
    Message { content: String },
    /// Local image file
    LocalImage { path: String },
    /// Base64 encoded image
    Base64Image { data: String, media_type: String },
}

/// Request body for starting a turn
#[derive(Deserialize)]
pub struct StartTurnRequest {
    /// Input items (messages, images)
    pub input: Vec<InputItem>,
    /// Whether to stream the response
    #[serde(default)]
    pub stream: bool,
    /// Previous response ID for multi-turn conversations
    #[serde(default)]
    pub previous_response_id: Option<String>,
}

/// Non-streaming response for a turn
#[derive(Serialize)]
pub struct TurnResponse {
    pub turn_id: String,
    pub output: Vec<OutputItem>,
    pub status: String,
}

/// Output item from a turn
#[derive(Serialize, Clone, Debug)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum OutputItem {
    /// Agent message
    Message { content: String },
    /// Reasoning content
    Reasoning { content: String },
    /// Tool call
    ToolCall {
        id: String,
        name: String,
        arguments: serde_json::Value,
    },
    /// Tool result
    ToolResult {
        tool_call_id: String,
        output: String,
    },
    /// File change
    FileChange {
        path: String,
        operation: String,
        content: Option<String>,
    },
    /// Command execution
    CommandExecution {
        command: String,
        exit_code: i32,
        stdout: String,
        stderr: String,
    },
}

/// SSE event for streaming turns
#[derive(Serialize, Clone, Debug)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum TurnStreamEvent {
    /// Turn started
    TurnStarted { turn_id: String },
    /// Message content delta
    MessageDelta { content: String },
    /// Message completed
    MessageComplete { content: String },
    /// Reasoning content delta
    ReasoningDelta { content: String },
    /// Tool call started
    ToolCallStarted { id: String, name: String },
    /// Tool call completed
    ToolCallCompleted {
        id: String,
        name: String,
        arguments: serde_json::Value,
    },
    /// Tool result
    ToolResult { tool_call_id: String, output: String },
    /// File change
    FileChange { path: String, operation: String },
    /// Command execution begin
    CommandBegin { command: String },
    /// Command execution end
    CommandEnd {
        command: String,
        exit_code: i32,
        stdout: String,
        stderr: String,
    },
    /// Turn completed
    TurnCompleted { turn_id: String, status: String },
    /// Error
    Error { code: String, message: String },
}

fn convert_input_items(items: Vec<InputItem>) -> Vec<CoreUserInput> {
    items
        .into_iter()
        .map(|item| match item {
            InputItem::Message { content } => CoreUserInput::Text {
                text: content,
                text_elements: vec![],
            },
            InputItem::LocalImage { path } => CoreUserInput::LocalImage {
                path: PathBuf::from(path),
            },
            InputItem::Base64Image { data, media_type } => {
                // Convert to data URL format
                let image_url = format!("data:{};base64,{}", media_type, data);
                CoreUserInput::Image { image_url }
            }
        })
        .collect()
}

/// POST /threads/:id/turns - Start a new turn (non-streaming)
pub async fn start_turn(
    State(state): State<AppState>,
    Path(thread_id): Path<String>,
    Json(request): Json<StartTurnRequest>,
) -> Result<Json<TurnResponse>, ApiError> {
    let thread = state.get_thread(&thread_id).await?;
    let config = state.config();

    // Convert input items
    let items = convert_input_items(request.input);

    // Submit the user turn
    let turn_id = thread
        .submit(Op::UserTurn {
            items,
            cwd: config.cwd.clone(),
            approval_policy: AskForApproval::OnRequest,
            sandbox_policy: SandboxPolicy::new_workspace_write_policy(),
            model: config.model.clone().unwrap_or_default(),
            effort: None,
            summary: ReasoningSummary::default(),
            final_output_json_schema: None,
        })
        .await
        .map_err(|e| ApiError::Internal(format!("Failed to start turn: {}", e)))?;

    // Collect output from events
    let mut output = Vec::new();
    let mut current_message = String::new();

    loop {
        match thread.next_event().await {
            Ok(event) => match event.msg {
                EventMsg::AgentMessage(msg) => {
                    output.push(OutputItem::Message {
                        content: msg.message,
                    });
                }
                EventMsg::AgentMessageDelta(delta) => {
                    current_message.push_str(&delta.delta);
                }
                EventMsg::ExecCommandBegin(cmd) => {
                    // Track command start
                    tracing::debug!("Command started: {}", cmd.call_id);
                }
                EventMsg::ExecCommandEnd(cmd) => {
                    output.push(OutputItem::CommandExecution {
                        command: cmd.call_id.clone(),
                        exit_code: cmd.exit_code,
                        stdout: cmd.stdout.clone(),
                        stderr: cmd.stderr.clone(),
                    });
                }
                EventMsg::TurnComplete(_) => {
                    // Add final message if accumulated
                    if !current_message.is_empty() {
                        output.push(OutputItem::Message {
                            content: current_message.clone(),
                        });
                    }
                    break;
                }
                EventMsg::TurnAborted(_) => {
                    break;
                }
                EventMsg::Error(e) => {
                    return Err(ApiError::Internal(format!("Turn error: {}", e.message)));
                }
                _ => {
                    // Other events we don't need to handle for non-streaming
                }
            },
            Err(e) => {
                return Err(ApiError::Internal(format!("Failed to get event: {}", e)));
            }
        }
    }

    Ok(Json(TurnResponse {
        turn_id,
        output,
        status: "completed".to_string(),
    }))
}

/// POST /threads/:id/turns/stream - Start a new turn (streaming via SSE)
pub async fn start_turn_stream(
    State(state): State<AppState>,
    Path(thread_id): Path<String>,
    Json(request): Json<StartTurnRequest>,
) -> Result<Sse<impl Stream<Item = Result<Event, Infallible>>>, ApiError> {
    let thread = state.get_thread(&thread_id).await?;
    let config = state.config().clone();

    // Convert input items
    let items = convert_input_items(request.input);

    // Submit the user turn
    let turn_id = thread
        .submit(Op::UserTurn {
            items,
            cwd: config.cwd.clone(),
            approval_policy: AskForApproval::OnRequest,
            sandbox_policy: SandboxPolicy::new_workspace_write_policy(),
            model: config.model.clone().unwrap_or_default(),
            effort: None,
            summary: ReasoningSummary::default(),
            final_output_json_schema: None,
        })
        .await
        .map_err(|e| ApiError::Internal(format!("Failed to start turn: {}", e)))?;

    let turn_id_for_stream = turn_id.clone();

    let stream = async_stream::stream! {
        // Turn started event
        let event = TurnStreamEvent::TurnStarted {
            turn_id: turn_id_for_stream.clone(),
        };
        yield Ok(Event::default()
            .event("turn_started")
            .data(serde_json::to_string(&event).unwrap_or_default()));

        let mut accumulated_message = String::new();

        loop {
            match thread.next_event().await {
                Ok(event) => match event.msg {
                    EventMsg::AgentMessageDelta(delta) => {
                        accumulated_message.push_str(&delta.delta);
                        let sse_event = TurnStreamEvent::MessageDelta {
                            content: delta.delta,
                        };
                        yield Ok(Event::default()
                            .event("message_delta")
                            .data(serde_json::to_string(&sse_event).unwrap_or_default()));
                    }
                    EventMsg::AgentMessage(msg) => {
                        let sse_event = TurnStreamEvent::MessageComplete {
                            content: msg.message,
                        };
                        yield Ok(Event::default()
                            .event("message_complete")
                            .data(serde_json::to_string(&sse_event).unwrap_or_default()));
                    }
                    EventMsg::AgentReasoningDelta(delta) => {
                        let sse_event = TurnStreamEvent::ReasoningDelta {
                            content: delta.delta,
                        };
                        yield Ok(Event::default()
                            .event("reasoning_delta")
                            .data(serde_json::to_string(&sse_event).unwrap_or_default()));
                    }
                    EventMsg::ExecCommandBegin(cmd) => {
                        let sse_event = TurnStreamEvent::CommandBegin {
                            command: cmd.call_id,
                        };
                        yield Ok(Event::default()
                            .event("command_begin")
                            .data(serde_json::to_string(&sse_event).unwrap_or_default()));
                    }
                    EventMsg::ExecCommandEnd(cmd) => {
                        let sse_event = TurnStreamEvent::CommandEnd {
                            command: cmd.call_id,
                            exit_code: cmd.exit_code,
                            stdout: cmd.stdout.clone(),
                            stderr: cmd.stderr.clone(),
                        };
                        yield Ok(Event::default()
                            .event("command_end")
                            .data(serde_json::to_string(&sse_event).unwrap_or_default()));
                    }
                    EventMsg::PatchApplyBegin(patch) => {
                        // Extract first path from changes HashMap
                        let path = patch.changes.keys().next()
                            .map(|p| p.to_string_lossy().to_string())
                            .unwrap_or_default();
                        let sse_event = TurnStreamEvent::FileChange {
                            path,
                            operation: "patch".to_string(),
                        };
                        yield Ok(Event::default()
                            .event("file_change")
                            .data(serde_json::to_string(&sse_event).unwrap_or_default()));
                    }
                    EventMsg::McpToolCallBegin(tool) => {
                        let sse_event = TurnStreamEvent::ToolCallStarted {
                            id: tool.call_id.clone(),
                            name: tool.invocation.tool.clone(),
                        };
                        yield Ok(Event::default()
                            .event("tool_call_started")
                            .data(serde_json::to_string(&sse_event).unwrap_or_default()));
                    }
                    EventMsg::McpToolCallEnd(tool) => {
                        let sse_event = TurnStreamEvent::ToolResult {
                            tool_call_id: tool.call_id.clone(),
                            output: tool.result.as_ref()
                                .map(|r| serde_json::to_string(r).unwrap_or_default())
                                .unwrap_or_default(),
                        };
                        yield Ok(Event::default()
                            .event("tool_result")
                            .data(serde_json::to_string(&sse_event).unwrap_or_default()));
                    }
                    EventMsg::TurnComplete(_) => {
                        // If we have accumulated message content, emit it as complete
                        if !accumulated_message.is_empty() {
                            let sse_event = TurnStreamEvent::MessageComplete {
                                content: accumulated_message.clone(),
                            };
                            yield Ok(Event::default()
                                .event("message_complete")
                                .data(serde_json::to_string(&sse_event).unwrap_or_default()));
                        }

                        let sse_event = TurnStreamEvent::TurnCompleted {
                            turn_id: turn_id_for_stream.clone(),
                            status: "completed".to_string(),
                        };
                        yield Ok(Event::default()
                            .event("turn_completed")
                            .data(serde_json::to_string(&sse_event).unwrap_or_default()));

                        // Done event
                        yield Ok(Event::default().event("done").data("{}"));
                        break;
                    }
                    EventMsg::TurnAborted(_) => {
                        let sse_event = TurnStreamEvent::TurnCompleted {
                            turn_id: turn_id_for_stream.clone(),
                            status: "aborted".to_string(),
                        };
                        yield Ok(Event::default()
                            .event("turn_completed")
                            .data(serde_json::to_string(&sse_event).unwrap_or_default()));

                        yield Ok(Event::default().event("done").data("{}"));
                        break;
                    }
                    EventMsg::Error(e) => {
                        let sse_event = TurnStreamEvent::Error {
                            code: "turn_error".to_string(),
                            message: e.message,
                        };
                        yield Ok(Event::default()
                            .event("error")
                            .data(serde_json::to_string(&sse_event).unwrap_or_default()));
                        break;
                    }
                    _ => {
                        // Other events we can skip for now
                    }
                },
                Err(e) => {
                    let sse_event = TurnStreamEvent::Error {
                        code: "stream_error".to_string(),
                        message: e.to_string(),
                    };
                    yield Ok(Event::default()
                        .event("error")
                        .data(serde_json::to_string(&sse_event).unwrap_or_default()));
                    break;
                }
            }
        }
    };

    Ok(Sse::new(stream).keep_alive(
        KeepAlive::new()
            .interval(Duration::from_secs(15))
            .text("keep-alive"),
    ))
}

/// POST /threads/:id/interrupt - Interrupt the current turn
pub async fn interrupt_turn(
    State(state): State<AppState>,
    Path(thread_id): Path<String>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let thread = state.get_thread(&thread_id).await?;

    // Submit interrupt operation
    thread
        .submit(Op::Interrupt)
        .await
        .map_err(|e| ApiError::Internal(format!("Failed to interrupt turn: {}", e)))?;

    Ok(Json(serde_json::json!({
        "success": true,
        "thread_id": thread_id,
        "message": "Turn interrupted"
    })))
}
