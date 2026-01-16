//! Thread management endpoints

use axum::{
    extract::{Path, State},
    Json,
};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

use crate::error::ApiError;
use crate::state::AppState;

/// Request body for creating a new thread
#[derive(Deserialize)]
pub struct CreateThreadRequest {
    /// Working directory for the thread
    #[serde(default)]
    pub working_directory: Option<String>,
    /// Model to use
    #[serde(default)]
    pub model: Option<String>,
    /// Instructions for the agent
    #[serde(default)]
    pub instructions: Option<String>,
}

/// Response for thread creation
#[derive(Serialize)]
pub struct CreateThreadResponse {
    pub thread_id: String,
    pub created_at: String,
    pub model: Option<String>,
    pub rollout_path: Option<String>,
}

/// POST /threads - Create a new thread
pub async fn create_thread(
    State(state): State<AppState>,
    Json(request): Json<CreateThreadRequest>,
) -> Result<Json<CreateThreadResponse>, ApiError> {
    let working_directory = request.working_directory.map(PathBuf::from);

    let new_thread = state
        .create_thread(working_directory, request.model, request.instructions)
        .await?;

    Ok(Json(CreateThreadResponse {
        thread_id: new_thread.thread_id.to_string(),
        created_at: chrono::Utc::now().to_rfc3339(),
        model: Some(new_thread.session_configured.model),
        rollout_path: Some(
            new_thread
                .session_configured
                .rollout_path
                .to_string_lossy()
                .to_string(),
        ),
    }))
}

/// Response for listing threads
#[derive(Serialize)]
pub struct ListThreadsResponse {
    pub threads: Vec<ThreadInfo>,
}

#[derive(Serialize)]
pub struct ThreadInfo {
    pub thread_id: String,
    pub status: String,
    pub created_at: Option<String>,
}

/// GET /threads - List all threads
pub async fn list_threads(
    State(state): State<AppState>,
) -> Result<Json<ListThreadsResponse>, ApiError> {
    let thread_ids = state.list_thread_ids().await;
    let mut threads = Vec::new();

    for id in thread_ids {
        let created_at = state
            .get_session(&id)
            .await
            .map(|s| s.created_at.to_rfc3339());

        threads.push(ThreadInfo {
            thread_id: id,
            status: "active".to_string(),
            created_at,
        });
    }

    Ok(Json(ListThreadsResponse { threads }))
}

/// Response for getting a single thread
#[derive(Serialize)]
pub struct GetThreadResponse {
    pub thread_id: String,
    pub status: String,
    pub created_at: Option<String>,
    pub working_directory: Option<String>,
    pub model: Option<String>,
}

/// GET /threads/:id - Get a specific thread
pub async fn get_thread(
    State(state): State<AppState>,
    Path(thread_id): Path<String>,
) -> Result<Json<GetThreadResponse>, ApiError> {
    // Verify the thread exists
    let _thread = state.get_thread(&thread_id).await?;
    let session = state.get_session(&thread_id).await;

    Ok(Json(GetThreadResponse {
        thread_id,
        status: "active".to_string(),
        created_at: session.as_ref().map(|s| s.created_at.to_rfc3339()),
        working_directory: session
            .as_ref()
            .and_then(|s| s.working_directory.as_ref().map(|p| p.to_string_lossy().to_string())),
        model: session.as_ref().and_then(|s| s.model.clone()),
    }))
}

/// Request body for resuming a thread
#[derive(Deserialize)]
pub struct ResumeThreadRequest {
    #[serde(default)]
    pub model: Option<String>,
}

/// POST /threads/:id/resume - Resume a thread
pub async fn resume_thread(
    State(state): State<AppState>,
    Path(thread_id): Path<String>,
    Json(_request): Json<ResumeThreadRequest>,
) -> Result<Json<GetThreadResponse>, ApiError> {
    // Verify the thread exists
    let _thread = state.get_thread(&thread_id).await?;
    let session = state.get_session(&thread_id).await;

    Ok(Json(GetThreadResponse {
        thread_id,
        status: "active".to_string(),
        created_at: session.as_ref().map(|s| s.created_at.to_rfc3339()),
        working_directory: session
            .as_ref()
            .and_then(|s| s.working_directory.as_ref().map(|p| p.to_string_lossy().to_string())),
        model: session.as_ref().and_then(|s| s.model.clone()),
    }))
}

/// Request body for forking a thread
#[derive(Deserialize)]
pub struct ForkThreadRequest {
    /// Turn index to fork from (optional)
    #[serde(default)]
    pub turn_index: Option<u32>,
}

/// Response for forking a thread
#[derive(Serialize)]
pub struct ForkThreadResponse {
    pub thread_id: String,
    pub forked_from: String,
}

/// POST /threads/:id/fork - Fork a thread
pub async fn fork_thread(
    State(state): State<AppState>,
    Path(thread_id): Path<String>,
    Json(request): Json<ForkThreadRequest>,
) -> Result<Json<ForkThreadResponse>, ApiError> {
    // Verify the original thread exists
    let original_thread = state.get_thread(&thread_id).await?;
    let rollout_path = original_thread.rollout_path();

    // Get session info for config
    let session = state.get_session(&thread_id).await;

    // Build config for forked thread (clone the inner Config, not the Arc)
    let mut config: hypercode_core::config::Config = (**state.config()).clone();
    if let Some(ref s) = session {
        if let Some(ref cwd) = s.working_directory {
            config.cwd = cwd.clone();
        }
        if let Some(ref model) = s.model {
            config.model = Some(model.clone());
        }
    }

    // Fork the thread
    let nth_user_message = request.turn_index.map(|i| i as usize).unwrap_or(usize::MAX);
    let new_thread = state
        .thread_manager()
        .fork_thread(nth_user_message, config, rollout_path)
        .await
        .map_err(|e| ApiError::Internal(format!("Failed to fork thread: {}", e)))?;

    Ok(Json(ForkThreadResponse {
        thread_id: new_thread.thread_id.to_string(),
        forked_from: thread_id,
    }))
}

/// DELETE /threads/:id - Archive a thread
pub async fn archive_thread(
    State(state): State<AppState>,
    Path(thread_id): Path<String>,
) -> Result<Json<serde_json::Value>, ApiError> {
    state.remove_thread(&thread_id).await?;

    Ok(Json(serde_json::json!({
        "success": true,
        "thread_id": thread_id
    })))
}

/// Request body for rolling back a thread
#[derive(Deserialize)]
pub struct RollbackThreadRequest {
    /// Turn index to rollback to
    pub turn_index: u32,
}

/// POST /threads/:id/rollback - Rollback a thread to a specific turn
pub async fn rollback_thread(
    State(state): State<AppState>,
    Path(thread_id): Path<String>,
    Json(request): Json<RollbackThreadRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    use hypercode_protocol::protocol::Op;

    let thread = state.get_thread(&thread_id).await?;

    // Submit rollback operation
    thread
        .submit(Op::ThreadRollback {
            num_turns: request.turn_index,
        })
        .await
        .map_err(|e| ApiError::Internal(format!("Failed to rollback thread: {}", e)))?;

    Ok(Json(serde_json::json!({
        "success": true,
        "thread_id": thread_id,
        "rolled_back_to": request.turn_index
    })))
}
