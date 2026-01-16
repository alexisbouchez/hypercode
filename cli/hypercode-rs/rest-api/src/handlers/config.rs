//! Configuration endpoints

use axum::{extract::State, Json};
use serde::{Deserialize, Serialize};

use crate::error::ApiError;
use crate::state::AppState;

/// Configuration value
#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(untagged)]
pub enum ConfigValue {
    String(String),
    Bool(bool),
    Number(i64),
    Array(Vec<ConfigValue>),
    Object(serde_json::Map<String, serde_json::Value>),
}

/// Response for reading configuration
#[derive(Serialize)]
pub struct ConfigReadResponse {
    pub config: serde_json::Value,
}

/// GET /config - Read current configuration
pub async fn read_config(State(state): State<AppState>) -> Result<Json<ConfigReadResponse>, ApiError> {
    let config = state.config();

    // Return relevant configuration values
    Ok(Json(ConfigReadResponse {
        config: serde_json::json!({
            "model": config.model,
            "model_provider": config.model_provider_id,
            "cwd": config.cwd.to_string_lossy(),
            "approval_policy": config.approval_policy.to_string(),
            "sandbox_policy": config.sandbox_policy.to_string(),
            "hide_agent_reasoning": config.hide_agent_reasoning,
            "show_raw_agent_reasoning": config.show_raw_agent_reasoning,
            "model_context_window": config.model_context_window,
            "hypercode_home": config.hypercode_home.to_string_lossy(),
        }),
    }))
}

/// Request body for writing a single config value
#[derive(Deserialize)]
pub struct ConfigWriteRequest {
    pub key: String,
    pub value: serde_json::Value,
}

/// Response for writing configuration
#[derive(Serialize)]
pub struct ConfigWriteResponse {
    pub success: bool,
    pub message: Option<String>,
}

/// POST /config - Write a configuration value
pub async fn write_config(
    State(_state): State<AppState>,
    Json(request): Json<ConfigWriteRequest>,
) -> Result<Json<ConfigWriteResponse>, ApiError> {
    // Config write is complex - for now, just log and return success
    // Full implementation would require using ConfigEditsBuilder with specific methods
    tracing::info!(
        "Config write requested: {} = {:?}",
        request.key,
        request.value
    );

    Ok(Json(ConfigWriteResponse {
        success: true,
        message: Some("Config write logged. Full persistence not yet implemented via REST API.".to_string()),
    }))
}

/// Request body for batch writing config values
#[derive(Deserialize)]
pub struct ConfigBatchWriteRequest {
    pub values: Vec<ConfigWriteRequest>,
}

/// POST /config/batch - Write multiple configuration values
pub async fn batch_write_config(
    State(_state): State<AppState>,
    Json(request): Json<ConfigBatchWriteRequest>,
) -> Result<Json<ConfigWriteResponse>, ApiError> {
    // Config write is complex - for now, just log and return success
    tracing::info!(
        "Batch config write requested: {} values",
        request.values.len()
    );

    Ok(Json(ConfigWriteResponse {
        success: true,
        message: Some("Batch config write logged. Full persistence not yet implemented via REST API.".to_string()),
    }))
}
