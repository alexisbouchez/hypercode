//! Model listing endpoints

use axum::{extract::State, Json};
use hypercode_core::models_manager::manager::RefreshStrategy;
use serde::Serialize;

use crate::error::ApiError;
use crate::state::AppState;

/// Model information
#[derive(Serialize, Clone)]
pub struct ModelInfo {
    pub id: String,
    pub name: String,
    pub description: Option<String>,
    pub default_reasoning_level: Option<String>,
    pub supported_reasoning_levels: Vec<ReasoningLevel>,
    pub context_window: Option<u32>,
}

#[derive(Serialize, Clone)]
pub struct ReasoningLevel {
    pub effort: String,
    pub description: String,
}

/// Response for listing models
#[derive(Serialize)]
pub struct ListModelsResponse {
    pub models: Vec<ModelInfo>,
}

/// GET /models - List available models
pub async fn list_models(
    State(state): State<AppState>,
) -> Result<Json<ListModelsResponse>, ApiError> {
    let config = state.config();
    let thread_manager = state.thread_manager();

    // Get models from the thread manager
    let model_presets = thread_manager
        .list_models(&config, RefreshStrategy::OnlineIfUncached)
        .await;

    let models = model_presets
        .into_iter()
        .map(|preset| {
            let supported_reasoning_levels = preset
                .supported_reasoning_efforts
                .iter()
                .map(|level| ReasoningLevel {
                    effort: level.effort.to_string(),
                    description: level.description.clone(),
                })
                .collect();

            ModelInfo {
                id: preset.id.clone(),
                name: preset.display_name.clone(),
                description: Some(preset.description.clone()),
                default_reasoning_level: Some(preset.default_reasoning_effort.to_string()),
                supported_reasoning_levels,
                context_window: None, // Not available in ModelPreset
            }
        })
        .collect();

    Ok(Json(ListModelsResponse { models }))
}
