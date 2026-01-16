//! Account management endpoints

use axum::{extract::State, Json};
use hypercode_core::auth::{login_with_api_key, login_with_mistral_api_key};
use serde::{Deserialize, Serialize};

use crate::error::ApiError;
use crate::state::AppState;

/// Account information
#[derive(Serialize)]
pub struct AccountInfo {
    pub authenticated: bool,
    pub auth_mode: Option<String>,
}

/// GET /account - Get account information
pub async fn get_account(State(state): State<AppState>) -> Result<Json<AccountInfo>, ApiError> {
    let auth_manager = state.auth_manager();
    let auth = auth_manager.auth().await;

    Ok(Json(AccountInfo {
        authenticated: auth.is_some(),
        auth_mode: auth.as_ref().map(|a| a.mode.to_string()),
    }))
}

/// Rate limits information
#[derive(Serialize)]
pub struct RateLimitsResponse {
    pub requests_per_minute: Option<u32>,
    pub tokens_per_minute: Option<u32>,
    pub tokens_per_day: Option<u32>,
    pub remaining_requests: Option<u32>,
    pub remaining_tokens: Option<u32>,
}

/// GET /account/rate-limits - Get rate limits
pub async fn get_rate_limits(
    State(_state): State<AppState>,
) -> Result<Json<RateLimitsResponse>, ApiError> {
    // Rate limits are not easily accessible from the auth manager
    // Return reasonable defaults
    Ok(Json(RateLimitsResponse {
        requests_per_minute: None,
        tokens_per_minute: None,
        tokens_per_day: None,
        remaining_requests: None,
        remaining_tokens: None,
    }))
}

/// Request body for API key login
#[derive(Deserialize)]
pub struct LoginApiKeyRequest {
    pub api_key: String,
    pub provider: Option<String>, // "openai" or "mistral"
}

/// Response for login
#[derive(Serialize)]
pub struct LoginResponse {
    pub success: bool,
    pub auth_mode: String,
}

/// POST /account/login - Login with API key
pub async fn login(
    State(state): State<AppState>,
    Json(request): Json<LoginApiKeyRequest>,
) -> Result<Json<LoginResponse>, ApiError> {
    let config = state.config();
    let provider = request.provider.unwrap_or_else(|| "openai".to_string());

    // Use hypercode-core's login functions based on provider
    let result = if provider.to_lowercase() == "mistral" {
        login_with_mistral_api_key(
            &config.hypercode_home,
            &request.api_key,
            config.cli_auth_credentials_store_mode,
        )
    } else {
        login_with_api_key(
            &config.hypercode_home,
            &request.api_key,
            config.cli_auth_credentials_store_mode,
        )
    };

    match result {
        Ok(_) => {
            tracing::info!("Login successful with {} API key", provider);
            Ok(Json(LoginResponse {
                success: true,
                auth_mode: provider,
            }))
        }
        Err(e) => {
            tracing::error!("Login failed: {}", e);
            Err(ApiError::Unauthorized(format!("Login failed: {}", e)))
        }
    }
}

/// POST /account/logout - Logout
pub async fn logout(State(state): State<AppState>) -> Result<Json<serde_json::Value>, ApiError> {
    let config = state.config();

    // Delete the auth file to logout
    let auth_path = config.hypercode_home.join("auth.json");
    if auth_path.exists() {
        std::fs::remove_file(&auth_path)
            .map_err(|e| ApiError::Internal(format!("Failed to logout: {}", e)))?;
    }

    Ok(Json(serde_json::json!({
        "success": true,
        "message": "Logged out successfully"
    })))
}
