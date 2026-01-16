//! API router configuration

use axum::{
    routing::{delete, get, post},
    Router,
};
use tower_http::cors::{Any, CorsLayer};
use tower_http::trace::TraceLayer;

use crate::handlers;
use crate::state::AppState;

/// Create the API router with all routes
pub fn create_router(state: AppState) -> Router {
    let cors = CorsLayer::new()
        .allow_origin(Any)
        .allow_methods(Any)
        .allow_headers(Any);

    Router::new()
        // Health
        .route("/health", get(handlers::health_check))
        // Threads
        .route("/threads", post(handlers::create_thread))
        .route("/threads", get(handlers::list_threads))
        .route("/threads/{id}", get(handlers::get_thread))
        .route("/threads/{id}", delete(handlers::archive_thread))
        .route("/threads/{id}/resume", post(handlers::resume_thread))
        .route("/threads/{id}/fork", post(handlers::fork_thread))
        .route("/threads/{id}/rollback", post(handlers::rollback_thread))
        // Turns
        .route("/threads/{id}/turns", post(handlers::start_turn))
        .route("/threads/{id}/turns/stream", post(handlers::start_turn_stream))
        .route("/threads/{id}/interrupt", post(handlers::interrupt_turn))
        // Models
        .route("/models", get(handlers::list_models))
        // Config
        .route("/config", get(handlers::read_config))
        .route("/config", post(handlers::write_config))
        .route("/config/batch", post(handlers::batch_write_config))
        // Account
        .route("/account", get(handlers::get_account))
        .route("/account/rate-limits", get(handlers::get_rate_limits))
        .route("/account/login", post(handlers::login))
        .route("/account/logout", post(handlers::logout))
        // Middleware
        .layer(cors)
        .layer(TraceLayer::new_for_http())
        .with_state(state)
}
