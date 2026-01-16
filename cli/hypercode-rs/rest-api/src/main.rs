//! Hypercode REST API Server
//!
//! Run with: cargo run -p hypercode-rest-api
//! Or: ./hypercode-rest-api --port 8080

use std::net::SocketAddr;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

use hypercode_rest_api::{create_router, AppState};

#[tokio::main]
async fn main() {
    // Initialize logging
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "hypercode_rest_api=debug,tower_http=debug".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    // Parse command line arguments
    let port: u16 = std::env::args()
        .position(|arg| arg == "--port")
        .and_then(|i| std::env::args().nth(i + 1))
        .and_then(|p| p.parse().ok())
        .unwrap_or(8080);

    let api_key = std::env::var("HYPERCODE_API_KEY").ok();

    // Create application state with hypercode-core integration
    let state = match AppState::new(api_key).await {
        Ok(state) => state,
        Err(e) => {
            tracing::error!("Failed to initialize application state: {}", e);
            std::process::exit(1);
        }
    };

    // Create router
    let app = create_router(state);

    // Start server
    let addr = SocketAddr::from(([0, 0, 0, 0], port));
    tracing::info!("Starting Hypercode REST API server on {}", addr);
    tracing::info!("Endpoints:");
    tracing::info!("  POST /threads - Create a new thread");
    tracing::info!("  GET  /threads - List all threads");
    tracing::info!("  POST /threads/:id/turns - Start a turn (non-streaming)");
    tracing::info!("  POST /threads/:id/turns/stream - Start a turn (streaming SSE)");
    tracing::info!("  GET  /models - List available models");
    tracing::info!("  GET  /config - Read configuration");
    tracing::info!("  GET  /account - Get account info");
    tracing::info!("  GET  /health - Health check");

    let listener = tokio::net::TcpListener::bind(addr).await.unwrap();
    axum::serve(listener, app).await.unwrap();
}
