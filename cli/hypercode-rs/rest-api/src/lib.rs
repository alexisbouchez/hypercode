//! Hypercode REST API Server
//!
//! This crate provides an HTTP REST API gateway for the Hypercode agent,
//! exposing all functionality through standard REST endpoints with SSE streaming.

pub mod error;
pub mod handlers;
pub mod router;
pub mod state;
pub mod sse;

pub use router::create_router;
pub use state::AppState;
