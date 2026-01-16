//! Application state management with hypercode-core integration

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use hypercode_core::config::Config;
use hypercode_core::config::ConfigBuilder;
use hypercode_core::AuthManager;
use hypercode_core::HypercodeThread;
use hypercode_core::NewThread;
use hypercode_core::ThreadManager;
use hypercode_protocol::protocol::SessionSource;
use hypercode_protocol::ThreadId;
use tokio::sync::RwLock;

use crate::error::ApiError;

/// Thread session information
#[derive(Debug, Clone)]
pub struct ThreadSession {
    pub thread_id: ThreadId,
    pub thread_id_str: String,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub working_directory: Option<PathBuf>,
    pub model: Option<String>,
}

/// Application state shared across all handlers
#[derive(Clone)]
pub struct AppState {
    /// Thread manager for hypercode-core
    thread_manager: Arc<ThreadManager>,
    /// Auth manager for authentication
    auth_manager: Arc<AuthManager>,
    /// Base configuration
    config: Arc<Config>,
    /// Thread sessions keyed by thread ID string
    sessions: Arc<RwLock<HashMap<String, ThreadSession>>>,
    /// API key for REST API authentication (optional)
    api_key: Option<String>,
}

impl AppState {
    /// Create a new AppState with hypercode-core integration
    pub async fn new(api_key: Option<String>) -> Result<Self, anyhow::Error> {
        // Load default configuration
        let config = ConfigBuilder::default().build().await?;
        let config = Arc::new(config);

        // Create auth manager
        let auth_manager = AuthManager::shared(
            config.hypercode_home.clone(),
            false,
            config.cli_auth_credentials_store_mode,
        );

        // Create thread manager - use Mcp as the closest general-purpose source
        let thread_manager = Arc::new(ThreadManager::new(
            config.hypercode_home.clone(),
            auth_manager.clone(),
            SessionSource::Mcp,
        ));

        Ok(Self {
            thread_manager,
            auth_manager,
            config,
            sessions: Arc::new(RwLock::new(HashMap::new())),
            api_key,
        })
    }

    /// Get the thread manager
    pub fn thread_manager(&self) -> &Arc<ThreadManager> {
        &self.thread_manager
    }

    /// Get the auth manager
    pub fn auth_manager(&self) -> &Arc<AuthManager> {
        &self.auth_manager
    }

    /// Get the base configuration
    pub fn config(&self) -> &Arc<Config> {
        &self.config
    }

    /// Create a new thread
    pub async fn create_thread(
        &self,
        working_directory: Option<PathBuf>,
        model: Option<String>,
        instructions: Option<String>,
    ) -> Result<NewThread, ApiError> {
        // Build config for this thread
        let mut config = (*self.config).clone();

        if let Some(ref cwd) = working_directory {
            config.cwd = cwd.clone();
        }

        if let Some(ref model_id) = model {
            config.model = Some(model_id.clone());
        }

        if let Some(ref instr) = instructions {
            config.user_instructions = Some(instr.clone());
        }

        // Start the thread
        let new_thread = self
            .thread_manager
            .start_thread(config)
            .await
            .map_err(|e| ApiError::Internal(format!("Failed to create thread: {}", e)))?;

        // Store session info
        let session = ThreadSession {
            thread_id: new_thread.thread_id,
            thread_id_str: new_thread.thread_id.to_string(),
            created_at: chrono::Utc::now(),
            working_directory,
            model,
        };
        self.sessions
            .write()
            .await
            .insert(new_thread.thread_id.to_string(), session);

        Ok(new_thread)
    }

    /// Get a thread by ID string
    pub async fn get_thread(
        &self,
        thread_id_str: &str,
    ) -> Result<Arc<HypercodeThread>, ApiError> {
        let thread_id = ThreadId::from_string(thread_id_str)
            .map_err(|_| ApiError::ThreadNotFound(thread_id_str.to_string()))?;

        self.thread_manager
            .get_thread(thread_id)
            .await
            .map_err(|_| ApiError::ThreadNotFound(thread_id_str.to_string()))
    }

    /// Get thread session info
    pub async fn get_session(&self, thread_id_str: &str) -> Option<ThreadSession> {
        self.sessions.read().await.get(thread_id_str).cloned()
    }

    /// Remove a thread
    pub async fn remove_thread(&self, thread_id_str: &str) -> Result<(), ApiError> {
        let thread_id = ThreadId::from_string(thread_id_str)
            .map_err(|_| ApiError::ThreadNotFound(thread_id_str.to_string()))?;

        self.thread_manager.remove_thread(&thread_id).await;
        self.sessions.write().await.remove(thread_id_str);
        Ok(())
    }

    /// List all thread IDs
    pub async fn list_thread_ids(&self) -> Vec<String> {
        self.thread_manager
            .list_thread_ids()
            .await
            .into_iter()
            .map(|id| id.to_string())
            .collect()
    }

    /// Validate API key
    pub fn validate_api_key(&self, provided_key: Option<&str>) -> bool {
        match (&self.api_key, provided_key) {
            (None, _) => true, // No API key required
            (Some(expected), Some(provided)) => expected == provided,
            (Some(_), None) => false,
        }
    }
}
