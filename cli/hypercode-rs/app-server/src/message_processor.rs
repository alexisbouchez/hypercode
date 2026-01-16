use std::path::PathBuf;
use std::sync::Arc;

use crate::hypercode_message_processor::HypercodeMessageProcessor;
use crate::config_api::ConfigApi;
use crate::error_code::INVALID_REQUEST_ERROR_CODE;
use crate::outgoing_message::OutgoingMessageSender;
use hypercode_app_server_protocol::ClientInfo;
use hypercode_app_server_protocol::ClientRequest;
use hypercode_app_server_protocol::ConfigBatchWriteParams;
use hypercode_app_server_protocol::ConfigReadParams;
use hypercode_app_server_protocol::ConfigValueWriteParams;
use hypercode_app_server_protocol::ConfigWarningNotification;
use hypercode_app_server_protocol::InitializeResponse;
use hypercode_app_server_protocol::JSONRPCError;
use hypercode_app_server_protocol::JSONRPCErrorError;
use hypercode_app_server_protocol::JSONRPCNotification;
use hypercode_app_server_protocol::JSONRPCRequest;
use hypercode_app_server_protocol::JSONRPCResponse;
use hypercode_app_server_protocol::RequestId;
use hypercode_app_server_protocol::ServerNotification;
use hypercode_core::AuthManager;
use hypercode_core::ThreadManager;
use hypercode_core::config::Config;
use hypercode_core::config_loader::LoaderOverrides;
use hypercode_core::default_client::SetOriginatorError;
use hypercode_core::default_client::USER_AGENT_SUFFIX;
use hypercode_core::default_client::get_hypercode_user_agent;
use hypercode_core::default_client::set_default_originator;
use hypercode_feedback::HypercodeFeedback;
use hypercode_protocol::ThreadId;
use hypercode_protocol::protocol::SessionSource;
use tokio::sync::broadcast;
use toml::Value as TomlValue;

pub(crate) struct MessageProcessor {
    outgoing: Arc<OutgoingMessageSender>,
    hypercode_message_processor: HypercodeMessageProcessor,
    config_api: ConfigApi,
    initialized: bool,
    config_warnings: Vec<ConfigWarningNotification>,
}

impl MessageProcessor {
    /// Create a new `MessageProcessor`, retaining a handle to the outgoing
    /// `Sender` so handlers can enqueue messages to be written to stdout.
    pub(crate) fn new(
        outgoing: OutgoingMessageSender,
        hypercode_linux_sandbox_exe: Option<PathBuf>,
        config: Arc<Config>,
        cli_overrides: Vec<(String, TomlValue)>,
        loader_overrides: LoaderOverrides,
        feedback: HypercodeFeedback,
        config_warnings: Vec<ConfigWarningNotification>,
    ) -> Self {
        let outgoing = Arc::new(outgoing);
        let auth_manager = AuthManager::shared(
            config.hypercode_home.clone(),
            false,
            config.cli_auth_credentials_store_mode,
        );
        let thread_manager = Arc::new(ThreadManager::new(
            config.hypercode_home.clone(),
            auth_manager.clone(),
            SessionSource::VSCode,
        ));
        let hypercode_message_processor = HypercodeMessageProcessor::new(
            auth_manager,
            thread_manager,
            outgoing.clone(),
            hypercode_linux_sandbox_exe,
            Arc::clone(&config),
            cli_overrides.clone(),
            feedback,
        );
        let config_api = ConfigApi::new(config.hypercode_home.clone(), cli_overrides, loader_overrides);

        Self {
            outgoing,
            hypercode_message_processor,
            config_api,
            initialized: false,
            config_warnings,
        }
    }

    pub(crate) async fn process_request(&mut self, request: JSONRPCRequest) {
        let request_id = request.id.clone();
        let request_json = match serde_json::to_value(&request) {
            Ok(request_json) => request_json,
            Err(err) => {
                let error = JSONRPCErrorError {
                    code: INVALID_REQUEST_ERROR_CODE,
                    message: format!("Invalid request: {err}"),
                    data: None,
                };
                self.outgoing.send_error(request_id, error).await;
                return;
            }
        };

        let hypercode_request = match serde_json::from_value::<ClientRequest>(request_json) {
            Ok(hypercode_request) => hypercode_request,
            Err(err) => {
                let error = JSONRPCErrorError {
                    code: INVALID_REQUEST_ERROR_CODE,
                    message: format!("Invalid request: {err}"),
                    data: None,
                };
                self.outgoing.send_error(request_id, error).await;
                return;
            }
        };

        match hypercode_request {
            // Handle Initialize internally so HypercodeMessageProcessor does not have to concern
            // itself with the `initialized` bool.
            ClientRequest::Initialize { request_id, params } => {
                if self.initialized {
                    let error = JSONRPCErrorError {
                        code: INVALID_REQUEST_ERROR_CODE,
                        message: "Already initialized".to_string(),
                        data: None,
                    };
                    self.outgoing.send_error(request_id, error).await;
                    return;
                } else {
                    let ClientInfo {
                        name,
                        title: _title,
                        version,
                    } = params.client_info;
                    if let Err(error) = set_default_originator(name.clone()) {
                        match error {
                            SetOriginatorError::InvalidHeaderValue => {
                                let error = JSONRPCErrorError {
                                    code: INVALID_REQUEST_ERROR_CODE,
                                    message: format!(
                                        "Invalid clientInfo.name: '{name}'. Must be a valid HTTP header value."
                                    ),
                                    data: None,
                                };
                                self.outgoing.send_error(request_id, error).await;
                                return;
                            }
                            SetOriginatorError::AlreadyInitialized => {
                                // No-op. This is expected to happen if the originator is already set via env var.
                                // TODO(owen): Once we remove support for CODEX_INTERNAL_ORIGINATOR_OVERRIDE,
                                // this will be an unexpected state and we can return a JSON-RPC error indicating
                                // internal server error.
                            }
                        }
                    }
                    let user_agent_suffix = format!("{name}; {version}");
                    if let Ok(mut suffix) = USER_AGENT_SUFFIX.lock() {
                        *suffix = Some(user_agent_suffix);
                    }

                    let user_agent = get_hypercode_user_agent();
                    let response = InitializeResponse { user_agent };
                    self.outgoing.send_response(request_id, response).await;

                    self.initialized = true;
                    if !self.config_warnings.is_empty() {
                        for notification in self.config_warnings.drain(..) {
                            self.outgoing
                                .send_server_notification(ServerNotification::ConfigWarning(
                                    notification,
                                ))
                                .await;
                        }
                    }

                    return;
                }
            }
            _ => {
                if !self.initialized {
                    let error = JSONRPCErrorError {
                        code: INVALID_REQUEST_ERROR_CODE,
                        message: "Not initialized".to_string(),
                        data: None,
                    };
                    self.outgoing.send_error(request_id, error).await;
                    return;
                }
            }
        }

        match hypercode_request {
            ClientRequest::ConfigRead { request_id, params } => {
                self.handle_config_read(request_id, params).await;
            }
            ClientRequest::ConfigValueWrite { request_id, params } => {
                self.handle_config_value_write(request_id, params).await;
            }
            ClientRequest::ConfigBatchWrite { request_id, params } => {
                self.handle_config_batch_write(request_id, params).await;
            }
            ClientRequest::ConfigRequirementsRead {
                request_id,
                params: _,
            } => {
                self.handle_config_requirements_read(request_id).await;
            }
            other => {
                self.hypercode_message_processor.process_request(other).await;
            }
        }
    }

    pub(crate) async fn process_notification(&self, notification: JSONRPCNotification) {
        // Currently, we do not expect to receive any notifications from the
        // client, so we just log them.
        tracing::info!("<- notification: {:?}", notification);
    }

    pub(crate) fn thread_created_receiver(&self) -> broadcast::Receiver<ThreadId> {
        self.hypercode_message_processor.thread_created_receiver()
    }

    pub(crate) async fn try_attach_thread_listener(&mut self, thread_id: ThreadId) {
        if !self.initialized {
            return;
        }
        self.hypercode_message_processor
            .try_attach_thread_listener(thread_id)
            .await;
    }

    /// Handle a standalone JSON-RPC response originating from the peer.
    pub(crate) async fn process_response(&mut self, response: JSONRPCResponse) {
        tracing::info!("<- response: {:?}", response);
        let JSONRPCResponse { id, result, .. } = response;
        self.outgoing.notify_client_response(id, result).await
    }

    /// Handle an error object received from the peer.
    pub(crate) fn process_error(&mut self, err: JSONRPCError) {
        tracing::error!("<- error: {:?}", err);
    }

    async fn handle_config_read(&self, request_id: RequestId, params: ConfigReadParams) {
        match self.config_api.read(params).await {
            Ok(response) => self.outgoing.send_response(request_id, response).await,
            Err(error) => self.outgoing.send_error(request_id, error).await,
        }
    }

    async fn handle_config_value_write(
        &self,
        request_id: RequestId,
        params: ConfigValueWriteParams,
    ) {
        match self.config_api.write_value(params).await {
            Ok(response) => self.outgoing.send_response(request_id, response).await,
            Err(error) => self.outgoing.send_error(request_id, error).await,
        }
    }

    async fn handle_config_batch_write(
        &self,
        request_id: RequestId,
        params: ConfigBatchWriteParams,
    ) {
        match self.config_api.batch_write(params).await {
            Ok(response) => self.outgoing.send_response(request_id, response).await,
            Err(error) => self.outgoing.send_error(request_id, error).await,
        }
    }

    async fn handle_config_requirements_read(&self, request_id: RequestId) {
        match self.config_api.config_requirements_read().await {
            Ok(response) => self.outgoing.send_response(request_id, response).await,
            Err(error) => self.outgoing.send_error(request_id, error).await,
        }
    }
}
