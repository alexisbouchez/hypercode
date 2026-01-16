use std::mem::swap;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Result;
use hypercode_core::HypercodeAuth;
use hypercode_core::HypercodeThread;
use hypercode_core::ModelProviderInfo;
use hypercode_core::ThreadManager;
use hypercode_core::WireApi;
use hypercode_core::built_in_model_providers;
use hypercode_core::config::Config;
use hypercode_core::features::Feature;
use hypercode_core::protocol::AskForApproval;
use hypercode_core::protocol::EventMsg;
use hypercode_core::protocol::Op;
use hypercode_core::protocol::SandboxPolicy;
use hypercode_core::protocol::SessionConfiguredEvent;
use hypercode_protocol::config_types::ReasoningSummary;
use hypercode_protocol::user_input::UserInput;
use serde_json::Value;
use tempfile::TempDir;
use wiremock::MockServer;

use crate::load_default_config_for_test;
use crate::responses::WebSocketTestServer;
use crate::responses::start_mock_server;
use crate::streaming_sse::StreamingSseServer;
use crate::wait_for_event;
use wiremock::Match;
use wiremock::matchers::path_regex;

type ConfigMutator = dyn FnOnce(&mut Config) + Send;
type PreBuildHook = dyn FnOnce(&Path) + Send + 'static;

/// A collection of different ways the model can output an apply_patch call
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ApplyPatchModelOutput {
    Freeform,
    Function,
    Shell,
    ShellViaHeredoc,
    ShellCommandViaHeredoc,
}

/// A collection of different ways the model can output an apply_patch call
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ShellModelOutput {
    Shell,
    ShellCommand,
    LocalShell,
    // UnifiedExec has its own set of tests
}

pub struct TestHypercodeBuilder {
    config_mutators: Vec<Box<ConfigMutator>>,
    auth: HypercodeAuth,
    pre_build_hooks: Vec<Box<PreBuildHook>>,
}

impl TestHypercodeBuilder {
    pub fn with_config<T>(mut self, mutator: T) -> Self
    where
        T: FnOnce(&mut Config) + Send + 'static,
    {
        self.config_mutators.push(Box::new(mutator));
        self
    }

    pub fn with_auth(mut self, auth: HypercodeAuth) -> Self {
        self.auth = auth;
        self
    }

    pub fn with_model(self, model: &str) -> Self {
        let new_model = model.to_string();
        self.with_config(move |config| {
            config.model = Some(new_model.clone());
        })
    }

    pub fn with_pre_build_hook<F>(mut self, hook: F) -> Self
    where
        F: FnOnce(&Path) + Send + 'static,
    {
        self.pre_build_hooks.push(Box::new(hook));
        self
    }

    pub async fn build(&mut self, server: &wiremock::MockServer) -> anyhow::Result<TestHypercode> {
        let home = Arc::new(TempDir::new()?);
        self.build_with_home(server, home, None).await
    }

    pub async fn build_with_streaming_server(
        &mut self,
        server: &StreamingSseServer,
    ) -> anyhow::Result<TestHypercode> {
        let base_url = server.uri();
        let home = Arc::new(TempDir::new()?);
        self.build_with_home_and_base_url(format!("{base_url}/v1"), home, None)
            .await
    }

    pub async fn build_with_websocket_server(
        &mut self,
        server: &WebSocketTestServer,
    ) -> anyhow::Result<TestHypercode> {
        let base_url = format!("{}/v1", server.uri());
        let home = Arc::new(TempDir::new()?);
        let base_url_clone = base_url.clone();
        self.config_mutators.push(Box::new(move |config| {
            config.model_provider.base_url = Some(base_url_clone);
            config.model_provider.wire_api = WireApi::ResponsesWebsocket;
        }));
        self.build_with_home_and_base_url(base_url, home, None)
            .await
    }

    pub async fn resume(
        &mut self,
        server: &wiremock::MockServer,
        home: Arc<TempDir>,
        rollout_path: PathBuf,
    ) -> anyhow::Result<TestHypercode> {
        self.build_with_home(server, home, Some(rollout_path)).await
    }

    async fn build_with_home(
        &mut self,
        server: &wiremock::MockServer,
        home: Arc<TempDir>,
        resume_from: Option<PathBuf>,
    ) -> anyhow::Result<TestHypercode> {
        let base_url = format!("{}/v1", server.uri());
        let (config, cwd) = self.prepare_config(base_url, &home).await?;
        self.build_from_config(config, cwd, home, resume_from).await
    }

    async fn build_with_home_and_base_url(
        &mut self,
        base_url: String,
        home: Arc<TempDir>,
        resume_from: Option<PathBuf>,
    ) -> anyhow::Result<TestHypercode> {
        let (config, cwd) = self.prepare_config(base_url, &home).await?;
        self.build_from_config(config, cwd, home, resume_from).await
    }

    async fn build_from_config(
        &mut self,
        config: Config,
        cwd: Arc<TempDir>,
        home: Arc<TempDir>,
        resume_from: Option<PathBuf>,
    ) -> anyhow::Result<TestHypercode> {
        let auth = self.auth.clone();
        let thread_manager = ThreadManager::with_models_provider_and_home(
            auth.clone(),
            config.model_provider.clone(),
            config.hypercode_home.clone(),
        );
        let thread_manager = Arc::new(thread_manager);

        let new_conversation = match resume_from {
            Some(path) => {
                let auth_manager = hypercode_core::AuthManager::from_auth_for_testing(auth);
                thread_manager
                    .resume_thread_from_rollout(config.clone(), path, auth_manager)
                    .await?
            }
            None => thread_manager.start_thread(config.clone()).await?,
        };

        Ok(TestHypercode {
            home,
            cwd,
            config,
            hypercode: new_conversation.thread,
            session_configured: new_conversation.session_configured,
            thread_manager,
        })
    }

    async fn prepare_config(
        &mut self,
        base_url: String,
        home: &TempDir,
    ) -> anyhow::Result<(Config, Arc<TempDir>)> {
        let model_provider = ModelProviderInfo {
            base_url: Some(base_url),
            ..built_in_model_providers()["openai"].clone()
        };
        let cwd = Arc::new(TempDir::new()?);
        let mut config = load_default_config_for_test(home).await;
        config.cwd = cwd.path().to_path_buf();
        config.model_provider = model_provider;
        for hook in self.pre_build_hooks.drain(..) {
            hook(home.path());
        }
        if let Ok(path) = hypercode_utils_cargo_bin::cargo_bin("hypercode") {
            config.hypercode_linux_sandbox_exe = Some(path);
        }

        let mut mutators = vec![];
        swap(&mut self.config_mutators, &mut mutators);
        for mutator in mutators {
            mutator(&mut config);
        }

        if config.include_apply_patch_tool {
            config.features.enable(Feature::ApplyPatchFreeform);
        } else {
            config.features.disable(Feature::ApplyPatchFreeform);
        }

        Ok((config, cwd))
    }
}

pub struct TestHypercode {
    pub home: Arc<TempDir>,
    pub cwd: Arc<TempDir>,
    pub hypercode: Arc<HypercodeThread>,
    pub session_configured: SessionConfiguredEvent,
    pub config: Config,
    pub thread_manager: Arc<ThreadManager>,
}

impl TestHypercode {
    pub fn cwd_path(&self) -> &Path {
        self.cwd.path()
    }

    pub fn hypercode_home_path(&self) -> &Path {
        self.config.hypercode_home.as_path()
    }

    pub fn workspace_path(&self, rel: impl AsRef<Path>) -> PathBuf {
        self.cwd_path().join(rel)
    }

    pub async fn submit_turn(&self, prompt: &str) -> Result<()> {
        self.submit_turn_with_policies(
            prompt,
            AskForApproval::Never,
            SandboxPolicy::DangerFullAccess,
        )
        .await
    }

    pub async fn submit_turn_with_policy(
        &self,
        prompt: &str,
        sandbox_policy: SandboxPolicy,
    ) -> Result<()> {
        self.submit_turn_with_policies(prompt, AskForApproval::Never, sandbox_policy)
            .await
    }

    pub async fn submit_turn_with_policies(
        &self,
        prompt: &str,
        approval_policy: AskForApproval,
        sandbox_policy: SandboxPolicy,
    ) -> Result<()> {
        let session_model = self.session_configured.model.clone();
        self.hypercode
            .submit(Op::UserTurn {
                items: vec![UserInput::Text {
                    text: prompt.into(),
                    text_elements: Vec::new(),
                }],
                final_output_json_schema: None,
                cwd: self.cwd.path().to_path_buf(),
                approval_policy,
                sandbox_policy,
                model: session_model,
                effort: None,
                summary: ReasoningSummary::Auto,
            })
            .await?;

        wait_for_event(&self.hypercode, |event| {
            matches!(event, EventMsg::TurnComplete(_))
        })
        .await;
        Ok(())
    }
}

pub struct TestHypercodeHarness {
    server: MockServer,
    test: TestHypercode,
}

impl TestHypercodeHarness {
    pub async fn new() -> Result<Self> {
        Self::with_builder(test_hypercode()).await
    }

    pub async fn with_config(mutator: impl FnOnce(&mut Config) + Send + 'static) -> Result<Self> {
        Self::with_builder(test_hypercode().with_config(mutator)).await
    }

    pub async fn with_builder(mut builder: TestHypercodeBuilder) -> Result<Self> {
        let server = start_mock_server().await;
        let test = builder.build(&server).await?;
        Ok(Self { server, test })
    }

    pub fn server(&self) -> &MockServer {
        &self.server
    }

    pub fn test(&self) -> &TestHypercode {
        &self.test
    }

    pub fn cwd(&self) -> &Path {
        self.test.cwd_path()
    }

    pub fn path(&self, rel: impl AsRef<Path>) -> PathBuf {
        self.test.workspace_path(rel)
    }

    pub async fn submit(&self, prompt: &str) -> Result<()> {
        self.test.submit_turn(prompt).await
    }

    pub async fn submit_with_policy(
        &self,
        prompt: &str,
        sandbox_policy: SandboxPolicy,
    ) -> Result<()> {
        self.test
            .submit_turn_with_policy(prompt, sandbox_policy)
            .await
    }

    pub async fn request_bodies(&self) -> Vec<Value> {
        let path_matcher = path_regex(".*/responses$");
        self.server
            .received_requests()
            .await
            .expect("mock server should not fail")
            .into_iter()
            .filter(|req| path_matcher.matches(req))
            .map(|req| {
                req.body_json::<Value>()
                    .expect("request body to be valid JSON")
            })
            .collect()
    }

    pub async fn function_call_output_value(&self, call_id: &str) -> Value {
        let bodies = self.request_bodies().await;
        function_call_output(&bodies, call_id).clone()
    }

    pub async fn function_call_stdout(&self, call_id: &str) -> String {
        self.function_call_output_value(call_id)
            .await
            .get("output")
            .and_then(Value::as_str)
            .expect("output string")
            .to_string()
    }

    pub async fn custom_tool_call_output(&self, call_id: &str) -> String {
        let bodies = self.request_bodies().await;
        custom_tool_call_output(&bodies, call_id)
            .get("output")
            .and_then(Value::as_str)
            .expect("output string")
            .to_string()
    }

    pub async fn apply_patch_output(
        &self,
        call_id: &str,
        output_type: ApplyPatchModelOutput,
    ) -> String {
        match output_type {
            ApplyPatchModelOutput::Freeform => self.custom_tool_call_output(call_id).await,
            ApplyPatchModelOutput::Function
            | ApplyPatchModelOutput::Shell
            | ApplyPatchModelOutput::ShellViaHeredoc
            | ApplyPatchModelOutput::ShellCommandViaHeredoc => {
                self.function_call_stdout(call_id).await
            }
        }
    }
}

fn custom_tool_call_output<'a>(bodies: &'a [Value], call_id: &str) -> &'a Value {
    for body in bodies {
        if let Some(items) = body.get("input").and_then(Value::as_array) {
            for item in items {
                if item.get("type").and_then(Value::as_str) == Some("custom_tool_call_output")
                    && item.get("call_id").and_then(Value::as_str) == Some(call_id)
                {
                    return item;
                }
            }
        }
    }
    panic!("custom_tool_call_output {call_id} not found");
}

fn function_call_output<'a>(bodies: &'a [Value], call_id: &str) -> &'a Value {
    for body in bodies {
        if let Some(items) = body.get("input").and_then(Value::as_array) {
            for item in items {
                if item.get("type").and_then(Value::as_str) == Some("function_call_output")
                    && item.get("call_id").and_then(Value::as_str) == Some(call_id)
                {
                    return item;
                }
            }
        }
    }
    panic!("function_call_output {call_id} not found");
}

pub fn test_hypercode() -> TestHypercodeBuilder {
    TestHypercodeBuilder {
        config_mutators: vec![],
        auth: HypercodeAuth::from_api_key("dummy"),
        pre_build_hooks: vec![],
    }
}
