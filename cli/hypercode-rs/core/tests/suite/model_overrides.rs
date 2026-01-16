use hypercode_core::HypercodeAuth;
use hypercode_core::ThreadManager;
use hypercode_core::protocol::EventMsg;
use hypercode_core::protocol::Op;
use hypercode_protocol::openai_models::ReasoningEffort;
use core_test_support::load_default_config_for_test;
use core_test_support::wait_for_event;
use pretty_assertions::assert_eq;
use tempfile::TempDir;

const CONFIG_TOML: &str = "config.toml";

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn override_turn_context_does_not_persist_when_config_exists() {
    let hypercode_home = TempDir::new().unwrap();
    let config_path = hypercode_home.path().join(CONFIG_TOML);
    let initial_contents = "model = \"gpt-4o\"\n";
    tokio::fs::write(&config_path, initial_contents)
        .await
        .expect("seed config.toml");

    let mut config = load_default_config_for_test(&hypercode_home).await;
    config.model = Some("gpt-4o".to_string());

    let thread_manager = ThreadManager::with_models_provider(
        HypercodeAuth::from_api_key("Test API Key"),
        config.model_provider.clone(),
    );
    let hypercode = thread_manager
        .start_thread(config)
        .await
        .expect("create conversation")
        .thread;

    hypercode
        .submit(Op::OverrideTurnContext {
            cwd: None,
            approval_policy: None,
            sandbox_policy: None,
            model: Some("o3".to_string()),
            effort: Some(Some(ReasoningEffort::High)),
            summary: None,
        })
        .await
        .expect("submit override");

    hypercode.submit(Op::Shutdown).await.expect("request shutdown");
    wait_for_event(&hypercode, |ev| matches!(ev, EventMsg::ShutdownComplete)).await;

    let contents = tokio::fs::read_to_string(&config_path)
        .await
        .expect("read config.toml after override");
    assert_eq!(contents, initial_contents);
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn override_turn_context_does_not_create_config_file() {
    let hypercode_home = TempDir::new().unwrap();
    let config_path = hypercode_home.path().join(CONFIG_TOML);
    assert!(
        !config_path.exists(),
        "test setup should start without config"
    );

    let config = load_default_config_for_test(&hypercode_home).await;

    let thread_manager = ThreadManager::with_models_provider(
        HypercodeAuth::from_api_key("Test API Key"),
        config.model_provider.clone(),
    );
    let hypercode = thread_manager
        .start_thread(config)
        .await
        .expect("create conversation")
        .thread;

    hypercode
        .submit(Op::OverrideTurnContext {
            cwd: None,
            approval_policy: None,
            sandbox_policy: None,
            model: Some("o3".to_string()),
            effort: Some(Some(ReasoningEffort::Medium)),
            summary: None,
        })
        .await
        .expect("submit override");

    hypercode.submit(Op::Shutdown).await.expect("request shutdown");
    wait_for_event(&hypercode, |ev| matches!(ev, EventMsg::ShutdownComplete)).await;

    assert!(
        !config_path.exists(),
        "override should not create config.toml"
    );
}
