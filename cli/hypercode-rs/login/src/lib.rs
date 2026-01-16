mod device_code_auth;
mod pkce;
mod server;

pub use device_code_auth::DeviceCode;
pub use device_code_auth::complete_device_code_login;
pub use device_code_auth::request_device_code;
pub use device_code_auth::run_device_code_login;
pub use server::LoginServer;
pub use server::ServerOptions;
pub use server::ShutdownHandle;
pub use server::run_login_server;

// Re-export commonly used auth types and helpers from hypercode-core for compatibility
pub use hypercode_app_server_protocol::AuthMode;
pub use hypercode_core::AuthManager;
pub use hypercode_core::HypercodeAuth;
pub use hypercode_core::auth::AuthDotJson;
pub use hypercode_core::auth::CLIENT_ID;
pub use hypercode_core::auth::CODEX_API_KEY_ENV_VAR;
pub use hypercode_core::auth::OPENAI_API_KEY_ENV_VAR;
pub use hypercode_core::auth::login_with_api_key;
pub use hypercode_core::auth::logout;
pub use hypercode_core::auth::save_auth;
pub use hypercode_core::token_data::TokenData;
