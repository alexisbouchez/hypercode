#![allow(clippy::unwrap_used)]

use hypercode_core::AuthManager;
use hypercode_core::auth::AuthCredentialsStoreMode;
use hypercode_core::auth::CLIENT_ID;
use hypercode_core::auth::login_with_api_key;
use hypercode_core::auth::login_with_mistral_api_key;
use hypercode_core::auth::read_mistral_api_key_from_env;
use hypercode_core::auth::read_openai_api_key_from_env;
use hypercode_core::env::is_headless_environment;
use hypercode_login::DeviceCode;
use hypercode_login::ServerOptions;
use hypercode_login::ShutdownHandle;
use hypercode_login::run_login_server;
use crossterm::event::KeyCode;
use crossterm::event::KeyEvent;
use crossterm::event::KeyEventKind;
use crossterm::event::KeyModifiers;
use ratatui::buffer::Buffer;
use ratatui::layout::Constraint;
use ratatui::layout::Layout;
use ratatui::layout::Rect;
use ratatui::prelude::Widget;
use ratatui::style::Color;
use ratatui::style::Modifier;
use ratatui::style::Style;
use ratatui::style::Stylize;
use ratatui::text::Line;
use ratatui::widgets::Block;
use ratatui::widgets::BorderType;
use ratatui::widgets::Borders;
use ratatui::widgets::Paragraph;
use ratatui::widgets::WidgetRef;
use ratatui::widgets::Wrap;

use hypercode_app_server_protocol::AuthMode;
use hypercode_protocol::config_types::ForcedLoginMethod;
use std::sync::RwLock;

use crate::LoginStatus;
use crate::onboarding::onboarding_screen::KeyboardHandler;
use crate::onboarding::onboarding_screen::StepStateProvider;
use crate::shimmer::shimmer_spans;
use crate::tui::FrameRequester;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::Notify;

use super::onboarding_screen::StepState;

mod headless_chatgpt_login;

/// Provider selection for the first step of onboarding
#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub(crate) enum SelectedProvider {
    #[default]
    OpenAI,
    Mistral,
}

/// OpenAI auth method selection (web login vs API key)
#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub(crate) enum OpenAIAuthMethod {
    #[default]
    ChatGPT,
    ApiKey,
}

#[derive(Clone)]
pub(crate) enum SignInState {
    /// First step: choose between OpenAI and Mistral
    PickProvider(SelectedProvider),
    /// Second step for OpenAI: choose between ChatGPT login and API key
    PickOpenAIMethod(OpenAIAuthMethod),
    ChatGptContinueInBrowser(ContinueInBrowserState),
    ChatGptDeviceCode(ContinueWithDeviceCodeState),
    ChatGptSuccessMessage,
    ChatGptSuccess,
    ApiKeyEntry(ApiKeyInputState),
    ApiKeyConfigured,
    MistralApiKeyEntry(ApiKeyInputState),
    MistralApiKeyConfigured,
}

const API_KEY_DISABLED_MESSAGE: &str = "API key login is disabled.";

#[derive(Clone, Default)]
pub(crate) struct ApiKeyInputState {
    value: String,
    prepopulated_from_env: bool,
}

#[derive(Clone)]
/// Used to manage the lifecycle of SpawnedLogin and ensure it gets cleaned up.
pub(crate) struct ContinueInBrowserState {
    auth_url: String,
    shutdown_flag: Option<ShutdownHandle>,
}

#[derive(Clone)]
pub(crate) struct ContinueWithDeviceCodeState {
    device_code: Option<DeviceCode>,
    cancel: Option<Arc<Notify>>,
}

impl Drop for ContinueInBrowserState {
    fn drop(&mut self) {
        if let Some(handle) = &self.shutdown_flag {
            handle.shutdown();
        }
    }
}

impl KeyboardHandler for AuthModeWidget {
    fn handle_key_event(&mut self, key_event: KeyEvent) {
        if self.handle_api_key_entry_key_event(&key_event) {
            return;
        }

        let sign_in_state = { (*self.sign_in_state.read().unwrap()).clone() };

        match key_event.code {
            KeyCode::Up | KeyCode::Char('k') => {
                match &sign_in_state {
                    SignInState::PickProvider(selected) => {
                        let new_selection = match selected {
                            SelectedProvider::Mistral => SelectedProvider::OpenAI,
                            SelectedProvider::OpenAI => SelectedProvider::OpenAI,
                        };
                        *self.sign_in_state.write().unwrap() =
                            SignInState::PickProvider(new_selection);
                        self.request_frame.schedule_frame();
                    }
                    SignInState::PickOpenAIMethod(method) => {
                        let new_method = match method {
                            OpenAIAuthMethod::ApiKey if self.is_chatgpt_login_allowed() => {
                                OpenAIAuthMethod::ChatGPT
                            }
                            _ => *method,
                        };
                        *self.sign_in_state.write().unwrap() =
                            SignInState::PickOpenAIMethod(new_method);
                        self.request_frame.schedule_frame();
                    }
                    _ => {}
                }
            }
            KeyCode::Down | KeyCode::Char('j') => {
                match &sign_in_state {
                    SignInState::PickProvider(selected) => {
                        let new_selection = match selected {
                            SelectedProvider::OpenAI => SelectedProvider::Mistral,
                            SelectedProvider::Mistral => SelectedProvider::Mistral,
                        };
                        *self.sign_in_state.write().unwrap() =
                            SignInState::PickProvider(new_selection);
                        self.request_frame.schedule_frame();
                    }
                    SignInState::PickOpenAIMethod(method) => {
                        let new_method = match method {
                            OpenAIAuthMethod::ChatGPT if self.is_api_login_allowed() => {
                                OpenAIAuthMethod::ApiKey
                            }
                            _ => *method,
                        };
                        *self.sign_in_state.write().unwrap() =
                            SignInState::PickOpenAIMethod(new_method);
                        self.request_frame.schedule_frame();
                    }
                    _ => {}
                }
            }
            KeyCode::Char('1') => match &sign_in_state {
                SignInState::PickProvider(_) => {
                    // Select OpenAI -> go to method selection
                    *self.sign_in_state.write().unwrap() =
                        SignInState::PickOpenAIMethod(OpenAIAuthMethod::default());
                    self.request_frame.schedule_frame();
                }
                SignInState::PickOpenAIMethod(_) if self.is_chatgpt_login_allowed() => {
                    self.start_chatgpt_login();
                }
                _ => {}
            },
            KeyCode::Char('2') => match &sign_in_state {
                SignInState::PickProvider(_) => {
                    // Select Mistral -> go directly to API key entry
                    if self.is_api_login_allowed() {
                        self.start_mistral_api_key_entry();
                    } else {
                        self.disallow_api_login();
                    }
                }
                SignInState::PickOpenAIMethod(_) => {
                    if self.is_api_login_allowed() {
                        self.start_api_key_entry();
                    } else {
                        self.disallow_api_login();
                    }
                }
                _ => {}
            },
            KeyCode::Enter => {
                match sign_in_state {
                    SignInState::PickProvider(selected) => match selected {
                        SelectedProvider::OpenAI => {
                            *self.sign_in_state.write().unwrap() =
                                SignInState::PickOpenAIMethod(OpenAIAuthMethod::default());
                            self.request_frame.schedule_frame();
                        }
                        SelectedProvider::Mistral => {
                            if self.is_api_login_allowed() {
                                self.start_mistral_api_key_entry();
                            } else {
                                self.disallow_api_login();
                            }
                        }
                    },
                    SignInState::PickOpenAIMethod(method) => match method {
                        OpenAIAuthMethod::ChatGPT if self.is_chatgpt_login_allowed() => {
                            self.start_chatgpt_login();
                        }
                        OpenAIAuthMethod::ApiKey if self.is_api_login_allowed() => {
                            self.start_api_key_entry();
                        }
                        OpenAIAuthMethod::ChatGPT => {}
                        OpenAIAuthMethod::ApiKey => {
                            self.disallow_api_login();
                        }
                    },
                    SignInState::ChatGptSuccessMessage => {
                        *self.sign_in_state.write().unwrap() = SignInState::ChatGptSuccess;
                    }
                    _ => {}
                }
            }
            KeyCode::Esc => {
                tracing::info!("Esc pressed");
                let mut sign_in_state = self.sign_in_state.write().unwrap();
                match &*sign_in_state {
                    SignInState::PickOpenAIMethod(_) => {
                        // Go back to provider selection
                        *sign_in_state = SignInState::PickProvider(SelectedProvider::OpenAI);
                        drop(sign_in_state);
                        self.request_frame.schedule_frame();
                    }
                    SignInState::ChatGptContinueInBrowser(_) => {
                        *sign_in_state =
                            SignInState::PickOpenAIMethod(OpenAIAuthMethod::ChatGPT);
                        drop(sign_in_state);
                        self.request_frame.schedule_frame();
                    }
                    SignInState::ChatGptDeviceCode(state) => {
                        if let Some(cancel) = &state.cancel {
                            cancel.notify_one();
                        }
                        *sign_in_state =
                            SignInState::PickOpenAIMethod(OpenAIAuthMethod::ChatGPT);
                        drop(sign_in_state);
                        self.request_frame.schedule_frame();
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    fn handle_paste(&mut self, pasted: String) {
        let _ = self.handle_api_key_entry_paste(pasted);
    }
}

#[derive(Clone)]
pub(crate) struct AuthModeWidget {
    pub request_frame: FrameRequester,
    pub error: Option<String>,
    pub sign_in_state: Arc<RwLock<SignInState>>,
    pub hypercode_home: PathBuf,
    pub cli_auth_credentials_store_mode: AuthCredentialsStoreMode,
    pub login_status: LoginStatus,
    pub auth_manager: Arc<AuthManager>,
    pub forced_chatgpt_workspace_id: Option<String>,
    pub forced_login_method: Option<ForcedLoginMethod>,
    pub animations_enabled: bool,
}

impl AuthModeWidget {
    fn is_api_login_allowed(&self) -> bool {
        !matches!(self.forced_login_method, Some(ForcedLoginMethod::Chatgpt))
    }

    fn is_chatgpt_login_allowed(&self) -> bool {
        !matches!(self.forced_login_method, Some(ForcedLoginMethod::Api))
    }

    fn disallow_api_login(&mut self) {
        self.error = Some(API_KEY_DISABLED_MESSAGE.to_string());
        *self.sign_in_state.write().unwrap() =
            SignInState::PickProvider(SelectedProvider::OpenAI);
        self.request_frame.schedule_frame();
    }

    fn render_pick_provider(&self, area: Rect, buf: &mut Buffer, selected: SelectedProvider) {
        let mut lines: Vec<Line> = vec![
            Line::from(vec!["  ".into(), "Choose your AI provider".bold()]),
            "".into(),
        ];

        let create_provider_item =
            |idx: usize, provider: SelectedProvider, text: &str, description: &str| -> Vec<Line<'static>> {
                let is_selected = selected == provider;
                let caret = if is_selected { ">" } else { " " };

                let line1 = if is_selected {
                    Line::from(vec![
                        format!("{} {}. ", caret, idx + 1).cyan().dim(),
                        text.to_string().cyan(),
                    ])
                } else {
                    format!("  {}. {text}", idx + 1).into()
                };

                let line2 = if is_selected {
                    Line::from(format!("     {description}"))
                        .fg(Color::Cyan)
                        .add_modifier(Modifier::DIM)
                } else {
                    Line::from(format!("     {description}"))
                        .style(Style::default().add_modifier(Modifier::DIM))
                };

                vec![line1, line2]
            };

        lines.extend(create_provider_item(
            0,
            SelectedProvider::OpenAI,
            "OpenAI",
            "Use GPT models with ChatGPT login or API key",
        ));
        lines.push("".into());
        lines.extend(create_provider_item(
            1,
            SelectedProvider::Mistral,
            "Mistral",
            "Use Mistral models like devstral-small-latest",
        ));
        lines.push("".into());

        lines.push("  Press Enter to continue".dim().into());
        if let Some(err) = &self.error {
            lines.push("".into());
            lines.push(err.as_str().red().into());
        }

        Paragraph::new(lines)
            .wrap(Wrap { trim: false })
            .render(area, buf);
    }

    fn render_pick_openai_method(&self, area: Rect, buf: &mut Buffer, selected: OpenAIAuthMethod) {
        let mut lines: Vec<Line> = vec![
            Line::from(vec!["  ".into(), "OpenAI Authentication".bold()]),
            Line::from(vec![
                "  ".into(),
                "Sign in with ChatGPT or use an API key".dim(),
            ]),
            "".into(),
        ];

        let create_method_item =
            |idx: usize, method: OpenAIAuthMethod, text: &str, description: &str| -> Vec<Line<'static>> {
                let is_selected = selected == method;
                let caret = if is_selected { ">" } else { " " };

                let line1 = if is_selected {
                    Line::from(vec![
                        format!("{} {}. ", caret, idx + 1).cyan().dim(),
                        text.to_string().cyan(),
                    ])
                } else {
                    format!("  {}. {text}", idx + 1).into()
                };

                let line2 = if is_selected {
                    Line::from(format!("     {description}"))
                        .fg(Color::Cyan)
                        .add_modifier(Modifier::DIM)
                } else {
                    Line::from(format!("     {description}"))
                        .style(Style::default().add_modifier(Modifier::DIM))
                };

                vec![line1, line2]
            };

        let chatgpt_description = if !self.is_chatgpt_login_allowed() {
            "ChatGPT login is disabled"
        } else if is_headless_environment() {
            "Uses device code login (headless environment detected)"
        } else {
            "Usage included with Plus, Pro, Team, and Enterprise plans"
        };

        lines.extend(create_method_item(
            0,
            OpenAIAuthMethod::ChatGPT,
            "Sign in with ChatGPT",
            chatgpt_description,
        ));
        lines.push("".into());

        if self.is_api_login_allowed() {
            lines.extend(create_method_item(
                1,
                OpenAIAuthMethod::ApiKey,
                "Use OpenAI API key",
                "Pay for what you use with your API key",
            ));
            lines.push("".into());
        } else {
            lines.push(
                "  API key login is disabled by this workspace."
                    .dim()
                    .into(),
            );
            lines.push("".into());
        }

        lines.push("  Press Enter to continue, Esc to go back".dim().into());
        if let Some(err) = &self.error {
            lines.push("".into());
            lines.push(err.as_str().red().into());
        }

        Paragraph::new(lines)
            .wrap(Wrap { trim: false })
            .render(area, buf);
    }

    fn render_continue_in_browser(&self, area: Rect, buf: &mut Buffer) {
        let mut spans = vec!["  ".into()];
        if self.animations_enabled {
            // Schedule a follow-up frame to keep the shimmer animation going.
            self.request_frame
                .schedule_frame_in(std::time::Duration::from_millis(100));
            spans.extend(shimmer_spans("Finish signing in via your browser"));
        } else {
            spans.push("Finish signing in via your browser".into());
        }
        let mut lines = vec![spans.into(), "".into()];

        let sign_in_state = self.sign_in_state.read().unwrap();
        if let SignInState::ChatGptContinueInBrowser(state) = &*sign_in_state
            && !state.auth_url.is_empty()
        {
            lines.push("  If the link doesn't open automatically, open the following link to authenticate:".into());
            lines.push("".into());
            lines.push(Line::from(vec![
                "  ".into(),
                state.auth_url.as_str().cyan().underlined(),
            ]));
            lines.push("".into());
            lines.push(Line::from(vec![
                "  On a remote or headless machine? Use ".into(),
                "hypercode login --device-auth".cyan(),
                " instead".into(),
            ]));
            lines.push("".into());
        }

        lines.push("  Press Esc to cancel".dim().into());
        Paragraph::new(lines)
            .wrap(Wrap { trim: false })
            .render(area, buf);
    }

    fn render_chatgpt_success_message(&self, area: Rect, buf: &mut Buffer) {
        let lines = vec![
            "✓ Signed in with your ChatGPT account".fg(Color::Green).into(),
            "".into(),
            "  Before you start:".into(),
            "".into(),
            "  Decide how much autonomy you want to grant Hypercode".into(),
            Line::from(vec![
                "  For more details see the ".into(),
                "\u{1b}]8;;https://github.com/openai/hypercode\u{7}Hypercode docs\u{1b}]8;;\u{7}".underlined(),
            ])
            .dim(),
            "".into(),
            "  Hypercode can make mistakes".into(),
            "  Review the code it writes and commands it runs".dim().into(),
            "".into(),
            "  Powered by your ChatGPT account".into(),
            Line::from(vec![
                "  Uses your plan's rate limits and ".into(),
                "\u{1b}]8;;https://chatgpt.com/#settings\u{7}training data preferences\u{1b}]8;;\u{7}".underlined(),
            ])
            .dim(),
            "".into(),
            "  Press Enter to continue".fg(Color::Cyan).into(),
        ];

        Paragraph::new(lines)
            .wrap(Wrap { trim: false })
            .render(area, buf);
    }

    fn render_chatgpt_success(&self, area: Rect, buf: &mut Buffer) {
        let lines = vec![
            "✓ Signed in with your ChatGPT account"
                .fg(Color::Green)
                .into(),
        ];

        Paragraph::new(lines)
            .wrap(Wrap { trim: false })
            .render(area, buf);
    }

    fn render_api_key_configured(&self, area: Rect, buf: &mut Buffer) {
        let lines = vec![
            "✓ API key configured".fg(Color::Green).into(),
            "".into(),
            "  Hypercode will use usage-based billing with your API key.".into(),
        ];

        Paragraph::new(lines)
            .wrap(Wrap { trim: false })
            .render(area, buf);
    }

    fn render_mistral_api_key_entry(&self, area: Rect, buf: &mut Buffer, state: &ApiKeyInputState) {
        let [intro_area, input_area, footer_area] = Layout::vertical([
            Constraint::Min(4),
            Constraint::Length(3),
            Constraint::Min(2),
        ])
        .areas(area);

        let mut intro_lines: Vec<Line> = vec![
            Line::from(vec![
                "> ".into(),
                "Use your own Mistral API key".bold(),
            ]),
            "".into(),
            "  Paste or type your Mistral API key below. It will be stored locally in auth.json.".into(),
            "".into(),
        ];
        if state.prepopulated_from_env {
            intro_lines.push("  Detected MISTRAL_API_KEY environment variable.".into());
            intro_lines.push(
                "  Paste a different key if you prefer to use another account."
                    .dim()
                    .into(),
            );
            intro_lines.push("".into());
        }
        Paragraph::new(intro_lines)
            .wrap(Wrap { trim: false })
            .render(intro_area, buf);

        let content_line: Line = if state.value.is_empty() {
            vec!["Paste or type your Mistral API key".dim()].into()
        } else {
            Line::from(state.value.clone())
        };
        Paragraph::new(content_line)
            .wrap(Wrap { trim: false })
            .block(
                Block::default()
                    .title("Mistral API key")
                    .borders(Borders::ALL)
                    .border_type(BorderType::Rounded)
                    .border_style(Style::default().fg(Color::Cyan)),
            )
            .render(input_area, buf);

        let mut footer_lines: Vec<Line> = vec![
            "  Press Enter to save".dim().into(),
            "  Press Esc to go back".dim().into(),
        ];
        if let Some(error) = &self.error {
            footer_lines.push("".into());
            footer_lines.push(error.as_str().red().into());
        }
        Paragraph::new(footer_lines)
            .wrap(Wrap { trim: false })
            .render(footer_area, buf);
    }

    fn render_mistral_api_key_configured(&self, area: Rect, buf: &mut Buffer) {
        let lines = vec![
            "✓ Mistral API key configured".fg(Color::Green).into(),
            "".into(),
            "  Hypercode will use Mistral models with your API key.".into(),
        ];

        Paragraph::new(lines)
            .wrap(Wrap { trim: false })
            .render(area, buf);
    }

    fn render_api_key_entry(&self, area: Rect, buf: &mut Buffer, state: &ApiKeyInputState) {
        let [intro_area, input_area, footer_area] = Layout::vertical([
            Constraint::Min(4),
            Constraint::Length(3),
            Constraint::Min(2),
        ])
        .areas(area);

        let mut intro_lines: Vec<Line> = vec![
            Line::from(vec![
                "> ".into(),
                "Use your own OpenAI API key for usage-based billing".bold(),
            ]),
            "".into(),
            "  Paste or type your API key below. It will be stored locally in auth.json.".into(),
            "".into(),
        ];
        if state.prepopulated_from_env {
            intro_lines.push("  Detected OPENAI_API_KEY environment variable.".into());
            intro_lines.push(
                "  Paste a different key if you prefer to use another account."
                    .dim()
                    .into(),
            );
            intro_lines.push("".into());
        }
        Paragraph::new(intro_lines)
            .wrap(Wrap { trim: false })
            .render(intro_area, buf);

        let content_line: Line = if state.value.is_empty() {
            vec!["Paste or type your API key".dim()].into()
        } else {
            Line::from(state.value.clone())
        };
        Paragraph::new(content_line)
            .wrap(Wrap { trim: false })
            .block(
                Block::default()
                    .title("API key")
                    .borders(Borders::ALL)
                    .border_type(BorderType::Rounded)
                    .border_style(Style::default().fg(Color::Cyan)),
            )
            .render(input_area, buf);

        let mut footer_lines: Vec<Line> = vec![
            "  Press Enter to save".dim().into(),
            "  Press Esc to go back".dim().into(),
        ];
        if let Some(error) = &self.error {
            footer_lines.push("".into());
            footer_lines.push(error.as_str().red().into());
        }
        Paragraph::new(footer_lines)
            .wrap(Wrap { trim: false })
            .render(footer_area, buf);
    }

    fn handle_api_key_entry_key_event(&mut self, key_event: &KeyEvent) -> bool {
        let mut should_save_openai: Option<String> = None;
        let mut should_save_mistral: Option<String> = None;
        let mut should_request_frame = false;

        {
            let mut guard = self.sign_in_state.write().unwrap();
            let is_mistral = matches!(&*guard, SignInState::MistralApiKeyEntry(_));
            let state = match &mut *guard {
                SignInState::ApiKeyEntry(s) => s,
                SignInState::MistralApiKeyEntry(s) => s,
                _ => return false,
            };

            match key_event.code {
                KeyCode::Esc => {
                    // Go back to the appropriate screen based on which API key entry we're in
                    *guard = if is_mistral {
                        SignInState::PickProvider(SelectedProvider::Mistral)
                    } else {
                        SignInState::PickOpenAIMethod(OpenAIAuthMethod::ApiKey)
                    };
                    self.error = None;
                    should_request_frame = true;
                }
                KeyCode::Enter => {
                    let trimmed = state.value.trim().to_string();
                    if trimmed.is_empty() {
                        self.error = Some("API key cannot be empty".to_string());
                        should_request_frame = true;
                    } else if is_mistral {
                        should_save_mistral = Some(trimmed);
                    } else {
                        should_save_openai = Some(trimmed);
                    }
                }
                KeyCode::Backspace => {
                    if state.prepopulated_from_env {
                        state.value.clear();
                        state.prepopulated_from_env = false;
                    } else {
                        state.value.pop();
                    }
                    self.error = None;
                    should_request_frame = true;
                }
                KeyCode::Char(c)
                    if key_event.kind == KeyEventKind::Press
                        && !key_event.modifiers.contains(KeyModifiers::SUPER)
                        && !key_event.modifiers.contains(KeyModifiers::CONTROL)
                        && !key_event.modifiers.contains(KeyModifiers::ALT) =>
                {
                    if state.prepopulated_from_env {
                        state.value.clear();
                        state.prepopulated_from_env = false;
                    }
                    state.value.push(c);
                    self.error = None;
                    should_request_frame = true;
                }
                _ => {}
            }
            // handled; let guard drop before potential save
        }

        if let Some(api_key) = should_save_openai {
            self.save_api_key(api_key);
        } else if let Some(api_key) = should_save_mistral {
            self.save_mistral_api_key(api_key);
        } else if should_request_frame {
            self.request_frame.schedule_frame();
        }
        true
    }

    fn handle_api_key_entry_paste(&mut self, pasted: String) -> bool {
        let trimmed = pasted.trim();
        if trimmed.is_empty() {
            return false;
        }

        let mut guard = self.sign_in_state.write().unwrap();
        let state = match &mut *guard {
            SignInState::ApiKeyEntry(s) => s,
            SignInState::MistralApiKeyEntry(s) => s,
            _ => return false,
        };

        if state.prepopulated_from_env {
            state.value = trimmed.to_string();
            state.prepopulated_from_env = false;
        } else {
            state.value.push_str(trimmed);
        }
        self.error = None;

        drop(guard);
        self.request_frame.schedule_frame();
        true
    }

    fn start_api_key_entry(&mut self) {
        if !self.is_api_login_allowed() {
            self.disallow_api_login();
            return;
        }
        self.error = None;
        let prefill_from_env = read_openai_api_key_from_env();
        let mut guard = self.sign_in_state.write().unwrap();
        match &mut *guard {
            SignInState::ApiKeyEntry(state) => {
                if state.value.is_empty() {
                    if let Some(prefill) = prefill_from_env {
                        state.value = prefill;
                        state.prepopulated_from_env = true;
                    } else {
                        state.prepopulated_from_env = false;
                    }
                }
            }
            _ => {
                *guard = SignInState::ApiKeyEntry(ApiKeyInputState {
                    value: prefill_from_env.clone().unwrap_or_default(),
                    prepopulated_from_env: prefill_from_env.is_some(),
                });
            }
        }
        drop(guard);
        self.request_frame.schedule_frame();
    }

    fn save_api_key(&mut self, api_key: String) {
        if !self.is_api_login_allowed() {
            self.disallow_api_login();
            return;
        }
        match login_with_api_key(
            &self.hypercode_home,
            &api_key,
            self.cli_auth_credentials_store_mode,
        ) {
            Ok(()) => {
                self.error = None;
                self.login_status = LoginStatus::AuthMode(AuthMode::ApiKey);
                self.auth_manager.reload();
                *self.sign_in_state.write().unwrap() = SignInState::ApiKeyConfigured;
            }
            Err(err) => {
                self.error = Some(format!("Failed to save API key: {err}"));
                let mut guard = self.sign_in_state.write().unwrap();
                if let SignInState::ApiKeyEntry(existing) = &mut *guard {
                    if existing.value.is_empty() {
                        existing.value.push_str(&api_key);
                    }
                    existing.prepopulated_from_env = false;
                } else {
                    *guard = SignInState::ApiKeyEntry(ApiKeyInputState {
                        value: api_key,
                        prepopulated_from_env: false,
                    });
                }
            }
        }

        self.request_frame.schedule_frame();
    }

    fn start_mistral_api_key_entry(&mut self) {
        if !self.is_api_login_allowed() {
            self.disallow_api_login();
            return;
        }
        self.error = None;
        let prefill_from_env = read_mistral_api_key_from_env();
        let mut guard = self.sign_in_state.write().unwrap();
        match &mut *guard {
            SignInState::MistralApiKeyEntry(state) => {
                if state.value.is_empty() {
                    if let Some(prefill) = prefill_from_env {
                        state.value = prefill;
                        state.prepopulated_from_env = true;
                    } else {
                        state.prepopulated_from_env = false;
                    }
                }
            }
            _ => {
                *guard = SignInState::MistralApiKeyEntry(ApiKeyInputState {
                    value: prefill_from_env.clone().unwrap_or_default(),
                    prepopulated_from_env: prefill_from_env.is_some(),
                });
            }
        }
        drop(guard);
        self.request_frame.schedule_frame();
    }

    fn save_mistral_api_key(&mut self, api_key: String) {
        if !self.is_api_login_allowed() {
            self.disallow_api_login();
            return;
        }
        match login_with_mistral_api_key(
            &self.hypercode_home,
            &api_key,
            self.cli_auth_credentials_store_mode,
        ) {
            Ok(()) => {
                self.error = None;
                self.login_status = LoginStatus::AuthMode(AuthMode::Mistral);
                self.auth_manager.reload();
                *self.sign_in_state.write().unwrap() = SignInState::MistralApiKeyConfigured;
            }
            Err(err) => {
                self.error = Some(format!("Failed to save Mistral API key: {err}"));
                let mut guard = self.sign_in_state.write().unwrap();
                if let SignInState::MistralApiKeyEntry(existing) = &mut *guard {
                    if existing.value.is_empty() {
                        existing.value.push_str(&api_key);
                    }
                    existing.prepopulated_from_env = false;
                } else {
                    *guard = SignInState::MistralApiKeyEntry(ApiKeyInputState {
                        value: api_key,
                        prepopulated_from_env: false,
                    });
                }
            }
        }

        self.request_frame.schedule_frame();
    }

    /// Kicks off the ChatGPT auth flow and keeps the UI state consistent with the attempt.
    fn start_chatgpt_login(&mut self) {
        // If we're already authenticated with ChatGPT, don't start a new login –
        // just proceed to the success message flow.
        if matches!(self.login_status, LoginStatus::AuthMode(AuthMode::ChatGPT)) {
            *self.sign_in_state.write().unwrap() = SignInState::ChatGptSuccess;
            self.request_frame.schedule_frame();
            return;
        }

        self.error = None;
        let opts = ServerOptions::new(
            self.hypercode_home.clone(),
            CLIENT_ID.to_string(),
            self.forced_chatgpt_workspace_id.clone(),
            self.cli_auth_credentials_store_mode,
        );

        if is_headless_environment() {
            headless_chatgpt_login::start_headless_chatgpt_login(self, opts);
            return;
        }

        match run_login_server(opts) {
            Ok(child) => {
                let sign_in_state = self.sign_in_state.clone();
                let request_frame = self.request_frame.clone();
                let auth_manager = self.auth_manager.clone();
                tokio::spawn(async move {
                    let auth_url = child.auth_url.clone();
                    {
                        *sign_in_state.write().unwrap() =
                            SignInState::ChatGptContinueInBrowser(ContinueInBrowserState {
                                auth_url,
                                shutdown_flag: Some(child.cancel_handle()),
                            });
                    }
                    request_frame.schedule_frame();
                    let r = child.block_until_done().await;
                    match r {
                        Ok(()) => {
                            // Force the auth manager to reload the new auth information.
                            auth_manager.reload();

                            *sign_in_state.write().unwrap() = SignInState::ChatGptSuccessMessage;
                            request_frame.schedule_frame();
                        }
                        _ => {
                            *sign_in_state.write().unwrap() =
                                SignInState::PickOpenAIMethod(OpenAIAuthMethod::ChatGPT);
                            // self.error = Some(e.to_string());
                            request_frame.schedule_frame();
                        }
                    }
                });
            }
            Err(e) => {
                *self.sign_in_state.write().unwrap() =
                    SignInState::PickOpenAIMethod(OpenAIAuthMethod::ChatGPT);
                self.error = Some(e.to_string());
                self.request_frame.schedule_frame();
            }
        }
    }
}

impl StepStateProvider for AuthModeWidget {
    fn get_step_state(&self) -> StepState {
        let sign_in_state = self.sign_in_state.read().unwrap();
        match &*sign_in_state {
            SignInState::PickProvider(_)
            | SignInState::PickOpenAIMethod(_)
            | SignInState::ApiKeyEntry(_)
            | SignInState::MistralApiKeyEntry(_)
            | SignInState::ChatGptContinueInBrowser(_)
            | SignInState::ChatGptDeviceCode(_)
            | SignInState::ChatGptSuccessMessage => StepState::InProgress,
            SignInState::ChatGptSuccess
            | SignInState::ApiKeyConfigured
            | SignInState::MistralApiKeyConfigured => StepState::Complete,
        }
    }
}

impl WidgetRef for AuthModeWidget {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        let sign_in_state = self.sign_in_state.read().unwrap();
        match &*sign_in_state {
            SignInState::PickProvider(selected) => {
                self.render_pick_provider(area, buf, *selected);
            }
            SignInState::PickOpenAIMethod(selected) => {
                self.render_pick_openai_method(area, buf, *selected);
            }
            SignInState::ChatGptContinueInBrowser(_) => {
                self.render_continue_in_browser(area, buf);
            }
            SignInState::ChatGptDeviceCode(state) => {
                headless_chatgpt_login::render_device_code_login(self, area, buf, state);
            }
            SignInState::ChatGptSuccessMessage => {
                self.render_chatgpt_success_message(area, buf);
            }
            SignInState::ChatGptSuccess => {
                self.render_chatgpt_success(area, buf);
            }
            SignInState::ApiKeyEntry(state) => {
                self.render_api_key_entry(area, buf, state);
            }
            SignInState::ApiKeyConfigured => {
                self.render_api_key_configured(area, buf);
            }
            SignInState::MistralApiKeyEntry(state) => {
                self.render_mistral_api_key_entry(area, buf, state);
            }
            SignInState::MistralApiKeyConfigured => {
                self.render_mistral_api_key_configured(area, buf);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use tempfile::TempDir;

    use hypercode_core::auth::AuthCredentialsStoreMode;

    fn widget_forced_chatgpt() -> (AuthModeWidget, TempDir) {
        let hypercode_home = TempDir::new().unwrap();
        let hypercode_home_path = hypercode_home.path().to_path_buf();
        let widget = AuthModeWidget {
            request_frame: FrameRequester::test_dummy(),
            error: None,
            sign_in_state: Arc::new(RwLock::new(SignInState::PickProvider(
                SelectedProvider::OpenAI,
            ))),
            hypercode_home: hypercode_home_path.clone(),
            cli_auth_credentials_store_mode: AuthCredentialsStoreMode::File,
            login_status: LoginStatus::NotAuthenticated,
            auth_manager: AuthManager::shared(
                hypercode_home_path,
                false,
                AuthCredentialsStoreMode::File,
            ),
            forced_chatgpt_workspace_id: None,
            forced_login_method: Some(ForcedLoginMethod::Chatgpt),
            animations_enabled: true,
        };
        (widget, hypercode_home)
    }

    #[test]
    fn api_key_flow_disabled_when_chatgpt_forced() {
        let (mut widget, _tmp) = widget_forced_chatgpt();

        widget.start_api_key_entry();

        assert_eq!(widget.error.as_deref(), Some(API_KEY_DISABLED_MESSAGE));
        assert!(matches!(
            &*widget.sign_in_state.read().unwrap(),
            SignInState::PickProvider(SelectedProvider::OpenAI)
        ));
    }

    #[test]
    fn saving_api_key_is_blocked_when_chatgpt_forced() {
        let (mut widget, _tmp) = widget_forced_chatgpt();

        widget.save_api_key("sk-test".to_string());

        assert_eq!(widget.error.as_deref(), Some(API_KEY_DISABLED_MESSAGE));
        assert!(matches!(
            &*widget.sign_in_state.read().unwrap(),
            SignInState::PickProvider(SelectedProvider::OpenAI)
        ));
        assert_eq!(widget.login_status, LoginStatus::NotAuthenticated);
    }
}
