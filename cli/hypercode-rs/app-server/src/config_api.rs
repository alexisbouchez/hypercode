use crate::error_code::INTERNAL_ERROR_CODE;
use crate::error_code::INVALID_REQUEST_ERROR_CODE;
use hypercode_app_server_protocol::ConfigBatchWriteParams;
use hypercode_app_server_protocol::ConfigReadParams;
use hypercode_app_server_protocol::ConfigReadResponse;
use hypercode_app_server_protocol::ConfigRequirements;
use hypercode_app_server_protocol::ConfigRequirementsReadResponse;
use hypercode_app_server_protocol::ConfigValueWriteParams;
use hypercode_app_server_protocol::ConfigWriteErrorCode;
use hypercode_app_server_protocol::ConfigWriteResponse;
use hypercode_app_server_protocol::JSONRPCErrorError;
use hypercode_app_server_protocol::SandboxMode;
use hypercode_core::config::ConfigService;
use hypercode_core::config::ConfigServiceError;
use hypercode_core::config_loader::ConfigRequirementsToml;
use hypercode_core::config_loader::LoaderOverrides;
use hypercode_core::config_loader::SandboxModeRequirement as CoreSandboxModeRequirement;
use serde_json::json;
use std::path::PathBuf;
use toml::Value as TomlValue;

#[derive(Clone)]
pub(crate) struct ConfigApi {
    service: ConfigService,
}

impl ConfigApi {
    pub(crate) fn new(
        hypercode_home: PathBuf,
        cli_overrides: Vec<(String, TomlValue)>,
        loader_overrides: LoaderOverrides,
    ) -> Self {
        Self {
            service: ConfigService::new(hypercode_home, cli_overrides, loader_overrides),
        }
    }

    pub(crate) async fn read(
        &self,
        params: ConfigReadParams,
    ) -> Result<ConfigReadResponse, JSONRPCErrorError> {
        self.service.read(params).await.map_err(map_error)
    }

    pub(crate) async fn config_requirements_read(
        &self,
    ) -> Result<ConfigRequirementsReadResponse, JSONRPCErrorError> {
        let requirements = self
            .service
            .read_requirements()
            .await
            .map_err(map_error)?
            .map(map_requirements_toml_to_api);

        Ok(ConfigRequirementsReadResponse { requirements })
    }

    pub(crate) async fn write_value(
        &self,
        params: ConfigValueWriteParams,
    ) -> Result<ConfigWriteResponse, JSONRPCErrorError> {
        self.service.write_value(params).await.map_err(map_error)
    }

    pub(crate) async fn batch_write(
        &self,
        params: ConfigBatchWriteParams,
    ) -> Result<ConfigWriteResponse, JSONRPCErrorError> {
        self.service.batch_write(params).await.map_err(map_error)
    }
}

fn map_requirements_toml_to_api(requirements: ConfigRequirementsToml) -> ConfigRequirements {
    ConfigRequirements {
        allowed_approval_policies: requirements.allowed_approval_policies.map(|policies| {
            policies
                .into_iter()
                .map(hypercode_app_server_protocol::AskForApproval::from)
                .collect()
        }),
        allowed_sandbox_modes: requirements.allowed_sandbox_modes.map(|modes| {
            modes
                .into_iter()
                .filter_map(map_sandbox_mode_requirement_to_api)
                .collect()
        }),
    }
}

fn map_sandbox_mode_requirement_to_api(mode: CoreSandboxModeRequirement) -> Option<SandboxMode> {
    match mode {
        CoreSandboxModeRequirement::ReadOnly => Some(SandboxMode::ReadOnly),
        CoreSandboxModeRequirement::WorkspaceWrite => Some(SandboxMode::WorkspaceWrite),
        CoreSandboxModeRequirement::DangerFullAccess => Some(SandboxMode::DangerFullAccess),
        CoreSandboxModeRequirement::ExternalSandbox => None,
    }
}

fn map_error(err: ConfigServiceError) -> JSONRPCErrorError {
    if let Some(code) = err.write_error_code() {
        return config_write_error(code, err.to_string());
    }

    JSONRPCErrorError {
        code: INTERNAL_ERROR_CODE,
        message: err.to_string(),
        data: None,
    }
}

fn config_write_error(code: ConfigWriteErrorCode, message: impl Into<String>) -> JSONRPCErrorError {
    JSONRPCErrorError {
        code: INVALID_REQUEST_ERROR_CODE,
        message: message.into(),
        data: Some(json!({
            "config_write_error_code": code,
        })),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hypercode_protocol::protocol::AskForApproval as CoreAskForApproval;
    use pretty_assertions::assert_eq;

    #[test]
    fn map_requirements_toml_to_api_converts_core_enums() {
        let requirements = ConfigRequirementsToml {
            allowed_approval_policies: Some(vec![
                CoreAskForApproval::Never,
                CoreAskForApproval::OnRequest,
            ]),
            allowed_sandbox_modes: Some(vec![
                CoreSandboxModeRequirement::ReadOnly,
                CoreSandboxModeRequirement::ExternalSandbox,
            ]),
            mcp_servers: None,
        };

        let mapped = map_requirements_toml_to_api(requirements);

        assert_eq!(
            mapped.allowed_approval_policies,
            Some(vec![
                hypercode_app_server_protocol::AskForApproval::Never,
                hypercode_app_server_protocol::AskForApproval::OnRequest,
            ])
        );
        assert_eq!(
            mapped.allowed_sandbox_modes,
            Some(vec![SandboxMode::ReadOnly]),
        );
    }
}
