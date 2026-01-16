use crate::agent::AgentStatus;
use crate::hypercode::Hypercode;
use crate::error::Result as HypercodeResult;
use crate::protocol::Event;
use crate::protocol::Op;
use crate::protocol::Submission;
use std::path::PathBuf;
use tokio::sync::watch;

pub struct HypercodeThread {
    hypercode: Hypercode,
    rollout_path: PathBuf,
}

/// Conduit for the bidirectional stream of messages that compose a thread
/// (formerly called a conversation) in Hypercode.
impl HypercodeThread {
    pub(crate) fn new(hypercode: Hypercode, rollout_path: PathBuf) -> Self {
        Self {
            hypercode,
            rollout_path,
        }
    }

    pub async fn submit(&self, op: Op) -> HypercodeResult<String> {
        self.hypercode.submit(op).await
    }

    /// Use sparingly: this is intended to be removed soon.
    pub async fn submit_with_id(&self, sub: Submission) -> HypercodeResult<()> {
        self.hypercode.submit_with_id(sub).await
    }

    pub async fn next_event(&self) -> HypercodeResult<Event> {
        self.hypercode.next_event().await
    }

    pub async fn agent_status(&self) -> AgentStatus {
        self.hypercode.agent_status().await
    }

    pub(crate) fn subscribe_status(&self) -> watch::Receiver<AgentStatus> {
        self.hypercode.agent_status.clone()
    }

    pub fn rollout_path(&self) -> PathBuf {
        self.rollout_path.clone()
    }
}
