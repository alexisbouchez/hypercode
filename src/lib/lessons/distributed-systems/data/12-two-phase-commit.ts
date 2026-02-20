import type { Lesson } from "../../types";

export const twoPhaseCommit: Lesson = {
	id: "two-phase-commit",
	title: "Two-Phase Commit",
	chapterId: "consensus",
	content: `## Two-Phase Commit (2PC)

A **distributed transaction** must either commit on all participating nodes or abort on all of them — never a partial commit. **Two-Phase Commit (2PC)** is the classic protocol to achieve this.

### The Two Phases

**Phase 1 — Prepare (Voting):**
1. The **coordinator** sends a PREPARE message to all **participants**.
2. Each participant checks if it can commit (checks its local state, writes to a WAL).
3. Each participant votes YES (ready) or NO (abort).

**Phase 2 — Commit (Decision):**
- If all votes are YES: coordinator sends COMMIT to all participants.
- If any vote is NO (or a timeout): coordinator sends ABORT to all participants.

\`\`\`
Coordinator          Participant A      Participant B
    │── PREPARE ──────────►│                 │
    │── PREPARE ───────────────────────────►│
    │◄── YES ──────────────│                 │
    │◄── YES ──────────────────────────────│
    │── COMMIT ────────────►│                │
    │── COMMIT ────────────────────────────►│
\`\`\`

### The Problem

2PC has a **blocking failure mode**: if the coordinator crashes after Phase 1 but before sending the commit/abort decision, participants are stuck — they cannot commit or abort without the coordinator. Modern protocols like **3PC** and **Paxos/Raft** solve this.

### Your Task

Implement a \`TwoPhaseCommit\` coordinator class. It should:
- Accept a list of participant functions (each returns \`true\` for YES or \`false\` for NO when prepared).
- \`prepare()\` — call all participants, collect votes.
- \`commit()\` — if all voted YES, commit (return \`"COMMITTED"\`). Otherwise abort (return \`"ABORTED"\`).`,

	starterCode: `class TwoPhaseCommit {
	constructor(participants) {
		// participants: array of functions () => boolean (true=YES, false=NO)
		this.participants = participants;
		this.votes = [];
	}

	prepare() {
		// Call each participant and collect their vote (true/false)
		// Store votes in this.votes
	}

	commit() {
		// If all votes are true, return "COMMITTED"
		// Otherwise return "ABORTED"
	}
}

// All participants vote YES
const allYes = new TwoPhaseCommit([
	() => true,
	() => true,
	() => true,
]);
allYes.prepare();
console.log(allYes.commit());

// One participant votes NO
const oneNo = new TwoPhaseCommit([
	() => true,
	() => false,
	() => true,
]);
oneNo.prepare();
console.log(oneNo.commit());
`,

	solution: `class TwoPhaseCommit {
	constructor(participants) {
		this.participants = participants;
		this.votes = [];
	}

	prepare() {
		this.votes = this.participants.map(p => p());
	}

	commit() {
		if (this.votes.every(v => v === true)) {
			return "COMMITTED";
		}
		return "ABORTED";
	}
}

const allYes = new TwoPhaseCommit([
	() => true,
	() => true,
	() => true,
]);
allYes.prepare();
console.log(allYes.commit());

const oneNo = new TwoPhaseCommit([
	() => true,
	() => false,
	() => true,
]);
oneNo.prepare();
console.log(oneNo.commit());
`,

	tests: [
		{
			name: "all YES → COMMITTED, one NO → ABORTED",
			expected: "COMMITTED\nABORTED\n",
		},
		{
			name: "single participant YES",
			code: `{{FUNC}}
const t = new TwoPhaseCommit([() => true]);
t.prepare();
console.log(t.commit());`,
			expected: "COMMITTED\n",
		},
		{
			name: "single participant NO",
			code: `{{FUNC}}
const t = new TwoPhaseCommit([() => false]);
t.prepare();
console.log(t.commit());`,
			expected: "ABORTED\n",
		},
		{
			name: "all NO → ABORTED",
			code: `{{FUNC}}
const t = new TwoPhaseCommit([() => false, () => false, () => false]);
t.prepare();
console.log(t.commit());`,
			expected: "ABORTED\n",
		},
	],
};
