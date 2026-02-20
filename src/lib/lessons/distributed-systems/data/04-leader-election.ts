import type { Lesson } from "../../types";

export const leaderElection: Lesson = {
	id: "leader-election",
	title: "Leader Election",
	chapterId: "consensus",
	content: `## Leader Election

Many distributed systems need a single coordinator — a **leader** — to make decisions, coordinate writes, or manage state. When the current leader fails, the remaining nodes must elect a new one.

### The Bully Algorithm

The Bully Algorithm is one of the simplest election algorithms. Each node has a unique ID. When a node detects that the leader is down, it starts an election by contacting all nodes with **higher IDs**:

- If a higher-ID node responds, it takes over the election.
- If no higher-ID node responds, the current node declares itself leader.

The node with the **highest ID** always wins — hence "bully."

\`\`\`js
function bullyElection(nodes, failedLeaderId) {
  // Remove the failed leader
  const alive = nodes.filter(id => id !== failedLeaderId);
  if (alive.length === 0) return null;
  // The highest ID among alive nodes wins
  return Math.max(...alive);
}
\`\`\`

### The Ring Algorithm

In a ring topology, nodes are arranged in a logical ring. When a node starts an election, it sends an election message around the ring. Each node appends its ID. When the message returns to the initiator, the node with the highest ID is declared leader.

\`\`\`js
function ringElection(ring, startIdx) {
  // The ring is an array of node IDs in order
  // We simulate: collect all alive IDs and pick the max
  const seen = new Set();
  let maxId = -1;
  for (let i = 0; i < ring.length; i++) {
    const id = ring[(startIdx + i) % ring.length];
    if (!seen.has(id)) {
      seen.add(id);
      maxId = Math.max(maxId, id);
    }
  }
  return maxId;
}
\`\`\`

### Used By

- **Apache ZooKeeper** — uses a leader election protocol based on Paxos.
- **Raft consensus** — each term starts with an election; candidates request votes.
- **Kafka** — a controller node is elected among brokers.
- **Kubernetes** — controller-manager uses leader election via leases.

### Your Task

Implement \`electLeader(nodes, failedId)\` that takes an array of node IDs and the ID of the failed node, removes the failed node, and returns the highest remaining ID as the new leader. Return \`null\` if no nodes remain.`,

	starterCode: `function electLeader(nodes, failedId) {
	// Remove failedId from the list
	// Return the highest remaining ID
	// Return null if no nodes remain
}

console.log(electLeader([1, 2, 3, 4, 5], 5));
console.log(electLeader([1, 2, 3, 4, 5], 3));
console.log(electLeader([42], 42));
`,

	solution: `function electLeader(nodes, failedId) {
	const alive = nodes.filter(id => id !== failedId);
	if (alive.length === 0) return null;
	return Math.max(...alive);
}

console.log(electLeader([1, 2, 3, 4, 5], 5));
console.log(electLeader([1, 2, 3, 4, 5], 3));
console.log(electLeader([42], 42));
`,

	tests: [
		{
			name: "fails leader 5 → elects 4; fails 3 → elects 5; sole node fails → null",
			expected: "4\n5\nnull\n",
		},
		{
			name: "two nodes, leader fails",
			code: `{{FUNC}}
console.log(electLeader([10, 20], 20));`,
			expected: "10\n",
		},
		{
			name: "lowest id fails",
			code: `{{FUNC}}
console.log(electLeader([1, 5, 3, 7, 2], 1));`,
			expected: "7\n",
		},
		{
			name: "non-existent id returns max",
			code: `{{FUNC}}
console.log(electLeader([3, 1, 4, 1, 5], 99));`,
			expected: "5\n",
		},
	],
};
