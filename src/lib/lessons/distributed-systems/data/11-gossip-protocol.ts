import type { Lesson } from "../../types";

export const gossipProtocol: Lesson = {
	id: "gossip-protocol",
	title: "Gossip Protocol",
	chapterId: "replication",
	content: `## Gossip Protocol

A Gossip Protocol (also called epidemic protocol) is a way for nodes in a distributed system to **disseminate information** to all other nodes — without any central coordinator. Each round, every node picks one or more random peers and exchanges state.

### Why Gossip?

- **Decentralized** — no single point of failure.
- **Scalable** — information spreads logarithmically. With N nodes, it takes O(log N) rounds for all nodes to have the information.
- **Fault-tolerant** — nodes can fail without stopping propagation.

### How It Works

Each node maintains a version number for each piece of information. When two nodes gossip:
1. Each sends its known state to the other.
2. Both update their state by taking the **maximum version** for each key.

\`\`\`js
function gossipRound(nodes, sourceIdx, targetIdx) {
  const source = nodes[sourceIdx];
  const target = nodes[targetIdx];

  // Merge: take max version for each key
  for (const key in source.state) {
    if (!target.state[key] || source.state[key].version > target.state[key].version) {
      target.state[key] = { ...source.state[key] };
    }
  }
  for (const key in target.state) {
    if (!source.state[key] || target.state[key].version > source.state[key].version) {
      source.state[key] = { ...target.state[key] };
    }
  }
}
\`\`\`

### Convergence

In each round, any node that knows a piece of information can spread it to one random node. The probability that after \`r\` rounds ALL nodes know the information follows the epidemic model — it spreads exponentially fast.

### Real-World Uses

- **Amazon DynamoDB** — nodes gossip to propagate ring membership changes.
- **Apache Cassandra** — uses gossip for node health, schema, and token ring changes.
- **Bitcoin** — transactions propagate via gossip over the peer-to-peer network.
- **Redis Cluster** — cluster topology changes propagate via gossip.

### Your Task

Implement \`gossipRound(nodes, sourceIdx, targetIdx)\` that merges state between two nodes. Each node has a \`state\` object mapping keys to \`{ value, version }\` objects. Take the higher version for each key.

Also implement \`spreadInfo(nodes, key, value, fromIdx)\` that sets a value on one node and simulates 3 rounds of gossip to all other nodes sequentially.`,

	starterCode: `function gossipRound(nodes, sourceIdx, targetIdx) {
	const source = nodes[sourceIdx];
	const target = nodes[targetIdx];

	// For each key in source: if source has higher version, update target
	// For each key in target: if target has higher version, update source
}

function spreadInfo(nodes, key, value, fromIdx) {
	// Set the value on nodes[fromIdx] with version 1
	// Simulate gossip: for each other node, do a gossipRound between fromIdx and that node
}

// 4 nodes, each starts with empty state
const nodes = [
	{ id: 0, state: {} },
	{ id: 1, state: {} },
	{ id: 2, state: {} },
	{ id: 3, state: {} },
];

spreadInfo(nodes, "leader", "node-0", 0);

// All nodes should now know the leader
for (const node of nodes) {
	console.log(\`node-\${node.id} leader: \${node.state["leader"]?.value}\`);
}
`,

	solution: `function gossipRound(nodes, sourceIdx, targetIdx) {
	const source = nodes[sourceIdx];
	const target = nodes[targetIdx];

	for (const key in source.state) {
		if (!target.state[key] || source.state[key].version > target.state[key].version) {
			target.state[key] = { ...source.state[key] };
		}
	}
	for (const key in target.state) {
		if (!source.state[key] || target.state[key].version > source.state[key].version) {
			source.state[key] = { ...target.state[key] };
		}
	}
}

function spreadInfo(nodes, key, value, fromIdx) {
	nodes[fromIdx].state[key] = { value, version: 1 };
	for (let i = 0; i < nodes.length; i++) {
		if (i !== fromIdx) {
			gossipRound(nodes, fromIdx, i);
		}
	}
}

const nodes = [
	{ id: 0, state: {} },
	{ id: 1, state: {} },
	{ id: 2, state: {} },
	{ id: 3, state: {} },
];

spreadInfo(nodes, "leader", "node-0", 0);

for (const node of nodes) {
	console.log(\`node-\${node.id} leader: \${node.state["leader"]?.value}\`);
}
`,

	tests: [
		{
			name: "all 4 nodes learn the leader after spreading",
			expected: "node-0 leader: node-0\nnode-1 leader: node-0\nnode-2 leader: node-0\nnode-3 leader: node-0\n",
		},
		{
			name: "gossipRound merges newer version",
			code: `{{FUNC}}
const nodes = [
	{ id: 0, state: { x: { value: "old", version: 1 } } },
	{ id: 1, state: { x: { value: "new", version: 2 } } },
];
gossipRound(nodes, 1, 0);
console.log(nodes[0].state.x.value);`,
			expected: "new\n",
		},
		{
			name: "gossipRound keeps higher version",
			code: `{{FUNC}}
const nodes = [
	{ id: 0, state: { y: { value: "v3", version: 3 } } },
	{ id: 1, state: { y: { value: "v1", version: 1 } } },
];
gossipRound(nodes, 0, 1);
console.log(nodes[1].state.y.value);`,
			expected: "v3\n",
		},
	],
};
