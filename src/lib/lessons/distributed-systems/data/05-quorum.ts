import type { Lesson } from "../../types";

export const quorum: Lesson = {
	id: "quorum",
	title: "Quorum-Based Replication",
	chapterId: "replication",
	content: `## Quorum-Based Replication

When data is replicated across N nodes, a **quorum** is the minimum number of nodes that must agree for an operation to succeed. Quorums ensure consistency even when some nodes are unavailable.

### The Quorum Rule

For N replica nodes:
- **Write quorum (W):** minimum nodes that must acknowledge a write.
- **Read quorum (R):** minimum nodes that must respond to a read.

For **strong consistency:** \`W + R > N\`

This guarantees that every read will overlap with at least one write, so reads always see the latest write.

### Common Configurations (N = 3)

| W | R | Guarantee |
|---|---|-----------|
| 2 | 2 | Strong consistency (W+R=4 > 3) |
| 3 | 1 | Strong consistency, slow writes |
| 1 | 3 | Strong consistency, slow reads |
| 1 | 1 | Fast but inconsistent |

\`\`\`js
function hasQuorum(n, w, r) {
  return w + r > n;
}

function canWrite(n, w, availableNodes) {
  return availableNodes >= w;
}

function canRead(n, r, availableNodes) {
  return availableNodes >= r;
}
\`\`\`

### Dynamo-Style Quorums

Amazon DynamoDB uses quorums with N=3. Default settings are W=2, R=2, providing strong consistency while tolerating 1 node failure for both reads and writes.

If you need lower latency, you can use W=1, R=1 (eventual consistency) or W=3, R=1 (all writes acknowledged, fast reads).

### Your Task

Implement three functions:
- \`hasQuorum(n, w, r)\` — returns true if \`W + R > N\` (strong consistency guaranteed)
- \`canWrite(n, w, available)\` — returns true if enough nodes are available to complete a write
- \`canRead(n, r, available)\` — returns true if enough nodes are available to complete a read`,

	starterCode: `function hasQuorum(n, w, r) {
	// Returns true if W + R > N
}

function canWrite(n, w, available) {
	// Returns true if available >= w
}

function canRead(n, r, available) {
	// Returns true if available >= r
}

// N=3 replicas, W=2, R=2
console.log(hasQuorum(3, 2, 2));  // true: 2+2=4 > 3
console.log(hasQuorum(3, 1, 1));  // false: 1+1=2 <= 3

// 1 node is down (2 available)
console.log(canWrite(3, 2, 2));   // true: can still write
console.log(canRead(3, 2, 2));    // true: can still read
console.log(canWrite(3, 2, 1));   // false: not enough nodes
`,

	solution: `function hasQuorum(n, w, r) {
	return w + r > n;
}

function canWrite(n, w, available) {
	return available >= w;
}

function canRead(n, r, available) {
	return available >= r;
}

console.log(hasQuorum(3, 2, 2));
console.log(hasQuorum(3, 1, 1));

console.log(canWrite(3, 2, 2));
console.log(canRead(3, 2, 2));
console.log(canWrite(3, 2, 1));
`,

	tests: [
		{
			name: "quorum checks: true, false, true, true, false",
			expected: "true\nfalse\ntrue\ntrue\nfalse\n",
		},
		{
			name: "N=5, W=3, R=3 has strong consistency",
			code: `{{FUNC}}
console.log(hasQuorum(5, 3, 3));`,
			expected: "true\n",
		},
		{
			name: "N=5, W=1, R=1 is eventually consistent",
			code: `{{FUNC}}
console.log(hasQuorum(5, 1, 1));`,
			expected: "false\n",
		},
		{
			name: "canRead with exact quorum",
			code: `{{FUNC}}
console.log(canRead(5, 3, 3));
console.log(canRead(5, 3, 2));`,
			expected: "true\nfalse\n",
		},
	],
};
