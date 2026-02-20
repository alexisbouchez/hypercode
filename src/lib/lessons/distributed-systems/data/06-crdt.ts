import type { Lesson } from "../../types";

export const crdt: Lesson = {
	id: "crdt",
	title: "CRDTs: G-Counter",
	chapterId: "replication",
	content: `## Conflict-Free Replicated Data Types (CRDTs)

In an eventually consistent distributed system, replicas can accept writes independently and sync later. This creates **conflicts**: two replicas might have different values for the same key.

**CRDTs** (Conflict-Free Replicated Data Types) are data structures designed so that all conflicts can be resolved automatically — merging is always deterministic and produces the correct result.

### The G-Counter (Grow-Only Counter)

The simplest CRDT is the G-Counter. It is a distributed counter that can only be incremented. Each node maintains an array of counters — one slot per node. A node only increments its own slot.

\`\`\`js
class GCounter {
  constructor(nodeId, numNodes) {
    this.nodeId = nodeId;
    this.counts = new Array(numNodes).fill(0);
  }

  increment() {
    this.counts[this.nodeId]++;
  }

  value() {
    return this.counts.reduce((sum, c) => sum + c, 0);
  }

  merge(other) {
    // Element-wise maximum
    for (let i = 0; i < this.counts.length; i++) {
      this.counts[i] = Math.max(this.counts[i], other.counts[i]);
    }
  }
}
\`\`\`

### Why It Works

- **No conflicts:** Each node writes to its own slot — no contention.
- **Merge is idempotent:** merging twice gives the same result as once.
- **Merge is commutative:** merging A into B gives the same result as merging B into A.
- **Merge is associative:** (A merge B) merge C = A merge (B merge C).

These properties mean replicas can sync in any order, at any time, and always converge to the same value.

### Real-World Uses

- **Redis CRDT** — distributed Redis instances use CRDTs for conflict-free replication.
- **Riak** — uses CRDTs for sets, maps, and counters.
- **Collaborative editing** — operational transforms and CRDTs power Google Docs-style real-time collaboration.

### Your Task

Implement a \`GCounter\` class with \`increment()\`, \`value()\`, and \`merge(other)\` methods.`,

	starterCode: `class GCounter {
	constructor(nodeId, numNodes) {
		this.nodeId = nodeId;
		this.counts = new Array(numNodes).fill(0);
	}

	increment() {
		// Increment only this node's slot
	}

	value() {
		// Sum all counts
	}

	merge(other) {
		// Element-wise max of counts
	}
}

// Simulate 3 nodes
const node0 = new GCounter(0, 3);
const node1 = new GCounter(1, 3);
const node2 = new GCounter(2, 3);

node0.increment();
node0.increment();
node1.increment();
node2.increment();
node2.increment();
node2.increment();

// Merge all into node0
node0.merge(node1);
node0.merge(node2);

console.log("Total count:", node0.value());
`,

	solution: `class GCounter {
	constructor(nodeId, numNodes) {
		this.nodeId = nodeId;
		this.counts = new Array(numNodes).fill(0);
	}

	increment() {
		this.counts[this.nodeId]++;
	}

	value() {
		return this.counts.reduce((sum, c) => sum + c, 0);
	}

	merge(other) {
		for (let i = 0; i < this.counts.length; i++) {
			this.counts[i] = Math.max(this.counts[i], other.counts[i]);
		}
	}
}

const node0 = new GCounter(0, 3);
const node1 = new GCounter(1, 3);
const node2 = new GCounter(2, 3);

node0.increment();
node0.increment();
node1.increment();
node2.increment();
node2.increment();
node2.increment();

node0.merge(node1);
node0.merge(node2);

console.log("Total count:", node0.value());
`,

	tests: [
		{
			name: "3 nodes: 2+1+3 increments = 6 total",
			expected: "Total count: 6\n",
		},
		{
			name: "single node counter",
			code: `{{FUNC}}
const c = new GCounter(0, 1);
c.increment();
c.increment();
c.increment();
console.log(c.value());`,
			expected: "3\n",
		},
		{
			name: "merge is idempotent",
			code: `{{FUNC}}
const a = new GCounter(0, 2);
const b = new GCounter(1, 2);
a.increment();
b.increment();
a.merge(b);
a.merge(b); // merge again
console.log(a.value());`,
			expected: "2\n",
		},
		{
			name: "merge is commutative",
			code: `{{FUNC}}
const a = new GCounter(0, 2);
const b = new GCounter(1, 2);
a.increment(); a.increment();
b.increment();
const copyA = new GCounter(0, 2);
copyA.counts = [...a.counts];
const copyB = new GCounter(1, 2);
copyB.counts = [...b.counts];
a.merge(b);
copyB.merge(copyA);
console.log(a.value() === copyB.value());`,
			expected: "true\n",
		},
	],
};
