import type { Lesson } from "../../types";

export const capTheorem: Lesson = {
	id: "cap-theorem",
	title: "CAP Theorem",
	chapterId: "fault-tolerance",
	content: `## CAP Theorem

The **CAP theorem** (Brewer's theorem, 2000) states that a distributed data store can provide at most **two out of three** guarantees simultaneously:

- **Consistency (C):** Every read receives the most recent write or an error. All nodes see the same data at the same time.
- **Availability (A):** Every request receives a non-error response, without guaranteeing it reflects the most recent write.
- **Partition Tolerance (P):** The system continues to operate despite arbitrary message loss or failure between nodes.

### Why You Can Only Pick Two

In any real network, **partitions will happen** — cables get cut, switches fail, packets get lost. So partition tolerance is not optional. This means the real choice is between **CP** and **AP**:

\`\`\`
         C
        / \\
       /   \\
     CP     CA ← only possible if no partitions (single datacenter)
     /       \\
    P ─── AP ─ A
\`\`\`

**Proof intuition:** Suppose nodes A and B are partitioned. A client writes value \`v2\` to node A. Another client reads from node B.
- If we guarantee **consistency**, B must return \`v2\`, but B cannot reach A → we must reject the read (sacrifice **availability**) → **CP**.
- If we guarantee **availability**, B must respond, but it only has stale \`v1\` → we sacrifice **consistency** → **AP**.
- You cannot have both during a partition.

### Real-World Systems

| System | Type | Behavior During Partition |
|--------|------|--------------------------|
| **HBase**, **MongoDB** (default) | **CP** | Rejects writes/reads to minority partition; guarantees consistency |
| **DynamoDB**, **Cassandra** | **AP** | Accepts reads/writes on all nodes; resolves conflicts later (eventual consistency) |
| **Traditional RDBMS** (single-node PostgreSQL) | **CA** | No partition tolerance; fully consistent and available on one machine |

### Your Task

Implement a \`CAPSimulator\` class that models a two-node distributed system. It should:
- Store a value on each node (\`nodeA\`, \`nodeB\`).
- Support \`write(value)\` which writes to node A. If not partitioned, it replicates to node B.
- Support \`read(node)\` which returns the value at the given node — but behavior depends on the mode:
  - In **"cp"** mode: during a partition, reading from node B throws an error (consistency over availability).
  - In **"ap"** mode: during a partition, reading from node B returns its (possibly stale) value (availability over consistency).
- \`partition()\` and \`heal()\` toggle the network partition. When healed, node B syncs to node A's value.`,

	starterCode: `class CAPSimulator {
	constructor(mode) {
		// mode: "cp" or "ap"
		this.mode = mode;
		this.nodeA = null;
		this.nodeB = null;
		this.partitioned = false;
	}

	write(value) {
		// Write to node A
		// If not partitioned, replicate to node B
	}

	read(node) {
		// node: "A" or "B"
		// In CP mode + partitioned + reading B → throw Error("UNAVAILABLE")
		// In AP mode + partitioned + reading B → return stale nodeB value
		// Otherwise return the node's current value
	}

	partition() {
		// Simulate a network partition
	}

	heal() {
		// Heal the partition and sync node B to node A's value
	}
}

// CP mode test
const cp = new CAPSimulator("cp");
cp.write("v1");
console.log(cp.read("A"), cp.read("B"));
cp.partition();
cp.write("v2");
console.log(cp.read("A"));
try { cp.read("B"); } catch(e) { console.log(e.message); }
cp.heal();
console.log(cp.read("B"));

// AP mode test
const ap = new CAPSimulator("ap");
ap.write("v1");
ap.partition();
ap.write("v2");
console.log(ap.read("A"), ap.read("B"));
ap.heal();
console.log(ap.read("B"));
`,

	solution: `class CAPSimulator {
	constructor(mode) {
		this.mode = mode;
		this.nodeA = null;
		this.nodeB = null;
		this.partitioned = false;
	}

	write(value) {
		this.nodeA = value;
		if (!this.partitioned) {
			this.nodeB = value;
		}
	}

	read(node) {
		if (node === "A") {
			return this.nodeA;
		}
		if (this.partitioned && this.mode === "cp") {
			throw new Error("UNAVAILABLE");
		}
		return this.nodeB;
	}

	partition() {
		this.partitioned = true;
	}

	heal() {
		this.partitioned = false;
		this.nodeB = this.nodeA;
	}
}

const cp = new CAPSimulator("cp");
cp.write("v1");
console.log(cp.read("A"), cp.read("B"));
cp.partition();
cp.write("v2");
console.log(cp.read("A"));
try { cp.read("B"); } catch(e) { console.log(e.message); }
cp.heal();
console.log(cp.read("B"));

const ap = new CAPSimulator("ap");
ap.write("v1");
ap.partition();
ap.write("v2");
console.log(ap.read("A"), ap.read("B"));
ap.heal();
console.log(ap.read("B"));
`,

	tests: [
		{
			name: "CP and AP default behavior",
			expected: "v1 v1\nv2\nUNAVAILABLE\nv2\nv2 v1\nv2\n",
		},
		{
			name: "CP mode rejects partitioned reads from B",
			code: `{{FUNC}}
const sim = new CAPSimulator("cp");
sim.write("x");
sim.partition();
sim.write("y");
console.log(sim.read("A"));
try { sim.read("B"); console.log("NO ERROR"); } catch(e) { console.log(e.message); }
sim.heal();
console.log(sim.read("A"), sim.read("B"));`,
			expected: "y\nUNAVAILABLE\ny y\n",
		},
		{
			name: "AP mode returns stale data during partition",
			code: `{{FUNC}}
const sim = new CAPSimulator("ap");
sim.write("first");
sim.partition();
sim.write("second");
sim.write("third");
console.log(sim.read("A"));
console.log(sim.read("B"));
sim.heal();
console.log(sim.read("B"));`,
			expected: "third\nfirst\nthird\n",
		},
	],
};
