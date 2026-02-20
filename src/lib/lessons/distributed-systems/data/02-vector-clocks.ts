import type { Lesson } from "../../types";

export const vectorClocks: Lesson = {
	id: "vector-clocks",
	title: "Vector Clocks",
	chapterId: "clocks",
	content: `## Vector Clocks

Lamport clocks tell us event ordering, but not **causality** in both directions. Vector clocks solve this: they can determine whether two events are causally related or **concurrent** (neither caused the other).

### How It Works

Each process maintains a vector — an array of counters, one per process. For N processes, each process \`i\` has a vector \`[c₀, c₁, ..., cₙ₋₁]\`.

**Rules:**
1. **On internal event:** increment your own counter: \`vc[i]++\`.
2. **Before sending:** increment your counter, then attach your vector.
3. **On receiving:** merge by taking the element-wise max, then increment your own counter.

\`\`\`js
class VectorClock {
  constructor(id, n) {
    this.id = id;       // this process's index
    this.vc = new Array(n).fill(0);
  }

  tick() {
    this.vc[this.id]++;
    return [...this.vc];
  }

  send() {
    this.vc[this.id]++;
    return [...this.vc];
  }

  receive(remoteVc) {
    for (let i = 0; i < this.vc.length; i++) {
      this.vc[i] = Math.max(this.vc[i], remoteVc[i]);
    }
    this.vc[this.id]++;
    return [...this.vc];
  }
}
\`\`\`

### Comparing Vectors

Given two vectors A and B:
- **A happened-before B** if every \`A[i] <= B[i]\` and at least one \`A[i] < B[i]\`.
- **A and B are concurrent** if neither happened-before the other.

This is exactly what systems like DynamoDB and Riak use to detect **write conflicts**.

### Your Task

Implement a \`VectorClock\` class for a system of \`n\` processes. Process \`id\` (0-indexed) maintains the vector.`,

	starterCode: `class VectorClock {
	constructor(id, n) {
		this.id = id;
		this.vc = new Array(n).fill(0);
	}

	tick() {
		// Increment own counter, return a copy of the vector
	}

	send() {
		// Increment own counter, return copy (attach to message)
	}

	receive(remoteVc) {
		// Element-wise max, then increment own counter
		// Return copy
	}
}

// 3 processes: p0, p1, p2
const p0 = new VectorClock(0, 3);
const p1 = new VectorClock(1, 3);

const msg1 = p0.send();
console.log("p0 sends:", msg1.join(","));

const after = p1.receive(msg1);
console.log("p1 receives:", after.join(","));

p1.tick();
console.log("p1 ticks:", p1.vc.join(","));
`,

	solution: `class VectorClock {
	constructor(id, n) {
		this.id = id;
		this.vc = new Array(n).fill(0);
	}

	tick() {
		this.vc[this.id]++;
		return [...this.vc];
	}

	send() {
		this.vc[this.id]++;
		return [...this.vc];
	}

	receive(remoteVc) {
		for (let i = 0; i < this.vc.length; i++) {
			this.vc[i] = Math.max(this.vc[i], remoteVc[i]);
		}
		this.vc[this.id]++;
		return [...this.vc];
	}
}

const p0 = new VectorClock(0, 3);
const p1 = new VectorClock(1, 3);

const msg1 = p0.send();
console.log("p0 sends:", msg1.join(","));

const after = p1.receive(msg1);
console.log("p1 receives:", after.join(","));

p1.tick();
console.log("p1 ticks:", p1.vc.join(","));
`,

	tests: [
		{
			name: "p0 sends [1,0,0], p1 receives [1,1,0], p1 ticks [1,2,0]",
			expected: "p0 sends: 1,0,0\np1 receives: 1,1,0\np1 ticks: 1,2,0\n",
		},
		{
			name: "tick increments only own slot",
			code: `{{FUNC}}
const p = new VectorClock(2, 3);
p.tick();
p.tick();
console.log(p.vc.join(","));`,
			expected: "0,0,2\n",
		},
		{
			name: "receive takes element-wise max",
			code: `{{FUNC}}
const p = new VectorClock(1, 3);
p.tick(); // [0,1,0]
const remote = [5, 0, 3];
p.receive(remote); // max([0,1,0],[5,0,3]) = [5,1,3], +1 own = [5,2,3]
console.log(p.vc.join(","));`,
			expected: "5,2,3\n",
		},
	],
};
