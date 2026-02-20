import type { Lesson } from "../../types";

export const lamportClocks: Lesson = {
	id: "lamport-clocks",
	title: "Lamport Clocks",
	chapterId: "clocks",
	content: `## Lamport Clocks

In a distributed system, there is no global clock. Machines run at different speeds, clocks drift, and network delays are unpredictable. How do we order events?

**Leslie Lamport** introduced logical clocks in 1978. The core idea: if event A caused event B (A **happened-before** B), then \`timestamp(A) < timestamp(B)\`.

### The Rules

Each process maintains an integer counter:

1. **Before sending a message:** increment your clock.
2. **On receiving a message:** set your clock to \`max(local, received) + 1\`.
3. **For any other event:** increment your clock.

\`\`\`js
class LamportClock {
  constructor() {
    this.time = 0;
  }

  tick() {
    this.time++;
    return this.time;
  }

  send() {
    this.time++;
    return this.time; // attach this to the message
  }

  receive(timestamp) {
    this.time = Math.max(this.time, timestamp) + 1;
    return this.time;
  }
}
\`\`\`

### Example

\`\`\`js
const p1 = new LamportClock();
const p2 = new LamportClock();

const t1 = p1.send();       // p1 sends at time 1
const t2 = p2.receive(t1);  // p2 receives: max(0, 1) + 1 = 2
const t3 = p2.send();       // p2 sends at time 3
const t4 = p1.receive(t3);  // p1 receives: max(1, 3) + 1 = 4
\`\`\`

### Limitation

Lamport clocks provide **partial ordering**: if A happened-before B, then \`ts(A) < ts(B)\`. But the converse is not guaranteed — \`ts(A) < ts(B)\` does not mean A caused B. For that, you need vector clocks.

### Your Task

Implement a \`LamportClock\` class with \`tick()\`, \`send()\`, and \`receive(timestamp)\` methods. Each should return the new clock value.`,

	starterCode: `class LamportClock {
	constructor() {
		this.time = 0;
	}

	tick() {
		// Increment clock by 1, return new value
	}

	send() {
		// Increment before sending, return timestamp to attach to message
	}

	receive(timestamp) {
		// Update clock: max(local, received) + 1
	}
}

const p1 = new LamportClock();
const p2 = new LamportClock();

const t1 = p1.send();
console.log("p1 sends:", t1);

const t2 = p2.receive(t1);
console.log("p2 receives:", t2);

const t3 = p2.send();
console.log("p2 sends:", t3);

const t4 = p1.receive(t3);
console.log("p1 receives:", t4);
`,

	solution: `class LamportClock {
	constructor() {
		this.time = 0;
	}

	tick() {
		this.time++;
		return this.time;
	}

	send() {
		this.time++;
		return this.time;
	}

	receive(timestamp) {
		this.time = Math.max(this.time, timestamp) + 1;
		return this.time;
	}
}

const p1 = new LamportClock();
const p2 = new LamportClock();

const t1 = p1.send();
console.log("p1 sends:", t1);

const t2 = p2.receive(t1);
console.log("p2 receives:", t2);

const t3 = p2.send();
console.log("p2 sends:", t3);

const t4 = p1.receive(t3);
console.log("p1 receives:", t4);
`,

	tests: [
		{
			name: "p1 sends:1, p2 receives:2, p2 sends:3, p1 receives:4",
			expected: "p1 sends: 1\np2 receives: 2\np2 sends: 3\np1 receives: 4\n",
		},
		{
			name: "receive with larger timestamp wins",
			code: `{{FUNC}}
const c = new LamportClock();
c.tick(); c.tick(); // time = 2
const t = c.receive(10); // max(2,10)+1 = 11
console.log(t);`,
			expected: "11\n",
		},
		{
			name: "tick increments by 1",
			code: `{{FUNC}}
const c = new LamportClock();
console.log(c.tick());
console.log(c.tick());
console.log(c.tick());`,
			expected: "1\n2\n3\n",
		},
		{
			name: "receive with smaller timestamp: local wins",
			code: `{{FUNC}}
const c = new LamportClock();
c.tick(); c.tick(); c.tick(); // time = 3
const t = c.receive(1); // max(3,1)+1 = 4
console.log(t);`,
			expected: "4\n",
		},
	],
};
