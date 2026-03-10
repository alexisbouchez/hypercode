import type { Lesson } from "../../types";

export const timeSynchronization: Lesson = {
	id: "time-synchronization",
	title: "Time Synchronization",
	chapterId: "clocks",
	content: `## Time Synchronization

In distributed systems, each node has its own physical clock that drifts over time. **Time synchronization** algorithms keep these clocks close enough to agree on ordering and deadlines.

### Clock Skew and Drift

- **Clock skew:** The difference between two clocks at a given instant.
- **Clock drift:** The rate at which a clock deviates from real time (typically 10–100 ppm for quartz oscillators — that is 1–10 ms per 100 seconds).

Without synchronization, clocks diverge unboundedly, breaking log ordering, lease expiry, cache TTLs, and causality assumptions.

### Cristian's Algorithm (1989)

A client asks a **time server** for the current time:

1. Client records \`t0\` (local time before request).
2. Server replies with its time \`T_server\`.
3. Client records \`t1\` (local time after response).
4. Client estimates round-trip time: \`RTT = t1 - t0\`.
5. Client sets its clock to: \`T_server + RTT / 2\`.

The \`RTT / 2\` term estimates one-way network delay (assuming symmetric paths).

\`\`\`
Client  ──── request (t0) ────▶  Server
        ◀── T_server reply ───  Server
Client records t1
New client time = T_server + (t1 - t0) / 2
\`\`\`

**Accuracy:** ±RTT/2 in the best case. Outlier RTTs can be discarded.

### Berkeley Algorithm (1989)

Unlike Cristian's (which uses an external time source), the **Berkeley algorithm** synchronizes clocks **among peers** with no authoritative server:

1. A **coordinator** polls all nodes for their clock values.
2. The coordinator computes the **average** of all times (including its own), discarding outliers.
3. The coordinator sends each node the **offset** (adjustment) needed to reach the average.
4. Each node adjusts its clock by the offset.

This does not give accurate real time — it gives **agreement** among nodes, which is often sufficient for internal coordination.

### NTP (Network Time Protocol)

NTP is the internet standard (RFC 5905). It uses a hierarchy of **strata**:

- **Stratum 0:** Atomic clocks, GPS receivers (reference clocks).
- **Stratum 1:** Servers directly connected to stratum 0.
- **Stratum 2:** Servers synced to stratum 1, and so on.

NTP estimates offset and delay from **four timestamps** (similar to Cristian's but with more filtering and a hierarchical trust model). It achieves sub-millisecond accuracy on LANs and ~10 ms over the internet.

### Your Task

Implement two functions:

1. \`cristian(t0, serverTime, t1)\` — returns the estimated synchronized time using Cristian's algorithm: \`serverTime + (t1 - t0) / 2\`.

2. \`berkeley(times)\` — takes an array of clock values (first element is the coordinator), computes the average, and returns an array of offsets each node must apply. Each offset is \`average - nodeTime\`, rounded to 2 decimal places.`,

	starterCode: `function cristian(t0, serverTime, t1) {
	// Return serverTime + estimated one-way delay
	// One-way delay ≈ RTT / 2 = (t1 - t0) / 2
}

function berkeley(times) {
	// 1. Compute the average of all clock values
	// 2. Return an array of offsets: average - times[i]
	// Round each offset to 2 decimal places
}

// Cristian's algorithm
console.log(cristian(100, 500, 120));
console.log(cristian(0, 1000, 30));

// Berkeley algorithm
console.log(berkeley([300, 310, 290]).join(", "));
console.log(berkeley([10, 20, 30, 40]).join(", "));
`,

	solution: `function cristian(t0, serverTime, t1) {
	return serverTime + (t1 - t0) / 2;
}

function berkeley(times) {
	const avg = times.reduce((a, b) => a + b, 0) / times.length;
	return times.map(t => Math.round((avg - t) * 100) / 100);
}

// Cristian's algorithm
console.log(cristian(100, 500, 120));
console.log(cristian(0, 1000, 30));

// Berkeley algorithm
console.log(berkeley([300, 310, 290]).join(", "));
console.log(berkeley([10, 20, 30, 40]).join(", "));
`,

	tests: [
		{
			name: "Cristian's algorithm and Berkeley offsets",
			expected: "510\n1015\n0, -10, 10\n15, 5, -5, -15\n",
		},
		{
			name: "Cristian's with asymmetric RTT values",
			code: `{{FUNC}}
console.log(cristian(50, 200, 60));
console.log(cristian(0, 0, 100));
console.log(cristian(10, 999, 10));`,
			expected: "205\n50\n999\n",
		},
		{
			name: "Berkeley algorithm with varied inputs",
			code: `{{FUNC}}
console.log(berkeley([100]).join(", "));
console.log(berkeley([10, 30]).join(", "));
console.log(berkeley([1, 2, 3, 4, 5]).join(", "));`,
			expected: "0\n10, -10\n2, 1, 0, -1, -2\n",
		},
	],
};
