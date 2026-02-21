import type { Lesson } from "../../types";

export const srLatch: Lesson = {
	id: "sr-latch",
	title: "SR Latch",
	chapterId: "sequential-circuits",
	content: `## SR Latch

A **latch** is the simplest sequential (memory) element. Unlike combinational circuits, its output depends on both current inputs **and** previous state.

The **SR latch** has two inputs:
- **S** (Set): forces output Q to \`1\`
- **R** (Reset): forces output Q to \`0\`

| S | R | Q (next) | Notes        |
|---|---|----------|--------------|
| 0 | 0 | Q (hold) | no change    |
| 1 | 0 |    1     | set          |
| 0 | 1 |    0     | reset        |
| 1 | 1 |    ?     | invalid (forbidden) |

The SR latch **holds** its state when both inputs are \`0\`. This is how single-bit memory works in digital circuits.

A function model takes the current state \`q\` and returns the next state:

\`\`\`js
function srLatch(s, r, q) {
  if (s === 1 && r === 0) return 1;  // set
  if (s === 0 && r === 1) return 0;  // reset
  if (s === 0 && r === 0) return q;  // hold
  return -1; // invalid
}
\`\`\`

### Your Task

Implement \`srLatch(s, r, q)\` that returns the next Q state (or -1 for invalid S=R=1).

Then simulate a sequence: start Q=0, apply Set, hold, hold, Reset, hold.`,

	starterCode: `function srLatch(s, r, q) {
  // Return next state or -1 for invalid
}

let q = 0;
const inputs = [[1,0],[0,0],[0,0],[0,1],[0,0]];
inputs.forEach(([s, r]) => {
  q = srLatch(s, r, q);
  console.log("S=" + s + " R=" + r + " Q=" + q);
});
`,

	solution: `function srLatch(s, r, q) {
  if (s === 1 && r === 0) return 1;
  if (s === 0 && r === 1) return 0;
  if (s === 0 && r === 0) return q;
  return -1;
}

let q = 0;
const inputs = [[1,0],[0,0],[0,0],[0,1],[0,0]];
inputs.forEach(([s, r]) => {
  q = srLatch(s, r, q);
  console.log("S=" + s + " R=" + r + " Q=" + q);
});
`,

	tests: [
		{
			name: "set hold hold reset hold sequence",
			expected: "S=1 R=0 Q=1\nS=0 R=0 Q=1\nS=0 R=0 Q=1\nS=0 R=1 Q=0\nS=0 R=0 Q=0\n",
		},
		{
			name: "invalid state returns -1",
			code: `{{FUNC}}
console.log(srLatch(1, 1, 0));`,
			expected: "-1\n",
		},
		{
			name: "hold preserves state 1",
			code: `{{FUNC}}
console.log(srLatch(0, 0, 1));`,
			expected: "1\n",
		},
	],
};
