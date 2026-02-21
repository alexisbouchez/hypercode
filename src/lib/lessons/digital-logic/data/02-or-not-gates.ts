import type { Lesson } from "../../types";

export const orNotGates: Lesson = {
	id: "or-not-gates",
	title: "OR & NOT Gates",
	chapterId: "logic-gates",
	content: `## OR and NOT Gates

The **OR gate** outputs \`1\` when **at least one** input is \`1\`:

| A | B | A OR B |
|---|---|--------|
| 0 | 0 |   0    |
| 0 | 1 |   1    |
| 1 | 0 |   1    |
| 1 | 1 |   1    |

The **NOT gate** (inverter) has a single input and flips it:

| A | NOT A |
|---|-------|
| 0 |   1   |
| 1 |   0   |

In JavaScript:
- OR uses the bitwise OR operator \`|\`
- NOT can be done with XOR: \`a ^ 1\` (flips the last bit)

\`\`\`js
1 | 0  // 1
0 | 0  // 0
1 ^ 1  // 0  (NOT 1)
0 ^ 1  // 1  (NOT 0)
\`\`\`

### Your Task

Implement \`or(a, b)\` and \`not(a)\`, then print:
- \`or(0, 0)\`, \`or(0, 1)\`, \`or(1, 1)\`
- \`not(0)\`, \`not(1)\``,

	starterCode: `function or(a, b) {
  // Return 1 if at least one input is 1
}

function not(a) {
  // Flip the bit: 0 -> 1, 1 -> 0
}

console.log(or(0, 0));
console.log(or(0, 1));
console.log(or(1, 1));
console.log(not(0));
console.log(not(1));
`,

	solution: `function or(a, b) {
  return a | b;
}

function not(a) {
  return a ^ 1;
}

console.log(or(0, 0));
console.log(or(0, 1));
console.log(or(1, 1));
console.log(not(0));
console.log(not(1));
`,

	tests: [
		{
			name: "or and not truth tables",
			expected: "0\n1\n1\n1\n0\n",
		},
		{
			name: "or(1,0) returns 1",
			code: `{{FUNC}}
console.log(or(1, 0));`,
			expected: "1\n",
		},
		{
			name: "not(0) returns 1",
			code: `{{FUNC}}
console.log(not(0));`,
			expected: "1\n",
		},
	],
};
