import type { Lesson } from "../../types";

export const universalGates: Lesson = {
	id: "universal-gates",
	title: "Universal Gates",
	chapterId: "logic-gates",
	content: `## Universal Gates

NAND is called a **universal gate** because you can build any logic function using only NAND gates:

\`\`\`
NOT(A)      = NAND(A, A)
AND(A, B)   = NAND(NAND(A, B), NAND(A, B))  = NOT(NAND(A,B))
OR(A, B)    = NAND(NAND(A, A), NAND(B, B))  = NAND(NOT(A), NOT(B))
\`\`\`

This matters for chip manufacturing: a factory that can make NAND gates can make any digital circuit.

Let's verify De Morgan's law for NAND:
> **NAND(A, B) = NOT(A) OR NOT(B)**

| A | B | NAND | NOT(A) OR NOT(B) |
|---|---|------|-----------------|
| 0 | 0 |  1   |      1          |
| 0 | 1 |  1   |      1          |
| 1 | 0 |  1   |      1          |
| 1 | 1 |  0   |      0          |

### Your Task

Using only \`nand(a, b)\`, implement:
- \`notFromNand(a)\` — NOT using only NAND
- \`andFromNand(a, b)\` — AND using only NAND
- \`orFromNand(a, b)\` — OR using only NAND

Then print the full truth table for \`orFromNand\`.`,

	starterCode: `function nand(a, b) {
  return (a & b) ^ 1;
}

function notFromNand(a) {
  // Hint: NAND(A, A)
}

function andFromNand(a, b) {
  // Hint: NOT(NAND(A, B))
}

function orFromNand(a, b) {
  // Hint: NAND(NOT(A), NOT(B))
}

console.log(orFromNand(0, 0), orFromNand(0, 1), orFromNand(1, 0), orFromNand(1, 1));
`,

	solution: `function nand(a, b) {
  return (a & b) ^ 1;
}

function notFromNand(a) {
  return nand(a, a);
}

function andFromNand(a, b) {
  return notFromNand(nand(a, b));
}

function orFromNand(a, b) {
  return nand(notFromNand(a), notFromNand(b));
}

console.log(orFromNand(0, 0), orFromNand(0, 1), orFromNand(1, 0), orFromNand(1, 1));
`,

	tests: [
		{
			name: "orFromNand truth table",
			expected: "0 1 1 1\n",
		},
		{
			name: "notFromNand(1) returns 0",
			code: `{{FUNC}}
console.log(notFromNand(1));`,
			expected: "0\n",
		},
		{
			name: "andFromNand(1,1) returns 1",
			code: `{{FUNC}}
console.log(andFromNand(1, 1));`,
			expected: "1\n",
		},
		{
			name: "andFromNand(1,0) returns 0",
			code: `{{FUNC}}
console.log(andFromNand(1, 0));`,
			expected: "0\n",
		},
	],
};
