import type { Lesson } from "../../types";

export const nandNorGates: Lesson = {
	id: "nand-nor-gates",
	title: "NAND & NOR Gates",
	chapterId: "logic-gates",
	content: `## NAND and NOR Gates

**NAND** is NOT-AND: it outputs \`0\` only when **all** inputs are \`1\`.
**NOR** is NOT-OR: it outputs \`1\` only when **all** inputs are \`0\`.

| A | B | NAND | NOR |
|---|---|------|-----|
| 0 | 0 |  1   |  1  |
| 0 | 1 |  1   |  0  |
| 1 | 0 |  1   |  0  |
| 1 | 1 |  0   |  0  |

NAND and NOR are called **universal gates** — you can build any other gate using only NAND (or only NOR) gates.

In JavaScript, using bitwise operators:

\`\`\`js
// NAND(a, b) = NOT(AND(a, b))
(a & b) ^ 1

// NOR(a, b) = NOT(OR(a, b))
(a | b) ^ 1
\`\`\`

### Your Task

Implement \`nand(a, b)\` and \`nor(a, b)\`, then print all four combinations for each.`,

	starterCode: `function nand(a, b) {
  // NOT(AND(a, b))
}

function nor(a, b) {
  // NOT(OR(a, b))
}

console.log(nand(0, 0), nand(0, 1), nand(1, 0), nand(1, 1));
console.log(nor(0, 0), nor(0, 1), nor(1, 0), nor(1, 1));
`,

	solution: `function nand(a, b) {
  return (a & b) ^ 1;
}

function nor(a, b) {
  return (a | b) ^ 1;
}

console.log(nand(0, 0), nand(0, 1), nand(1, 0), nand(1, 1));
console.log(nor(0, 0), nor(0, 1), nor(1, 0), nor(1, 1));
`,

	tests: [
		{
			name: "nand and nor truth tables",
			expected: "1 1 1 0\n1 0 0 0\n",
		},
		{
			name: "nand(1,1) returns 0",
			code: `{{FUNC}}
console.log(nand(1, 1));`,
			expected: "0\n",
		},
		{
			name: "nor(0,0) returns 1",
			code: `{{FUNC}}
console.log(nor(0, 0));`,
			expected: "1\n",
		},
	],
};
