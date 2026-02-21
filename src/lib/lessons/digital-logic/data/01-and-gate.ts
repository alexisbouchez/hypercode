import type { Lesson } from "../../types";

export const andGate: Lesson = {
	id: "and-gate",
	title: "AND Gate",
	chapterId: "logic-gates",
	content: `## The AND Gate

A logic gate is a basic building block of digital circuits. The **AND gate** outputs \`1\` only when **all** inputs are \`1\`. For any other combination, the output is \`0\`.

| A | B | A AND B |
|---|---|---------|
| 0 | 0 |    0    |
| 0 | 1 |    0    |
| 1 | 0 |    0    |
| 1 | 1 |    1    |

In JavaScript, the bitwise AND operator \`&\` performs this operation on individual bits:

\`\`\`js
1 & 1 // 1
1 & 0 // 0
0 & 1 // 0
0 & 0 // 0
\`\`\`

### Your Task

Implement \`and(a, b)\` that returns \`1\` if both inputs are \`1\`, and \`0\` otherwise. Then print the output for all four input combinations.`,

	starterCode: `function and(a, b) {
  // Return 1 if both a and b are 1, else 0
}

console.log(and(0, 0));
console.log(and(0, 1));
console.log(and(1, 0));
console.log(and(1, 1));
`,

	solution: `function and(a, b) {
  return a & b;
}

console.log(and(0, 0));
console.log(and(0, 1));
console.log(and(1, 0));
console.log(and(1, 1));
`,

	tests: [
		{
			name: "full truth table",
			expected: "0\n0\n0\n1\n",
		},
		{
			name: "and(1,1) returns 1",
			code: `{{FUNC}}
console.log(and(1, 1));`,
			expected: "1\n",
		},
		{
			name: "and(1,0) returns 0",
			code: `{{FUNC}}
console.log(and(1, 0));`,
			expected: "0\n",
		},
	],
};
