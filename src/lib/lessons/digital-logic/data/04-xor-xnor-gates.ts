import type { Lesson } from "../../types";

export const xorXnorGates: Lesson = {
	id: "xor-xnor-gates",
	title: "XOR & XNOR Gates",
	chapterId: "logic-gates",
	content: `## XOR and XNOR Gates

**XOR** (Exclusive OR) outputs \`1\` when inputs are **different**:

| A | B | XOR | XNOR |
|---|---|-----|------|
| 0 | 0 |  0  |  1   |
| 0 | 1 |  1  |  0   |
| 1 | 0 |  1  |  0   |
| 1 | 1 |  0  |  1   |

**XNOR** is the complement of XOR — it outputs \`1\` when inputs are **equal**.

XOR is fundamental in digital arithmetic:
- **Half adder**: sum bit = A XOR B
- **Parity checker**: XOR of all bits tells you if count of 1s is odd
- **Comparator**: A XNOR B tells you if two bits are equal

In JavaScript, XOR is the \`^\` operator:
\`\`\`js
1 ^ 0  // 1
1 ^ 1  // 0
\`\`\`

### Your Task

Implement \`xor(a, b)\` and \`xnor(a, b)\`, then print all four combinations for each.`,

	starterCode: `function xor(a, b) {
  // Return 1 when inputs differ
}

function xnor(a, b) {
  // Return 1 when inputs are equal
}

console.log(xor(0, 0), xor(0, 1), xor(1, 0), xor(1, 1));
console.log(xnor(0, 0), xnor(0, 1), xnor(1, 0), xnor(1, 1));
`,

	solution: `function xor(a, b) {
  return a ^ b;
}

function xnor(a, b) {
  return (a ^ b) ^ 1;
}

console.log(xor(0, 0), xor(0, 1), xor(1, 0), xor(1, 1));
console.log(xnor(0, 0), xnor(0, 1), xnor(1, 0), xnor(1, 1));
`,

	tests: [
		{
			name: "xor and xnor truth tables",
			expected: "0 1 1 0\n1 0 0 1\n",
		},
		{
			name: "xor(1,1) returns 0",
			code: `{{FUNC}}
console.log(xor(1, 1));`,
			expected: "0\n",
		},
		{
			name: "xnor(0,0) returns 1",
			code: `{{FUNC}}
console.log(xnor(0, 0));`,
			expected: "1\n",
		},
	],
};
