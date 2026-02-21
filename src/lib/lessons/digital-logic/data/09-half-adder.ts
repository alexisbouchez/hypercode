import type { Lesson } from "../../types";

export const halfAdder: Lesson = {
	id: "half-adder",
	title: "Half Adder",
	chapterId: "combinational-circuits",
	content: `## Half Adder

A **half adder** adds two single bits and produces two outputs:
- **Sum (S)**: the XOR of the inputs — the result bit
- **Carry (C)**: the AND of the inputs — the carry to the next bit position

| A | B | Sum | Carry |
|---|---|-----|-------|
| 0 | 0 |  0  |   0   |
| 0 | 1 |  1  |   0   |
| 1 | 0 |  1  |   0   |
| 1 | 1 |  0  |   1   |

When 1 + 1 = 2 in binary is \`10\`: sum bit is \`0\`, carry bit is \`1\`.

The half adder is called "half" because it cannot handle a carry-in from a previous addition — for that, you need a **full adder**.

### Circuit

\`\`\`
S = A XOR B
C = A AND B
\`\`\`

### Your Task

Implement \`halfAdder(a, b)\` that returns an object \`{ sum, carry }\`.

Then print the result of adding 1+1 and 1+0.`,

	starterCode: `function halfAdder(a, b) {
  // Return { sum: A XOR B, carry: A AND B }
}

const r1 = halfAdder(1, 1);
console.log("1+1: sum=" + r1.sum + " carry=" + r1.carry);
const r2 = halfAdder(1, 0);
console.log("1+0: sum=" + r2.sum + " carry=" + r2.carry);
`,

	solution: `function halfAdder(a, b) {
  return { sum: a ^ b, carry: a & b };
}

const r1 = halfAdder(1, 1);
console.log("1+1: sum=" + r1.sum + " carry=" + r1.carry);
const r2 = halfAdder(1, 0);
console.log("1+0: sum=" + r2.sum + " carry=" + r2.carry);
`,

	tests: [
		{
			name: "1+1 and 1+0",
			expected: "1+1: sum=0 carry=1\n1+0: sum=1 carry=0\n",
		},
		{
			name: "halfAdder(0,0)",
			code: `{{FUNC}}
const r = halfAdder(0, 0);
console.log(r.sum, r.carry);`,
			expected: "0 0\n",
		},
		{
			name: "halfAdder(0,1)",
			code: `{{FUNC}}
const r = halfAdder(0, 1);
console.log(r.sum, r.carry);`,
			expected: "1 0\n",
		},
	],
};
