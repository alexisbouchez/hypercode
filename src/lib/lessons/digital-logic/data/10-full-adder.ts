import type { Lesson } from "../../types";

export const fullAdder: Lesson = {
	id: "full-adder",
	title: "Full Adder",
	chapterId: "combinational-circuits",
	content: `## Full Adder

A **full adder** adds three bits: two inputs **A**, **B**, and a **carry-in** (Cin). It produces a **sum** and a **carry-out** (Cout).

| A | B | Cin | Sum | Cout |
|---|---|-----|-----|------|
| 0 | 0 |  0  |  0  |  0   |
| 0 | 0 |  1  |  1  |  0   |
| 0 | 1 |  0  |  1  |  0   |
| 0 | 1 |  1  |  0  |  1   |
| 1 | 0 |  0  |  1  |  0   |
| 1 | 0 |  1  |  0  |  1   |
| 1 | 1 |  0  |  0  |  1   |
| 1 | 1 |  1  |  1  |  1   |

The full adder is built from two half adders:

\`\`\`
step1 = halfAdder(A, B)        → {sum: s1, carry: c1}
step2 = halfAdder(s1, Cin)     → {sum: S, carry: c2}
Cout  = c1 OR c2
\`\`\`

### Your Task

Implement \`fullAdder(a, b, cin)\` that returns \`{ sum, carry }\`. Use your half adder internally.`,

	starterCode: `function halfAdder(a, b) {
  return { sum: a ^ b, carry: a & b };
}

function fullAdder(a, b, cin) {
  // Use two halfAdders, combine carries with OR
}

console.log(JSON.stringify(fullAdder(1, 1, 0)));
console.log(JSON.stringify(fullAdder(1, 1, 1)));
`,

	solution: `function halfAdder(a, b) {
  return { sum: a ^ b, carry: a & b };
}

function fullAdder(a, b, cin) {
  const step1 = halfAdder(a, b);
  const step2 = halfAdder(step1.sum, cin);
  return { sum: step2.sum, carry: step1.carry | step2.carry };
}

console.log(JSON.stringify(fullAdder(1, 1, 0)));
console.log(JSON.stringify(fullAdder(1, 1, 1)));
`,

	tests: [
		{
			name: "1+1+0 and 1+1+1",
			expected: '{"sum":0,"carry":1}\n{"sum":1,"carry":1}\n',
		},
		{
			name: "fullAdder(0,1,1)",
			code: `{{FUNC}}
console.log(JSON.stringify(fullAdder(0, 1, 1)));`,
			expected: '{"sum":0,"carry":1}\n',
		},
		{
			name: "fullAdder(0,0,0)",
			code: `{{FUNC}}
console.log(JSON.stringify(fullAdder(0, 0, 0)));`,
			expected: '{"sum":0,"carry":0}\n',
		},
	],
};
