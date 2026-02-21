import type { Lesson } from "../../types";

export const decoder: Lesson = {
	id: "decoder",
	title: "Decoder",
	chapterId: "combinational-circuits",
	content: `## Decoder

A **decoder** converts a binary code into one-hot output — exactly one output line goes high for each input combination.

A **2-to-4 decoder** has:
- 2 input lines: A1, A0
- 4 output lines: Y0, Y1, Y2, Y3
- Exactly one Yi is \`1\` for each input

| A1 | A0 | Y0 | Y1 | Y2 | Y3 |
|----|----|----|----|----|-----|
| 0  | 0  | 1  | 0  | 0  | 0  |
| 0  | 1  | 0  | 1  | 0  | 0  |
| 1  | 0  | 0  | 0  | 1  | 0  |
| 1  | 1  | 0  | 0  | 0  | 1  |

The selected output Yi = 1 when the binary address equals i.

Decoders are used for memory address decoding (selecting which memory chip to activate), instruction decoding in CPUs, and driving 7-segment displays.

### Your Task

Implement \`decoder2to4(a1, a0)\` that returns an array \`[Y0, Y1, Y2, Y3]\` where exactly one element is \`1\`.`,

	starterCode: `function decoder2to4(a1, a0) {
  // Return [Y0, Y1, Y2, Y3] with exactly one 1
}

console.log(decoder2to4(0, 0).join(" "));
console.log(decoder2to4(0, 1).join(" "));
console.log(decoder2to4(1, 0).join(" "));
console.log(decoder2to4(1, 1).join(" "));
`,

	solution: `function decoder2to4(a1, a0) {
  const sel = (a1 << 1) | a0;
  return [0, 1, 2, 3].map(i => i === sel ? 1 : 0);
}

console.log(decoder2to4(0, 0).join(" "));
console.log(decoder2to4(0, 1).join(" "));
console.log(decoder2to4(1, 0).join(" "));
console.log(decoder2to4(1, 1).join(" "));
`,

	tests: [
		{
			name: "2-to-4 decoder truth table",
			expected: "1 0 0 0\n0 1 0 0\n0 0 1 0\n0 0 0 1\n",
		},
		{
			name: "decoder2to4(1,0) activates Y2",
			code: `{{FUNC}}
const out = decoder2to4(1, 0);
console.log(out[2]);`,
			expected: "1\n",
		},
		{
			name: "only one output is 1",
			code: `{{FUNC}}
const out = decoder2to4(1, 1);
console.log(out.reduce((a, b) => a + b, 0));`,
			expected: "1\n",
		},
	],
};
