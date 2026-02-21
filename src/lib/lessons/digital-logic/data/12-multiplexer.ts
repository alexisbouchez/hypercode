import type { Lesson } from "../../types";

export const multiplexer: Lesson = {
	id: "multiplexer",
	title: "Multiplexer",
	chapterId: "combinational-circuits",
	content: `## Multiplexer (MUX)

A **multiplexer** (MUX) selects one of several input signals and routes it to the output, based on **select lines**.

A **4-to-1 MUX** has:
- 4 data inputs: D0, D1, D2, D3
- 2 select lines: S1, S0
- 1 output Y

| S1 | S0 | Y  |
|----|----|----|
| 0  | 0  | D0 |
| 0  | 1  | D1 |
| 1  | 0  | D2 |
| 1  | 1  | D3 |

The select lines form a 2-bit binary address: S1 is the MSB, S0 is the LSB. Together they select which input to pass through.

Multiplexers are used everywhere: bus arbitration, data routing, function generators, and implementing any Boolean function using a single MUX.

### Your Task

Implement \`mux4to1(d0, d1, d2, d3, s1, s0)\` that returns the selected input based on the select lines.`,

	starterCode: `function mux4to1(d0, d1, d2, d3, s1, s0) {
  // Select one of d0..d3 based on (s1, s0)
}

console.log(mux4to1(0, 1, 0, 1, 0, 0)); // D0 = 0
console.log(mux4to1(0, 1, 0, 1, 0, 1)); // D1 = 1
console.log(mux4to1(0, 1, 0, 1, 1, 0)); // D2 = 0
console.log(mux4to1(0, 1, 0, 1, 1, 1)); // D3 = 1
`,

	solution: `function mux4to1(d0, d1, d2, d3, s1, s0) {
  const sel = (s1 << 1) | s0;
  const inputs = [d0, d1, d2, d3];
  return inputs[sel];
}

console.log(mux4to1(0, 1, 0, 1, 0, 0));
console.log(mux4to1(0, 1, 0, 1, 0, 1));
console.log(mux4to1(0, 1, 0, 1, 1, 0));
console.log(mux4to1(0, 1, 0, 1, 1, 1));
`,

	tests: [
		{
			name: "4-to-1 MUX selection",
			expected: "0\n1\n0\n1\n",
		},
		{
			name: "mux4to1 selects D2",
			code: `{{FUNC}}
console.log(mux4to1(10, 20, 30, 40, 1, 0));`,
			expected: "30\n",
		},
		{
			name: "mux4to1 selects D3",
			code: `{{FUNC}}
console.log(mux4to1(10, 20, 30, 40, 1, 1));`,
			expected: "40\n",
		},
	],
};
