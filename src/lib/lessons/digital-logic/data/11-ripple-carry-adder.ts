import type { Lesson } from "../../types";

export const rippleCarryAdder: Lesson = {
	id: "ripple-carry-adder",
	title: "Ripple Carry Adder",
	chapterId: "combinational-circuits",
	content: `## Ripple Carry Adder

A **ripple carry adder** chains \`n\` full adders to add two n-bit numbers. The carry **ripples** from the least significant bit to the most significant bit.

To add two 4-bit numbers A = 0101 (5) and B = 0011 (3):

\`\`\`
Bit 0: FA(1, 1, 0) → sum=0, carry=1
Bit 1: FA(0, 1, 1) → sum=0, carry=1
Bit 2: FA(1, 0, 1) → sum=0, carry=1
Bit 3: FA(0, 0, 1) → sum=1, carry=0
Result: 1000 = 8  ✓
\`\`\`

The inputs \`a\` and \`b\` are arrays of bits with index 0 = LSB.

### Your Task

Implement \`rippleCarryAdder(aBits, bBits)\` where:
- \`aBits\` and \`bBits\` are arrays of bits, LSB first
- Returns \`{ sum: [...bits], carry: finalCarry }\`

Then add 5 (0101) and 3 (0011) as 4-bit numbers and print the sum bits and the decimal result.`,

	starterCode: `function fullAdder(a, b, cin) {
  const s1 = a ^ b;
  const c1 = a & b;
  const sum = s1 ^ cin;
  const carry = c1 | (s1 & cin);
  return { sum, carry };
}

function rippleCarryAdder(aBits, bBits) {
  // Chain fullAdders, propagate carry from LSB to MSB
}

// 5 = 0101, 3 = 0011, LSB first = [1,0,1,0] and [1,1,0,0]
const result = rippleCarryAdder([1,0,1,0], [1,1,0,0]);
console.log(result.sum.join(""));
const decimal = result.sum.reduce((acc, bit, i) => acc + bit * (1 << i), 0);
console.log(decimal);
`,

	solution: `function fullAdder(a, b, cin) {
  const s1 = a ^ b;
  const c1 = a & b;
  const sum = s1 ^ cin;
  const carry = c1 | (s1 & cin);
  return { sum, carry };
}

function rippleCarryAdder(aBits, bBits) {
  let carry = 0;
  const sumBits = [];
  for (let i = 0; i < aBits.length; i++) {
    const fa = fullAdder(aBits[i], bBits[i], carry);
    sumBits.push(fa.sum);
    carry = fa.carry;
  }
  return { sum: sumBits, carry };
}

const result = rippleCarryAdder([1,0,1,0], [1,1,0,0]);
console.log(result.sum.join(""));
const decimal = result.sum.reduce((acc, bit, i) => acc + bit * (1 << i), 0);
console.log(decimal);
`,

	tests: [
		{
			name: "5+3=8",
			expected: "0001\n8\n",
		},
		{
			name: "3+4=7",
			code: `{{FUNC}}
// 3 = 0011, 4 = 0100 (LSB first)
const r = rippleCarryAdder([1,1,0,0], [0,0,1,0]);
const d = r.sum.reduce((acc, bit, i) => acc + bit * (1 << i), 0);
console.log(d);`,
			expected: "7\n",
		},
		{
			name: "7+7=14",
			code: `{{FUNC}}
// 7 = 0111 (LSB first)
const r = rippleCarryAdder([1,1,1,0], [1,1,1,0]);
const d = r.sum.reduce((acc, bit, i) => acc + bit * (1 << i), 0) + r.carry * 16;
console.log(d);`,
			expected: "14\n",
		},
	],
};
