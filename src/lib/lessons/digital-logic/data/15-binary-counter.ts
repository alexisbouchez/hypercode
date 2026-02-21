import type { Lesson } from "../../types";

export const binaryCounter: Lesson = {
	id: "binary-counter",
	title: "4-Bit Binary Counter",
	chapterId: "sequential-circuits",
	content: `## 4-Bit Binary Counter

A **binary counter** increments its stored value by 1 on each clock edge. A 4-bit counter counts from \`0\` to \`15\` (0000 to 1111 in binary), then wraps back to \`0\`.

This is the fundamental building block for:
- Timing circuits (counting clock pulses)
- Address generators (for memory access)
- Frequency dividers (output bit N toggles every 2^N clocks)
- PWM generators

The 4-bit counter state transitions:
\`\`\`
0 → 1 → 2 → 3 → 4 → 5 → 6 → 7 → 8 → 9 → 10 → 11 → 12 → 13 → 14 → 15 → 0 → ...
\`\`\`

To convert the state to a 4-bit binary array (MSB first):
\`\`\`js
function toBits(n) {
  return [(n>>3)&1, (n>>2)&1, (n>>1)&1, n&1];
}
\`\`\`

### Your Task

Implement:
- \`nextCount(state)\`: returns \`(state + 1) % 16\`
- \`countSequence(n)\`: returns array of \`n+1\` states starting at \`0\`

Print the first 8 states as 4-bit binary strings.`,

	starterCode: `function nextCount(state) {
  // Increment with wraparound at 16
}

function countSequence(n) {
  // Return [0, 1, 2, ..., n] with wraparound
}

function toBits(n) {
  return [(n>>3)&1, (n>>2)&1, (n>>1)&1, n&1].join("");
}

countSequence(7).forEach(s => console.log(toBits(s)));
`,

	solution: `function nextCount(state) {
  return (state + 1) % 16;
}

function countSequence(n) {
  const seq = [0];
  for (let i = 0; i < n; i++) seq.push(nextCount(seq[seq.length - 1]));
  return seq;
}

function toBits(n) {
  return [(n>>3)&1, (n>>2)&1, (n>>1)&1, n&1].join("");
}

countSequence(7).forEach(s => console.log(toBits(s)));
`,

	tests: [
		{
			name: "first 8 binary counts",
			expected: "0000\n0001\n0010\n0011\n0100\n0101\n0110\n0111\n",
		},
		{
			name: "wraps at 16",
			code: `{{FUNC}}
console.log(nextCount(15));`,
			expected: "0\n",
		},
		{
			name: "countSequence(3) is [0,1,2,3]",
			code: `{{FUNC}}
console.log(countSequence(3).join(","));`,
			expected: "0,1,2,3\n",
		},
	],
};
