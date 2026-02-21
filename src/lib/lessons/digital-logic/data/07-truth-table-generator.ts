import type { Lesson } from "../../types";

export const truthTableGenerator: Lesson = {
	id: "truth-table-generator",
	title: "Truth Table Generator",
	chapterId: "boolean-algebra",
	content: `## Truth Table Generator

A truth table lists all possible input combinations and the corresponding output. For \`n\` inputs, there are \`2^n\` rows.

For a 2-input function, the rows are:
\`\`\`
A  B  | output
0  0  |  ?
0  1  |  ?
1  0  |  ?
1  1  |  ?
\`\`\`

To generate all combinations programmatically, iterate \`i\` from \`0\` to \`2^n - 1\`, and extract each input bit with a right-shift and mask:

\`\`\`js
// For n=2, i=3 (binary: 11):
// input 0 (MSB) = (3 >> 1) & 1 = 1
// input 1 (LSB) = (3 >> 0) & 1 = 1
\`\`\`

### Your Task

Implement \`truthTable(fn, n)\` where:
- \`fn\` is a function that takes \`n\` bit arguments (0 or 1)
- \`n\` is the number of inputs

Return an array of arrays. Each inner array contains the \`n\` inputs followed by the output.

Then generate and print the truth table for XOR (2 inputs), printing each row as \`"A B => out"\`.`,

	starterCode: `function truthTable(fn, n) {
  // Generate all 2^n input combinations
  // Return array of [...inputs, output]
}

const xor = (a, b) => a ^ b;
const table = truthTable(xor, 2);
table.forEach(row => {
  const inputs = row.slice(0, -1).join(" ");
  const output = row[row.length - 1];
  console.log(inputs + " => " + output);
});
`,

	solution: `function truthTable(fn, n) {
  const rows = 1 << n;
  const result = [];
  for (let i = 0; i < rows; i++) {
    const inputs = [];
    for (let j = n - 1; j >= 0; j--) {
      inputs.push((i >> j) & 1);
    }
    result.push([...inputs, fn(...inputs)]);
  }
  return result;
}

const xor = (a, b) => a ^ b;
const table = truthTable(xor, 2);
table.forEach(row => {
  const inputs = row.slice(0, -1).join(" ");
  const output = row[row.length - 1];
  console.log(inputs + " => " + output);
});
`,

	tests: [
		{
			name: "XOR truth table",
			expected: "0 0 => 0\n0 1 => 1\n1 0 => 1\n1 1 => 0\n",
		},
		{
			name: "AND truth table",
			code: `{{FUNC}}
const andFn = (a, b) => a & b;
const t = truthTable(andFn, 2);
t.forEach(row => {
  const inputs = row.slice(0, -1).join(" ");
  const output = row[row.length - 1];
  console.log(inputs + " => " + output);
});`,
			expected: "0 0 => 0\n0 1 => 0\n1 0 => 0\n1 1 => 1\n",
		},
		{
			name: "NOT truth table (1 input)",
			code: `{{FUNC}}
const notFn = (a) => a ^ 1;
const t = truthTable(notFn, 1);
t.forEach(row => console.log(row.join(" => ")));`,
			expected: "0 => 1\n1 => 0\n",
		},
	],
};
