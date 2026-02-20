import type { Lesson } from "../../types";

export const forLoops: Lesson = {
	id: "for-loops",
	title: "For Loops",
	chapterId: "control-flow",
	content: `## For Loops

The classic \`for\` loop has three parts: initializer, condition, and update:

\`\`\`js
for (let i = 0; i < 5; i++) {
    console.log(i);
}
// prints 0, 1, 2, 3, 4
\`\`\`

1. \`let i = 0\` — runs once at the start
2. \`i < 5\` — checked before each iteration; loop stops when false
3. \`i++\` — runs after each iteration

### Summing Numbers

\`\`\`js
let total = 0;
for (let i = 1; i <= 10; i++) {
    total += i;
}
console.log(total);  // 55
\`\`\`

### for...of

\`for...of\` iterates over the values in an array:

\`\`\`js
const fruits = ["apple", "banana", "cherry"];
for (const fruit of fruits) {
    console.log(fruit);
}
\`\`\`

### Your Task

Write \`function sumTo(n)\` that returns the sum of all integers from 1 to \`n\` (inclusive). Use a \`for\` loop.`,

	starterCode: `function sumTo(n) {
\t// Return the sum 1 + 2 + ... + n
}

console.log(sumTo(5));
console.log(sumTo(10));
console.log(sumTo(100));
`,

	solution: `function sumTo(n) {
\tlet total = 0;
\tfor (let i = 1; i <= n; i++) {
\t\ttotal += i;
\t}
\treturn total;
}

console.log(sumTo(5));
console.log(sumTo(10));
console.log(sumTo(100));
`,

	tests: [
		{
			name: "sumTo(5)=15, sumTo(10)=55, sumTo(100)=5050",
			expected: "15\n55\n5050\n",
		},
		{
			name: "sumTo(1) = 1",
			code: `{{FUNC}}
console.log(sumTo(1));`,
			expected: "1\n",
		},
		{
			name: "sumTo(0) = 0",
			code: `{{FUNC}}
console.log(sumTo(0));`,
			expected: "0\n",
		},
		{
			name: "sumTo(3) = 6",
			code: `{{FUNC}}
console.log(sumTo(3));`,
			expected: "6\n",
		},
	],
};
