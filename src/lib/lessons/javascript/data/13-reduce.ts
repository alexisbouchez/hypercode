import type { Lesson } from "../../types";

export const reduce: Lesson = {
	id: "reduce",
	title: "Array.reduce",
	chapterId: "arrays",
	content: `## Array.reduce

\`reduce\` collapses an array to a single value by applying a function repeatedly:

\`\`\`js
const numbers = [1, 2, 3, 4, 5];
const sum = numbers.reduce((acc, n) => acc + n, 0);
console.log(sum);  // 15
\`\`\`

The callback receives two arguments:
- \`acc\` — the **accumulator**, carrying the running result
- \`n\` — the current element

The second argument (\`0\` here) is the **initial value** of \`acc\`.

### How it Works

\`\`\`
acc=0,  n=1  →  acc=1
acc=1,  n=2  →  acc=3
acc=3,  n=3  →  acc=6
acc=6,  n=4  →  acc=10
acc=10, n=5  →  acc=15
\`\`\`

### Finding the Maximum

\`\`\`js
const max = [3, 1, 4, 1, 5, 9, 2, 6].reduce(
    (best, n) => n > best ? n : best,
    -Infinity
);
console.log(max);  // 9
\`\`\`

### Your Task

Write \`function product(numbers)\` that returns the product (multiplication) of all elements.`,

	starterCode: `function product(numbers) {
\t// Return the product of all numbers
}

console.log(product([1, 2, 3, 4, 5]));
console.log(product([2, 10]));
console.log(product([7]));
`,

	solution: `function product(numbers) {
\treturn numbers.reduce((acc, n) => acc * n, 1);
}

console.log(product([1, 2, 3, 4, 5]));
console.log(product([2, 10]));
console.log(product([7]));
`,

	tests: [
		{
			name: "product of [1,2,3,4,5]=120, [2,10]=20, [7]=7",
			expected: "120\n20\n7\n",
		},
		{
			name: "product([1]) = 1",
			code: `{{FUNC}}
console.log(product([1]));`,
			expected: "1\n",
		},
		{
			name: "product([3, 3, 3]) = 27",
			code: `{{FUNC}}
console.log(product([3, 3, 3]));`,
			expected: "27\n",
		},
		{
			name: "product([1, 1, 1, 1]) = 1",
			code: `{{FUNC}}
console.log(product([1, 1, 1, 1]));`,
			expected: "1\n",
		},
	],
};
