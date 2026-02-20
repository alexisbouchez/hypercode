import type { Lesson } from "../../types";

export const map: Lesson = {
	id: "map",
	title: "Array.map",
	chapterId: "arrays",
	content: `## Array.map

\`map\` transforms every element of an array and returns a new array:

\`\`\`js
const numbers = [1, 2, 3, 4, 5];
const doubled = numbers.map(n => n * 2);
console.log(doubled);  // [2, 4, 6, 8, 10]
\`\`\`

The original array is unchanged. \`map\` always returns an array of the same length.

### How it Works

\`map\` calls the function you provide once for each element, passing:
1. The element's value
2. Its index (optional)
3. The whole array (optional)

\`\`\`js
const words = ["hello", "world"];
const lengths = words.map(w => w.length);
console.log(lengths);  // [5, 5]
\`\`\`

### Your Task

Write \`function squareAll(numbers)\` that takes an array of numbers and returns a new array where each number is squared.

Use \`.map()\` and print the result with \`.join(", ")\`.`,

	starterCode: `function squareAll(numbers) {
\t// Return a new array with each number squared
}

console.log(squareAll([1, 2, 3, 4, 5]).join(", "));
console.log(squareAll([3, 5, 7]).join(", "));
`,

	solution: `function squareAll(numbers) {
\treturn numbers.map(n => n * n);
}

console.log(squareAll([1, 2, 3, 4, 5]).join(", "));
console.log(squareAll([3, 5, 7]).join(", "));
`,

	tests: [
		{
			name: "squares [1,2,3,4,5] and [3,5,7]",
			expected: "1, 4, 9, 16, 25\n9, 25, 49\n",
		},
		{
			name: "squareAll([0, 1]) = [0, 1]",
			code: `{{FUNC}}
console.log(squareAll([0, 1]).join(", "));`,
			expected: "0, 1\n",
		},
		{
			name: "squareAll([10]) = [100]",
			code: `{{FUNC}}
console.log(squareAll([10]).join(", "));`,
			expected: "100\n",
		},
		{
			name: "squareAll([2, 3, 4]) = [4, 9, 16]",
			code: `{{FUNC}}
console.log(squareAll([2, 3, 4]).join(", "));`,
			expected: "4, 9, 16\n",
		},
	],
};
