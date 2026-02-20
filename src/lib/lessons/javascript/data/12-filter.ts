import type { Lesson } from "../../types";

export const filter: Lesson = {
	id: "filter",
	title: "Array.filter",
	chapterId: "arrays",
	content: `## Array.filter

\`filter\` keeps only the elements that pass a test:

\`\`\`js
const numbers = [1, 2, 3, 4, 5, 6];
const evens = numbers.filter(n => n % 2 === 0);
console.log(evens);  // [2, 4, 6]
\`\`\`

The function you pass must return \`true\` (keep) or \`false\` (discard). The result may be shorter than the original.

### Chaining

\`map\` and \`filter\` can be chained:

\`\`\`js
const result = [1, 2, 3, 4, 5]
    .filter(n => n % 2 !== 0)   // keep odds: [1, 3, 5]
    .map(n => n * n);            // square them: [1, 9, 25]
console.log(result);
\`\`\`

### Filtering Strings

\`\`\`js
const words = ["apple", "ant", "banana", "avocado", "cherry"];
const aWords = words.filter(w => w.startsWith("a"));
console.log(aWords);  // ["apple", "ant", "avocado"]
\`\`\`

### Your Task

Write \`function positives(numbers)\` that returns only the positive numbers (greater than 0) from the array.`,

	starterCode: `function positives(numbers) {
\t// Return only numbers greater than 0
}

console.log(positives([1, -2, 3, -4, 5]).join(", "));
console.log(positives([-1, -2, -3]).join(", "));
`,

	solution: `function positives(numbers) {
\treturn numbers.filter(n => n > 0);
}

console.log(positives([1, -2, 3, -4, 5]).join(", "));
console.log(positives([-1, -2, -3]).join(", "));
`,

	tests: [
		{
			name: "filters [1,-2,3,-4,5] and [-1,-2,-3]",
			expected: "1, 3, 5\n\n",
		},
		{
			name: "all positive",
			code: `{{FUNC}}
console.log(positives([10, 20, 30]).join(", "));`,
			expected: "10, 20, 30\n",
		},
		{
			name: "mixed with zero",
			code: `{{FUNC}}
console.log(positives([0, 1, -1, 2]).join(", "));`,
			expected: "1, 2\n",
		},
		{
			name: "single positive",
			code: `{{FUNC}}
console.log(positives([42]).join(", "));`,
			expected: "42\n",
		},
	],
};
