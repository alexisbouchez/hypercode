import type { Lesson } from "../../types";

export const conditionals: Lesson = {
	id: "conditionals",
	title: "Conditionals",
	chapterId: "control-flow",
	content: `## Conditionals

\`if\`/\`else\` lets your program take different paths based on a condition:

\`\`\`js
if (temperature > 30) {
    console.log("hot");
} else if (temperature > 15) {
    console.log("warm");
} else {
    console.log("cold");
}
\`\`\`

### Comparison Operators

\`\`\`js
x === y   // strictly equal (same type and value)
x !== y   // strictly not equal
x > y     // greater than
x < y     // less than
x >= y    // greater than or equal
x <= y    // less than or equal
\`\`\`

Always use \`===\` (triple equals) instead of \`==\` (double equals) in JavaScript. \`==\` does type coercion and produces surprising results.

### Logical Operators

\`\`\`js
a && b    // true if both are true
a || b    // true if either is true
!a        // true if a is false
\`\`\`

### Your Task

Write \`function classify(n)\` that returns:
- \`"positive"\` if \`n > 0\`
- \`"negative"\` if \`n < 0\`
- \`"zero"\` if \`n === 0\``,

	starterCode: `function classify(n) {
\t// Return "positive", "negative", or "zero"
}

console.log(classify(5));
console.log(classify(-3));
console.log(classify(0));
`,

	solution: `function classify(n) {
\tif (n > 0) {
\t\treturn "positive";
\t} else if (n < 0) {
\t\treturn "negative";
\t} else {
\t\treturn "zero";
\t}
}

console.log(classify(5));
console.log(classify(-3));
console.log(classify(0));
`,

	tests: [
		{
			name: "classify 5, -3, 0",
			expected: "positive\nnegative\nzero\n",
		},
		{
			name: "classify positive number",
			code: `{{FUNC}}
console.log(classify(42));`,
			expected: "positive\n",
		},
		{
			name: "classify negative number",
			code: `{{FUNC}}
console.log(classify(-100));`,
			expected: "negative\n",
		},
		{
			name: "classify zero",
			code: `{{FUNC}}
console.log(classify(0));`,
			expected: "zero\n",
		},
	],
};
