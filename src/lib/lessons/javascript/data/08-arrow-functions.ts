import type { Lesson } from "../../types";

export const arrowFunctions: Lesson = {
	id: "arrow-functions",
	title: "Arrow Functions",
	chapterId: "functions",
	content: `## Arrow Functions

Arrow functions are a shorter syntax for writing functions:

\`\`\`js
// Traditional function
function double(n) {
    return n * 2;
}

// Arrow function — same behavior, shorter syntax
const double = (n) => n * 2;

// Single parameter — parentheses optional
const double = n => n * 2;

// Multiple parameters — parentheses required
const add = (a, b) => a + b;
\`\`\`

When the body is a single expression, the result is returned automatically. No \`return\` keyword needed.

### Multi-line Arrow Functions

For more complex logic, use curly braces with an explicit \`return\`:

\`\`\`js
const factorial = (n) => {
    let result = 1;
    for (let i = 2; i <= n; i++) result *= i;
    return result;
};
\`\`\`

### Arrow Functions as Arguments

Arrow functions shine when passed as arguments to other functions:

\`\`\`js
const numbers = [1, 2, 3, 4, 5];
const squares = numbers.map(n => n * n);
console.log(squares);  // [1, 4, 9, 16, 25]
\`\`\`

### Your Task

Write two arrow functions:
- \`const double\` — takes a number, returns double
- \`const square\` — takes a number, returns its square

Then print \`double(7)\` and \`square(6)\`.`,

	starterCode: `const double = (n) => // your expression here
const square = (n) => // your expression here

console.log(double(7));
console.log(square(6));
`,

	solution: `const double = (n) => n * 2;
const square = (n) => n * n;

console.log(double(7));
console.log(square(6));
`,

	tests: [
		{
			name: "double(7)=14, square(6)=36",
			expected: "14\n36\n",
		},
		{
			name: "double(0) = 0",
			code: `{{FUNC}}
console.log(double(0));`,
			expected: "0\n",
		},
		{
			name: "square(5) = 25",
			code: `{{FUNC}}
console.log(square(5));`,
			expected: "25\n",
		},
		{
			name: "double(100) = 200",
			code: `{{FUNC}}
console.log(double(100));`,
			expected: "200\n",
		},
	],
};
