import type { Lesson } from "../../types";

export const variables: Lesson = {
	id: "variables",
	title: "Variables",
	chapterId: "basics",
	content: `## Variables

JavaScript has two modern ways to declare variables: \`let\` and \`const\`.

### let

\`let\` declares a variable you can reassign:

\`\`\`js
let score = 0;
score = 10;    // OK — reassigning
score = score + 5;  // OK
console.log(score);  // 15
\`\`\`

### const

\`const\` declares a constant that cannot be reassigned:

\`\`\`js
const pi = 3.14159;
const name = "Alice";
// pi = 3;  // Error: Assignment to constant variable
\`\`\`

Use \`const\` by default. Use \`let\` only when you need to reassign.

### Types

Variables in JavaScript can hold any type:

\`\`\`js
let count = 42;          // number
let price = 9.99;        // number (no separate int/float)
let message = "hello";   // string
let active = true;       // boolean
let empty = null;        // null — intentional absence of value
let missing;             // undefined — no value assigned yet
\`\`\`

### Your Task

Declare three variables: \`city\` (string \`"Paris"\`), \`population\` (number \`2161000\`), and \`capital\` (boolean \`true\`). Print each one.`,

	starterCode: `// Declare and print city, population, capital
`,

	solution: `const city = "Paris";
const population = 2161000;
const capital = true;

console.log(city);
console.log(population);
console.log(capital);
`,

	tests: [
		{
			name: "prints city, population, capital",
			expected: "Paris\n2161000\ntrue\n",
		},
	],
};
