import type { Lesson } from "../../types";

export const strings: Lesson = {
	id: "strings",
	title: "Strings",
	chapterId: "basics",
	content: `## Strings

Strings hold text. You can write them with double quotes, single quotes, or backticks:

\`\`\`js
let a = "hello";
let b = 'world';
let c = \`hello world\`;  // template literal
\`\`\`

### Concatenation

Use \`+\` to join strings together:

\`\`\`js
let first = "Hello";
let second = "World";
console.log(first + ", " + second + "!");  // Hello, World!
\`\`\`

### Template Literals

Backtick strings let you embed expressions with \`\${...}\`:

\`\`\`js
let name = "Alice";
let age = 30;
console.log(\`\${name} is \${age} years old\`);  // Alice is 30 years old
\`\`\`

### Useful Methods

\`\`\`js
let s = "Hello, World!";
console.log(s.length);           // 13
console.log(s.toUpperCase());    // HELLO, WORLD!
console.log(s.toLowerCase());    // hello, world!
console.log(s.includes("World")); // true
console.log(s.slice(7, 12));      // World
\`\`\`

### Your Task

Declare a \`greeting\` variable with value \`"Hello, JavaScript!"\`. Print the greeting, its length, and its uppercase version.`,

	starterCode: `// Declare greeting and print it, its length, and its uppercase
`,

	solution: `const greeting = "Hello, JavaScript!";
console.log(greeting);
console.log(greeting.length);
console.log(greeting.toUpperCase());
`,

	tests: [
		{
			name: "prints greeting, length, uppercase",
			expected: "Hello, JavaScript!\n18\nHELLO, JAVASCRIPT!\n",
		},
	],
};
