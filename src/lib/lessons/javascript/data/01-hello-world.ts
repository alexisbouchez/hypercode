import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
	id: "hello-world",
	title: "Hello, World!",
	chapterId: "basics",
	content: `## Your First JavaScript Program

JavaScript's built-in function for printing output is \`console.log()\`:

\`\`\`js
console.log("Hello, World!");
\`\`\`

This prints the string to the console, followed by a newline. You can log any value — strings, numbers, booleans:

\`\`\`js
console.log("hello");   // hello
console.log(42);        // 42
console.log(true);      // true
\`\`\`

### Comments

Single-line comments start with \`//\`. Everything after \`//\` on the same line is ignored:

\`\`\`js
// This is a comment
console.log("code");  // This is also a comment
\`\`\`

Multi-line comments use \`/* ... */\`:

\`\`\`js
/* This spans
   multiple lines */
console.log("done");
\`\`\`

### Your Task

Print exactly \`Hello, World!\` using \`console.log\`.`,

	starterCode: `// Print "Hello, World!"
`,

	solution: `console.log("Hello, World!");
`,

	tests: [
		{
			name: "prints Hello, World!",
			expected: "Hello, World!\n",
		},
	],
};
