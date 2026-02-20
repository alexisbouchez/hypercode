import type { Lesson } from "../../types";

export const optionalProperties: Lesson = {
	id: "optional-properties",
	title: "Optional Properties",
	chapterId: "interfaces",
	content: `## Optional Properties

Mark a property optional with \`?\`. It can be omitted when creating the object:

\`\`\`ts
interface User {
    name: string;
    email: string;
    age?: number;      // optional
    admin?: boolean;   // optional
}

const u1: User = { name: "Alice", email: "alice@example.com" };
const u2: User = { name: "Bob", email: "bob@example.com", age: 30 };
\`\`\`

Inside the function, optional properties have type \`T | undefined\`. You must check before using:

\`\`\`ts
function greetUser(u: User): string {
    const suffix = u.age !== undefined ? " (age " + u.age + ")" : "";
    return "Hello, " + u.name + suffix;
}
\`\`\`

### Optional Parameters

Function parameters can also be optional:

\`\`\`ts
function repeat(s: string, times?: number): string {
    const n = times !== undefined ? times : 1;
    return s.repeat(n);
}

console.log(repeat("ha"));      // ha
console.log(repeat("ha", 3));   // hahaha
\`\`\`

### Your Task

Write \`function formatName(first: string, last?: string): string\` that returns:
- \`first + " " + last\` if \`last\` is provided
- Just \`first\` if \`last\` is omitted`,

	starterCode: `function formatName(first: string, last?: string): string {
\t// Return full name if last is provided, just first otherwise
}

console.log(formatName("Alice", "Smith"));
console.log(formatName("Bob"));
`,

	solution: `function formatName(first: string, last?: string): string {
\tif (last !== undefined) {
\t\treturn first + " " + last;
\t}
\treturn first;
}

console.log(formatName("Alice", "Smith"));
console.log(formatName("Bob"));
`,

	tests: [
		{
			name: "with last name and without",
			expected: "Alice Smith\nBob\n",
		},
		{
			name: "first and last provided",
			code: `{{FUNC}}
console.log(formatName("Grace", "Hopper"));`,
			expected: "Grace Hopper\n",
		},
		{
			name: "first name only",
			code: `{{FUNC}}
console.log(formatName("Cher"));`,
			expected: "Cher\n",
		},
		{
			name: "another full name",
			code: `{{FUNC}}
console.log(formatName("Alan", "Turing"));`,
			expected: "Alan Turing\n",
		},
	],
};
