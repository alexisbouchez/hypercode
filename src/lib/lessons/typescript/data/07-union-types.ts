import type { Lesson } from "../../types";

export const unionTypes: Lesson = {
	id: "union-types",
	title: "Union Types",
	chapterId: "type-system",
	content: `## Union Types

A **union type** means a value can be one of several types, written with \`|\`:

\`\`\`ts
let input: string | number;
input = "hello";  // OK
input = 42;       // OK
input = true;     // Error: 'boolean' not in the union
\`\`\`

### Unions in Functions

\`\`\`ts
function stringify(val: string | number | boolean): string {
    return String(val);
}

console.log(stringify("hello"));  // hello
console.log(stringify(42));       // 42
console.log(stringify(true));     // true
\`\`\`

### Unions with null and undefined

A common union is \`T | null\` for values that might be absent:

\`\`\`ts
function find(id: number): string | null {
    if (id === 1) return "Alice";
    return null;
}

const name = find(1);   // string | null
const missing = find(2); // string | null
\`\`\`

### Your Task

Write \`function describe(val: string | number): string\` that returns:
- \`"string of length N"\` if \`val\` is a string (where N is the length)
- \`"number: N"\` if \`val\` is a number`,

	starterCode: `function describe(val: string | number): string {
\t// Return description based on type
}

console.log(describe("hello"));
console.log(describe(42));
console.log(describe("TypeScript"));
`,

	solution: `function describe(val: string | number): string {
\tif (typeof val === "string") {
\t\treturn "string of length " + val.length;
\t}
\treturn "number: " + val;
}

console.log(describe("hello"));
console.log(describe(42));
console.log(describe("TypeScript"));
`,

	tests: [
		{
			name: "describe hello, 42, TypeScript",
			expected: "string of length 5\nnumber: 42\nstring of length 10\n",
		},
		{
			name: "describe empty string",
			code: `{{FUNC}}
console.log(describe(""));`,
			expected: "string of length 0\n",
		},
		{
			name: "describe zero",
			code: `{{FUNC}}
console.log(describe(0));`,
			expected: "number: 0\n",
		},
		{
			name: "describe negative number",
			code: `{{FUNC}}
console.log(describe(-5));`,
			expected: "number: -5\n",
		},
	],
};
