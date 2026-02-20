import type { Lesson } from "../../types";

export const typeAliases: Lesson = {
	id: "type-aliases",
	title: "Type Aliases",
	chapterId: "interfaces",
	content: `## Type Aliases

The \`type\` keyword creates an alias for any type — primitives, unions, objects, functions:

\`\`\`ts
type Name = string;
type Age = number;
type ID = string | number;   // union type
\`\`\`

### Object Type Aliases

\`type\` can define object shapes just like \`interface\`:

\`\`\`ts
type Point = {
    x: number;
    y: number;
};
\`\`\`

### When to Use type vs interface

Both \`type\` and \`interface\` can define object shapes. The key difference:
- \`interface\` can be extended with \`extends\` and merged via declaration merging
- \`type\` can represent unions, intersections, and primitives that \`interface\` cannot

In practice: use \`interface\` for object shapes, \`type\` for everything else.

### Union Types

A union type accepts multiple possible types:

\`\`\`ts
type StringOrNumber = string | number;

function format(val: StringOrNumber): string {
    return "Value: " + val;
}

console.log(format("hello"));  // Value: hello
console.log(format(42));       // Value: 42
\`\`\`

### Your Task

Define \`type ID = string | number\`. Write \`function formatID(id: ID): string\` that returns \`"ID:" + id\`.`,

	starterCode: `type ID = string | number;

function formatID(id: ID): string {
\t// Return "ID:" + id
}

console.log(formatID(42));
console.log(formatID("abc-123"));
`,

	solution: `type ID = string | number;

function formatID(id: ID): string {
\treturn "ID:" + id;
}

console.log(formatID(42));
console.log(formatID("abc-123"));
`,

	tests: [
		{
			name: "formats number and string IDs",
			expected: "ID:42\nID:abc-123\n",
		},
		{
			name: "numeric ID",
			code: `{{FUNC}}
console.log(formatID(1));`,
			expected: "ID:1\n",
		},
		{
			name: "string ID with dashes",
			code: `{{FUNC}}
console.log(formatID("user-99"));`,
			expected: "ID:user-99\n",
		},
	],
};
