import type { Lesson } from "../../types";

export const typeAnnotations: Lesson = {
	id: "type-annotations",
	title: "Type Annotations",
	chapterId: "basics",
	content: `## Type Annotations

TypeScript adds **type annotations** to JavaScript. A type annotation is a \`: type\` after a variable name or parameter that tells TypeScript what kind of value is expected:

\`\`\`ts
let name: string = "Alice";
let age: number = 30;
let active: boolean = true;
\`\`\`

TypeScript catches mistakes at compile time:

\`\`\`ts
let score: number = "high";  // Error: Type 'string' is not assignable to type 'number'
\`\`\`

### Type Inference

You do not always need to write types. TypeScript **infers** types from values:

\`\`\`ts
let name = "Alice";   // inferred as string
let age = 30;         // inferred as number
\`\`\`

Only add annotations when inference is insufficient or unclear.

### The Primitive Types

- \`string\` — text values
- \`number\` — integers and floats (no separate int/float)
- \`boolean\` — \`true\` or \`false\`
- \`null\` — intentional absence of value
- \`undefined\` — uninitialized variable
- \`unknown\` — value of unknown type (safe alternative to \`any\`)
- \`any\` — disables type checking (avoid when possible)

### Your Task

Declare three typed variables: \`language\` (string \`"TypeScript"\`), \`version\` (number \`5\`), and \`typed\` (boolean \`true\`). Print each one.`,

	starterCode: `// Declare language (string), version (number), typed (boolean)
`,

	solution: `const language: string = "TypeScript";
const version: number = 5;
const typed: boolean = true;

console.log(language);
console.log(version);
console.log(typed);
`,

	tests: [
		{
			name: "prints TypeScript, 5, true",
			expected: "TypeScript\n5\ntrue\n",
		},
	],
};
