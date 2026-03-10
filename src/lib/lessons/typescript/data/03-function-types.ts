import type { Lesson } from "../../types";

export const functionTypes: Lesson = {
	id: "function-types",
	title: "Function Types",
	chapterId: "basics",
	content: `## Function Types

TypeScript lets you annotate function parameters and return types:

\`\`\`ts
function add(a: number, b: number): number {
    return a + b;
}
\`\`\`

- \`a: number\` — parameter \`a\` must be a number
- \`b: number\` — parameter \`b\` must be a number
- \`: number\` after the parentheses — the function returns a number

TypeScript verifies the return statement matches the declared return type:

\`\`\`ts
function greet(name: string): string {
    return 42;  // Error: 'number' is not assignable to type 'string'
}
\`\`\`

### void

Functions that do not return a value have return type \`void\`:

\`\`\`ts
function log(message: string): void {
    console.log(message);
}
\`\`\`

### Function Types as Values

Arrow functions can also be typed:

\`\`\`ts
const multiply = (a: number, b: number): number => a * b;
\`\`\`

### Optional Parameters

Mark a parameter with \`?\` to make it optional. Optional parameters must come after required ones:

\`\`\`ts
function greet(name: string, greeting?: string): string {
    return (greeting || "Hello") + ", " + name;
}
greet("Alice");            // "Hello, Alice"
greet("Alice", "Howdy");   // "Howdy, Alice"
\`\`\`

Inside the function, an optional parameter has type \`T | undefined\`.

### Rest Parameters

Use \`...args: T[]\` to accept any number of arguments of the same type:

\`\`\`ts
function sum(...nums: number[]): number {
    return nums.reduce((a, b) => a + b, 0);
}
sum(1, 2, 3);    // 6
sum(10, 20);     // 30
\`\`\`

Rest parameters must be the last parameter and collect all remaining arguments into an array.

### Your Task

1. Write \`function clamp(n: number, min: number, max: number): number\` that returns \`n\` if it is between \`min\` and \`max\`, \`min\` if it is below, and \`max\` if it is above.
2. Write \`function joinWords(separator: string, ...words: string[]): string\` that joins all words using the separator.`,

	starterCode: `function clamp(n: number, min: number, max: number): number {
\t// Return n clamped to [min, max]
}

function joinWords(separator: string, ...words: string[]): string {
\t// Join all words using the separator
}

console.log(clamp(5, 0, 10));
console.log(clamp(-3, 0, 10));
console.log(clamp(15, 0, 10));
console.log(joinWords("-", "alpha", "beta", "gamma"));
console.log(joinWords(", ", "one", "two"));
`,

	solution: `function clamp(n: number, min: number, max: number): number {
\tif (n < min) return min;
\tif (n > max) return max;
\treturn n;
}

function joinWords(separator: string, ...words: string[]): string {
\treturn words.join(separator);
}

console.log(clamp(5, 0, 10));
console.log(clamp(-3, 0, 10));
console.log(clamp(15, 0, 10));
console.log(joinWords("-", "alpha", "beta", "gamma"));
console.log(joinWords(", ", "one", "two"));
`,

	tests: [
		{
			name: "clamp and joinWords default output",
			expected: "5\n0\n10\nalpha-beta-gamma\none, two\n",
		},
		{
			name: "clamp at exact boundaries",
			code: `{{FUNC}}
console.log(clamp(0, 0, 10));
console.log(clamp(10, 0, 10));`,
			expected: "0\n10\n",
		},
		{
			name: "clamp with negative range",
			code: `{{FUNC}}
console.log(clamp(-5, -10, -1));`,
			expected: "-5\n",
		},
		{
			name: "clamp below range",
			code: `{{FUNC}}
console.log(clamp(1, 5, 100));`,
			expected: "5\n",
		},
		{
			name: "joinWords with single word (rest gets one element)",
			code: `{{FUNC}}
console.log(joinWords(":", "solo"));`,
			expected: "solo\n",
		},
		{
			name: "joinWords with no words (rest gets empty array)",
			code: `{{FUNC}}
console.log(joinWords(","));`,
			expected: "\n",
		},
	],
};
