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

### Your Task

Write \`function clamp(n: number, min: number, max: number): number\` that returns \`n\` if it is between \`min\` and \`max\`, \`min\` if it is below, and \`max\` if it is above.`,

	starterCode: `function clamp(n: number, min: number, max: number): number {
\t// Return n clamped to [min, max]
}

console.log(clamp(5, 0, 10));
console.log(clamp(-3, 0, 10));
console.log(clamp(15, 0, 10));
`,

	solution: `function clamp(n: number, min: number, max: number): number {
\tif (n < min) return min;
\tif (n > max) return max;
\treturn n;
}

console.log(clamp(5, 0, 10));
console.log(clamp(-3, 0, 10));
console.log(clamp(15, 0, 10));
`,

	tests: [
		{
			name: "clamp(5,0,10)=5, clamp(-3,0,10)=0, clamp(15,0,10)=10",
			expected: "5\n0\n10\n",
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
	],
};
