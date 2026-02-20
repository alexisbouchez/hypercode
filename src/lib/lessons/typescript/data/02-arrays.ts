import type { Lesson } from "../../types";

export const arrays: Lesson = {
	id: "arrays",
	title: "Arrays & Tuples",
	chapterId: "basics",
	content: `## Arrays

Type a JavaScript array with \`Type[]\`:

\`\`\`ts
const scores: number[] = [95, 87, 92, 78];
const tags: string[] = ["typescript", "javascript"];
const flags: boolean[] = [true, false, true];
\`\`\`

TypeScript ensures every element matches the declared type:

\`\`\`ts
const scores: number[] = [1, 2, "three"];  // Error: 'string' is not assignable to 'number'
\`\`\`

An alternative syntax uses \`Array<Type>\`:

\`\`\`ts
const scores: Array<number> = [1, 2, 3];
\`\`\`

### Tuples

A **tuple** is a fixed-length array where each position has a known type:

\`\`\`ts
const point: [number, number] = [3, 4];
const entry: [string, number] = ["Alice", 30];

console.log(point[0]);   // 3
console.log(entry[0]);   // Alice
console.log(entry[1]);   // 30
\`\`\`

Tuples are useful when a function needs to return multiple values of different types.

### Your Task

Create a \`scores\` array of type \`number[]\` containing \`[95, 87, 92, 78, 100]\`. Print the first score, the last score, and the length.`,

	starterCode: `// Create scores: number[] and print first, last, length
`,

	solution: `const scores: number[] = [95, 87, 92, 78, 100];
console.log(scores[0]);
console.log(scores[scores.length - 1]);
console.log(scores.length);
`,

	tests: [
		{
			name: "prints 95, 100, 5",
			expected: "95\n100\n5\n",
		},
	],
};
