import type { Lesson } from "../../types";

export const generics: Lesson = {
	id: "generics",
	title: "Generics",
	chapterId: "generics",
	content: `## Generics

A **generic** function works with any type, while still being type-safe. Use \`<T>\` to declare a **type parameter**:

\`\`\`ts
function identity<T>(val: T): T {
    return val;
}

console.log(identity("hello"));  // string
console.log(identity(42));       // number
console.log(identity(true));     // boolean
\`\`\`

TypeScript infers \`T\` from the argument — no need to specify it manually.

### Generic Arrays

\`\`\`ts
function first<T>(arr: T[]): T | undefined {
    return arr[0];
}

console.log(first([1, 2, 3]));        // 1
console.log(first(["a", "b", "c"]));  // a
console.log(first([]));               // undefined
\`\`\`

### Multiple Type Parameters

\`\`\`ts
function pair<A, B>(a: A, b: B): [A, B] {
    return [a, b];
}

const p = pair("Alice", 30);   // [string, number]
\`\`\`

### Why Generics?

Without generics, you would use \`any\`, which loses all type information. Generics preserve the relationship between input and output types.

### Your Task

Write \`function last<T>(arr: T[]): T | undefined\` that returns the last element of an array, or \`undefined\` if the array is empty.`,

	starterCode: `function last<T>(arr: T[]): T | undefined {
\t// Return the last element, or undefined if empty
}

console.log(last([1, 2, 3]));
console.log(last(["a", "b", "c"]));
console.log(last([]));
`,

	solution: `function last<T>(arr: T[]): T | undefined {
\tif (arr.length === 0) return undefined;
\treturn arr[arr.length - 1];
}

console.log(last([1, 2, 3]));
console.log(last(["a", "b", "c"]));
console.log(last([]));
`,

	tests: [
		{
			name: "last of numbers, strings, empty",
			expected: "3\nc\nundefined\n",
		},
		{
			name: "last of single element",
			code: `{{FUNC}}
console.log(last([42]));`,
			expected: "42\n",
		},
		{
			name: "last of booleans",
			code: `{{FUNC}}
console.log(last([true, false, true]));`,
			expected: "true\n",
		},
		{
			name: "last of strings",
			code: `{{FUNC}}
console.log(last(["x", "y", "z"]));`,
			expected: "z\n",
		},
	],
};
