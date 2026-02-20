import type { Lesson } from "../../types";

export const typeNarrowing: Lesson = {
	id: "type-narrowing",
	title: "Type Narrowing",
	chapterId: "type-system",
	content: `## Type Narrowing

When you have a union type, TypeScript does not know which specific type you have. **Narrowing** uses runtime checks to determine the actual type, letting TypeScript then give you the correct methods:

\`\`\`ts
function process(val: string | number): string {
    if (typeof val === "string") {
        // TypeScript knows val is string here
        return val.toUpperCase();
    } else {
        // TypeScript knows val is number here
        return val.toFixed(2);
    }
}
\`\`\`

### typeof Narrowing

\`typeof\` narrows primitive types:

\`\`\`ts
typeof val === "string"   // narrows to string
typeof val === "number"   // narrows to number
typeof val === "boolean"  // narrows to boolean
\`\`\`

### in Narrowing

The \`in\` operator checks if an object has a property:

\`\`\`ts
interface Cat { meow(): void; }
interface Dog { bark(): void; }

function makeSound(animal: Cat | Dog): void {
    if ("meow" in animal) {
        animal.meow();  // TypeScript knows it's a Cat
    } else {
        animal.bark();  // TypeScript knows it's a Dog
    }
}
\`\`\`

### Your Task

Write \`function format(val: string | number): string\` that:
- If \`val\` is a string: returns it in uppercase
- If \`val\` is a number: returns it with 2 decimal places (use \`toFixed(2)\`)`,

	starterCode: `function format(val: string | number): string {
\t// Uppercase strings, toFixed(2) for numbers
}

console.log(format("hello"));
console.log(format(3.14159));
console.log(format(42));
`,

	solution: `function format(val: string | number): string {
\tif (typeof val === "string") {
\t\treturn val.toUpperCase();
\t}
\treturn val.toFixed(2);
}

console.log(format("hello"));
console.log(format(3.14159));
console.log(format(42));
`,

	tests: [
		{
			name: "uppercases string, fixes number decimals",
			expected: "HELLO\n3.14\n42.00\n",
		},
		{
			name: "format short string",
			code: `{{FUNC}}
console.log(format("ts"));`,
			expected: "TS\n",
		},
		{
			name: "format integer",
			code: `{{FUNC}}
console.log(format(100));`,
			expected: "100.00\n",
		},
		{
			name: "format float",
			code: `{{FUNC}}
console.log(format(1.5));`,
			expected: "1.50\n",
		},
	],
};
