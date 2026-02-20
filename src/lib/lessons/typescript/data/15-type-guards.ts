import type { Lesson } from "../../types";

export const typeGuards: Lesson = {
	id: "type-guards",
	title: "Type Guards",
	chapterId: "advanced",
	content: `## Type Guards

A **type guard** is a function that returns a boolean and narrows the type of a value inside conditional blocks. Use the \`is\` keyword in the return type:

\`\`\`ts
function isString(val: unknown): val is string {
    return typeof val === "string";
}

function print(val: unknown): void {
    if (isString(val)) {
        console.log("String: " + val.toUpperCase());
    } else {
        console.log("Not a string");
    }
}

print("hello");  // String: HELLO
print(42);       // Not a string
\`\`\`

### instanceof Guards

Use \`instanceof\` to narrow class types:

\`\`\`ts
class Dog {
    bark(): string { return "woof"; }
}

class Cat {
    meow(): string { return "meow"; }
}

function speak(animal: Dog | Cat): string {
    if (animal instanceof Dog) {
        return animal.bark();
    }
    return animal.meow();
}
\`\`\`

### Your Task

Write \`function isNumber(val: unknown): val is number\` that checks if a value is a number. Then write \`function double(val: unknown): string\` that returns \`"Double: " + (val * 2)\` if \`val\` is a number, or \`"Not a number"\` otherwise.`,

	starterCode: `function isNumber(val: unknown): val is number {
\t// Return true if val is a number
}

function double(val: unknown): string {
\t// If val is a number, return "Double: " + (val * 2)
\t// Otherwise return "Not a number"
}

console.log(double(5));
console.log(double("hello"));
console.log(double(3.5));
`,

	solution: `function isNumber(val: unknown): val is number {
\treturn typeof val === "number";
}

function double(val: unknown): string {
\tif (isNumber(val)) {
\t\treturn "Double: " + (val * 2);
\t}
\treturn "Not a number";
}

console.log(double(5));
console.log(double("hello"));
console.log(double(3.5));
`,

	tests: [
		{
			name: "double number, string, float",
			expected: "Double: 10\nNot a number\nDouble: 7\n",
		},
		{
			name: "double zero",
			code: `{{FUNC}}
console.log(double(0));`,
			expected: "Double: 0\n",
		},
		{
			name: "double boolean and number",
			code: `{{FUNC}}
console.log(double(true));
console.log(double(100));`,
			expected: "Not a number\nDouble: 200\n",
		},
	],
};
