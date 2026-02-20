import type { Lesson } from "../../types";

export const genericConstraints: Lesson = {
	id: "generic-constraints",
	title: "Generic Constraints",
	chapterId: "generics",
	content: `## Generic Constraints

Sometimes you need a generic that accepts any type — but only types with certain properties. Use \`extends\` to **constrain** the type parameter:

\`\`\`ts
interface HasLength {
    length: number;
}

function longest<T extends HasLength>(a: T, b: T): T {
    return a.length >= b.length ? a : b;
}

console.log(longest("hello", "hi"));         // hello
console.log(longest([1, 2, 3], [1, 2]));    // [1, 2, 3]
\`\`\`

\`T extends HasLength\` means: \`T\` can be any type that has a \`length: number\` property. Both \`string\` and \`Array\` qualify.

### keyof Constraint

\`keyof T\` is the union of a type's property names. It lets you write functions that safely access an object's properties:

\`\`\`ts
function getProperty<T, K extends keyof T>(obj: T, key: K): T[K] {
    return obj[key];
}

const user = { name: "Alice", age: 30 };
console.log(getProperty(user, "name"));  // Alice
console.log(getProperty(user, "age"));   // 30
// getProperty(user, "missing");         // Error: not a key of user
\`\`\`

### Your Task

Write \`function getProperty<T, K extends keyof T>(obj: T, key: K): T[K]\` and use it to access properties of an object.`,

	starterCode: `function getProperty<T, K extends keyof T>(obj: T, key: K): T[K] {
\t// Return obj[key]
}

const car = { make: "Toyota", year: 2023, electric: false };
console.log(getProperty(car, "make"));
console.log(getProperty(car, "year"));
console.log(getProperty(car, "electric"));
`,

	solution: `function getProperty<T, K extends keyof T>(obj: T, key: K): T[K] {
\treturn obj[key];
}

const car = { make: "Toyota", year: 2023, electric: false };
console.log(getProperty(car, "make"));
console.log(getProperty(car, "year"));
console.log(getProperty(car, "electric"));
`,

	tests: [
		{
			name: "gets make, year, electric from car",
			expected: "Toyota\n2023\nfalse\n",
		},
		{
			name: "gets string property",
			code: `{{FUNC}}
const p = { x: 10, y: 20, label: "origin" };
console.log(getProperty(p, "label"));`,
			expected: "origin\n",
		},
		{
			name: "gets numeric property",
			code: `{{FUNC}}
const p = { x: 10, y: 20, label: "origin" };
console.log(getProperty(p, "x"));`,
			expected: "10\n",
		},
	],
};
