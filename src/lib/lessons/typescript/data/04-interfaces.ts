import type { Lesson } from "../../types";

export const interfaces: Lesson = {
	id: "interfaces",
	title: "Interfaces",
	chapterId: "interfaces",
	content: `## Interfaces

An **interface** defines the shape of an object — what properties it must have and their types:

\`\`\`ts
interface Point {
    x: number;
    y: number;
}

const origin: Point = { x: 0, y: 0 };
const p: Point = { x: 3, y: 4 };
\`\`\`

TypeScript checks that the object has all required properties with the correct types:

\`\`\`ts
const bad: Point = { x: 1 };         // Error: missing 'y'
const bad2: Point = { x: "1", y: 2 }; // Error: 'string' not assignable to 'number'
\`\`\`

### Using Interfaces as Parameters

Interfaces shine as function parameter types:

\`\`\`ts
function distance(p: Point): number {
    return Math.sqrt(p.x * p.x + p.y * p.y);
}

console.log(distance({ x: 3, y: 4 }));  // 5
\`\`\`

### Nested Interfaces

\`\`\`ts
interface Rectangle {
    topLeft: Point;
    bottomRight: Point;
}
\`\`\`

### Your Task

Define an \`interface Person\` with \`name: string\` and \`age: number\`. Write \`function introduce(p: Person): string\` that returns \`"Hi, I'm [name] and I'm [age] years old"\`.`,

	starterCode: `interface Person {
\t// name: string
\t// age: number
}

function introduce(p: Person): string {
\t// Return the introduction string
}

console.log(introduce({ name: "Alice", age: 30 }));
console.log(introduce({ name: "Bob", age: 25 }));
`,

	solution: `interface Person {
\tname: string;
\tage: number;
}

function introduce(p: Person): string {
\treturn "Hi, I'm " + p.name + " and I'm " + p.age + " years old";
}

console.log(introduce({ name: "Alice", age: 30 }));
console.log(introduce({ name: "Bob", age: 25 }));
`,

	tests: [
		{
			name: "introduces Alice and Bob",
			expected: "Hi, I'm Alice and I'm 30 years old\nHi, I'm Bob and I'm 25 years old\n",
		},
		{
			name: "introduces another person",
			code: `{{FUNC}}
console.log(introduce({ name: "Carol", age: 20 }));`,
			expected: "Hi, I'm Carol and I'm 20 years old\n",
		},
		{
			name: "introduces with different age",
			code: `{{FUNC}}
console.log(introduce({ name: "Dave", age: 65 }));`,
			expected: "Hi, I'm Dave and I'm 65 years old\n",
		},
	],
};
