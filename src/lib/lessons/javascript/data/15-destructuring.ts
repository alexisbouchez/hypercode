import type { Lesson } from "../../types";

export const destructuring: Lesson = {
	id: "destructuring",
	title: "Destructuring",
	chapterId: "objects",
	content: `## Destructuring

Destructuring extracts values from arrays or objects into named variables in a single statement.

### Array Destructuring

\`\`\`js
const [first, second, third] = [10, 20, 30];
console.log(first);   // 10
console.log(second);  // 20
\`\`\`

Skip elements with commas:

\`\`\`js
const [, , last] = [1, 2, 3];
console.log(last);  // 3
\`\`\`

### Object Destructuring

\`\`\`js
const person = { name: "Alice", age: 30, city: "Paris" };
const { name, age } = person;
console.log(name);  // Alice
console.log(age);   // 30
\`\`\`

### Default Values

\`\`\`js
const { x = 0, y = 0 } = { x: 5 };
console.log(x);  // 5
console.log(y);  // 0  (default)
\`\`\`

### Your Task

Write \`function coordinates(point)\` that destructures \`point.x\` and \`point.y\` from the object argument and returns the string \`"(x, y)"\`.`,

	starterCode: `function coordinates(point) {
\t// Destructure x and y, return "(x, y)"
}

console.log(coordinates({ x: 3, y: 4 }));
console.log(coordinates({ x: -1, y: 2 }));
`,

	solution: `function coordinates(point) {
\tconst { x, y } = point;
\treturn "(" + x + ", " + y + ")";
}

console.log(coordinates({ x: 3, y: 4 }));
console.log(coordinates({ x: -1, y: 2 }));
`,

	tests: [
		{
			name: "(3, 4) and (-1, 2)",
			expected: "(3, 4)\n(-1, 2)\n",
		},
		{
			name: "origin (0, 0)",
			code: `{{FUNC}}
console.log(coordinates({ x: 0, y: 0 }));`,
			expected: "(0, 0)\n",
		},
		{
			name: "negative coordinates",
			code: `{{FUNC}}
console.log(coordinates({ x: -5, y: -10 }));`,
			expected: "(-5, -10)\n",
		},
		{
			name: "large coordinates",
			code: `{{FUNC}}
console.log(coordinates({ x: 100, y: 200 }));`,
			expected: "(100, 200)\n",
		},
	],
};
