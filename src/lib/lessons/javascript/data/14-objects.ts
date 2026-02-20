import type { Lesson } from "../../types";

export const objects: Lesson = {
	id: "objects",
	title: "Objects",
	chapterId: "objects",
	content: `## Objects

Objects store key-value pairs. Keys are strings (called **properties**), values can be anything:

\`\`\`js
const person = {
    name: "Alice",
    age: 30,
    active: true,
};
\`\`\`

### Accessing Properties

Use dot notation or bracket notation:

\`\`\`js
console.log(person.name);       // Alice
console.log(person["age"]);     // 30
\`\`\`

### Modifying Objects

\`\`\`js
person.age = 31;          // update
person.email = "a@b.com"; // add new property
\`\`\`

### Methods

Objects can have functions as properties — these are called **methods**:

\`\`\`js
const circle = {
    radius: 5,
    area: function() {
        return Math.PI * this.radius * this.radius;
    },
};
console.log(circle.area().toFixed(2));  // 78.54
\`\`\`

### Your Task

Write \`function fullName(person)\` that returns the person's full name by joining \`person.first\` and \`person.last\` with a space.`,

	starterCode: `function fullName(person) {
\t// Return first + " " + last
}

console.log(fullName({ first: "Ada", last: "Lovelace" }));
console.log(fullName({ first: "Alan", last: "Turing" }));
`,

	solution: `function fullName(person) {
\treturn person.first + " " + person.last;
}

console.log(fullName({ first: "Ada", last: "Lovelace" }));
console.log(fullName({ first: "Alan", last: "Turing" }));
`,

	tests: [
		{
			name: "Ada Lovelace, Alan Turing",
			expected: "Ada Lovelace\nAlan Turing\n",
		},
		{
			name: "single name parts",
			code: `{{FUNC}}
console.log(fullName({ first: "Grace", last: "Hopper" }));`,
			expected: "Grace Hopper\n",
		},
		{
			name: "short names",
			code: `{{FUNC}}
console.log(fullName({ first: "A", last: "B" }));`,
			expected: "A B\n",
		},
		{
			name: "Dennis Ritchie",
			code: `{{FUNC}}
console.log(fullName({ first: "Dennis", last: "Ritchie" }));`,
			expected: "Dennis Ritchie\n",
		},
	],
};
