import type { Lesson } from "../../types";

export const enums: Lesson = {
	id: "enums",
	title: "Enums",
	chapterId: "classes",
	content: `## Enums

An **enum** defines a set of named constants. Use it when a variable should only hold one of a fixed set of values:

\`\`\`ts
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

let dir: Direction = Direction.Up;
console.log(dir);          // 0 (numeric by default)
console.log(Direction.Up); // 0
\`\`\`

By default, enum members are numbered starting from 0.

### String Enums

String enums are more readable and explicit:

\`\`\`ts
enum Status {
    Active = "active",
    Inactive = "inactive",
    Pending = "pending",
}

function getStatus(s: Status): string {
    return "Status: " + s;
}

console.log(getStatus(Status.Active));   // Status: active
console.log(getStatus(Status.Pending));  // Status: pending
\`\`\`

String enums are safer than numeric enums because the values are explicit and readable.

### Your Task

Define \`enum Color\` with values \`Red = "red"\`, \`Green = "green"\`, \`Blue = "blue"\`. Write \`function paintColor(c: Color): string\` that returns \`"Painting in " + c\`.`,

	starterCode: `enum Color {
\tRed = "red",
\tGreen = "green",
\tBlue = "blue",
}

function paintColor(c: Color): string {
\t// Return "Painting in " + c
}

console.log(paintColor(Color.Red));
console.log(paintColor(Color.Blue));
`,

	solution: `enum Color {
\tRed = "red",
\tGreen = "green",
\tBlue = "blue",
}

function paintColor(c: Color): string {
\treturn "Painting in " + c;
}

console.log(paintColor(Color.Red));
console.log(paintColor(Color.Blue));
`,

	tests: [
		{
			name: "paints red and blue",
			expected: "Painting in red\nPainting in blue\n",
		},
		{
			name: "paints green",
			code: `{{FUNC}}
console.log(paintColor(Color.Green));`,
			expected: "Painting in green\n",
		},
		{
			name: "paints all colors",
			code: `{{FUNC}}
console.log(paintColor(Color.Red));
console.log(paintColor(Color.Green));
console.log(paintColor(Color.Blue));`,
			expected: "Painting in red\nPainting in green\nPainting in blue\n",
		},
	],
};
