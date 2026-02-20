import type { Lesson } from "../../types";

export const classes: Lesson = {
	id: "classes",
	title: "Classes",
	chapterId: "classes",
	content: `## Classes

TypeScript classes are JavaScript classes with type annotations. Declare property types in the class body:

\`\`\`ts
class Animal {
    name: string;
    sound: string;

    constructor(name: string, sound: string) {
        this.name = name;
        this.sound = sound;
    }

    speak(): string {
        return this.name + " says " + this.sound;
    }
}

const dog = new Animal("Dog", "woof");
console.log(dog.speak());  // Dog says woof
\`\`\`

### Access Modifiers

TypeScript adds access modifiers:

- \`public\` — accessible anywhere (default)
- \`private\` — accessible only within the class
- \`readonly\` — can only be assigned once (in the constructor)

\`\`\`ts
class Counter {
    private count: number = 0;

    increment(): void {
        this.count++;
    }

    value(): number {
        return this.count;
    }
}
\`\`\`

### Constructor Shorthand

TypeScript lets you declare and assign in one step:

\`\`\`ts
class Point {
    constructor(public x: number, public y: number) {}

    toString(): string {
        return "(" + this.x + ", " + this.y + ")";
    }
}
\`\`\`

### Your Task

Write a \`class Circle\` with a \`radius: number\` property, a constructor, an \`area()\` method returning \`Math.PI * r * r\` rounded to 2 decimal places, and a \`perimeter()\` method returning \`2 * Math.PI * r\` rounded to 2 decimal places.`,

	starterCode: `class Circle {
\tradius: number;

\tconstructor(radius: number) {
\t\tthis.radius = radius;
\t}

\tarea(): number {
\t\t// Return PI * r^2, rounded to 2 decimals
\t}

\tperimeter(): number {
\t\t// Return 2 * PI * r, rounded to 2 decimals
\t}
}

const c = new Circle(5);
console.log(c.area());
console.log(c.perimeter());
`,

	solution: `class Circle {
\tradius: number;

\tconstructor(radius: number) {
\t\tthis.radius = radius;
\t}

\tarea(): number {
\t\treturn Math.round(Math.PI * this.radius * this.radius * 100) / 100;
\t}

\tperimeter(): number {
\t\treturn Math.round(2 * Math.PI * this.radius * 100) / 100;
\t}
}

const c = new Circle(5);
console.log(c.area());
console.log(c.perimeter());
`,

	tests: [
		{
			name: "Circle(5) area and perimeter",
			expected: "78.54\n31.42\n",
		},
		{
			name: "Circle(1)",
			code: `{{FUNC}}
const c = new Circle(1);
console.log(c.area());
console.log(c.perimeter());`,
			expected: "3.14\n6.28\n",
		},
		{
			name: "Circle(10) area",
			code: `{{FUNC}}
const c = new Circle(10);
console.log(c.area());`,
			expected: "314.16\n",
		},
	],
};
