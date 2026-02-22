import type { Lesson } from "../../types";

export const classes: Lesson = {
	id: "classes",
	title: "Classes",
	chapterId: "oop",
	content: `## Classes

A class is a blueprint for objects. It bundles data (fields) and behavior (methods):

\`\`\`csharp
class Dog {
    public string Name;
    public int Age;

    public Dog(string name, int age) {
        Name = name;
        Age = age;
    }

    public string Describe() => $"{Name} is {Age} years old.";
}

var dog = new Dog("Rex", 3);
Console.WriteLine(dog.Describe()); // Rex is 3 years old.
Console.WriteLine(dog.Name);       // Rex
\`\`\`

### Constructor

The constructor runs when you create an object with \`new\`. It initializes the object's state.

### Access Modifiers

- \`public\` — accessible from anywhere
- \`private\` — only accessible within the class (default)
- \`protected\` — accessible within the class and subclasses

### Your Task

Create a \`Rectangle\` class with:
- \`double Width\` and \`double Height\` fields
- A constructor taking \`width\` and \`height\`
- A \`double Area()\` method that returns width × height
- A \`double Perimeter()\` method that returns 2 × (width + height)`,

	starterCode: `class Rectangle
{
    // Add Width, Height fields, constructor, Area() and Perimeter() methods
}
`,

	solution: `class Rectangle
{
    public double Width;
    public double Height;

    public Rectangle(double width, double height) {
        Width = width;
        Height = height;
    }

    public double Area() => Width * Height;
    public double Perimeter() => 2 * (Width + Height);
}
`,

	tests: [
		{
			name: "area of 4x5 rectangle",
			expected: "20\n",
			code: `var r = new Rectangle(4, 5);
Console.WriteLine(r.Area());
{{FUNC}}`,
		},
		{
			name: "perimeter of 3x7 rectangle",
			expected: "20\n",
			code: `var r = new Rectangle(3, 7);
Console.WriteLine(r.Perimeter());
{{FUNC}}`,
		},
	],
};
