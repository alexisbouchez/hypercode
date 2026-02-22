import type { Lesson } from "../../types";

export const inheritance: Lesson = {
	id: "inheritance",
	title: "Inheritance",
	chapterId: "oop",
	content: `## Inheritance

Inheritance lets a class extend another, reusing and overriding behavior:

\`\`\`csharp
class Animal {
    public string Name { get; set; }
    public Animal(string name) { Name = name; }
    public virtual string Sound() => "...";  // virtual = overridable
    public string Describe() => $"{Name} says {Sound()}";
}

class Dog : Animal {
    public Dog(string name) : base(name) {}        // call parent constructor
    public override string Sound() => "Woof";      // override parent method
}

class Cat : Animal {
    public Cat(string name) : base(name) {}
    public override string Sound() => "Meow";
}

var animals = new Animal[] { new Dog("Rex"), new Cat("Luna") };
foreach (var a in animals) {
    Console.WriteLine(a.Describe());
}
// Rex says Woof
// Luna says Meow
\`\`\`

### sealed and abstract

- \`abstract\` — must be overridden; the class cannot be instantiated
- \`sealed\` — cannot be further inherited

### Your Task

Create:
- A base class \`Shape\` with a \`virtual double Area()\` method returning \`0\`
- A \`Square\` subclass with a \`Side\` property that overrides \`Area()\` to return side²
- A \`Circle\` subclass with a \`Radius\` property that overrides \`Area()\` to return π × radius²`,

	starterCode: `class Shape
{
    public virtual double Area() => 0;
}

class Square : Shape
{
    // Add Side property and override Area()
}

class Circle : Shape
{
    // Add Radius property and override Area()
}
`,

	solution: `class Shape
{
    public virtual double Area() => 0;
}

class Square : Shape
{
    public double Side { get; set; }
    public Square(double side) { Side = side; }
    public override double Area() => Side * Side;
}

class Circle : Shape
{
    public double Radius { get; set; }
    public Circle(double radius) { Radius = radius; }
    public override double Area() => Math.PI * Radius * Radius;
}
`,

	tests: [
		{
			name: "square area",
			expected: "25\n",
			code: `var s = new Square(5);
Console.WriteLine(s.Area());
{{FUNC}}`,
		},
		{
			name: "circle area rounded",
			expected: "78.54\n",
			code: `var c = new Circle(5);
Console.WriteLine(Math.Round(c.Area(), 2));
{{FUNC}}`,
		},
	],
};
