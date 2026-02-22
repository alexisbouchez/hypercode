import type { Lesson } from "../../types";

export const properties: Lesson = {
	id: "properties",
	title: "Properties",
	chapterId: "oop",
	content: `## Properties

Properties are the idiomatic C# way to expose data with optional validation:

\`\`\`csharp
class Person {
    private int _age;

    public string Name { get; set; }  // auto-property

    public int Age {
        get => _age;
        set {
            if (value < 0) throw new ArgumentException("Age cannot be negative");
            _age = value;
        }
    }

    public Person(string name, int age) {
        Name = name;
        Age = age;
    }
}
\`\`\`

### Computed Properties

A property with only a \`get\` computes its value on the fly:

\`\`\`csharp
class Circle {
    public double Radius { get; set; }
    public double Area => Math.PI * Radius * Radius;      // computed
    public double Diameter => 2 * Radius;                 // computed
}
\`\`\`

### Init-Only Properties (C# 9+)

\`\`\`csharp
class Point {
    public int X { get; init; }  // set only in constructor or init
    public int Y { get; init; }
}
var p = new Point { X = 3, Y = 4 };
\`\`\`

### Your Task

Create a \`Temperature\` class with:
- \`double Celsius { get; set; }\` property
- A computed \`double Fahrenheit\` property that returns \`Celsius * 9.0 / 5.0 + 32\`
- A constructor taking a \`celsius\` value`,

	starterCode: `class Temperature
{
    // Add Celsius property, Fahrenheit computed property, and constructor
}
`,

	solution: `class Temperature
{
    public double Celsius { get; set; }
    public double Fahrenheit => Celsius * 9.0 / 5.0 + 32;

    public Temperature(double celsius) {
        Celsius = celsius;
    }
}
`,

	tests: [
		{
			name: "0°C is 32°F",
			expected: "32\n",
			code: `var t = new Temperature(0);
Console.WriteLine(t.Fahrenheit);
{{FUNC}}`,
		},
		{
			name: "100°C is 212°F",
			expected: "212\n",
			code: `var t = new Temperature(100);
Console.WriteLine(t.Fahrenheit);
{{FUNC}}`,
		},
	],
};
