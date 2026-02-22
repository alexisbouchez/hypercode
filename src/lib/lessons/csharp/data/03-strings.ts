import type { Lesson } from "../../types";

export const strings: Lesson = {
	id: "strings",
	title: "String Interpolation",
	chapterId: "basics",
	content: `## Working with Strings

C# strings support **interpolation** with the \`$\` prefix — embed expressions directly inside curly braces:

\`\`\`csharp
string name = "Alice";
int age = 30;
Console.WriteLine($"My name is {name} and I am {age} years old.");
// Output: My name is Alice and I am 30 years old.
\`\`\`

You can also perform operations inside the braces:

\`\`\`csharp
double price = 9.99;
int qty = 3;
Console.WriteLine($"Total: {price * qty:F2}");
// Output: Total: 29.97
\`\`\`

### Common String Operations

\`\`\`csharp
string s = "Hello, World!";
Console.WriteLine(s.Length);          // 13
Console.WriteLine(s.ToUpper());       // HELLO, WORLD!
Console.WriteLine(s.ToLower());       // hello, world!
Console.WriteLine(s.Contains("World")); // True
Console.WriteLine(s.Replace("World", "C#")); // Hello, C#!
Console.WriteLine(s.Substring(7, 5)); // World
\`\`\`

### Your Task

Given \`string city = "Paris"\` and \`int year = 2024\`, print:

\`\`\`
Paris hosted the Olympics in 2024.
\`\`\``,

	starterCode: `string city = "Paris";
int year = 2024;
// Print: Paris hosted the Olympics in 2024.
`,

	solution: `string city = "Paris";
int year = 2024;
Console.WriteLine($"{city} hosted the Olympics in {year}.");
`,

	tests: [
		{
			name: "prints interpolated string",
			expected: "Paris hosted the Olympics in 2024.\n",
		},
	],
};
