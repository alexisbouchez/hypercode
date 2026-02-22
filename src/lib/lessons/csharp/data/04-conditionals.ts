import type { Lesson } from "../../types";

export const conditionals: Lesson = {
	id: "conditionals",
	title: "Conditionals",
	chapterId: "control_flow",
	content: `## Conditionals

C# uses \`if\`, \`else if\`, and \`else\` for branching:

\`\`\`csharp
int score = 85;

if (score >= 90) {
    Console.WriteLine("A");
} else if (score >= 80) {
    Console.WriteLine("B");
} else if (score >= 70) {
    Console.WriteLine("C");
} else {
    Console.WriteLine("F");
}
// Output: B
\`\`\`

### Ternary Operator

A compact one-liner for simple conditions:

\`\`\`csharp
int x = 10;
string result = x > 0 ? "positive" : "non-positive";
\`\`\`

### Your Task

Write a static method \`Sign(int n)\` that returns:
- \`"positive"\` if \`n > 0\`
- \`"negative"\` if \`n < 0\`
- \`"zero"\` if \`n == 0\``,

	starterCode: `static string Sign(int n)
{
    // Return "positive", "negative", or "zero"
}
`,

	solution: `static string Sign(int n)
{
    if (n > 0) return "positive";
    else if (n < 0) return "negative";
    else return "zero";
}
`,

	tests: [
		{
			name: "positive number",
			expected: "positive\n",
			code: `{{FUNC}}
Console.WriteLine(Sign(5));
`,
		},
		{
			name: "negative number",
			expected: "negative\n",
			code: `{{FUNC}}
Console.WriteLine(Sign(-3));
`,
		},
		{
			name: "zero",
			expected: "zero\n",
			code: `{{FUNC}}
Console.WriteLine(Sign(0));
`,
		},
	],
};
