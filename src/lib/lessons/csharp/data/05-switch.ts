import type { Lesson } from "../../types";

export const switchExpr: Lesson = {
	id: "switch",
	title: "Switch Expressions",
	chapterId: "control_flow",
	content: `## Switch Expressions

Modern C# (8+) has **switch expressions** — a concise pattern-matching syntax:

\`\`\`csharp
int day = 3;
string name = day switch {
    1 => "Monday",
    2 => "Tuesday",
    3 => "Wednesday",
    4 => "Thursday",
    5 => "Friday",
    _ => "Weekend",
};
Console.WriteLine(name); // Wednesday
\`\`\`

The \`_\` is the **discard pattern** — it matches anything not handled above.

You can also use switch expressions with conditions:

\`\`\`csharp
int score = 75;
string grade = score switch {
    >= 90 => "A",
    >= 80 => "B",
    >= 70 => "C",
    >= 60 => "D",
    _ => "F",
};
\`\`\`

### Your Task

Write a static method \`Season(int month)\` that returns the season:
- Months 12, 1, 2 → \`"Winter"\`
- Months 3, 4, 5 → \`"Spring"\`
- Months 6, 7, 8 → \`"Summer"\`
- Months 9, 10, 11 → \`"Autumn"\`
- Anything else → \`"Unknown"\``,

	starterCode: `static string Season(int month)
{
    // Use a switch expression to return the season
}
`,

	solution: `static string Season(int month)
{
    return month switch {
        12 or 1 or 2 => "Winter",
        3 or 4 or 5 => "Spring",
        6 or 7 or 8 => "Summer",
        9 or 10 or 11 => "Autumn",
        _ => "Unknown",
    };
}
`,

	tests: [
		{
			name: "winter month",
			expected: "Winter\n",
			code: `{{FUNC}}
Console.WriteLine(Season(1));
`,
		},
		{
			name: "summer month",
			expected: "Summer\n",
			code: `{{FUNC}}
Console.WriteLine(Season(7));
`,
		},
		{
			name: "autumn month",
			expected: "Autumn\n",
			code: `{{FUNC}}
Console.WriteLine(Season(10));
`,
		},
	],
};
