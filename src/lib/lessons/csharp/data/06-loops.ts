import type { Lesson } from "../../types";

export const loops: Lesson = {
	id: "loops",
	title: "Loops",
	chapterId: "control_flow",
	content: `## Loops

C# has three main loop types:

### for Loop
\`\`\`csharp
for (int i = 1; i <= 5; i++) {
    Console.WriteLine(i);
}
// Prints 1, 2, 3, 4, 5
\`\`\`

### while Loop
\`\`\`csharp
int n = 1;
while (n <= 3) {
    Console.WriteLine(n);
    n++;
}
\`\`\`

### foreach Loop
Iterates over a collection:
\`\`\`csharp
string[] fruits = { "apple", "banana", "cherry" };
foreach (string fruit in fruits) {
    Console.WriteLine(fruit);
}
\`\`\`

### Your Task

Use a \`for\` loop to print the first 5 multiples of 3 (3, 6, 9, 12, 15).`,

	starterCode: `// Print multiples of 3: 3, 6, 9, 12, 15
`,

	solution: `for (int i = 1; i <= 5; i++) {
    Console.WriteLine(i * 3);
}
`,

	tests: [
		{
			name: "prints multiples of 3",
			expected: "3\n6\n9\n12\n15\n",
		},
	],
};
