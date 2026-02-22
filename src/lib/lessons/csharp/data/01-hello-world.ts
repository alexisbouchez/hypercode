import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
	id: "hello-world",
	title: "Hello, World!",
	chapterId: "basics",
	content: `## Your First C# Program

C# uses \`Console.WriteLine\` to print output with a newline:

\`\`\`csharp
Console.WriteLine("Hello, World!");
\`\`\`

\`Console.Write\` prints without a trailing newline. Comments use \`//\`:

\`\`\`csharp
Console.WriteLine("first line");   // prints with newline
Console.Write("no newline");       // no trailing newline
// This is a comment
\`\`\`

### Your Task

Print exactly \`Hello, World!\` using \`Console.WriteLine\`.`,

	starterCode: `// Print "Hello, World!"
`,

	solution: `Console.WriteLine("Hello, World!");
`,

	tests: [
		{
			name: "prints Hello, World!",
			expected: "Hello, World!\n",
		},
	],
};
