import type { Lesson } from "../../types";

export const variables: Lesson = {
	id: "variables",
	title: "Variables & Types",
	chapterId: "basics",
	content: `## Variables and Types

C# is statically typed — every variable has a type declared at compile time:

\`\`\`csharp
int age = 25;           // integer (whole number)
double gpa = 3.95;      // floating-point number
string name = "Alice";  // text
bool isStudent = true;  // true or false
\`\`\`

You can also use \`var\` to let the compiler infer the type:

\`\`\`csharp
var x = 42;         // compiler infers int
var pi = 3.14159;   // compiler infers double
\`\`\`

### Type Conversion

Convert between types with casting or conversion methods:

\`\`\`csharp
int x = 7;
double d = (double)x;         // cast int to double
string s = x.ToString();      // int to string
int n = int.Parse("42");      // string to int
\`\`\`

### Your Task

Declare these variables and print each on its own line:
- \`int\` named \`age\` with value \`30\`
- \`string\` named \`name\` with value \`"Bob"\`
- \`bool\` named \`active\` with value \`true\`
- \`double\` named \`score\` with value \`9.5\``,

	starterCode: `// Declare and print the four variables
`,

	solution: `int age = 30;
string name = "Bob";
bool active = true;
double score = 9.5;
Console.WriteLine(age);
Console.WriteLine(name);
Console.WriteLine(active);
Console.WriteLine(score);
`,

	tests: [
		{
			name: "prints all four variables",
			expected: "30\nBob\nTrue\n9.5\n",
		},
	],
};
