import type { Lesson } from "../../types";

export const methods: Lesson = {
	id: "methods",
	title: "Methods",
	chapterId: "oop",
	content: `## Methods

Methods are reusable blocks of code. In C#, they can have parameters and return values:

\`\`\`csharp
static int Add(int a, int b) {
    return a + b;
}
Console.WriteLine(Add(3, 4)); // 7
\`\`\`

### Expression Body

Short methods can use \`=>\` (arrow syntax):

\`\`\`csharp
static int Square(int n) => n * n;
static string Greet(string name) => $"Hello, {name}!";
\`\`\`

### Multiple Return Values with Tuples

\`\`\`csharp
static (int min, int max) MinMax(int[] arr) {
    return (arr.Min(), arr.Max());
}
var (lo, hi) = MinMax(new[] { 3, 1, 4, 1, 5 });
Console.WriteLine($"{lo} {hi}"); // 1 5
\`\`\`

### Your Task

Write a static method \`IsPrime(int n)\` that returns \`true\` if \`n\` is a prime number, \`false\` otherwise.`,

	starterCode: `static bool IsPrime(int n)
{
    // Return true if n is prime, false otherwise
}
`,

	solution: `static bool IsPrime(int n)
{
    if (n < 2) return false;
    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) return false;
    }
    return true;
}
`,

	tests: [
		{
			name: "2 is prime",
			expected: "True\n",
			code: `{{FUNC}}
Console.WriteLine(IsPrime(2));
`,
		},
		{
			name: "17 is prime",
			expected: "True\n",
			code: `{{FUNC}}
Console.WriteLine(IsPrime(17));
`,
		},
		{
			name: "4 is not prime",
			expected: "False\n",
			code: `{{FUNC}}
Console.WriteLine(IsPrime(4));
`,
		},
		{
			name: "1 is not prime",
			expected: "False\n",
			code: `{{FUNC}}
Console.WriteLine(IsPrime(1));
`,
		},
	],
};
