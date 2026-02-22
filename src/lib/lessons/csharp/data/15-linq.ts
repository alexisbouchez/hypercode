import type { Lesson } from "../../types";

export const linq: Lesson = {
	id: "linq",
	title: "LINQ",
	chapterId: "advanced",
	content: `## LINQ

**Language Integrated Query (LINQ)** lets you query collections with expressive, composable operators:

\`\`\`csharp
int[] nums = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

// Filter
var evens = nums.Where(n => n % 2 == 0);
// { 2, 4, 6, 8, 10 }

// Transform
var squares = evens.Select(n => n * n);
// { 4, 16, 36, 64, 100 }

// Aggregate
int total = nums.Sum();       // 55
int max = nums.Max();         // 10
double avg = nums.Average();  // 5.5
\`\`\`

### Chaining

LINQ operators return \`IEnumerable<T>\`, so they can be chained:

\`\`\`csharp
var result = nums
    .Where(n => n > 3)
    .Select(n => n * 2)
    .OrderByDescending(n => n)
    .Take(3);
// { 20, 18, 16 }
\`\`\`

### Your Task

Write a static method \`TopSquares(int[] nums)\` that:
1. Filters to only even numbers
2. Computes each even number's square
3. Returns the sum of those squares`,

	starterCode: `static int TopSquares(int[] nums)
{
    // Filter evens, square them, return the sum
}
`,

	solution: `static int TopSquares(int[] nums)
{
    return nums
        .Where(n => n % 2 == 0)
        .Select(n => n * n)
        .Sum();
}
`,

	tests: [
		{
			name: "sum of squares of evens in 1-6",
			expected: "56\n",
			code: `{{FUNC}}
Console.WriteLine(TopSquares(new[] { 1, 2, 3, 4, 5, 6 }));
`,
		},
		{
			name: "sum of squares of evens in [2,4,6]",
			expected: "56\n",
			code: `{{FUNC}}
Console.WriteLine(TopSquares(new[] { 2, 4, 6 }));
`,
		},
	],
};
