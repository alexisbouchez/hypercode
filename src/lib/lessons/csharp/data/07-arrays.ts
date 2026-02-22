import type { Lesson } from "../../types";

export const arrays: Lesson = {
	id: "arrays",
	title: "Arrays",
	chapterId: "collections",
	content: `## Arrays

Arrays store a fixed-size sequence of elements of the same type:

\`\`\`csharp
int[] numbers = { 4, 2, 7, 1, 9 };
Console.WriteLine(numbers[0]);  // 4 (zero-indexed)
Console.WriteLine(numbers.Length); // 5
\`\`\`

### Sorting and Searching

\`\`\`csharp
int[] nums = { 3, 1, 4, 1, 5, 9 };
Array.Sort(nums);
// nums is now { 1, 1, 3, 4, 5, 9 }
\`\`\`

### Iterating

\`\`\`csharp
foreach (int n in numbers) {
    Console.WriteLine(n);
}
\`\`\`

### Your Task

Write a static method \`Sum(int[] arr)\` that returns the sum of all elements.`,

	starterCode: `static int Sum(int[] arr)
{
    // Return the sum of all elements
}
`,

	solution: `static int Sum(int[] arr)
{
    int total = 0;
    foreach (int n in arr) {
        total += n;
    }
    return total;
}
`,

	tests: [
		{
			name: "sum of [1,2,3,4,5]",
			expected: "15\n",
			code: `{{FUNC}}
Console.WriteLine(Sum(new int[] { 1, 2, 3, 4, 5 }));
`,
		},
		{
			name: "sum of [10,20,30]",
			expected: "60\n",
			code: `{{FUNC}}
Console.WriteLine(Sum(new int[] { 10, 20, 30 }));
`,
		},
		{
			name: "sum of empty array",
			expected: "0\n",
			code: `{{FUNC}}
Console.WriteLine(Sum(new int[] { }));
`,
		},
	],
};
