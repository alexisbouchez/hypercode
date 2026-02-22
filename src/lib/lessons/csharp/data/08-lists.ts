import type { Lesson } from "../../types";

export const lists: Lesson = {
	id: "lists",
	title: "Lists",
	chapterId: "collections",
	content: `## Lists

\`List<T>\` is a dynamic array that grows automatically:

\`\`\`csharp
var numbers = new List<int> { 1, 2, 3 };
numbers.Add(4);           // [1, 2, 3, 4]
numbers.Remove(2);        // [1, 3, 4]
numbers.Insert(0, 10);    // [10, 1, 3, 4]
Console.WriteLine(numbers.Count);  // 4
Console.WriteLine(numbers[0]);     // 10
\`\`\`

### Useful Methods

\`\`\`csharp
numbers.Contains(3);          // true
numbers.IndexOf(3);           // 2
numbers.Sort();               // sort in place
numbers.Reverse();            // reverse in place
numbers.Clear();              // empty the list
\`\`\`

### Converting from Array

\`\`\`csharp
int[] arr = { 1, 2, 3 };
var list = new List<int>(arr);
\`\`\`

### Your Task

Create a \`List<string>\` with \`"alpha"\`, \`"beta"\`, \`"gamma"\`. Add \`"delta"\`, remove \`"beta"\`, then print each element on its own line.`,

	starterCode: `// Build the list, add "delta", remove "beta", print each element
`,

	solution: `var items = new List<string> { "alpha", "beta", "gamma" };
items.Add("delta");
items.Remove("beta");
foreach (string s in items) {
    Console.WriteLine(s);
}
`,

	tests: [
		{
			name: "prints alpha, gamma, delta",
			expected: "alpha\ngamma\ndelta\n",
		},
	],
};
