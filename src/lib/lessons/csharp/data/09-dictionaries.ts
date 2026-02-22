import type { Lesson } from "../../types";

export const dictionaries: Lesson = {
	id: "dictionaries",
	title: "Dictionaries",
	chapterId: "collections",
	content: `## Dictionaries

\`Dictionary<TKey, TValue>\` maps keys to values (like a hash map):

\`\`\`csharp
var ages = new Dictionary<string, int>();
ages["Alice"] = 30;
ages["Bob"] = 25;
ages["Carol"] = 35;

Console.WriteLine(ages["Alice"]);     // 30
Console.WriteLine(ages.Count);       // 3
Console.WriteLine(ages.ContainsKey("Bob")); // True
\`\`\`

### Iterating

\`\`\`csharp
foreach (var pair in ages) {
    Console.WriteLine($"{pair.Key}: {pair.Value}");
}
\`\`\`

### Safe Lookup

\`\`\`csharp
if (ages.TryGetValue("Dave", out int age)) {
    Console.WriteLine(age);
} else {
    Console.WriteLine("Not found");
}
\`\`\`

### Your Task

Write a static method \`WordCount(string[] words)\` that returns a \`Dictionary<string, int>\` counting occurrences of each word. Then print the count for \`"hello"\` given the input \`["hello", "world", "hello", "csharp", "hello"]\`.`,

	starterCode: `static Dictionary<string, int> WordCount(string[] words)
{
    // Count occurrences of each word
}
`,

	solution: `static Dictionary<string, int> WordCount(string[] words)
{
    var counts = new Dictionary<string, int>();
    foreach (string w in words) {
        if (counts.ContainsKey(w)) counts[w]++;
        else counts[w] = 1;
    }
    return counts;
}
`,

	tests: [
		{
			name: "counts hello correctly",
			expected: "3\n",
			code: `{{FUNC}}
var result = WordCount(new[] { "hello", "world", "hello", "csharp", "hello" });
Console.WriteLine(result["hello"]);
`,
		},
		{
			name: "counts unique words",
			expected: "2\n",
			code: `{{FUNC}}
var result = WordCount(new[] { "a", "b", "a", "c" });
Console.WriteLine(result["a"]);
`,
		},
	],
};
