import type { Lesson } from "../../types";

export const linearSearch: Lesson = {
	id: "linear-search",
	title: "Linear Search",
	chapterId: "searching",
	content: `## Linear Search

Linear Search is the simplest search algorithm. It scans each element of the array one by one until it finds the target or reaches the end.

### How It Works

Start at index 0. Check each element:
- If \`arr[i] === target\`, return \`i\`.
- Otherwise, move to the next element.
- If the end is reached without finding the target, return \`-1\`.

\`\`\`js
function linearSearch(arr, target) {
  for (let i = 0; i < arr.length; i++) {
    if (arr[i] === target) return i;
  }
  return -1;
}
\`\`\`

### Complexity

| Case | Time | Space |
|------|------|-------|
| Best (first element) | O(1) | O(1) |
| Average | O(n) | O(1) |
| Worst (not found) | O(n) | O(1) |

Linear search works on **unsorted arrays** and any collection where elements can be compared one at a time. It is the only option when the array is unsorted or when you have a linked list (no random access).

### When to Use It

- The array is small (n < 100).
- The array is unsorted and not worth sorting for a single search.
- You are searching a linked list.
- The search criterion is complex and not easily expressed as a comparison.

### Your Task

Implement \`linearSearch(arr, target)\` that returns the index of \`target\` in \`arr\`, or \`-1\` if not found.`,

	starterCode: `function linearSearch(arr, target) {
	// Scan each element and return its index if found
	// Return -1 if not found
}

console.log(linearSearch([4, 2, 7, 1, 9, 3], 7));
console.log(linearSearch([4, 2, 7, 1, 9, 3], 5));
`,

	solution: `function linearSearch(arr, target) {
	for (let i = 0; i < arr.length; i++) {
		if (arr[i] === target) return i;
	}
	return -1;
}

console.log(linearSearch([4, 2, 7, 1, 9, 3], 7));
console.log(linearSearch([4, 2, 7, 1, 9, 3], 5));
`,

	tests: [
		{
			name: "finds 7 at index 2, returns -1 for 5",
			expected: "2\n-1\n",
		},
		{
			name: "finds first element",
			code: `{{FUNC}}
console.log(linearSearch([10, 20, 30], 10));`,
			expected: "0\n",
		},
		{
			name: "finds last element",
			code: `{{FUNC}}
console.log(linearSearch([10, 20, 30], 30));`,
			expected: "2\n",
		},
		{
			name: "returns -1 for empty array",
			code: `{{FUNC}}
console.log(linearSearch([], 5));`,
			expected: "-1\n",
		},
	],
};
