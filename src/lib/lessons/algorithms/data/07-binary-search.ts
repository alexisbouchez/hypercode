import type { Lesson } from "../../types";

export const binarySearch: Lesson = {
	id: "binary-search",
	title: "Binary Search",
	chapterId: "searching",
	content: `## Binary Search

Binary Search finds an element in a **sorted array** by repeatedly halving the search space. Each comparison eliminates half the remaining candidates.

### How It Works

Maintain two pointers: \`lo\` (start) and \`hi\` (end). While \`lo <= hi\`:

1. Calculate \`mid = Math.floor((lo + hi) / 2)\`.
2. If \`arr[mid] === target\`, return \`mid\`. Found it.
3. If \`arr[mid] < target\`, the target must be to the right: set \`lo = mid + 1\`.
4. If \`arr[mid] > target\`, the target must be to the left: set \`hi = mid - 1\`.
5. If the loop ends without finding the target, return \`-1\`.

\`\`\`js
function binarySearch(arr, target) {
  let lo = 0, hi = arr.length - 1;
  while (lo <= hi) {
    const mid = Math.floor((lo + hi) / 2);
    if (arr[mid] === target) return mid;
    if (arr[mid] < target) lo = mid + 1;
    else hi = mid - 1;
  }
  return -1;
}
\`\`\`

### Complexity

| Case | Time | Space |
|------|------|-------|
| Best (middle element) | O(1) | O(1) |
| Average | O(log n) | O(1) |
| Worst | O(log n) | O(1) |

Binary search on 1 billion elements needs at most **30 comparisons**. Linear search needs up to 1 billion. This is the power of O(log n).

**Requirement:** The array must be sorted. If you will search many times, sorting once (O(n log n)) and binary searching repeatedly (O(log n) each) is far faster than linear searching each time.

### Your Task

Implement \`binarySearch(arr, target)\` that returns the index of \`target\` in the sorted array \`arr\`, or \`-1\` if not found.`,

	starterCode: `function binarySearch(arr, target) {
	// Keep track of lo and hi boundaries
	// Calculate mid and compare to target
	// Narrow the search space each iteration
}

const sorted = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19];
console.log(binarySearch(sorted, 7));
console.log(binarySearch(sorted, 10));
`,

	solution: `function binarySearch(arr, target) {
	let lo = 0, hi = arr.length - 1;
	while (lo <= hi) {
		const mid = Math.floor((lo + hi) / 2);
		if (arr[mid] === target) return mid;
		if (arr[mid] < target) lo = mid + 1;
		else hi = mid - 1;
	}
	return -1;
}

const sorted = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19];
console.log(binarySearch(sorted, 7));
console.log(binarySearch(sorted, 10));
`,

	tests: [
		{
			name: "finds 7 at index 3, returns -1 for 10",
			expected: "3\n-1\n",
		},
		{
			name: "finds first element",
			code: `{{FUNC}}
console.log(binarySearch([1, 3, 5, 7, 9], 1));`,
			expected: "0\n",
		},
		{
			name: "finds last element",
			code: `{{FUNC}}
console.log(binarySearch([1, 3, 5, 7, 9], 9));`,
			expected: "4\n",
		},
		{
			name: "returns -1 for empty array",
			code: `{{FUNC}}
console.log(binarySearch([], 5));`,
			expected: "-1\n",
		},
	],
};
