import type { Lesson } from "../../types";

export const quickSort: Lesson = {
	id: "quick-sort",
	title: "Quick Sort",
	chapterId: "sorting",
	content: `## Quick Sort

Quick Sort is the most widely used sorting algorithm in practice. Like merge sort, it uses divide and conquer. It picks a **pivot** element and partitions the array so that all elements smaller than the pivot come before it, and all larger elements come after.

### How It Works

1. If the array has 0 or 1 element, return it (base case).
2. Pick a pivot (commonly the last element).
3. Partition: collect elements ≤ pivot into \`left\`, elements > pivot into \`right\`.
4. Recursively sort \`left\` and \`right\`.
5. Return \`[...quickSort(left), pivot, ...quickSort(right)]\`.

\`\`\`js
function quickSort(arr) {
  if (arr.length <= 1) return arr;
  const pivot = arr[arr.length - 1];
  const left = arr.slice(0, -1).filter(x => x <= pivot);
  const right = arr.slice(0, -1).filter(x => x > pivot);
  return [...quickSort(left), pivot, ...quickSort(right)];
}
\`\`\`

### Complexity

| Case | Time | Space |
|------|------|-------|
| Best | O(n log n) | O(log n) |
| Average | O(n log n) | O(log n) |
| Worst (sorted input, bad pivot) | O(n²) | O(n) |

Quick sort is **in-place** in its classic implementation (using index-based partitioning instead of creating new arrays). It is typically faster than merge sort in practice due to better cache performance. C's \`qsort\` and C++'s \`std::sort\` are based on quicksort variants.

### Your Task

Implement \`quickSort(arr)\` that returns a new sorted array. Use the last element as the pivot.`,

	starterCode: `function quickSort(arr) {
	// Base case: return arrays of length 0 or 1
	// Pick pivot (last element)
	// Partition into left and right
	// Recursively sort and combine
}

console.log(quickSort([10, 7, 8, 9, 1, 5]).join(", "));
console.log(quickSort([3, 6, 8, 10, 1, 2, 1]).join(", "));
`,

	solution: `function quickSort(arr) {
	if (arr.length <= 1) return arr;
	const pivot = arr[arr.length - 1];
	const left = arr.slice(0, -1).filter(x => x <= pivot);
	const right = arr.slice(0, -1).filter(x => x > pivot);
	return [...quickSort(left), pivot, ...quickSort(right)];
}

console.log(quickSort([10, 7, 8, 9, 1, 5]).join(", "));
console.log(quickSort([3, 6, 8, 10, 1, 2, 1]).join(", "));
`,

	tests: [
		{
			name: "sorts [10,7,8,9,1,5] and [3,6,8,10,1,2,1]",
			expected: "1, 5, 7, 8, 9, 10\n1, 1, 2, 3, 6, 8, 10\n",
		},
		{
			name: "sorts [4,2,6,1,3,5]",
			code: `{{FUNC}}
console.log(quickSort([4, 2, 6, 1, 3, 5]).join(", "));`,
			expected: "1, 2, 3, 4, 5, 6\n",
		},
		{
			name: "handles single element",
			code: `{{FUNC}}
console.log(quickSort([1]).join(", "));`,
			expected: "1\n",
		},
		{
			name: "handles duplicates",
			code: `{{FUNC}}
console.log(quickSort([2, 2, 1, 1, 3]).join(", "));`,
			expected: "1, 1, 2, 2, 3\n",
		},
	],
};
