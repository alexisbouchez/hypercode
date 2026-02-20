import type { Lesson } from "../../types";

export const selectionSort: Lesson = {
	id: "selection-sort",
	title: "Selection Sort",
	chapterId: "sorting",
	content: `## Selection Sort

Selection Sort divides the array into a sorted region (left) and an unsorted region (right). Each pass finds the minimum element in the unsorted region and moves it to the end of the sorted region.

### How It Works

1. Find the minimum element in the unsorted portion \`arr[i..n-1]\`.
2. Swap it with \`arr[i]\`.
3. Advance \`i\` by one — the sorted region grows.
4. Repeat until the entire array is sorted.

\`\`\`js
function selectionSort(arr) {
  const a = [...arr];
  for (let i = 0; i < a.length; i++) {
    let minIdx = i;
    for (let j = i + 1; j < a.length; j++) {
      if (a[j] < a[minIdx]) minIdx = j;
    }
    [a[i], a[minIdx]] = [a[minIdx], a[i]];
  }
  return a;
}
\`\`\`

### Complexity

| Case | Time | Space |
|------|------|-------|
| Best | O(n²) | O(1) |
| Average | O(n²) | O(1) |
| Worst | O(n²) | O(1) |

Unlike bubble sort, selection sort always makes exactly **n-1 swaps**, regardless of input order. This makes it useful when writes (swaps) are expensive.

### Your Task

Implement \`selectionSort(arr)\` that returns a new array sorted in ascending order.`,

	starterCode: `function selectionSort(arr) {
	// Your implementation here
}

console.log(selectionSort([64, 25, 12, 22, 11]).join(", "));
console.log(selectionSort([5, 4, 3, 2, 1]).join(", "));
`,

	solution: `function selectionSort(arr) {
	const a = [...arr];
	for (let i = 0; i < a.length; i++) {
		let minIdx = i;
		for (let j = i + 1; j < a.length; j++) {
			if (a[j] < a[minIdx]) minIdx = j;
		}
		[a[i], a[minIdx]] = [a[minIdx], a[i]];
	}
	return a;
}

console.log(selectionSort([64, 25, 12, 22, 11]).join(", "));
console.log(selectionSort([5, 4, 3, 2, 1]).join(", "));
`,

	tests: [
		{
			name: "sorts [64,25,12,22,11] and [5,4,3,2,1]",
			expected: "11, 12, 22, 25, 64\n1, 2, 3, 4, 5\n",
		},
		{
			name: "sorts [3,1,2]",
			code: `{{FUNC}}
console.log(selectionSort([3, 1, 2]).join(", "));`,
			expected: "1, 2, 3\n",
		},
		{
			name: "handles single element",
			code: `{{FUNC}}
console.log(selectionSort([7]).join(", "));`,
			expected: "7\n",
		},
		{
			name: "handles duplicates",
			code: `{{FUNC}}
console.log(selectionSort([3, 1, 3, 2, 1]).join(", "));`,
			expected: "1, 1, 2, 3, 3\n",
		},
	],
};
