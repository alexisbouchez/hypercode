import type { Lesson } from "../../types";

export const mergeSort: Lesson = {
	id: "merge-sort",
	title: "Merge Sort",
	chapterId: "sorting",
	content: `## Merge Sort

Merge Sort is a **divide and conquer** algorithm. It recursively splits the array in half, sorts each half, then merges the two sorted halves into a single sorted array.

### How It Works

**Divide:** Split the array into two halves at the midpoint.

**Conquer:** Recursively sort each half.

**Merge:** Combine the two sorted halves. Compare front elements from each half, take the smaller one, repeat until both halves are exhausted.

\`\`\`js
function mergeSort(arr) {
  if (arr.length <= 1) return arr;
  const mid = Math.floor(arr.length / 2);
  const left = mergeSort(arr.slice(0, mid));
  const right = mergeSort(arr.slice(mid));
  return merge(left, right);
}

function merge(left, right) {
  const result = [];
  let i = 0, j = 0;
  while (i < left.length && j < right.length) {
    if (left[i] <= right[j]) result.push(left[i++]);
    else result.push(right[j++]);
  }
  return result.concat(left.slice(i), right.slice(j));
}
\`\`\`

### Complexity

| Case | Time | Space |
|------|------|-------|
| Best | O(n log n) | O(n) |
| Average | O(n log n) | O(n) |
| Worst | O(n log n) | O(n) |

Merge sort is **stable** (preserves relative order of equal elements) and has guaranteed O(n log n) performance. It is used in Python's \`sorted()\` and Java's \`Arrays.sort()\` for objects.

### Your Task

Implement \`mergeSort(arr)\` that returns a new sorted array. You will need a helper \`merge(left, right)\` function.`,

	starterCode: `function merge(left, right) {
	// Merge two sorted arrays into one sorted array
}

function mergeSort(arr) {
	// Base case: arrays of length 0 or 1 are already sorted
	// Divide, conquer, and merge
}

console.log(mergeSort([38, 27, 43, 3, 9, 82, 10]).join(", "));
console.log(mergeSort([5, 1, 4, 2, 8]).join(", "));
`,

	solution: `function merge(left, right) {
	const result = [];
	let i = 0, j = 0;
	while (i < left.length && j < right.length) {
		if (left[i] <= right[j]) result.push(left[i++]);
		else result.push(right[j++]);
	}
	return result.concat(left.slice(i), right.slice(j));
}

function mergeSort(arr) {
	if (arr.length <= 1) return arr;
	const mid = Math.floor(arr.length / 2);
	const left = mergeSort(arr.slice(0, mid));
	const right = mergeSort(arr.slice(mid));
	return merge(left, right);
}

console.log(mergeSort([38, 27, 43, 3, 9, 82, 10]).join(", "));
console.log(mergeSort([5, 1, 4, 2, 8]).join(", "));
`,

	tests: [
		{
			name: "sorts [38,27,43,3,9,82,10] and [5,1,4,2,8]",
			expected: "3, 9, 10, 27, 38, 43, 82\n1, 2, 4, 5, 8\n",
		},
		{
			name: "sorts [2,1]",
			code: `{{FUNC}}
console.log(mergeSort([2, 1]).join(", "));`,
			expected: "1, 2\n",
		},
		{
			name: "handles single element",
			code: `{{FUNC}}
console.log(mergeSort([5]).join(", "));`,
			expected: "5\n",
		},
		{
			name: "handles already sorted array",
			code: `{{FUNC}}
console.log(mergeSort([1, 2, 3, 4]).join(", "));`,
			expected: "1, 2, 3, 4\n",
		},
	],
};
