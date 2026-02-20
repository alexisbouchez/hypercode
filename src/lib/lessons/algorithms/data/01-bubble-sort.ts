import type { Lesson } from "../../types";

export const bubbleSort: Lesson = {
	id: "bubble-sort",
	title: "Bubble Sort",
	chapterId: "sorting",
	content: `## Bubble Sort

Bubble Sort is the simplest sorting algorithm. It works by repeatedly stepping through the array, comparing adjacent elements, and swapping them if they are in the wrong order. Each pass "bubbles" the largest unsorted element to its correct position.

### How It Works

1. Compare element at index \`j\` with element at \`j + 1\`.
2. If \`arr[j] > arr[j+1]\`, swap them.
3. Repeat for the entire array.
4. After each pass, the largest remaining element is in its final position — so each pass can be one element shorter.

\`\`\`js
function bubbleSort(arr) {
  const a = [...arr]; // copy to avoid mutation
  for (let i = 0; i < a.length - 1; i++) {
    for (let j = 0; j < a.length - 1 - i; j++) {
      if (a[j] > a[j + 1]) {
        [a[j], a[j + 1]] = [a[j + 1], a[j]]; // swap
      }
    }
  }
  return a;
}
\`\`\`

### Complexity

| Case | Time | Space |
|------|------|-------|
| Best | O(n) | O(1) |
| Average | O(n²) | O(1) |
| Worst | O(n²) | O(1) |

Bubble sort is rarely used in practice due to its O(n²) worst case, but it is a great algorithm to understand fundamentals like in-place swapping and loop invariants.

### Your Task

Implement \`bubbleSort(arr)\` that takes an array of numbers and returns a new sorted array in ascending order. Do not mutate the input array.`,

	starterCode: `function bubbleSort(arr) {
	// Your implementation here
}

console.log(bubbleSort([5, 3, 1, 4, 2]).join(", "));
console.log(bubbleSort([9, 7, 5, 3, 1]).join(", "));
`,

	solution: `function bubbleSort(arr) {
	const a = [...arr];
	for (let i = 0; i < a.length - 1; i++) {
		for (let j = 0; j < a.length - 1 - i; j++) {
			if (a[j] > a[j + 1]) {
				[a[j], a[j + 1]] = [a[j + 1], a[j]];
			}
		}
	}
	return a;
}

console.log(bubbleSort([5, 3, 1, 4, 2]).join(", "));
console.log(bubbleSort([9, 7, 5, 3, 1]).join(", "));
`,

	tests: [
		{
			name: "sorts [5,3,1,4,2] and [9,7,5,3,1]",
			expected: "1, 2, 3, 4, 5\n1, 3, 5, 7, 9\n",
		},
		{
			name: "sorts [3,1,2]",
			code: `{{FUNC}}
console.log(bubbleSort([3, 1, 2]).join(", "));`,
			expected: "1, 2, 3\n",
		},
		{
			name: "handles single element",
			code: `{{FUNC}}
console.log(bubbleSort([42]).join(", "));`,
			expected: "42\n",
		},
		{
			name: "handles already sorted array",
			code: `{{FUNC}}
console.log(bubbleSort([1, 2, 3, 4, 5]).join(", "));`,
			expected: "1, 2, 3, 4, 5\n",
		},
	],
};
