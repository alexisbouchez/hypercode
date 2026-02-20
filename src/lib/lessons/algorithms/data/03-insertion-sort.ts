import type { Lesson } from "../../types";

export const insertionSort: Lesson = {
	id: "insertion-sort",
	title: "Insertion Sort",
	chapterId: "sorting",
	content: `## Insertion Sort

Insertion Sort builds the sorted array one element at a time. Think of sorting a hand of playing cards: you pick up each card and insert it into the correct position among the cards you already hold.

### How It Works

1. Start with the second element (index 1). The first element is trivially sorted.
2. Store the current element as \`key\`.
3. Shift all sorted elements that are greater than \`key\` one position to the right.
4. Insert \`key\` into the gap.
5. Move to the next element and repeat.

\`\`\`js
function insertionSort(arr) {
  const a = [...arr];
  for (let i = 1; i < a.length; i++) {
    const key = a[i];
    let j = i - 1;
    while (j >= 0 && a[j] > key) {
      a[j + 1] = a[j];
      j--;
    }
    a[j + 1] = key;
  }
  return a;
}
\`\`\`

### Complexity

| Case | Time | Space |
|------|------|-------|
| Best (sorted) | O(n) | O(1) |
| Average | O(n²) | O(1) |
| Worst (reversed) | O(n²) | O(1) |

Insertion sort is excellent for **nearly sorted arrays** and **small datasets**. Many production sort implementations (like Timsort) use insertion sort for small subarrays.

### Your Task

Implement \`insertionSort(arr)\` that returns a new array sorted in ascending order.`,

	starterCode: `function insertionSort(arr) {
	// Your implementation here
}

console.log(insertionSort([12, 11, 13, 5, 6]).join(", "));
console.log(insertionSort([2, 1]).join(", "));
`,

	solution: `function insertionSort(arr) {
	const a = [...arr];
	for (let i = 1; i < a.length; i++) {
		const key = a[i];
		let j = i - 1;
		while (j >= 0 && a[j] > key) {
			a[j + 1] = a[j];
			j--;
		}
		a[j + 1] = key;
	}
	return a;
}

console.log(insertionSort([12, 11, 13, 5, 6]).join(", "));
console.log(insertionSort([2, 1]).join(", "));
`,

	tests: [
		{
			name: "sorts [12,11,13,5,6] and [2,1]",
			expected: "5, 6, 11, 12, 13\n1, 2\n",
		},
		{
			name: "sorts [5,2,4,6,1,3]",
			code: `{{FUNC}}
console.log(insertionSort([5, 2, 4, 6, 1, 3]).join(", "));`,
			expected: "1, 2, 3, 4, 5, 6\n",
		},
		{
			name: "handles single element",
			code: `{{FUNC}}
console.log(insertionSort([99]).join(", "));`,
			expected: "99\n",
		},
		{
			name: "handles already sorted array (O(n) case)",
			code: `{{FUNC}}
console.log(insertionSort([1, 2, 3, 4, 5]).join(", "));`,
			expected: "1, 2, 3, 4, 5\n",
		},
	],
};
