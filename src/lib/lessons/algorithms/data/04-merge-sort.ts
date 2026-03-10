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
		{
			name: "property: output is always sorted and has the same length as input",
			code: `{{FUNC}}
// Generate several pseudo-random arrays and verify sort properties
const inputs = [
  [42, 17, 3, 88, 5, 23, 71, 9],
  [1],
  [],
  [5, 5, 5, 5],
  [100, 1, 50, 25, 75, 10, 90, 30, 60, 40],
  Array.from({length: 20}, (_, i) => (i * 31 + 7) % 50),
];
let allPassed = true;
for (const input of inputs) {
  const result = mergeSort(input);
  // Property 1: same length
  if (result.length !== input.length) { allPassed = false; break; }
  // Property 2: output is sorted (each element <= next)
  for (let i = 0; i < result.length - 1; i++) {
    if (result[i] > result[i + 1]) { allPassed = false; break; }
  }
  // Property 3: output is a permutation (same elements)
  const sortedInput = [...input].sort((a, b) => a - b);
  if (JSON.stringify(result) !== JSON.stringify(sortedInput)) { allPassed = false; break; }
}
console.log(allPassed ? "PASS" : "FAIL");`,
			expected: "PASS\n",
		},
		{
			name: "property: sorting any permutation produces the same result",
			code: `{{FUNC}}
// Commutativity: different orderings of the same elements must produce identical output
const perms = [
  [15, 8, 23, 4, 42, 16],
  [42, 15, 4, 23, 8, 16],
  [4, 8, 15, 16, 23, 42],
  [23, 42, 16, 8, 4, 15],
];
const ref = mergeSort(perms[0]).join(",");
const allMatch = perms.every(p => mergeSort(p).join(",") === ref);
console.log(allMatch ? "PASS" : "FAIL");`,
			expected: "PASS\n",
		},
		{
			name: "handles 200 elements (efficiency check)",
			code: `{{FUNC}}
const input = Array.from({length: 200}, (_, i) => i + 1);
for (let k = 0; k < 200; k++) { const j = (k * 67 + 29) % 200; [input[k], input[j]] = [input[j], input[k]]; }
const result = mergeSort(input);
console.log(result.length);
console.log(result[0] + "," + result[99] + "," + result[199]);
console.log(JSON.stringify(result) === JSON.stringify(Array.from({length: 200}, (_, i) => i + 1)));`,
			expected: "200\n1,100,200\ntrue\n",
		},
	],
};
