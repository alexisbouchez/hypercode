import type { Lesson } from "../../types";

export const minHeap: Lesson = {
	id: "heap",
	title: "Min Heap",
	chapterId: "data-structures",
	content: `## Min Heap

A **Min Heap** is a complete binary tree where every parent node is smaller than or equal to its children. The smallest element is always at the root.

### Array Representation

A binary heap is stored as a flat array. For a node at index \`i\`:
- **Left child**: \`2 * i + 1\`
- **Right child**: \`2 * i + 2\`
- **Parent**: \`Math.floor((i - 1) / 2)\`

\`\`\`
        1
       / \\
      3    5
     / \\
    7    9

Array: [1, 3, 5, 7, 9]
\`\`\`

### Insert (Bubble Up)

1. Push the new value to the end of the array.
2. Compare it with its parent — if smaller, swap them.
3. Repeat until the heap property is restored.

\`\`\`js
insert(val) {
  this.data.push(val);
  let i = this.data.length - 1;
  while (i > 0) {
    const parent = Math.floor((i - 1) / 2);
    if (this.data[i] >= this.data[parent]) break;
    [this.data[i], this.data[parent]] = [this.data[parent], this.data[i]];
    i = parent;
  }
}
\`\`\`

### Extract Min (Bubble Down)

1. Save the root (minimum).
2. Move the last element to the root.
3. Compare with children — swap with the smaller child if needed.
4. Repeat until the heap property is restored.

\`\`\`js
extractMin() {
  if (this.data.length === 0) return undefined;
  const min = this.data[0];
  const last = this.data.pop();
  if (this.data.length > 0) {
    this.data[0] = last;
    // bubble down from index 0
  }
  return min;
}
\`\`\`

### Complexity

| Operation   | Time     |
|-------------|----------|
| insert      | O(log n) |
| extractMin  | O(log n) |
| peek (min)  | O(1)     |

### Your Task

Implement a \`MinHeap\` class with \`insert\`, \`extractMin\`, and \`peek\` methods.`,

	starterCode: `class MinHeap {
	constructor() {
		this.data = [];
	}

	insert(val) {
		// Add value and bubble up
	}

	extractMin() {
		// Remove and return the minimum value
	}

	peek() {
		// Return the minimum without removing
	}

	size() {
		return this.data.length;
	}
}

const heap = new MinHeap();
heap.insert(5);
heap.insert(3);
heap.insert(8);
heap.insert(1);
console.log(heap.peek());
console.log(heap.extractMin());
console.log(heap.extractMin());
console.log(heap.extractMin());
console.log(heap.extractMin());
`,

	solution: `class MinHeap {
	constructor() {
		this.data = [];
	}

	insert(val) {
		this.data.push(val);
		let i = this.data.length - 1;
		while (i > 0) {
			const parent = Math.floor((i - 1) / 2);
			if (this.data[i] >= this.data[parent]) break;
			[this.data[i], this.data[parent]] = [this.data[parent], this.data[i]];
			i = parent;
		}
	}

	extractMin() {
		if (this.data.length === 0) return undefined;
		const min = this.data[0];
		const last = this.data.pop();
		if (this.data.length > 0) {
			this.data[0] = last;
			let i = 0;
			while (true) {
				let smallest = i;
				const left = 2 * i + 1;
				const right = 2 * i + 2;
				if (left < this.data.length && this.data[left] < this.data[smallest]) smallest = left;
				if (right < this.data.length && this.data[right] < this.data[smallest]) smallest = right;
				if (smallest === i) break;
				[this.data[i], this.data[smallest]] = [this.data[smallest], this.data[i]];
				i = smallest;
			}
		}
		return min;
	}

	peek() {
		return this.data[0];
	}

	size() {
		return this.data.length;
	}
}

const heap = new MinHeap();
heap.insert(5);
heap.insert(3);
heap.insert(8);
heap.insert(1);
console.log(heap.peek());
console.log(heap.extractMin());
console.log(heap.extractMin());
console.log(heap.extractMin());
console.log(heap.extractMin());
`,

	tests: [
		{
			name: "peek and extractMin in sorted order",
			expected: "1\n1\n3\n5\n8\n",
		},
		{
			name: "extractMin on empty heap returns undefined",
			code: `{{FUNC}}
const heap = new MinHeap();
console.log(heap.extractMin());`,
			expected: "undefined\n",
		},
		{
			name: "insert maintains heap order",
			code: `{{FUNC}}
const heap = new MinHeap();
heap.insert(10);
heap.insert(4);
heap.insert(15);
heap.insert(1);
heap.insert(7);
console.log(heap.extractMin());
console.log(heap.extractMin());
console.log(heap.extractMin());
console.log(heap.extractMin());
console.log(heap.extractMin());`,
			expected: "1\n4\n7\n10\n15\n",
		},
		{
			name: "size tracks correctly",
			code: `{{FUNC}}
const heap = new MinHeap();
console.log(heap.size());
heap.insert(5);
heap.insert(3);
console.log(heap.size());
heap.extractMin();
console.log(heap.size());`,
			expected: "0\n2\n1\n",
		},
	],
};
