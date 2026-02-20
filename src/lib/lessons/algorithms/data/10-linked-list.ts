import type { Lesson } from "../../types";

export const linkedList: Lesson = {
	id: "linked-list",
	title: "Linked List",
	chapterId: "data-structures",
	content: `## Linked List

A Linked List is a sequence of **nodes**, where each node holds a value and a reference (pointer) to the next node. Unlike arrays, there is no contiguous memory — nodes can be anywhere in memory.

\`\`\`
head → [1 | →] → [2 | →] → [3 | null]
\`\`\`

### Node Structure

\`\`\`js
class ListNode {
  constructor(value) {
    this.value = value;
    this.next = null;
  }
}
\`\`\`

### Key Operations

\`\`\`js
class LinkedList {
  constructor() { this.head = null; }

  append(value) {
    const node = new ListNode(value);
    if (!this.head) { this.head = node; return; }
    let curr = this.head;
    while (curr.next) curr = curr.next;
    curr.next = node;
  }

  prepend(value) {
    const node = new ListNode(value);
    node.next = this.head;
    this.head = node;
  }

  delete(value) {
    if (!this.head) return;
    if (this.head.value === value) { this.head = this.head.next; return; }
    let curr = this.head;
    while (curr.next && curr.next.value !== value) curr = curr.next;
    if (curr.next) curr.next = curr.next.next;
  }

  toArray() {
    const result = [];
    let curr = this.head;
    while (curr) { result.push(curr.value); curr = curr.next; }
    return result;
  }
}
\`\`\`

### Complexity vs Array

| Operation | Array | Linked List |
|-----------|-------|-------------|
| Access by index | O(1) | O(n) |
| Prepend | O(n) | O(1) |
| Append | O(1) amortized | O(n) (or O(1) with tail pointer) |
| Delete (by value) | O(n) | O(n) |
| Insert at middle | O(n) | O(1) if you have the node |

### Your Task

Implement a \`LinkedList\` class with \`append\`, \`prepend\`, \`delete\`, and \`toArray\` methods.`,

	starterCode: `class ListNode {
	constructor(value) {
		this.value = value;
		this.next = null;
	}
}

class LinkedList {
	constructor() {
		this.head = null;
	}

	append(value) {
		// Add node to the end
	}

	prepend(value) {
		// Add node to the beginning
	}

	delete(value) {
		// Remove the first node with this value
	}

	toArray() {
		// Return all values as an array
	}
}

const list = new LinkedList();
list.append(1);
list.append(2);
list.append(3);
list.prepend(0);
console.log(list.toArray().join(", "));
list.delete(2);
console.log(list.toArray().join(", "));
`,

	solution: `class ListNode {
	constructor(value) {
		this.value = value;
		this.next = null;
	}
}

class LinkedList {
	constructor() {
		this.head = null;
	}

	append(value) {
		const node = new ListNode(value);
		if (!this.head) { this.head = node; return; }
		let curr = this.head;
		while (curr.next) curr = curr.next;
		curr.next = node;
	}

	prepend(value) {
		const node = new ListNode(value);
		node.next = this.head;
		this.head = node;
	}

	delete(value) {
		if (!this.head) return;
		if (this.head.value === value) { this.head = this.head.next; return; }
		let curr = this.head;
		while (curr.next && curr.next.value !== value) curr = curr.next;
		if (curr.next) curr.next = curr.next.next;
	}

	toArray() {
		const result = [];
		let curr = this.head;
		while (curr) { result.push(curr.value); curr = curr.next; }
		return result;
	}
}

const list = new LinkedList();
list.append(1);
list.append(2);
list.append(3);
list.prepend(0);
console.log(list.toArray().join(", "));
list.delete(2);
console.log(list.toArray().join(", "));
`,

	tests: [
		{
			name: "append, prepend, delete work correctly",
			expected: "0, 1, 2, 3\n0, 1, 3\n",
		},
		{
			name: "delete head node",
			code: `{{FUNC}}
const list = new LinkedList();
list.append(10);
list.append(20);
list.delete(10);
console.log(list.toArray().join(", "));`,
			expected: "20\n",
		},
		{
			name: "empty list toArray",
			code: `{{FUNC}}
const list = new LinkedList();
console.log(list.toArray().join(", "));`,
			expected: "\n",
		},
		{
			name: "prepend builds list in reverse",
			code: `{{FUNC}}
const list = new LinkedList();
list.prepend(3);
list.prepend(2);
list.prepend(1);
console.log(list.toArray().join(", "));`,
			expected: "1, 2, 3\n",
		},
	],
};
