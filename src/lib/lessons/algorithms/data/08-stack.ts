import type { Lesson } from "../../types";

export const stack: Lesson = {
	id: "stack",
	title: "Stack",
	chapterId: "data-structures",
	content: `## Stack

A Stack is a **Last In, First Out (LIFO)** data structure. Think of a stack of plates: you add a plate to the top, and when you take one, you take from the top.

### Operations

- \`push(item)\` — add an item to the top. O(1)
- \`pop()\` — remove and return the top item. O(1)
- \`peek()\` — return the top item without removing it. O(1)
- \`isEmpty()\` — return true if the stack is empty. O(1)
- \`size()\` — return the number of items. O(1)

\`\`\`js
class Stack {
  constructor() {
    this.items = [];
  }
  push(item) { this.items.push(item); }
  pop() { return this.items.pop(); }
  peek() { return this.items[this.items.length - 1]; }
  isEmpty() { return this.items.length === 0; }
  size() { return this.items.length; }
}

const s = new Stack();
s.push(1);
s.push(2);
s.push(3);
console.log(s.pop());   // 3
console.log(s.peek());  // 2
console.log(s.size());  // 2
\`\`\`

### Real-World Uses

- **Function call stack** — when a function calls another, the return address is pushed onto the call stack.
- **Undo/redo** — text editors push every change onto a stack; undo pops it.
- **Expression evaluation** — compilers use stacks to evaluate \`(3 + 4) * 2\`.
- **Backtracking** — DFS graph traversal uses a stack (either explicit or via recursion).

### Your Task

Implement a \`Stack\` class with \`push\`, \`pop\`, \`peek\`, \`isEmpty\`, and \`size\` methods.`,

	starterCode: `class Stack {
	constructor() {
		// Initialize storage
	}

	push(item) {
		// Add item to top
	}

	pop() {
		// Remove and return top item
	}

	peek() {
		// Return top item without removing
	}

	isEmpty() {
		// Return true if stack is empty
	}

	size() {
		// Return number of items
	}
}

const s = new Stack();
s.push(10);
s.push(20);
s.push(30);
console.log(s.pop());
console.log(s.peek());
console.log(s.size());
console.log(s.isEmpty());
`,

	solution: `class Stack {
	constructor() {
		this.items = [];
	}

	push(item) {
		this.items.push(item);
	}

	pop() {
		return this.items.pop();
	}

	peek() {
		return this.items[this.items.length - 1];
	}

	isEmpty() {
		return this.items.length === 0;
	}

	size() {
		return this.items.length;
	}
}

const s = new Stack();
s.push(10);
s.push(20);
s.push(30);
console.log(s.pop());
console.log(s.peek());
console.log(s.size());
console.log(s.isEmpty());
`,

	tests: [
		{
			name: "pop returns 30, peek returns 20, size is 2, isEmpty is false",
			expected: "30\n20\n2\nfalse\n",
		},
		{
			name: "isEmpty returns true on empty stack",
			code: `{{FUNC}}
const s = new Stack();
console.log(s.isEmpty());`,
			expected: "true\n",
		},
		{
			name: "LIFO order",
			code: `{{FUNC}}
const s = new Stack();
s.push("a");
s.push("b");
s.push("c");
console.log(s.pop());
console.log(s.pop());
console.log(s.pop());`,
			expected: "c\nb\na\n",
		},
		{
			name: "size tracks correctly",
			code: `{{FUNC}}
const s = new Stack();
console.log(s.size());
s.push(1);
s.push(2);
console.log(s.size());
s.pop();
console.log(s.size());`,
			expected: "0\n2\n1\n",
		},
	],
};
