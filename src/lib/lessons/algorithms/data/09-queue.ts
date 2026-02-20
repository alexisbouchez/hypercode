import type { Lesson } from "../../types";

export const queue: Lesson = {
	id: "queue",
	title: "Queue",
	chapterId: "data-structures",
	content: `## Queue

A Queue is a **First In, First Out (FIFO)** data structure. Think of a line at a coffee shop: the first person to arrive is the first to be served.

### Operations

- \`enqueue(item)\` — add an item to the back. O(1)
- \`dequeue()\` — remove and return the item from the front. O(1) amortized
- \`front()\` — return the front item without removing it. O(1)
- \`isEmpty()\` — return true if the queue is empty. O(1)
- \`size()\` — return the number of items. O(1)

\`\`\`js
class Queue {
  constructor() {
    this.items = [];
  }
  enqueue(item) { this.items.push(item); }
  dequeue() { return this.items.shift(); }
  front() { return this.items[0]; }
  isEmpty() { return this.items.length === 0; }
  size() { return this.items.length; }
}

const q = new Queue();
q.enqueue("Alice");
q.enqueue("Bob");
q.enqueue("Carol");
console.log(q.dequeue());  // Alice
console.log(q.front());    // Bob
console.log(q.size());     // 2
\`\`\`

### Note on Performance

Using an array with \`shift()\` is O(n) because all elements shift left. For performance-critical queues, use a doubly-linked list or a circular buffer so both enqueue and dequeue are truly O(1). For typical use cases, the array implementation is fine.

### Real-World Uses

- **BFS traversal** — breadth-first search uses a queue to process nodes level by level.
- **Task queues** — job schedulers process tasks in the order they arrive.
- **Print spoolers** — documents print in the order they were submitted.
- **Message queues** — Kafka, RabbitMQ, and SQS all implement queues for asynchronous messaging.

### Your Task

Implement a \`Queue\` class with \`enqueue\`, \`dequeue\`, \`front\`, \`isEmpty\`, and \`size\` methods.`,

	starterCode: `class Queue {
	constructor() {
		// Initialize storage
	}

	enqueue(item) {
		// Add item to the back
	}

	dequeue() {
		// Remove and return item from the front
	}

	front() {
		// Return front item without removing
	}

	isEmpty() {
		// Return true if queue is empty
	}

	size() {
		// Return number of items
	}
}

const q = new Queue();
q.enqueue("task-1");
q.enqueue("task-2");
q.enqueue("task-3");
console.log(q.dequeue());
console.log(q.front());
console.log(q.size());
console.log(q.isEmpty());
`,

	solution: `class Queue {
	constructor() {
		this.items = [];
	}

	enqueue(item) {
		this.items.push(item);
	}

	dequeue() {
		return this.items.shift();
	}

	front() {
		return this.items[0];
	}

	isEmpty() {
		return this.items.length === 0;
	}

	size() {
		return this.items.length;
	}
}

const q = new Queue();
q.enqueue("task-1");
q.enqueue("task-2");
q.enqueue("task-3");
console.log(q.dequeue());
console.log(q.front());
console.log(q.size());
console.log(q.isEmpty());
`,

	tests: [
		{
			name: "dequeues task-1, front is task-2, size is 2, not empty",
			expected: "task-1\ntask-2\n2\nfalse\n",
		},
		{
			name: "FIFO order",
			code: `{{FUNC}}
const q = new Queue();
q.enqueue(1);
q.enqueue(2);
q.enqueue(3);
console.log(q.dequeue());
console.log(q.dequeue());
console.log(q.dequeue());`,
			expected: "1\n2\n3\n",
		},
		{
			name: "isEmpty on empty queue",
			code: `{{FUNC}}
const q = new Queue();
console.log(q.isEmpty());`,
			expected: "true\n",
		},
		{
			name: "size tracks correctly",
			code: `{{FUNC}}
const q = new Queue();
console.log(q.size());
q.enqueue("a");
q.enqueue("b");
console.log(q.size());
q.dequeue();
console.log(q.size());`,
			expected: "0\n2\n1\n",
		},
	],
};
