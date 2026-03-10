import type { Lesson } from "../../types";

export const hashTable: Lesson = {
	id: "hash-table",
	title: "Hash Table",
	chapterId: "data-structures",
	content: `## Hash Table

A Hash Table (or hash map) stores **key-value pairs** and provides near-**O(1)** average time for get, set, and delete operations.

### How It Works

1. A **hash function** converts a key into an integer index.
2. The value is stored in a **bucket** at that index.
3. When two keys hash to the same index, a **collision** occurs.

### Collision Handling: Chaining

The simplest strategy is **separate chaining** — each bucket holds a linked list (or array) of entries that hashed to that index.

\`\`\`
buckets[0] → []
buckets[1] → [["name", "Alice"], ["age", 30]]
buckets[2] → [["city", "Paris"]]
...
\`\`\`

### Hash Function

A simple string hash function sums character codes and takes the modulus of the bucket count:

\`\`\`js
hash(key) {
  let h = 0;
  for (const ch of String(key)) h = (h + ch.charCodeAt(0)) % this.size;
  return h;
}
\`\`\`

### Operations

\`\`\`js
class HashTable {
  constructor(size = 53) {
    this.size = size;
    this.buckets = new Array(size).fill(null).map(() => []);
  }

  hash(key) {
    let h = 0;
    for (const ch of String(key)) h = (h + ch.charCodeAt(0)) % this.size;
    return h;
  }

  set(key, value) {
    const idx = this.hash(key);
    const bucket = this.buckets[idx];
    const entry = bucket.find(e => e[0] === key);
    if (entry) { entry[1] = value; }
    else { bucket.push([key, value]); }
  }

  get(key) {
    const idx = this.hash(key);
    const entry = this.buckets[idx].find(e => e[0] === key);
    return entry ? entry[1] : undefined;
  }

  delete(key) {
    const idx = this.hash(key);
    const bucket = this.buckets[idx];
    const i = bucket.findIndex(e => e[0] === key);
    if (i !== -1) bucket.splice(i, 1);
  }
}
\`\`\`

### Complexity

| Operation | Average | Worst (all collisions) |
|-----------|---------|------------------------|
| set       | O(1)    | O(n)                   |
| get       | O(1)    | O(n)                   |
| delete    | O(1)    | O(n)                   |

### Your Task

Implement a \`HashTable\` class with \`hash\`, \`set\`, \`get\`, and \`delete\` methods using separate chaining.`,

	starterCode: `class HashTable {
	constructor(size = 53) {
		this.size = size;
		this.buckets = new Array(size).fill(null).map(() => []);
	}

	hash(key) {
		// Convert key to bucket index
	}

	set(key, value) {
		// Insert or update key-value pair
	}

	get(key) {
		// Return value for key, or undefined
	}

	delete(key) {
		// Remove key-value pair
	}
}

const ht = new HashTable();
ht.set("name", "Alice");
ht.set("age", 30);
console.log(ht.get("name"));
console.log(ht.get("age"));
ht.set("name", "Bob");
console.log(ht.get("name"));
ht.delete("age");
console.log(ht.get("age"));
`,

	solution: `class HashTable {
	constructor(size = 53) {
		this.size = size;
		this.buckets = new Array(size).fill(null).map(() => []);
	}

	hash(key) {
		let h = 0;
		for (const ch of String(key)) h = (h + ch.charCodeAt(0)) % this.size;
		return h;
	}

	set(key, value) {
		const idx = this.hash(key);
		const bucket = this.buckets[idx];
		const entry = bucket.find(e => e[0] === key);
		if (entry) { entry[1] = value; }
		else { bucket.push([key, value]); }
	}

	get(key) {
		const idx = this.hash(key);
		const entry = this.buckets[idx].find(e => e[0] === key);
		return entry ? entry[1] : undefined;
	}

	delete(key) {
		const idx = this.hash(key);
		const bucket = this.buckets[idx];
		const i = bucket.findIndex(e => e[0] === key);
		if (i !== -1) bucket.splice(i, 1);
	}
}

const ht = new HashTable();
ht.set("name", "Alice");
ht.set("age", 30);
console.log(ht.get("name"));
console.log(ht.get("age"));
ht.set("name", "Bob");
console.log(ht.get("name"));
ht.delete("age");
console.log(ht.get("age"));
`,

	tests: [
		{
			name: "set, get, update, and delete",
			expected: "Alice\n30\nBob\nundefined\n",
		},
		{
			name: "get missing key returns undefined",
			code: `{{FUNC}}
const ht = new HashTable();
console.log(ht.get("missing"));`,
			expected: "undefined\n",
		},
		{
			name: "handles multiple keys",
			code: `{{FUNC}}
const ht = new HashTable();
ht.set("a", 1);
ht.set("b", 2);
ht.set("c", 3);
console.log(ht.get("a"));
console.log(ht.get("b"));
console.log(ht.get("c"));`,
			expected: "1\n2\n3\n",
		},
		{
			name: "delete only removes target key",
			code: `{{FUNC}}
const ht = new HashTable();
ht.set("x", 10);
ht.set("y", 20);
ht.delete("x");
console.log(ht.get("x"));
console.log(ht.get("y"));`,
			expected: "undefined\n20\n",
		},
	],
};
