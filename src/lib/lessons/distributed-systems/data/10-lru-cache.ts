import type { Lesson } from "../../types";

export const lruCache: Lesson = {
	id: "lru-cache",
	title: "LRU Cache",
	chapterId: "fault-tolerance",
	content: `## LRU Cache

A **Least Recently Used (LRU) Cache** is a fixed-capacity cache that evicts the least recently used item when it is full. This is one of the most important data structures in distributed systems.

### Why LRU?

Caches hold a small amount of frequently used data to avoid expensive operations (database queries, API calls, disk reads). LRU is based on temporal locality: if you accessed something recently, you are likely to access it again soon.

### Implementation Strategy

The classic LRU cache is implemented with two data structures:

1. **HashMap** — for O(1) lookups by key.
2. **Doubly Linked List** — to track recency. Most recent at front, least recent at back.

In JavaScript, a **Map** preserves insertion order and provides O(1) access — making it perfect for LRU:

\`\`\`js
class LRUCache {
  constructor(capacity) {
    this.capacity = capacity;
    this.cache = new Map();
  }

  get(key) {
    if (!this.cache.has(key)) return -1;
    const value = this.cache.get(key);
    // Move to most recent: delete and re-insert
    this.cache.delete(key);
    this.cache.set(key, value);
    return value;
  }

  put(key, value) {
    if (this.cache.has(key)) {
      this.cache.delete(key);  // remove to update position
    } else if (this.cache.size >= this.capacity) {
      // Evict least recently used (first entry in Map)
      const lruKey = this.cache.keys().next().value;
      this.cache.delete(lruKey);
    }
    this.cache.set(key, value);
  }
}
\`\`\`

**Time complexity:** O(1) for both \`get\` and \`put\`.

### Real-World Uses

- **CPU L1/L2/L3 caches** — hardware implements LRU (or approximations of it).
- **Database buffer pools** — MySQL and PostgreSQL use LRU to manage page caches.
- **Redis** — can be configured with LRU or LFU eviction policies.
- **CDN edge servers** — cached content is evicted LRU when storage is full.
- **Browser cache** — tab memory management.

### Your Task

Implement an \`LRUCache\` class with \`get(key)\` (returns value or \`-1\`) and \`put(key, value)\` methods. The cache should have a fixed \`capacity\` and evict the least recently used item when full.`,

	starterCode: `class LRUCache {
	constructor(capacity) {
		this.capacity = capacity;
		this.cache = new Map();
	}

	get(key) {
		// Return -1 if not found
		// Otherwise move to most recent and return value
	}

	put(key, value) {
		// If key exists, remove it (to update position)
		// If at capacity, evict the least recently used (first Map entry)
		// Insert the new key-value
	}
}

const cache = new LRUCache(3);
cache.put("a", 1);
cache.put("b", 2);
cache.put("c", 3);
console.log(cache.get("a")); // 1  (a is now most recent)
cache.put("d", 4);           // evicts "b" (least recently used)
console.log(cache.get("b")); // -1 (evicted)
console.log(cache.get("c")); // 3
console.log(cache.get("d")); // 4
`,

	solution: `class LRUCache {
	constructor(capacity) {
		this.capacity = capacity;
		this.cache = new Map();
	}

	get(key) {
		if (!this.cache.has(key)) return -1;
		const value = this.cache.get(key);
		this.cache.delete(key);
		this.cache.set(key, value);
		return value;
	}

	put(key, value) {
		if (this.cache.has(key)) {
			this.cache.delete(key);
		} else if (this.cache.size >= this.capacity) {
			const lruKey = this.cache.keys().next().value;
			this.cache.delete(lruKey);
		}
		this.cache.set(key, value);
	}
}

const cache = new LRUCache(3);
cache.put("a", 1);
cache.put("b", 2);
cache.put("c", 3);
console.log(cache.get("a"));
cache.put("d", 4);
console.log(cache.get("b"));
console.log(cache.get("c"));
console.log(cache.get("d"));
`,

	tests: [
		{
			name: "get returns 1, evicts b, b returns -1, c and d return values",
			expected: "1\n-1\n3\n4\n",
		},
		{
			name: "capacity of 1 evicts on every put",
			code: `{{FUNC}}
const c = new LRUCache(1);
c.put("x", 10);
c.put("y", 20);
console.log(c.get("x"));
console.log(c.get("y"));`,
			expected: "-1\n20\n",
		},
		{
			name: "get updates recency",
			code: `{{FUNC}}
const c = new LRUCache(2);
c.put("a", 1);
c.put("b", 2);
c.get("a");    // a is now most recent
c.put("c", 3); // evicts b (not a)
console.log(c.get("a"));
console.log(c.get("b"));`,
			expected: "1\n-1\n",
		},
		{
			name: "put overwrites existing key",
			code: `{{FUNC}}
const c = new LRUCache(2);
c.put("k", 1);
c.put("k", 99);
console.log(c.get("k"));`,
			expected: "99\n",
		},
	],
};
