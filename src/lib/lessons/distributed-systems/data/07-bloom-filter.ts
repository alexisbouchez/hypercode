import type { Lesson } from "../../types";

export const bloomFilter: Lesson = {
	id: "bloom-filter",
	title: "Bloom Filter",
	chapterId: "probabilistic",
	content: `## Bloom Filter

A Bloom Filter is a space-efficient probabilistic data structure that answers: **"Is this element in the set?"**

- If it says **NO**, the element is definitely not in the set.
- If it says **YES**, the element is **probably** in the set (false positives are possible).

No false negatives. Possible false positives. No storage of actual elements.

### How It Works

A Bloom filter is a **bit array** of size \`m\`, initialized to all zeros. You also have \`k\` independent hash functions.

**Adding an element:**
1. Hash the element with each of the \`k\` hash functions.
2. Set bits at each of the \`k\` positions to 1.

**Checking membership:**
1. Hash the element with each of the \`k\` hash functions.
2. If ALL \`k\` positions are 1, the element is **probably** in the set.
3. If ANY position is 0, the element is **definitely not** in the set.

\`\`\`js
class BloomFilter {
  constructor(size) {
    this.size = size;
    this.bits = new Array(size).fill(0);
  }

  // Simple hash functions using different multipliers
  hash1(str) {
    let h = 0;
    for (let i = 0; i < str.length; i++) h = (h * 31 + str.charCodeAt(i)) >>> 0;
    return h % this.size;
  }

  hash2(str) {
    let h = 5381;
    for (let i = 0; i < str.length; i++) h = (h * 33 + str.charCodeAt(i)) >>> 0;
    return h % this.size;
  }

  hash3(str) {
    let h = 0;
    for (let i = 0; i < str.length; i++) h = (h * 37 + str.charCodeAt(i)) >>> 0;
    return h % this.size;
  }

  add(item) {
    this.bits[this.hash1(item)] = 1;
    this.bits[this.hash2(item)] = 1;
    this.bits[this.hash3(item)] = 1;
  }

  mightContain(item) {
    return this.bits[this.hash1(item)] === 1
        && this.bits[this.hash2(item)] === 1
        && this.bits[this.hash3(item)] === 1;
  }
}
\`\`\`

### Real-World Uses

- **Google Chrome** — checks if a URL is malicious before sending it to Google's servers (saves bandwidth for the 99.9% of safe URLs).
- **Apache Cassandra** — avoids disk lookups for keys that definitely do not exist.
- **Medium** — tracks which articles each user has already seen.
- **Bitcoin** — SPV clients use Bloom filters to request only relevant transactions.

### Your Task

Implement a \`BloomFilter\` class using the three hash functions provided. Add \`add(item)\` and \`mightContain(item)\` methods.`,

	starterCode: `class BloomFilter {
	constructor(size) {
		this.size = size;
		this.bits = new Array(size).fill(0);
	}

	hash1(str) {
		let h = 0;
		for (let i = 0; i < str.length; i++) h = (h * 31 + str.charCodeAt(i)) >>> 0;
		return h % this.size;
	}

	hash2(str) {
		let h = 5381;
		for (let i = 0; i < str.length; i++) h = (h * 33 + str.charCodeAt(i)) >>> 0;
		return h % this.size;
	}

	hash3(str) {
		let h = 0;
		for (let i = 0; i < str.length; i++) h = (h * 37 + str.charCodeAt(i)) >>> 0;
		return h % this.size;
	}

	add(item) {
		// Set the 3 hash positions to 1
	}

	mightContain(item) {
		// Return true only if all 3 hash positions are 1
	}
}

const bf = new BloomFilter(100);
bf.add("apple");
bf.add("banana");
bf.add("cherry");

console.log(bf.mightContain("apple"));
console.log(bf.mightContain("banana"));
console.log(bf.mightContain("grape"));
`,

	solution: `class BloomFilter {
	constructor(size) {
		this.size = size;
		this.bits = new Array(size).fill(0);
	}

	hash1(str) {
		let h = 0;
		for (let i = 0; i < str.length; i++) h = (h * 31 + str.charCodeAt(i)) >>> 0;
		return h % this.size;
	}

	hash2(str) {
		let h = 5381;
		for (let i = 0; i < str.length; i++) h = (h * 33 + str.charCodeAt(i)) >>> 0;
		return h % this.size;
	}

	hash3(str) {
		let h = 0;
		for (let i = 0; i < str.length; i++) h = (h * 37 + str.charCodeAt(i)) >>> 0;
		return h % this.size;
	}

	add(item) {
		this.bits[this.hash1(item)] = 1;
		this.bits[this.hash2(item)] = 1;
		this.bits[this.hash3(item)] = 1;
	}

	mightContain(item) {
		return this.bits[this.hash1(item)] === 1
			&& this.bits[this.hash2(item)] === 1
			&& this.bits[this.hash3(item)] === 1;
	}
}

const bf = new BloomFilter(100);
bf.add("apple");
bf.add("banana");
bf.add("cherry");

console.log(bf.mightContain("apple"));
console.log(bf.mightContain("banana"));
console.log(bf.mightContain("grape"));
`,

	tests: [
		{
			name: "added items return true, non-added returns false",
			expected: "true\ntrue\nfalse\n",
		},
		{
			name: "empty filter returns false",
			code: `{{FUNC}}
const bf = new BloomFilter(200);
console.log(bf.mightContain("anything"));`,
			expected: "false\n",
		},
		{
			name: "added item is always found",
			code: `{{FUNC}}
const bf = new BloomFilter(50);
bf.add("test-key");
console.log(bf.mightContain("test-key"));`,
			expected: "true\n",
		},
	],
};
