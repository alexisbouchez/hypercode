import type { Lesson } from "../../types";

export const unionFind: Lesson = {
	id: "union-find",
	title: "Union-Find",
	chapterId: "data-structures",
	content: `## Union-Find (Disjoint Set Union)

**Union-Find** is a data structure that tracks elements partitioned into disjoint (non-overlapping) sets. It supports two primary operations efficiently:

- **find(x)** — determine which set \`x\` belongs to (returns the root representative).
- **union(x, y)** — merge the sets containing \`x\` and \`y\`.

### Key Optimizations

**Union by Rank**: Always attach the shorter tree under the root of the taller tree. This keeps trees shallow.

**Path Compression**: During \`find\`, make every node on the path point directly to the root. This flattens the structure for future queries.

\`\`\`js
find(x) {
  if (this.parent[x] !== x) {
    this.parent[x] = this.find(this.parent[x]); // path compression
  }
  return this.parent[x];
}

union(x, y) {
  const rootX = this.find(x);
  const rootY = this.find(y);
  if (rootX === rootY) return;
  // union by rank
  if (this.rank[rootX] < this.rank[rootY]) this.parent[rootX] = rootY;
  else if (this.rank[rootX] > this.rank[rootY]) this.parent[rootY] = rootX;
  else { this.parent[rootY] = rootX; this.rank[rootX]++; }
}
\`\`\`

### Complexity

| Operation | Time (amortized) |
|-----------|-----------------|
| find      | O(α(n)) ≈ O(1) |
| union     | O(α(n)) ≈ O(1) |

Where α is the inverse Ackermann function — effectively constant for all practical inputs.

### Real-World Uses

- Network connectivity (are two computers connected?)
- Kruskal's minimum spanning tree algorithm
- Image processing (connected components)
- Social network friend groups

### Your Task

Implement a \`UnionFind\` class with \`find\`, \`union\`, and \`connected\` methods using union by rank and path compression.`,

	starterCode: `class UnionFind {
	constructor(n) {
		this.parent = Array.from({ length: n }, (_, i) => i);
		this.rank = new Array(n).fill(0);
	}

	find(x) {
		// Return root of x with path compression
	}

	union(x, y) {
		// Merge sets of x and y using union by rank
	}

	connected(x, y) {
		// Return true if x and y are in the same set
	}
}

const uf = new UnionFind(5);
uf.union(0, 1);
uf.union(2, 3);
console.log(uf.connected(0, 1));
console.log(uf.connected(0, 2));
uf.union(1, 3);
console.log(uf.connected(0, 2));
`,

	solution: `class UnionFind {
	constructor(n) {
		this.parent = Array.from({ length: n }, (_, i) => i);
		this.rank = new Array(n).fill(0);
	}

	find(x) {
		if (this.parent[x] !== x) {
			this.parent[x] = this.find(this.parent[x]);
		}
		return this.parent[x];
	}

	union(x, y) {
		const rootX = this.find(x);
		const rootY = this.find(y);
		if (rootX === rootY) return;
		if (this.rank[rootX] < this.rank[rootY]) this.parent[rootX] = rootY;
		else if (this.rank[rootX] > this.rank[rootY]) this.parent[rootY] = rootX;
		else { this.parent[rootY] = rootX; this.rank[rootX]++; }
	}

	connected(x, y) {
		return this.find(x) === this.find(y);
	}
}

const uf = new UnionFind(5);
uf.union(0, 1);
uf.union(2, 3);
console.log(uf.connected(0, 1));
console.log(uf.connected(0, 2));
uf.union(1, 3);
console.log(uf.connected(0, 2));
`,

	tests: [
		{
			name: "union and connected basics",
			expected: "true\nfalse\ntrue\n",
		},
		{
			name: "initially all disconnected",
			code: `{{FUNC}}
const uf = new UnionFind(4);
console.log(uf.connected(0, 1));
console.log(uf.connected(1, 2));
console.log(uf.connected(2, 3));`,
			expected: "false\nfalse\nfalse\n",
		},
		{
			name: "transitive connectivity",
			code: `{{FUNC}}
const uf = new UnionFind(6);
uf.union(0, 1);
uf.union(1, 2);
uf.union(3, 4);
uf.union(4, 5);
console.log(uf.connected(0, 2));
console.log(uf.connected(3, 5));
console.log(uf.connected(0, 5));
uf.union(2, 3);
console.log(uf.connected(0, 5));`,
			expected: "true\ntrue\nfalse\ntrue\n",
		},
		{
			name: "self-connected",
			code: `{{FUNC}}
const uf = new UnionFind(3);
console.log(uf.connected(0, 0));
console.log(uf.connected(2, 2));`,
			expected: "true\ntrue\n",
		},
	],
};
