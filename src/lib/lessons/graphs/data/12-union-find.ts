import type { Lesson } from "../../types";

export const unionFind: Lesson = {
	id: "union-find",
	title: "Union-Find",
	chapterId: "advanced-algorithms",
	content: `## Union-Find (Disjoint Set Union)

Union-Find is a data structure that tracks a partition of elements into disjoint sets. It supports two operations:

- **find(x):** which set does x belong to? (returns the set's representative)
- **union(a, b):** merge the sets containing a and b

### Implementation

\`\`\`python
class UnionFind:
    def __init__(self, n):
        self.parent = list(range(n))  # each node is its own parent
        self.rank = [0] * n

    def find(self, x):
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])  # path compression
        return self.parent[x]

    def union(self, a, b):
        ra, rb = self.find(a), self.find(b)
        if ra == rb:
            return False   # already in the same set
        if self.rank[ra] < self.rank[rb]:
            ra, rb = rb, ra
        self.parent[rb] = ra
        if self.rank[ra] == self.rank[rb]:
            self.rank[ra] += 1
        return True

    def connected(self, a, b):
        return self.find(a) == self.find(b)
\`\`\`

### Two Optimizations

**Path Compression:** \`find\` flattens the tree as it walks up — every node on the path points directly to the root after the call.

**Union by Rank:** always attach the smaller tree under the larger one to keep the tree shallow.

With both: nearly O(1) amortized per operation (O(α(n)) — inverse Ackermann).

### Applications

- Kruskal's MST
- Cycle detection
- Network connectivity
- Percolation theory

### Your Task

Implement the \`UnionFind\` class with \`find(x)\`, \`union(a, b)\`, and \`connected(a, b)\` methods using path compression and union by rank.`,

	starterCode: `class UnionFind:
    def __init__(self, n):
        self.parent = list(range(n))
        self.rank = [0] * n

    def find(self, x):
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])
        return self.parent[x]

    def union(self, a, b):
        ra, rb = self.find(a), self.find(b)
        if ra == rb:
            return False
        if self.rank[ra] < self.rank[rb]:
            ra, rb = rb, ra
        self.parent[rb] = ra
        if self.rank[ra] == self.rank[rb]:
            self.rank[ra] += 1
        return True

    def connected(self, a, b):
        return self.find(a) == self.find(b)

uf = UnionFind(5)
print(uf.connected(0, 1))
uf.union(0, 1)
uf.union(1, 2)
print(uf.connected(0, 2))
print(uf.connected(0, 3))
`,

	solution: `class UnionFind:
    def __init__(self, n):
        self.parent = list(range(n))
        self.rank = [0] * n

    def find(self, x):
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])
        return self.parent[x]

    def union(self, a, b):
        ra, rb = self.find(a), self.find(b)
        if ra == rb:
            return False
        if self.rank[ra] < self.rank[rb]:
            ra, rb = rb, ra
        self.parent[rb] = ra
        if self.rank[ra] == self.rank[rb]:
            self.rank[ra] += 1
        return True

    def connected(self, a, b):
        return self.find(a) == self.find(b)

uf = UnionFind(5)
print(uf.connected(0, 1))
uf.union(0, 1)
uf.union(1, 2)
print(uf.connected(0, 2))
print(uf.connected(0, 3))
`,

	tests: [
		{
			name: "initially not connected",
			code: `{{FUNC}}
uf = UnionFind(4)
print(uf.connected(0, 1))`,
			expected: "False\n",
		},
		{
			name: "connected after union",
			code: `{{FUNC}}
uf = UnionFind(4)
uf.union(0, 1)
print(uf.connected(0, 1))`,
			expected: "True\n",
		},
		{
			name: "transitive connectivity",
			code: `{{FUNC}}
uf = UnionFind(4)
uf.union(0, 1)
uf.union(1, 2)
print(uf.connected(0, 2))`,
			expected: "True\n",
		},
		{
			name: "union returns False for same set",
			code: `{{FUNC}}
uf = UnionFind(3)
uf.union(0, 1)
print(uf.union(0, 1))`,
			expected: "False\n",
		},
	],
};
