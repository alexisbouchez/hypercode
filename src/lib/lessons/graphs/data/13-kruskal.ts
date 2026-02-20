import type { Lesson } from "../../types";

export const kruskal: Lesson = {
	id: "kruskal",
	title: "Kruskal's MST",
	chapterId: "advanced-algorithms",
	content: `## Kruskal's Minimum Spanning Tree

A **Minimum Spanning Tree (MST)** connects all nodes with the minimum total edge weight using exactly \`n-1\` edges and no cycles.

\`\`\`
Graph:          MST:
0 -1- 1         0 -1- 1
|     |               |
4     2               2
|     |               |
3 -3- 2         3 -3- 2

MST weight = 1 + 2 + 3 = 6
\`\`\`

### Kruskal's Algorithm

1. Sort all edges by weight (ascending)
2. For each edge \`(u, v, weight)\`:
   - If \`u\` and \`v\` are in **different components** (use Union-Find): add edge to MST
   - Otherwise: skip (would create a cycle)
3. Stop when you have \`n-1\` edges

\`\`\`python
def kruskal_mst(n, edges):
    # edges = [(weight, u, v)]
    parent = list(range(n))

    def find(x):
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    def union(a, b):
        ra, rb = find(a), find(b)
        if ra == rb: return False
        parent[rb] = ra
        return True

    total = 0
    for weight, u, v in sorted(edges):
        if union(u, v):
            total += weight
    return total
\`\`\`

### Complexity

- **Time:** O(E log E) — dominated by sorting
- **Space:** O(V) — the Union-Find structure

### Your Task

Implement \`kruskal_mst(n, edges)\` that returns the **total weight** of the minimum spanning tree. Edges are given as \`(weight, u, v)\` tuples.`,

	starterCode: `def kruskal_mst(n, edges):
    # edges = [(weight, u, v)]
    parent = list(range(n))

    def find(x):
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    def union(a, b):
        ra, rb = find(a), find(b)
        if ra == rb:
            return False
        parent[rb] = ra
        return True

    total = 0
    for weight, u, v in sorted(edges):
        if union(u, v):
            # Add weight to total
            pass
    return total

edges = [(1,0,1),(4,0,3),(2,1,2),(5,1,3),(3,2,3)]
print(kruskal_mst(4, edges))

edges2 = [(1,0,1),(2,1,2),(3,0,2)]
print(kruskal_mst(3, edges2))
`,

	solution: `def kruskal_mst(n, edges):
    parent = list(range(n))

    def find(x):
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    def union(a, b):
        ra, rb = find(a), find(b)
        if ra == rb:
            return False
        parent[rb] = ra
        return True

    total = 0
    for weight, u, v in sorted(edges):
        if union(u, v):
            total += weight
    return total

edges = [(1,0,1),(4,0,3),(2,1,2),(5,1,3),(3,2,3)]
print(kruskal_mst(4, edges))

edges2 = [(1,0,1),(2,1,2),(3,0,2)]
print(kruskal_mst(3, edges2))
`,

	tests: [
		{
			name: "4-node MST weight = 6",
			code: `{{FUNC}}
edges = [(1,0,1),(4,0,3),(2,1,2),(5,1,3),(3,2,3)]
print(kruskal_mst(4, edges))`,
			expected: "6\n",
		},
		{
			name: "triangle MST weight = 3",
			code: `{{FUNC}}
edges = [(1,0,1),(2,1,2),(3,0,2)]
print(kruskal_mst(3, edges))`,
			expected: "3\n",
		},
		{
			name: "single edge",
			code: `{{FUNC}}
print(kruskal_mst(2, [(5,0,1)]))`,
			expected: "5\n",
		},
		{
			name: "picks minimum edges",
			code: `{{FUNC}}
edges = [(10,0,1),(1,0,1),(2,1,2)]
print(kruskal_mst(3, edges))`,
			expected: "3\n",
		},
	],
};
