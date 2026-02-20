import type { Lesson } from "../../types";

export const adjacencyList: Lesson = {
	id: "adjacency-list",
	title: "Adjacency List",
	chapterId: "graph-basics",
	content: `## Graph Representation: Adjacency List

A graph is a set of **nodes** (vertices) connected by **edges**. The adjacency list is the most common representation — a dictionary mapping each node to its list of neighbors.

\`\`\`
Graph:    0 — 1
          |   |
          2 — 3

Adjacency list:
{0: [1, 2], 1: [0, 3], 2: [0, 3], 3: [1, 2]}
\`\`\`

### Why Adjacency List?

- **Space:** O(V + E) — much better than a matrix for sparse graphs
- **Iteration:** O(degree) to iterate a node's neighbors
- **Check edge:** O(degree) — use a set of neighbors for O(1)

### Building from Edges

\`\`\`python
def build_graph(n, edges):
    graph = {i: [] for i in range(n)}   # n nodes: 0..n-1
    for u, v in edges:
        graph[u].append(v)
        graph[v].append(u)              # undirected
    return graph
\`\`\`

### Directed vs Undirected

\`\`\`python
# Directed: only one direction
graph[u].append(v)

# Undirected: both directions
graph[u].append(v)
graph[v].append(u)
\`\`\`

### Your Task

Implement \`build_graph(n, edges)\` that creates an **undirected** adjacency list for a graph with \`n\` nodes (labeled \`0\` to \`n-1\`) from a list of edge tuples \`(u, v)\`.`,

	starterCode: `def build_graph(n, edges):
    graph = {i: [] for i in range(n)}
    for u, v in edges:
        # Add v to u's neighbors and u to v's neighbors
        pass
    return graph

g = build_graph(4, [(0,1),(0,2),(1,3),(2,3)])
print(sorted(g[0]))
print(sorted(g[3]))
print(len(g))
`,

	solution: `def build_graph(n, edges):
    graph = {i: [] for i in range(n)}
    for u, v in edges:
        graph[u].append(v)
        graph[v].append(u)
    return graph

g = build_graph(4, [(0,1),(0,2),(1,3),(2,3)])
print(sorted(g[0]))
print(sorted(g[3]))
print(len(g))
`,

	tests: [
		{
			name: "node 0 has neighbors 1 and 2",
			code: `{{FUNC}}
g = build_graph(4, [(0,1),(0,2),(1,3),(2,3)])
print(sorted(g[0]))`,
			expected: "[1, 2]\n",
		},
		{
			name: "undirected: 1 in g[0] and 0 in g[1]",
			code: `{{FUNC}}
g = build_graph(3, [(0,1)])
print(1 in g[0])
print(0 in g[1])`,
			expected: "True\nTrue\n",
		},
		{
			name: "graph has n nodes",
			code: `{{FUNC}}
g = build_graph(5, [(0,1)])
print(len(g))`,
			expected: "5\n",
		},
		{
			name: "isolated node has empty neighbors",
			code: `{{FUNC}}
g = build_graph(3, [(0,1)])
print(g[2])`,
			expected: "[]\n",
		},
	],
};
