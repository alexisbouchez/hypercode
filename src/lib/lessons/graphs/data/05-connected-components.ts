import type { Lesson } from "../../types";

export const connectedComponents: Lesson = {
	id: "connected-components",
	title: "Connected Components",
	chapterId: "graph-properties",
	content: `## Connected Components

A **connected component** is a maximal set of nodes where every pair has a path between them. A connected graph has exactly one component.

\`\`\`
Graph: 0 — 1     2 — 3     4

Components: {0,1}, {2,3}, {4}
count = 3
\`\`\`

### Algorithm

Iterate over all nodes. For each **unvisited** node, start a BFS/DFS — this discovers one complete component. Count how many times you start a new traversal.

\`\`\`python
def count_components(n, edges):
    graph = {i: [] for i in range(n)}
    for u, v in edges:
        graph[u].append(v)
        graph[v].append(u)

    seen = set()
    count = 0

    def visit(node):
        seen.add(node)
        for nb in graph[node]:
            if nb not in seen:
                visit(nb)

    for node in range(n):
        if node not in seen:
            count += 1
            visit(node)

    return count
\`\`\`

### Applications

- Network connectivity analysis
- Image segmentation (connected pixels)
- Finding islands in a grid
- Social network clustering

### Your Task

Implement \`count_components(n, edges)\` that returns the number of connected components in an undirected graph with \`n\` nodes.`,

	starterCode: `def count_components(n, edges):
    graph = {i: [] for i in range(n)}
    for u, v in edges:
        graph[u].append(v)
        graph[v].append(u)

    seen = set()
    count = 0

    def visit(node):
        seen.add(node)
        for nb in graph[node]:
            if nb not in seen:
                visit(nb)

    for node in range(n):
        if node not in seen:
            # Start a new component
            pass

    return count

print(count_components(5, [(0,1),(1,2),(3,4)]))
print(count_components(4, [(0,1),(1,2),(2,3)]))
print(count_components(4, []))
`,

	solution: `def count_components(n, edges):
    graph = {i: [] for i in range(n)}
    for u, v in edges:
        graph[u].append(v)
        graph[v].append(u)

    seen = set()
    count = 0

    def visit(node):
        seen.add(node)
        for nb in graph[node]:
            if nb not in seen:
                visit(nb)

    for node in range(n):
        if node not in seen:
            count += 1
            visit(node)

    return count

print(count_components(5, [(0,1),(1,2),(3,4)]))
print(count_components(4, [(0,1),(1,2),(2,3)]))
print(count_components(4, []))
`,

	tests: [
		{
			name: "two components: {0,1,2} and {3,4}",
			code: `{{FUNC}}
print(count_components(5, [(0,1),(1,2),(3,4)]))`,
			expected: "2\n",
		},
		{
			name: "fully connected → 1",
			code: `{{FUNC}}
print(count_components(4, [(0,1),(1,2),(2,3)]))`,
			expected: "1\n",
		},
		{
			name: "no edges → n components",
			code: `{{FUNC}}
print(count_components(4, []))`,
			expected: "4\n",
		},
		{
			name: "single node → 1",
			code: `{{FUNC}}
print(count_components(1, []))`,
			expected: "1\n",
		},
	],
};
