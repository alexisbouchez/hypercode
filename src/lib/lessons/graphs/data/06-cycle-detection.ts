import type { Lesson } from "../../types";

export const cycleDetection: Lesson = {
	id: "cycle-detection",
	title: "Cycle Detection",
	chapterId: "graph-properties",
	content: `## Cycle Detection (Undirected)

A **cycle** is a path that starts and ends at the same node. Detecting cycles is fundamental to many graph algorithms (e.g., checking if a graph is a tree).

\`\`\`
Tree (no cycle):   Cycle:
    0                0 — 1
    |                |   |
    1                2 — 3
    |
    2
\`\`\`

### DFS with Parent Tracking

In an undirected graph, during DFS, a cycle exists if we encounter an **already-visited neighbor that isn't our immediate parent**.

\`\`\`python
def has_cycle(graph):
    seen = set()

    def dfs(node, parent):
        seen.add(node)
        for nb in graph[node]:
            if nb not in seen:
                if dfs(nb, node):
                    return True
            elif nb != parent:      # visited neighbor ≠ parent → cycle!
                return True
        return False

    for node in graph:
        if node not in seen:
            if dfs(node, -1):
                return True
    return False
\`\`\`

### Why ignore the parent?

In an undirected graph, every edge appears in both directions. When we visit node 1 from node 0, node 0 is in 1's neighbors. That's not a cycle — it's just the edge we came from.

### Your Task

Implement \`has_cycle(graph)\` that returns \`True\` if the undirected graph contains a cycle, \`False\` otherwise. The graph is passed as an adjacency list.`,

	starterCode: `def has_cycle(graph):
    seen = set()

    def dfs(node, parent):
        seen.add(node)
        for nb in graph[node]:
            if nb not in seen:
                if dfs(nb, node):
                    return True
            elif nb != parent:
                return True
        return False

    for node in graph:
        if node not in seen:
            if dfs(node, -1):
                return True
    return False

# No cycle: a tree
g1 = {0: [1, 2], 1: [0, 3], 2: [0], 3: [1]}
print(has_cycle(g1))

# Cycle: 0-1-2-0
g2 = {0: [1, 2], 1: [0, 2], 2: [0, 1]}
print(has_cycle(g2))
`,

	solution: `def has_cycle(graph):
    seen = set()

    def dfs(node, parent):
        seen.add(node)
        for nb in graph[node]:
            if nb not in seen:
                if dfs(nb, node):
                    return True
            elif nb != parent:
                return True
        return False

    for node in graph:
        if node not in seen:
            if dfs(node, -1):
                return True
    return False

g1 = {0: [1, 2], 1: [0, 3], 2: [0], 3: [1]}
print(has_cycle(g1))

g2 = {0: [1, 2], 1: [0, 2], 2: [0, 1]}
print(has_cycle(g2))
`,

	tests: [
		{
			name: "tree graph → no cycle",
			code: `{{FUNC}}
g = {0: [1, 2], 1: [0, 3], 2: [0], 3: [1]}
print(has_cycle(g))`,
			expected: "False\n",
		},
		{
			name: "triangle → cycle",
			code: `{{FUNC}}
g = {0: [1, 2], 1: [0, 2], 2: [0, 1]}
print(has_cycle(g))`,
			expected: "True\n",
		},
		{
			name: "single edge → no cycle",
			code: `{{FUNC}}
g = {0: [1], 1: [0]}
print(has_cycle(g))`,
			expected: "False\n",
		},
		{
			name: "square graph → cycle",
			code: `{{FUNC}}
g = {0: [1, 3], 1: [0, 2], 2: [1, 3], 3: [2, 0]}
print(has_cycle(g))`,
			expected: "True\n",
		},
	],
};
