import type { Lesson } from "../../types";

export const dfs: Lesson = {
	id: "dfs",
	title: "Depth-First Search",
	chapterId: "graph-basics",
	content: `## Depth-First Search (DFS)

DFS explores a graph by going **as deep as possible** before backtracking. It uses a **stack** — either explicit (iterative) or the call stack (recursive).

\`\`\`
Graph: 0 — 1 — 3
       |
       2

DFS from 0: [0, 1, 3, 2]  (dive into 1 fully before visiting 2)
\`\`\`

### Recursive DFS

\`\`\`python
def dfs(graph, start):
    visited = []
    seen = set()

    def _dfs(node):
        seen.add(node)
        visited.append(node)
        for nb in graph[node]:
            if nb not in seen:
                _dfs(nb)

    _dfs(start)
    return visited
\`\`\`

### Properties

| Property | DFS |
|----------|-----|
| Data structure | Stack / recursion |
| Time | O(V + E) |
| Space | O(V) |
| Finds shortest path? | No |
| Order | Deep-first |

### DFS vs BFS

- **BFS** is ideal for finding the **shortest path** in unweighted graphs
- **DFS** is ideal for **cycle detection**, **topological sort**, and **SCC**
- Both visit all nodes in O(V + E)

### Applications

- Cycle detection
- Topological sorting
- Maze solving
- Connected components
- Finding strongly connected components

### Your Task

Implement \`dfs(graph, start)\` that returns all reachable nodes in DFS order using recursion.`,

	starterCode: `def dfs(graph, start):
    visited = []
    seen = set()

    def _dfs(node):
        seen.add(node)
        visited.append(node)
        for nb in graph[node]:
            if nb not in seen:
                _dfs(nb)

    _dfs(start)
    return visited

g = {0: [1, 2], 1: [0, 3], 2: [0, 3], 3: [1, 2]}
print(dfs(g, 0))
`,

	solution: `def dfs(graph, start):
    visited = []
    seen = set()

    def _dfs(node):
        seen.add(node)
        visited.append(node)
        for nb in graph[node]:
            if nb not in seen:
                _dfs(nb)

    _dfs(start)
    return visited

g = {0: [1, 2], 1: [0, 3], 2: [0, 3], 3: [1, 2]}
print(dfs(g, 0))
`,

	tests: [
		{
			name: "DFS starts at start node",
			code: `{{FUNC}}
g = {0: [1, 2], 1: [0, 3], 2: [0, 3], 3: [1, 2]}
result = dfs(g, 0)
print(result[0])`,
			expected: "0\n",
		},
		{
			name: "DFS visits all nodes",
			code: `{{FUNC}}
g = {0: [1, 2], 1: [0, 3], 2: [0, 3], 3: [1, 2]}
print(sorted(dfs(g, 0)))`,
			expected: "[0, 1, 2, 3]\n",
		},
		{
			name: "DFS dives deep: order 0,1,3,2",
			code: `{{FUNC}}
g = {0: [1, 2], 1: [0, 3], 2: [0, 3], 3: [1, 2]}
print(dfs(g, 0))`,
			expected: "[0, 1, 3, 2]\n",
		},
		{
			name: "DFS on linear path",
			code: `{{FUNC}}
g = {0: [1], 1: [0, 2], 2: [1, 3], 3: [2]}
print(dfs(g, 0))`,
			expected: "[0, 1, 2, 3]\n",
		},
	],
};
