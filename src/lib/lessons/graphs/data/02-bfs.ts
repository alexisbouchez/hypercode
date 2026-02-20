import type { Lesson } from "../../types";

export const bfs: Lesson = {
	id: "bfs",
	title: "Breadth-First Search",
	chapterId: "graph-basics",
	content: `## Breadth-First Search (BFS)

BFS explores a graph **level by level** — all nodes at distance 1, then distance 2, and so on. It uses a **queue** (FIFO).

\`\`\`
Graph: 0 — 1 — 3
       |
       2

BFS from 0: [0, 1, 2, 3]
\`\`\`

### Algorithm

\`\`\`python
from collections import deque

def bfs(graph, start):
    visited = []
    seen = {start}
    queue = deque([start])

    while queue:
        node = queue.popleft()   # FIFO
        visited.append(node)
        for nb in graph[node]:
            if nb not in seen:
                seen.add(nb)     # mark before enqueuing!
                queue.append(nb)
    return visited
\`\`\`

**Key:** mark nodes as seen **when enqueuing**, not when visiting. This prevents duplicates in the queue.

### Properties

| Property | BFS |
|----------|-----|
| Data structure | Queue (deque) |
| Time | O(V + E) |
| Space | O(V) |
| Finds shortest path? | **Yes** (unweighted) |
| Order | Level-by-level |

### Applications

- Shortest path in unweighted graphs
- Level-order traversal of trees
- Finding connected components
- Web crawlers, social network analysis

### Your Task

Implement \`bfs(graph, start)\` that returns a list of all reachable nodes in BFS order.`,

	starterCode: `from collections import deque

def bfs(graph, start):
    visited = []
    seen = {start}
    queue = deque([start])
    while queue:
        node = queue.popleft()
        visited.append(node)
        # Enqueue unvisited neighbors
    return visited

g = {0: [1, 2], 1: [0, 3], 2: [0, 3], 3: [1, 2]}
print(bfs(g, 0))
`,

	solution: `from collections import deque

def bfs(graph, start):
    visited = []
    seen = {start}
    queue = deque([start])
    while queue:
        node = queue.popleft()
        visited.append(node)
        for nb in graph[node]:
            if nb not in seen:
                seen.add(nb)
                queue.append(nb)
    return visited

g = {0: [1, 2], 1: [0, 3], 2: [0, 3], 3: [1, 2]}
print(bfs(g, 0))
`,

	tests: [
		{
			name: "BFS starts at start node",
			code: `{{FUNC}}
g = {0: [1, 2], 1: [0, 3], 2: [0, 3], 3: [1, 2]}
result = bfs(g, 0)
print(result[0])`,
			expected: "0\n",
		},
		{
			name: "BFS visits all nodes",
			code: `{{FUNC}}
g = {0: [1, 2], 1: [0, 3], 2: [0, 3], 3: [1, 2]}
print(sorted(bfs(g, 0)))`,
			expected: "[0, 1, 2, 3]\n",
		},
		{
			name: "BFS level order on simple graph",
			code: `{{FUNC}}
g = {0: [1, 2], 1: [0], 2: [0, 3], 3: [2]}
print(bfs(g, 0))`,
			expected: "[0, 1, 2, 3]\n",
		},
		{
			name: "BFS on single node",
			code: `{{FUNC}}
g = {0: []}
print(bfs(g, 0))`,
			expected: "[0]\n",
		},
	],
};
