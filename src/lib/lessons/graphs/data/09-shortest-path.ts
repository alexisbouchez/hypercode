import type { Lesson } from "../../types";

export const shortestPath: Lesson = {
	id: "shortest-path",
	title: "Shortest Path (BFS)",
	chapterId: "shortest-paths",
	content: `## Reconstructing the Shortest Path

Dijkstra finds shortest *distances*, but often you need the actual *path* — the sequence of nodes to traverse.

For **unweighted graphs**, BFS naturally finds the shortest path. For weighted graphs, modify Dijkstra to track predecessors.

### BFS with Parent Tracking

\`\`\`python
from collections import deque

def shortest_path(graph, src, dst):
    if src == dst:
        return [src]
    parent = {src: None}
    queue = deque([src])
    while queue:
        node = queue.popleft()
        for nb in graph[node]:
            if nb not in parent:
                parent[nb] = node
                if nb == dst:
                    # Reconstruct: follow parent pointers back
                    path = []
                    curr = dst
                    while curr is not None:
                        path.append(curr)
                        curr = parent[curr]
                    return path[::-1]
                queue.append(nb)
    return []    # no path found
\`\`\`

### Reconstruction Pattern

The trick is to store **where we came from** (\`parent\` dict). When we reach the destination, we walk backward through the parents and reverse the result:

\`\`\`python
path = []
curr = dst
while curr is not None:
    path.append(curr)
    curr = parent[curr]
return path[::-1]
\`\`\`

### Your Task

Implement \`shortest_path(graph, src, dst)\` that returns the list of nodes on the shortest path (BFS) from \`src\` to \`dst\`. Return an empty list if no path exists.`,

	starterCode: `from collections import deque

def shortest_path(graph, src, dst):
    if src == dst:
        return [src]
    parent = {src: None}
    queue = deque([src])
    while queue:
        node = queue.popleft()
        for nb in graph[node]:
            if nb not in parent:
                parent[nb] = node
                if nb == dst:
                    path = []
                    curr = dst
                    while curr is not None:
                        path.append(curr)
                        curr = parent[curr]
                    return path[::-1]
                queue.append(nb)
    return []

g = {0: [1], 1: [0, 2], 2: [1, 3], 3: [2]}
print(shortest_path(g, 0, 3))

g2 = {0: [1], 1: [0], 2: [3], 3: [2]}
print(shortest_path(g2, 0, 3))
`,

	solution: `from collections import deque

def shortest_path(graph, src, dst):
    if src == dst:
        return [src]
    parent = {src: None}
    queue = deque([src])
    while queue:
        node = queue.popleft()
        for nb in graph[node]:
            if nb not in parent:
                parent[nb] = node
                if nb == dst:
                    path = []
                    curr = dst
                    while curr is not None:
                        path.append(curr)
                        curr = parent[curr]
                    return path[::-1]
                queue.append(nb)
    return []

g = {0: [1], 1: [0, 2], 2: [1, 3], 3: [2]}
print(shortest_path(g, 0, 3))

g2 = {0: [1], 1: [0], 2: [3], 3: [2]}
print(shortest_path(g2, 0, 3))
`,

	tests: [
		{
			name: "path on linear graph",
			code: `{{FUNC}}
g = {0: [1], 1: [0, 2], 2: [1, 3], 3: [2]}
print(shortest_path(g, 0, 3))`,
			expected: "[0, 1, 2, 3]\n",
		},
		{
			name: "src == dst",
			code: `{{FUNC}}
g = {0: [1], 1: [0]}
print(shortest_path(g, 0, 0))`,
			expected: "[0]\n",
		},
		{
			name: "no path → empty list",
			code: `{{FUNC}}
g = {0: [1], 1: [0], 2: [3], 3: [2]}
print(shortest_path(g, 0, 3))`,
			expected: "[]\n",
		},
		{
			name: "path length is 2",
			code: `{{FUNC}}
g = {0: [1, 2], 1: [0, 3], 2: [0, 3], 3: [1, 2]}
path = shortest_path(g, 0, 3)
print(len(path))`,
			expected: "3\n",
		},
	],
};
