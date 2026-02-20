import type { Lesson } from "../../types";

export const hasPath: Lesson = {
	id: "has-path",
	title: "Has Path",
	chapterId: "graph-basics",
	content: `## Has Path

A fundamental graph question: is there a route from node A to node B?

This is solved with a standard BFS or DFS traversal starting from the source, stopping early when the destination is found.

### BFS Approach

\`\`\`python
from collections import deque

def has_path(graph, src, dst):
    if src == dst:
        return True
    seen = {src}
    queue = deque([src])
    while queue:
        node = queue.popleft()
        for nb in graph[node]:
            if nb == dst:
                return True
            if nb not in seen:
                seen.add(nb)
                queue.append(nb)
    return False
\`\`\`

### Disconnected Graphs

If the graph is **disconnected** (not all nodes reachable from each other), BFS/DFS will only explore the component reachable from \`src\`:

\`\`\`
Component 1: 0 — 1      Component 2: 2 — 3

has_path(g, 0, 3)  →  False
has_path(g, 0, 1)  →  True
\`\`\`

### Time and Space

- **Time:** O(V + E) — worst case visits all nodes and edges
- **Space:** O(V) — the seen set

### Your Task

Implement \`has_path(graph, src, dst)\` that returns \`True\` if there is a path from \`src\` to \`dst\`, \`False\` otherwise. Use BFS.`,

	starterCode: `from collections import deque

def has_path(graph, src, dst):
    if src == dst:
        return True
    seen = {src}
    queue = deque([src])
    while queue:
        node = queue.popleft()
        for nb in graph[node]:
            if nb == dst:
                return True
            if nb not in seen:
                # Mark seen and enqueue
                pass
    return False

# Connected component
g = {0: [1, 2], 1: [0, 3], 2: [0], 3: [1]}
print(has_path(g, 0, 3))

# Disconnected
g2 = {0: [1], 1: [0], 2: [3], 3: [2]}
print(has_path(g2, 0, 3))
`,

	solution: `from collections import deque

def has_path(graph, src, dst):
    if src == dst:
        return True
    seen = {src}
    queue = deque([src])
    while queue:
        node = queue.popleft()
        for nb in graph[node]:
            if nb == dst:
                return True
            if nb not in seen:
                seen.add(nb)
                queue.append(nb)
    return False

g = {0: [1, 2], 1: [0, 3], 2: [0], 3: [1]}
print(has_path(g, 0, 3))

g2 = {0: [1], 1: [0], 2: [3], 3: [2]}
print(has_path(g2, 0, 3))
`,

	tests: [
		{
			name: "src == dst → True",
			code: `{{FUNC}}
g = {0: [1], 1: [0]}
print(has_path(g, 0, 0))`,
			expected: "True\n",
		},
		{
			name: "directly connected → True",
			code: `{{FUNC}}
g = {0: [1], 1: [0]}
print(has_path(g, 0, 1))`,
			expected: "True\n",
		},
		{
			name: "path through multiple nodes",
			code: `{{FUNC}}
g = {0: [1, 2], 1: [0, 3], 2: [0], 3: [1]}
print(has_path(g, 0, 3))`,
			expected: "True\n",
		},
		{
			name: "disconnected components → False",
			code: `{{FUNC}}
g = {0: [1], 1: [0], 2: [3], 3: [2]}
print(has_path(g, 0, 3))`,
			expected: "False\n",
		},
	],
};
