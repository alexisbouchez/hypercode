import type { Lesson } from "../../types";

export const bipartite: Lesson = {
	id: "bipartite",
	title: "Bipartite Check",
	chapterId: "graph-properties",
	content: `## Bipartite Graphs

A graph is **bipartite** if its nodes can be split into two groups such that every edge connects a node from group A to a node from group B — never two nodes within the same group.

\`\`\`
Bipartite:         Not bipartite:
A: {0, 2}          Triangle
B: {1, 3}          0 — 1
0 — 1              |   |
|   |              2 ——
2 — 3
\`\`\`

A graph is bipartite if and only if it contains **no odd-length cycles**.

### BFS 2-Coloring

Assign colors 0 and 1 alternately. If two adjacent nodes end up the same color, the graph is not bipartite:

\`\`\`python
from collections import deque

def is_bipartite(graph):
    color = {}
    for start in graph:
        if start in color:
            continue
        color[start] = 0
        queue = deque([start])
        while queue:
            node = queue.popleft()
            for nb in graph[node]:
                if nb not in color:
                    color[nb] = 1 - color[node]  # opposite color
                    queue.append(nb)
                elif color[nb] == color[node]:   # same color → not bipartite
                    return False
    return True
\`\`\`

### Applications

- Job scheduling (workers ↔ tasks)
- Matching problems (e.g., marriage problem)
- Testing if a graph has an odd cycle
- 2-colorability (map coloring with 2 colors)

### Your Task

Implement \`is_bipartite(graph)\` using BFS 2-coloring.`,

	starterCode: `from collections import deque

def is_bipartite(graph):
    color = {}
    for start in graph:
        if start in color:
            continue
        color[start] = 0
        queue = deque([start])
        while queue:
            node = queue.popleft()
            for nb in graph[node]:
                if nb not in color:
                    color[nb] = 1 - color[node]
                    queue.append(nb)
                elif color[nb] == color[node]:
                    return False
    return True

# Even cycle (bipartite)
g1 = {0: [1, 3], 1: [0, 2], 2: [1, 3], 3: [2, 0]}
print(is_bipartite(g1))

# Triangle (not bipartite, odd cycle)
g2 = {0: [1, 2], 1: [0, 2], 2: [0, 1]}
print(is_bipartite(g2))
`,

	solution: `from collections import deque

def is_bipartite(graph):
    color = {}
    for start in graph:
        if start in color:
            continue
        color[start] = 0
        queue = deque([start])
        while queue:
            node = queue.popleft()
            for nb in graph[node]:
                if nb not in color:
                    color[nb] = 1 - color[node]
                    queue.append(nb)
                elif color[nb] == color[node]:
                    return False
    return True

g1 = {0: [1, 3], 1: [0, 2], 2: [1, 3], 3: [2, 0]}
print(is_bipartite(g1))

g2 = {0: [1, 2], 1: [0, 2], 2: [0, 1]}
print(is_bipartite(g2))
`,

	tests: [
		{
			name: "square (even cycle) → bipartite",
			code: `{{FUNC}}
g = {0: [1, 3], 1: [0, 2], 2: [1, 3], 3: [2, 0]}
print(is_bipartite(g))`,
			expected: "True\n",
		},
		{
			name: "triangle (odd cycle) → not bipartite",
			code: `{{FUNC}}
g = {0: [1, 2], 1: [0, 2], 2: [0, 1]}
print(is_bipartite(g))`,
			expected: "False\n",
		},
		{
			name: "path graph → bipartite",
			code: `{{FUNC}}
g = {0: [1], 1: [0, 2], 2: [1, 3], 3: [2]}
print(is_bipartite(g))`,
			expected: "True\n",
		},
		{
			name: "K2,2 complete bipartite",
			code: `{{FUNC}}
g = {0: [2, 3], 1: [2, 3], 2: [0, 1], 3: [0, 1]}
print(is_bipartite(g))`,
			expected: "True\n",
		},
	],
};
