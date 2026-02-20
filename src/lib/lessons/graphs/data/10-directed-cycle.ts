import type { Lesson } from "../../types";

export const directedCycle: Lesson = {
	id: "directed-cycle",
	title: "Directed Cycle Detection",
	chapterId: "graph-properties",
	content: `## Cycle Detection in Directed Graphs

Detecting cycles in **directed** graphs is more complex than undirected ones. A back edge in undirected DFS always means a cycle — but in directed graphs, we need to distinguish a **back edge** (true cycle) from a **cross edge** (harmless).

### Three-Color DFS

Track each node's state:
- **White (0):** unvisited
- **Gray (1):** currently in the DFS call stack (being explored)
- **Black (2):** fully explored

A cycle exists when we find a **gray neighbor** — a node still on the current path:

\`\`\`python
def has_cycle_directed(n, edges):
    graph = {i: [] for i in range(n)}
    for u, v in edges:
        graph[u].append(v)

    color = [0] * n  # 0=white, 1=gray, 2=black

    def dfs(node):
        color[node] = 1   # gray: in stack
        for nb in graph[node]:
            if color[nb] == 1:   # gray neighbor → back edge → cycle!
                return True
            if color[nb] == 0 and dfs(nb):
                return True
        color[node] = 2   # black: done
        return False

    for node in range(n):
        if color[node] == 0:
            if dfs(node):
                return True
    return False
\`\`\`

### Key Difference vs Undirected

\`\`\`
Directed:  0 → 1 → 2      0 → 1 → 2 → 0
                           No cycle    Cycle (0 turns gray again)

Undirected: 0 — 1 — 2 always has no cycle (it's just a path)
\`\`\`

### Your Task

Implement \`has_cycle_directed(n, edges)\` where \`edges\` is a list of directed edge tuples \`(u, v)\`.`,

	starterCode: `def has_cycle_directed(n, edges):
    graph = {i: [] for i in range(n)}
    for u, v in edges:
        graph[u].append(v)

    color = [0] * n  # 0=white, 1=gray, 2=black

    def dfs(node):
        color[node] = 1
        for nb in graph[node]:
            if color[nb] == 1:
                return True
            if color[nb] == 0 and dfs(nb):
                return True
        color[node] = 2
        return False

    for node in range(n):
        if color[node] == 0:
            if dfs(node):
                return True
    return False

print(has_cycle_directed(3, [(0,1),(1,2)]))
print(has_cycle_directed(3, [(0,1),(1,2),(2,0)]))
print(has_cycle_directed(4, [(0,1),(1,2),(2,3),(3,1)]))
`,

	solution: `def has_cycle_directed(n, edges):
    graph = {i: [] for i in range(n)}
    for u, v in edges:
        graph[u].append(v)

    color = [0] * n

    def dfs(node):
        color[node] = 1
        for nb in graph[node]:
            if color[nb] == 1:
                return True
            if color[nb] == 0 and dfs(nb):
                return True
        color[node] = 2
        return False

    for node in range(n):
        if color[node] == 0:
            if dfs(node):
                return True
    return False

print(has_cycle_directed(3, [(0,1),(1,2)]))
print(has_cycle_directed(3, [(0,1),(1,2),(2,0)]))
print(has_cycle_directed(4, [(0,1),(1,2),(2,3),(3,1)]))
`,

	tests: [
		{
			name: "linear chain → no cycle",
			code: `{{FUNC}}
print(has_cycle_directed(3, [(0,1),(1,2)]))`,
			expected: "False\n",
		},
		{
			name: "0→1→2→0 → cycle",
			code: `{{FUNC}}
print(has_cycle_directed(3, [(0,1),(1,2),(2,0)]))`,
			expected: "True\n",
		},
		{
			name: "back edge in chain",
			code: `{{FUNC}}
print(has_cycle_directed(4, [(0,1),(1,2),(2,3),(3,1)]))`,
			expected: "True\n",
		},
		{
			name: "no edges → no cycle",
			code: `{{FUNC}}
print(has_cycle_directed(4, []))`,
			expected: "False\n",
		},
	],
};
