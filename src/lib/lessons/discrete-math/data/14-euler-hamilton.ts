import type { Lesson } from "../../types";

export const eulerHamilton: Lesson = {
	id: "euler-hamilton",
	title: "Euler & Hamilton Paths",
	chapterId: "graph-theory",
	content: `## Traversal Problems

### Euler Paths and Circuits

An **Euler path** visits every edge exactly once. An **Euler circuit** is an Euler path that starts and ends at the same vertex.

**Euler's Theorem** (1736, the Königsberg bridge problem):
- A connected graph has an **Euler circuit** iff every vertex has **even degree**.
- A connected graph has an **Euler path** (not circuit) iff exactly **two vertices have odd degree**.

The classic Königsberg bridge graph has four vertices of odd degree (3, 3, 3, 5) — no Euler path exists.

### Hamilton Paths and Cycles

A **Hamilton path** visits every vertex exactly once. A **Hamilton cycle** returns to the start.

Unlike Euler, there is **no simple degree condition** for Hamilton paths. The problem is NP-complete in general (related to the Travelling Salesman Problem).

**Dirac's theorem**: If every vertex of a graph on $n \\geq 3$ vertices has degree $\\geq n/2$, a Hamilton cycle exists.

\`\`\`python
def has_euler_circuit(adj_list):
    return all(len(neighbors) % 2 == 0
               for neighbors in adj_list.values())

# Cycle C4: all degrees 2 → Euler circuit exists
C4 = {0: [1,3], 1: [0,2], 2: [1,3], 3: [2,0]}
print(has_euler_circuit(C4))  # True
\`\`\`

### Your Task

Implement \`has_euler_circuit\`, \`has_euler_path\`, and \`is_connected\`.`,

	starterCode: `def has_euler_circuit(adj_list):
    # True iff every vertex has even degree
    return all(len(neighbors) % 2 == 0
               for neighbors in adj_list.values())

def has_euler_path(adj_list):
    # True iff exactly 2 vertices have odd degree
    odd = sum(1 for neighbors in adj_list.values() if len(neighbors) % 2 != 0)
    return odd == 2

def is_connected(adj_list):
    # BFS/DFS to check that all vertices are reachable from the first
    if not adj_list:
        return True
    start = next(iter(adj_list))
    visited = set()
    stack = [start]
    while stack:
        v = stack.pop()
        if v not in visited:
            visited.add(v)
            for u in adj_list[v]:
                if u not in visited:
                    stack.append(u)
    return visited == set(adj_list.keys())

C4 = {0: [1,3], 1: [0,2], 2: [1,3], 3: [2,0]}
print(has_euler_circuit(C4))  # True
`,

	solution: `def has_euler_circuit(adj_list):
    return all(len(neighbors) % 2 == 0
               for neighbors in adj_list.values())

def has_euler_path(adj_list):
    odd = sum(1 for neighbors in adj_list.values() if len(neighbors) % 2 != 0)
    return odd == 2

def is_connected(adj_list):
    if not adj_list:
        return True
    start = next(iter(adj_list))
    visited = set()
    stack = [start]
    while stack:
        v = stack.pop()
        if v not in visited:
            visited.add(v)
            for u in adj_list[v]:
                if u not in visited:
                    stack.append(u)
    return visited == set(adj_list.keys())

C4 = {0: [1,3], 1: [0,2], 2: [1,3], 3: [2,0]}
print(has_euler_circuit(C4))
`,

	tests: [
		{
			name: "C4 has an Euler circuit (all degrees even)",
			expected: "True\n",
		},
		{
			name: "path graph P3 has Euler path (2 odd-degree vertices)",
			code: `{{FUNC}}
print(has_euler_path({0: [1], 1: [0, 2], 2: [1]}))`,
			expected: "True\n",
		},
		{
			name: "K2,3 has no Euler circuit (A and B have odd degree 3)",
			code: `{{FUNC}}
K23 = {'A': ['C','D','E'], 'B': ['C','D','E'], 'C': ['A','B'], 'D': ['A','B'], 'E': ['A','B']}
print(has_euler_circuit(K23))`,
			expected: "False\n",
		},
		{
			name: "triangle is connected",
			code: `{{FUNC}}
print(is_connected({0: [1,2], 1: [0,2], 2: [0,1]}))`,
			expected: "True\n",
		},
	],
};
