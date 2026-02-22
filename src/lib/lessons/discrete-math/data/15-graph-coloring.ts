import type { Lesson } from "../../types";

export const graphColoring: Lesson = {
	id: "graph-coloring",
	title: "Graph Coloring & Planar Graphs",
	chapterId: "graph-theory",
	content: `## Coloring and Planarity

### Graph Coloring

A **proper coloring** assigns colors to vertices so that no two adjacent vertices share a color. The **chromatic number** $\\chi(G)$ is the minimum number of colors needed.

- $K_n$ requires $n$ colors: $\\chi(K_n) = n$
- **Bipartite graphs**: $\\chi = 2$ (two-colorable). A graph is bipartite iff it has no odd cycles.
- **Four Color Theorem** (1976): every planar graph satisfies $\\chi(G) \\leq 4$.
- **Greedy coloring** gives an upper bound: $\\chi(G) \\leq \\Delta(G) + 1$ where $\\Delta$ is the maximum degree.

\`\`\`python
def greedy_coloring(adj_list):
    colors = {}
    for v in sorted(adj_list.keys()):
        used = {colors[u] for u in adj_list[v] if u in colors}
        c = 0
        while c in used:
            c += 1
        colors[v] = c
    return len(set(colors.values()))

# K3 needs 3 colors
print(greedy_coloring({0:[1,2], 1:[0,2], 2:[0,1]}))  # 3
\`\`\`

### Planar Graphs and Euler's Formula

A graph is **planar** if it can be drawn in the plane without edge crossings. For any connected planar graph:
$$V - E + F = 2$$
where $F$ is the number of faces (including the outer face). This is **Euler's polyhedron formula**.

**Example**: cube has $V=8$, $E=12$, $F=6$: $8 - 12 + 6 = 2$ ✓

### Your Task

Implement \`greedy_coloring\` and \`euler_formula_check\`.`,

	starterCode: `def greedy_coloring(adj_list):
    colors = {}
    for v in sorted(adj_list.keys()):
        used = {colors[u] for u in adj_list[v] if u in colors}
        c = 0
        while c in used:
            c += 1
        colors[v] = c
    return len(set(colors.values()))

def euler_formula_check(V, E, F):
    # Return True if V - E + F == 2
    pass

print(greedy_coloring({0: [1,2], 1: [0,2], 2: [0,1]}))  # 3
`,

	solution: `def greedy_coloring(adj_list):
    colors = {}
    for v in sorted(adj_list.keys()):
        used = {colors[u] for u in adj_list[v] if u in colors}
        c = 0
        while c in used:
            c += 1
        colors[v] = c
    return len(set(colors.values()))

def euler_formula_check(V, E, F):
    return V - E + F == 2

print(greedy_coloring({0: [1,2], 1: [0,2], 2: [0,1]}))
`,

	tests: [
		{
			name: "K3 needs 3 colors (triangle)",
			expected: "3\n",
		},
		{
			name: "C4 needs 2 colors (bipartite cycle)",
			code: `{{FUNC}}
print(greedy_coloring({0: [1,3], 1: [0,2], 2: [1,3], 3: [2,0]}))`,
			expected: "2\n",
		},
		{
			name: "Euler formula: tetrahedron V=4, E=6, F=4",
			code: `{{FUNC}}
print(euler_formula_check(4, 6, 4))`,
			expected: "True\n",
		},
		{
			name: "Euler formula: cube V=8, E=12, F=6",
			code: `{{FUNC}}
print(euler_formula_check(8, 12, 6))`,
			expected: "True\n",
		},
	],
};
