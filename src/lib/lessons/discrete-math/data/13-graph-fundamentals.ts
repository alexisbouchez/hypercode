import type { Lesson } from "../../types";

export const graphFundamentals: Lesson = {
	id: "graph-fundamentals",
	title: "Graph Fundamentals",
	chapterId: "graph-theory",
	content: `## Graphs: Structure Without Position

A **graph** $G = (V, E)$ consists of a set of **vertices** $V$ and **edges** $E \\subseteq V \\times V$. Graphs model pairwise relationships: social networks, road maps, circuit boards, dependencies.

### Terminology

- **Degree** $\\deg(v)$: number of edges incident to $v$
- **Degree sequence**: sorted list of all vertex degrees
- **Simple graph**: no self-loops, no multi-edges
- **Complete graph** $K_n$: every pair of vertices connected; $|E| = \\binom{n}{2}$

### Handshaking Lemma

$$\\sum_{v \\in V} \\deg(v) = 2|E|$$

*Proof*: each edge contributes 1 to each of its two endpoints' degrees.

**Corollary**: the number of odd-degree vertices is always even.

\`\`\`python
def degree_sequence(adj_list):
    return sorted([len(neighbors) for neighbors in adj_list.values()])

def handshaking_lemma(adj_list):
    degree_sum = sum(len(neighbors) for neighbors in adj_list.values())
    edges = sum(len(n) for n in adj_list.values()) // 2
    return degree_sum == 2 * edges

K4 = {0: [1,2,3], 1: [0,2,3], 2: [0,1,3], 3: [0,1,2]}
print(degree_sequence(K4))     # [3, 3, 3, 3]
print(handshaking_lemma(K4))   # True
\`\`\`

### Your Task

Implement \`degree_sequence\`, \`handshaking_lemma\`, and \`is_simple_graph\`.`,

	starterCode: `def degree_sequence(adj_list):
    # Return sorted list of vertex degrees
    return sorted([len(neighbors) for neighbors in adj_list.values()])

def handshaking_lemma(adj_list):
    # Verify: sum of degrees == 2 * |E|
    degree_sum = sum(len(neighbors) for neighbors in adj_list.values())
    edges = sum(len(n) for n in adj_list.values()) // 2
    return degree_sum == 2 * edges

def is_simple_graph(adj_list):
    # Return False if any self-loop or repeated edge exists
    pass

K4 = {0: [1,2,3], 1: [0,2,3], 2: [0,1,3], 3: [0,1,2]}
print(degree_sequence(K4))
print(handshaking_lemma(K4))
`,

	solution: `def degree_sequence(adj_list):
    return sorted([len(neighbors) for neighbors in adj_list.values()])

def handshaking_lemma(adj_list):
    degree_sum = sum(len(neighbors) for neighbors in adj_list.values())
    edges = sum(len(n) for n in adj_list.values()) // 2
    return degree_sum == 2 * edges

def is_simple_graph(adj_list):
    for v, neighbors in adj_list.items():
        if v in neighbors:
            return False
        if len(neighbors) != len(set(neighbors)):
            return False
    return True

K4 = {0: [1,2,3], 1: [0,2,3], 2: [0,1,3], 3: [0,1,2]}
print(degree_sequence(K4))
print(handshaking_lemma(K4))
`,

	tests: [
		{
			name: "K4 degree sequence = [3,3,3,3]",
			code: `{{FUNC}}
K4 = {0: [1,2,3], 1: [0,2,3], 2: [0,1,3], 3: [0,1,2]}
print(degree_sequence(K4))`,
			expected: "[3, 3, 3, 3]\n",
		},
		{
			name: "handshaking lemma holds for triangle",
			code: `{{FUNC}}
print(handshaking_lemma({0: [1,2], 1: [0,2], 2: [0,1]}))`,
			expected: "True\n",
		},
		{
			name: "sum of K4 degrees = 12",
			code: `{{FUNC}}
K4 = {0: [1,2,3], 1: [0,2,3], 2: [0,1,3], 3: [0,1,2]}
print(sum(degree_sequence(K4)))`,
			expected: "12\n",
		},
		{
			name: "triangle is a simple graph",
			code: `{{FUNC}}
print(is_simple_graph({0: [1,2], 1: [0,2], 2: [0,1]}))`,
			expected: "True\n",
		},
	],
};
