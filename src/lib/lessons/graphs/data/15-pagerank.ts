import type { Lesson } from "../../types";

export const pagerank: Lesson = {
	id: "pagerank",
	title: "PageRank",
	chapterId: "advanced-algorithms",
	content: `## PageRank

PageRank is the algorithm that made Google. It measures the importance of nodes in a graph based on the structure of incoming links — a node is important if important nodes link to it.

### The Formula

\`\`\`
PR(u) = (1 - d) / N  +  d * Σ PR(v) / out_degree(v)
                          v → u
\`\`\`

- \`d\` = damping factor (usually 0.85) — probability of following a link
- \`(1 - d) / N\` = probability of jumping to a random node
- The sum is over all nodes \`v\` that link to \`u\`

### Iterative Computation

Repeat until convergence (or for a fixed number of iterations):

\`\`\`python
def page_rank(graph, iterations=20, damping=0.85):
    n = len(graph)
    rank = {node: 1.0 / n for node in graph}

    for _ in range(iterations):
        new_rank = {}
        for node in graph:
            score = (1 - damping) / n
            for src in graph:
                if node in graph[src] and len(graph[src]) > 0:
                    score += damping * rank[src] / len(graph[src])
            new_rank[node] = score
        rank = new_rank

    return rank
\`\`\`

### Intuition

- Nodes with **many incoming links** get high rank
- Links from **high-rank nodes** are worth more than links from low-rank nodes
- The damping factor models a random surfer who occasionally jumps to a random page

### Your Task

Implement \`page_rank(graph, iterations, damping)\` where \`graph\` is a dict \`{node: [outgoing neighbors]}\`. Return a dict of \`{node: rank}\` scores.`,

	starterCode: `def page_rank(graph, iterations=20, damping=0.85):
    n = len(graph)
    rank = {node: 1.0 / n for node in graph}

    for _ in range(iterations):
        new_rank = {}
        for node in graph:
            score = (1 - damping) / n
            for src in graph:
                if node in graph[src] and len(graph[src]) > 0:
                    score += damping * rank[src] / len(graph[src])
            new_rank[node] = score
        rank = new_rank

    return rank

# Star: node 0 receives all links
g = {0: [], 1: [0], 2: [0], 3: [0]}
rank = page_rank(g, iterations=30)
print(max(rank, key=rank.get))

# Cycle: all equal
g2 = {0: [1], 1: [2], 2: [0]}
rank2 = page_rank(g2, iterations=50)
print(round(rank2[0], 2))
`,

	solution: `def page_rank(graph, iterations=20, damping=0.85):
    n = len(graph)
    rank = {node: 1.0 / n for node in graph}

    for _ in range(iterations):
        new_rank = {}
        for node in graph:
            score = (1 - damping) / n
            for src in graph:
                if node in graph[src] and len(graph[src]) > 0:
                    score += damping * rank[src] / len(graph[src])
            new_rank[node] = score
        rank = new_rank

    return rank

g = {0: [], 1: [0], 2: [0], 3: [0]}
rank = page_rank(g, iterations=30)
print(max(rank, key=rank.get))

g2 = {0: [1], 1: [2], 2: [0]}
rank2 = page_rank(g2, iterations=50)
print(round(rank2[0], 2))
`,

	tests: [
		{
			name: "hub node has highest rank",
			code: `{{FUNC}}
g = {0: [], 1: [0], 2: [0], 3: [0]}
rank = page_rank(g, iterations=30)
print(max(rank, key=rank.get))`,
			expected: "0\n",
		},
		{
			name: "symmetric cycle → equal ranks",
			code: `{{FUNC}}
g = {0: [1], 1: [2], 2: [0]}
rank = page_rank(g, iterations=50)
print(round(rank[0], 2))
print(round(rank[1], 2))`,
			expected: "0.33\n0.33\n",
		},
		{
			name: "returns all nodes",
			code: `{{FUNC}}
g = {0: [1], 1: [2], 2: [0]}
rank = page_rank(g)
print(sorted(rank.keys()))`,
			expected: "[0, 1, 2]\n",
		},
		{
			name: "ranks sum to approximately 1",
			code: `{{FUNC}}
g = {0: [1], 1: [2], 2: [0]}
rank = page_rank(g, iterations=100)
print(round(sum(rank.values()), 1))`,
			expected: "1.0\n",
		},
	],
};
