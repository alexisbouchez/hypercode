import type { Lesson } from "../../types";

export const bellmanFord: Lesson = {
	id: "bellman-ford",
	title: "Bellman-Ford",
	chapterId: "shortest-paths",
	content: `## Bellman-Ford Algorithm

Bellman-Ford finds shortest paths from a source like Dijkstra, but handles **negative edge weights**. It also detects **negative-weight cycles** (where the shortest path would be −∞).

### Algorithm

Relax all edges \`n-1\` times (the maximum number of edges in any shortest path without cycles):

\`\`\`python
def bellman_ford(n, edges, src):
    # edges = [(u, v, weight)]
    dist = [float('inf')] * n
    dist[src] = 0

    for _ in range(n - 1):
        for u, v, w in edges:
            if dist[u] + w < dist[v]:
                dist[v] = dist[u] + w

    # Check for negative cycles
    for u, v, w in edges:
        if dist[u] + w < dist[v]:
            return None   # negative cycle detected

    return dist
\`\`\`

### Why n-1 Iterations?

A shortest path in a graph without negative cycles visits at most \`n-1\` edges. After \`n-1\` relaxations, all shortest paths are found. A \`n\`-th relaxation that still improves means there's a negative cycle.

### Dijkstra vs Bellman-Ford

| | Dijkstra | Bellman-Ford |
|--|---------|-------------|
| Negative weights | ❌ No | ✅ Yes |
| Negative cycles | ❌ No | ✅ Detects |
| Time | O((V+E) log V) | O(V·E) |
| Best for | Dense positive | Negative weights |

### Your Task

Implement \`bellman_ford(n, edges, src)\` that returns a list of shortest distances from \`src\` to every node (index = node id), or \`None\` if a negative cycle is detected.`,

	starterCode: `def bellman_ford(n, edges, src):
    # edges = [(u, v, weight)]
    dist = [float('inf')] * n
    dist[src] = 0

    for _ in range(n - 1):
        for u, v, w in edges:
            if dist[u] + w < dist[v]:
                dist[v] = dist[u] + w

    # Check for negative cycles
    for u, v, w in edges:
        if dist[u] + w < dist[v]:
            return None

    return dist

edges = [(0,1,4),(0,2,1),(2,1,2),(1,3,1)]
d = bellman_ford(4, edges, 0)
print(d[1], d[2], d[3])

# Negative weight (no cycle)
edges2 = [(0,1,5),(1,2,-2),(0,2,3)]
d2 = bellman_ford(3, edges2, 0)
print(d2[2])
`,

	solution: `def bellman_ford(n, edges, src):
    dist = [float('inf')] * n
    dist[src] = 0

    for _ in range(n - 1):
        for u, v, w in edges:
            if dist[u] + w < dist[v]:
                dist[v] = dist[u] + w

    for u, v, w in edges:
        if dist[u] + w < dist[v]:
            return None

    return dist

edges = [(0,1,4),(0,2,1),(2,1,2),(1,3,1)]
d = bellman_ford(4, edges, 0)
print(d[1], d[2], d[3])

edges2 = [(0,1,5),(1,2,-2),(0,2,3)]
d2 = bellman_ford(3, edges2, 0)
print(d2[2])
`,

	tests: [
		{
			name: "distance to source is 0",
			code: `{{FUNC}}
edges = [(0,1,4),(0,2,1),(2,1,2),(1,3,1)]
d = bellman_ford(4, edges, 0)
print(d[0])`,
			expected: "0\n",
		},
		{
			name: "shortest path via intermediate node",
			code: `{{FUNC}}
edges = [(0,1,4),(0,2,1),(2,1,2),(1,3,1)]
d = bellman_ford(4, edges, 0)
print(d[1])`,
			expected: "3\n",
		},
		{
			name: "handles negative weight edge",
			code: `{{FUNC}}
edges = [(0,1,5),(1,2,-2),(0,2,3)]
d = bellman_ford(3, edges, 0)
print(d[2])`,
			expected: "3\n",
		},
		{
			name: "negative cycle → None",
			code: `{{FUNC}}
edges = [(0,1,1),(1,2,-3),(2,0,1)]
print(bellman_ford(3, edges, 0))`,
			expected: "None\n",
		},
	],
};
