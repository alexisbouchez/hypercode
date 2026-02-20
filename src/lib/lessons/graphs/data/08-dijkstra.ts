import type { Lesson } from "../../types";

export const dijkstra: Lesson = {
	id: "dijkstra",
	title: "Dijkstra's Algorithm",
	chapterId: "shortest-paths",
	content: `## Dijkstra's Algorithm

Dijkstra's algorithm finds the **shortest path from a source to all other nodes** in a graph with **non-negative weights**. It uses a min-heap (priority queue) for efficiency.

\`\`\`
      4
  0 ——— 1
  |     |
1 |     | 1
  |     |
  2 ——— 3
      2

dijkstra(graph, 0):
  dist[0] = 0
  dist[1] = 3   (0→2→1, cost 1+2=3, faster than direct edge 4)
  dist[2] = 1
  dist[3] = 3   (0→2→3, cost 1+2)
\`\`\`

### Algorithm

\`\`\`python
import heapq

def dijkstra(graph, start):
    # graph = {node: [(neighbor, weight), ...]}
    dist = {node: float('inf') for node in graph}
    dist[start] = 0
    heap = [(0, start)]    # (distance, node)

    while heap:
        d, node = heapq.heappop(heap)
        if d > dist[node]:
            continue        # outdated entry
        for nb, weight in graph[node]:
            new_dist = d + weight
            if new_dist < dist[nb]:
                dist[nb] = new_dist
                heapq.heappush(heap, (new_dist, nb))

    return dist
\`\`\`

### Complexity

- **Time:** O((V + E) log V) with a binary heap
- **Space:** O(V)
- **Limitation:** Doesn't work with negative edge weights (use Bellman-Ford instead)

### Your Task

Implement \`dijkstra(graph, start)\` that returns a dictionary of shortest distances from \`start\` to every other node. The graph is a dict \`{node: [(neighbor, weight), ...]}\`.`,

	starterCode: `import heapq

def dijkstra(graph, start):
    dist = {node: float('inf') for node in graph}
    dist[start] = 0
    heap = [(0, start)]

    while heap:
        d, node = heapq.heappop(heap)
        if d > dist[node]:
            continue
        for nb, weight in graph[node]:
            new_dist = d + weight
            if new_dist < dist[nb]:
                dist[nb] = new_dist
                # Push (new_dist, nb) onto heap
    return dist

g = {
    0: [(1, 4), (2, 1)],
    1: [(3, 1)],
    2: [(1, 2), (3, 5)],
    3: []
}
d = dijkstra(g, 0)
print(d[0], d[1], d[2], d[3])
`,

	solution: `import heapq

def dijkstra(graph, start):
    dist = {node: float('inf') for node in graph}
    dist[start] = 0
    heap = [(0, start)]

    while heap:
        d, node = heapq.heappop(heap)
        if d > dist[node]:
            continue
        for nb, weight in graph[node]:
            new_dist = d + weight
            if new_dist < dist[nb]:
                dist[nb] = new_dist
                heapq.heappush(heap, (new_dist, nb))
    return dist

g = {
    0: [(1, 4), (2, 1)],
    1: [(3, 1)],
    2: [(1, 2), (3, 5)],
    3: []
}
d = dijkstra(g, 0)
print(d[0], d[1], d[2], d[3])
`,

	tests: [
		{
			name: "distance to self is 0",
			code: `{{FUNC}}
g = {0: [(1, 4), (2, 1)], 1: [(3, 1)], 2: [(1, 2), (3, 5)], 3: []}
d = dijkstra(g, 0)
print(d[0])`,
			expected: "0\n",
		},
		{
			name: "shortest path 0→1 via node 2",
			code: `{{FUNC}}
g = {0: [(1, 4), (2, 1)], 1: [(3, 1)], 2: [(1, 2), (3, 5)], 3: []}
d = dijkstra(g, 0)
print(d[1])`,
			expected: "3\n",
		},
		{
			name: "direct edge 0→2 costs 1",
			code: `{{FUNC}}
g = {0: [(1, 4), (2, 1)], 1: [(3, 1)], 2: [(1, 2), (3, 5)], 3: []}
d = dijkstra(g, 0)
print(d[2])`,
			expected: "1\n",
		},
		{
			name: "shortest path 0→3 = 4",
			code: `{{FUNC}}
g = {0: [(1, 4), (2, 1)], 1: [(3, 1)], 2: [(1, 2), (3, 5)], 3: []}
d = dijkstra(g, 0)
print(d[3])`,
			expected: "4\n",
		},
	],
};
