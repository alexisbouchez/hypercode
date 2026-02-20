import type { Lesson } from "../../types";

export const topologicalSort: Lesson = {
	id: "topological-sort",
	title: "Topological Sort",
	chapterId: "graph-properties",
	content: `## Topological Sort

A **topological ordering** of a directed acyclic graph (DAG) lists nodes such that every edge \`u → v\` has \`u\` appearing before \`v\`.

\`\`\`
0 → 1 → 3
↓       ↑
2 ———→
Topological order: [0, 1, 2, 3]
\`\`\`

### Kahn's Algorithm (BFS-based)

1. Compute **in-degree** (number of incoming edges) for each node
2. Start with all nodes of **in-degree 0** (no dependencies)
3. Repeatedly remove a zero-in-degree node, add to result, and decrease in-degrees of its neighbors

\`\`\`python
import heapq
from collections import defaultdict

def topological_sort(n, edges):
    graph = defaultdict(list)
    in_degree = [0] * n
    for u, v in edges:
        graph[u].append(v)
        in_degree[v] += 1

    # Min-heap for deterministic output
    heap = [i for i in range(n) if in_degree[i] == 0]
    heapq.heapify(heap)
    result = []

    while heap:
        node = heapq.heappop(heap)
        result.append(node)
        for nb in graph[node]:
            in_degree[nb] -= 1
            if in_degree[nb] == 0:
                heapq.heappush(heap, nb)

    return result if len(result) == n else []  # [] if cycle
\`\`\`

### Applications

- Build systems (compile A before B)
- Course prerequisites
- Task scheduling
- Package dependency resolution

### Your Task

Implement \`topological_sort(n, edges)\` using Kahn's algorithm with a min-heap for deterministic output. Return an empty list if the graph has a cycle.`,

	starterCode: `import heapq
from collections import defaultdict

def topological_sort(n, edges):
    graph = defaultdict(list)
    in_degree = [0] * n
    for u, v in edges:
        graph[u].append(v)
        in_degree[v] += 1

    heap = [i for i in range(n) if in_degree[i] == 0]
    heapq.heapify(heap)
    result = []

    while heap:
        node = heapq.heappop(heap)
        result.append(node)
        for nb in graph[node]:
            in_degree[nb] -= 1
            if in_degree[nb] == 0:
                # Push nb onto heap
                pass

    return result if len(result) == n else []

print(topological_sort(4, [(0,1),(0,2),(1,3),(2,3)]))
print(topological_sort(3, [(0,1),(1,2)]))
`,

	solution: `import heapq
from collections import defaultdict

def topological_sort(n, edges):
    graph = defaultdict(list)
    in_degree = [0] * n
    for u, v in edges:
        graph[u].append(v)
        in_degree[v] += 1

    heap = [i for i in range(n) if in_degree[i] == 0]
    heapq.heapify(heap)
    result = []

    while heap:
        node = heapq.heappop(heap)
        result.append(node)
        for nb in graph[node]:
            in_degree[nb] -= 1
            if in_degree[nb] == 0:
                heapq.heappush(heap, nb)

    return result if len(result) == n else []

print(topological_sort(4, [(0,1),(0,2),(1,3),(2,3)]))
print(topological_sort(3, [(0,1),(1,2)]))
`,

	tests: [
		{
			name: "diamond DAG → [0, 1, 2, 3]",
			code: `{{FUNC}}
print(topological_sort(4, [(0,1),(0,2),(1,3),(2,3)]))`,
			expected: "[0, 1, 2, 3]\n",
		},
		{
			name: "linear chain → [0, 1, 2]",
			code: `{{FUNC}}
print(topological_sort(3, [(0,1),(1,2)]))`,
			expected: "[0, 1, 2]\n",
		},
		{
			name: "cycle → empty list",
			code: `{{FUNC}}
print(topological_sort(3, [(0,1),(1,2),(2,0)]))`,
			expected: "[]\n",
		},
		{
			name: "no edges → [0, 1, 2]",
			code: `{{FUNC}}
print(topological_sort(3, []))`,
			expected: "[0, 1, 2]\n",
		},
	],
};
