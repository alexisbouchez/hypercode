import type { Lesson } from "../../types";

export const topologicalSort: Lesson = {
	id: "topological-sort",
	title: "Topological Sort",
	chapterId: "graphs",
	content: `## Topological Sort

A **topological sort** produces a linear ordering of vertices in a **directed acyclic graph (DAG)** such that for every edge \`u → v\`, vertex \`u\` comes before \`v\` in the ordering.

### Use Case: Course Scheduling

Imagine courses with prerequisites:

\`\`\`
Math → Physics → Quantum
Math → Statistics
Statistics → ML
\`\`\`

A topological sort gives a valid order to take all courses: e.g., \`Math, Physics, Statistics, Quantum, ML\`.

### Kahn's Algorithm (BFS)

1. Compute the **in-degree** (number of incoming edges) for every node.
2. Add all nodes with in-degree 0 to a **queue**.
3. While the queue is not empty:
   - Dequeue a node, add it to the result.
   - For each neighbor, decrement its in-degree.
   - If a neighbor's in-degree reaches 0, enqueue it.
4. If the result contains all nodes, the sort is valid. Otherwise the graph has a cycle.

\`\`\`js
function topologicalSort(graph) {
  const inDegree = {};
  for (const node in graph) {
    if (!(node in inDegree)) inDegree[node] = 0;
    for (const neighbor of graph[node]) {
      inDegree[neighbor] = (inDegree[neighbor] || 0) + 1;
    }
  }

  const queue = [];
  for (const node in inDegree) {
    if (inDegree[node] === 0) queue.push(node);
  }

  const result = [];
  while (queue.length > 0) {
    const node = queue.shift();
    result.push(node);
    for (const neighbor of (graph[node] || [])) {
      inDegree[neighbor]--;
      if (inDegree[neighbor] === 0) queue.push(neighbor);
    }
  }
  return result;
}
\`\`\`

### Complexity

| Aspect | Value   |
|--------|---------|
| Time   | O(V + E) |
| Space  | O(V)    |

### Your Task

Implement \`topologicalSort(graph)\` using Kahn's algorithm. The graph is an adjacency list (object mapping node → array of neighbors). Return an array of nodes in topological order.`,

	starterCode: `function topologicalSort(graph) {
	// 1. Compute in-degrees
	// 2. Initialize queue with in-degree 0 nodes
	// 3. BFS: dequeue, add to result, decrement neighbors' in-degrees
	// 4. Return result
}

const graph = {
	"Math": ["Physics", "Statistics"],
	"Physics": ["Quantum"],
	"Statistics": ["ML"],
	"Quantum": [],
	"ML": [],
};

console.log(topologicalSort(graph).join(", "));
`,

	solution: `function topologicalSort(graph) {
	const inDegree = {};
	for (const node in graph) {
		if (!(node in inDegree)) inDegree[node] = 0;
		for (const neighbor of graph[node]) {
			inDegree[neighbor] = (inDegree[neighbor] || 0) + 1;
		}
	}

	const queue = [];
	for (const node in inDegree) {
		if (inDegree[node] === 0) queue.push(node);
	}

	const result = [];
	while (queue.length > 0) {
		const node = queue.shift();
		result.push(node);
		for (const neighbor of (graph[node] || [])) {
			inDegree[neighbor]--;
			if (inDegree[neighbor] === 0) queue.push(neighbor);
		}
	}
	return result;
}

const graph = {
	"Math": ["Physics", "Statistics"],
	"Physics": ["Quantum"],
	"Statistics": ["ML"],
	"Quantum": [],
	"ML": [],
};

console.log(topologicalSort(graph).join(", "));
`,

	tests: [
		{
			name: "course scheduling order",
			expected: "Math, Physics, Statistics, Quantum, ML\n",
		},
		{
			name: "linear dependency chain",
			code: `{{FUNC}}
const g = { "A": ["B"], "B": ["C"], "C": ["D"], "D": [] };
console.log(topologicalSort(g).join(", "));`,
			expected: "A, B, C, D\n",
		},
		{
			name: "multiple independent nodes",
			code: `{{FUNC}}
const g = { "X": [], "Y": [], "Z": [] };
console.log(topologicalSort(g).join(", "));`,
			expected: "X, Y, Z\n",
		},
		{
			name: "diamond dependency",
			code: `{{FUNC}}
const g = { "A": ["B", "C"], "B": ["D"], "C": ["D"], "D": [] };
const result = topologicalSort(g);
const idxA = result.indexOf("A");
const idxB = result.indexOf("B");
const idxC = result.indexOf("C");
const idxD = result.indexOf("D");
console.log(idxA < idxB && idxA < idxC && idxB < idxD && idxC < idxD);
console.log(result.length);`,
			expected: "true\n4\n",
		},
	],
};
