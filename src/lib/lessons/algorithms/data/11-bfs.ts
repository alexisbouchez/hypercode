import type { Lesson } from "../../types";

export const bfs: Lesson = {
	id: "bfs",
	title: "Breadth-First Search",
	chapterId: "graphs",
	content: `## Breadth-First Search

Breadth-First Search (BFS) explores a graph **level by level**. Starting from a source node, it visits all neighbors first, then their neighbors, and so on. BFS uses a **queue**.

### Graph Representation

We represent a graph as an adjacency list — an object mapping each node to its array of neighbors:

\`\`\`js
const graph = {
  "A": ["B", "C"],
  "B": ["A", "D", "E"],
  "C": ["A", "F"],
  "D": ["B"],
  "E": ["B"],
  "F": ["C"],
};
\`\`\`

### Algorithm

\`\`\`js
function bfs(graph, start) {
  const visited = new Set();
  const queue = [start];
  const result = [];
  visited.add(start);

  while (queue.length > 0) {
    const node = queue.shift();
    result.push(node);
    for (const neighbor of (graph[node] || [])) {
      if (!visited.has(neighbor)) {
        visited.add(neighbor);
        queue.push(neighbor);
      }
    }
  }
  return result;
}
\`\`\`

### Properties

- **Finds the shortest path** (by number of edges) in an unweighted graph.
- **Level-order traversal**: visits nodes by their distance from the start.
- Time complexity: **O(V + E)** where V = vertices, E = edges.
- Space complexity: **O(V)** for the visited set and queue.

### Real-World Uses

- Shortest path in maps, social networks (degrees of separation).
- Web crawlers (crawl pages breadth-first to avoid deep rabbit holes).
- Level-order traversal of binary trees.
- Finding all nodes within k hops of a source.

### Your Task

Implement \`bfs(graph, start)\` that returns an array of nodes visited in BFS order.`,

	starterCode: `function bfs(graph, start) {
	// Use a queue (array with shift)
	// Track visited nodes with a Set
	// Return nodes in visit order
}

const graph = {
	"A": ["B", "C"],
	"B": ["A", "D", "E"],
	"C": ["A", "F"],
	"D": ["B"],
	"E": ["B"],
	"F": ["C"],
};

console.log(bfs(graph, "A").join(", "));
`,

	solution: `function bfs(graph, start) {
	const visited = new Set();
	const queue = [start];
	const result = [];
	visited.add(start);

	while (queue.length > 0) {
		const node = queue.shift();
		result.push(node);
		for (const neighbor of (graph[node] || [])) {
			if (!visited.has(neighbor)) {
				visited.add(neighbor);
				queue.push(neighbor);
			}
		}
	}
	return result;
}

const graph = {
	"A": ["B", "C"],
	"B": ["A", "D", "E"],
	"C": ["A", "F"],
	"D": ["B"],
	"E": ["B"],
	"F": ["C"],
};

console.log(bfs(graph, "A").join(", "));
`,

	tests: [
		{
			name: "BFS from A visits all nodes level by level",
			expected: "A, B, C, D, E, F\n",
		},
		{
			name: "BFS on a linear graph",
			code: `{{FUNC}}
const g = { 1: [2], 2: [3], 3: [4], 4: [] };
console.log(bfs(g, 1).join(", "));`,
			expected: "1, 2, 3, 4\n",
		},
		{
			name: "BFS single node",
			code: `{{FUNC}}
console.log(bfs({ "X": [] }, "X").join(", "));`,
			expected: "X\n",
		},
		{
			name: "BFS from leaf node",
			code: `{{FUNC}}
const g = { "A": ["B", "C"], "B": ["A"], "C": ["A"] };
console.log(bfs(g, "B").join(", "));`,
			expected: "B, A, C\n",
		},
	],
};
