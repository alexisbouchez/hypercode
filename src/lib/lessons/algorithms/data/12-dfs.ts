import type { Lesson } from "../../types";

export const dfs: Lesson = {
	id: "dfs",
	title: "Depth-First Search",
	chapterId: "graphs",
	content: `## Depth-First Search

Depth-First Search (DFS) explores a graph by going as **deep as possible** before backtracking. It follows each path to its end before trying alternatives. DFS uses a **stack** — either an explicit one or the call stack via recursion.

### Algorithm (Recursive)

\`\`\`js
function dfs(graph, start) {
  const visited = new Set();
  const result = [];

  function visit(node) {
    visited.add(node);
    result.push(node);
    for (const neighbor of (graph[node] || [])) {
      if (!visited.has(neighbor)) visit(neighbor);
    }
  }

  visit(start);
  return result;
}
\`\`\`

### BFS vs DFS

| Property | BFS | DFS |
|----------|-----|-----|
| Data structure | Queue | Stack / Recursion |
| Order | Level by level | Deep first |
| Shortest path | Yes (unweighted) | No |
| Memory (wide graph) | More | Less |
| Memory (deep graph) | Less | More |
| Good for | Shortest paths, level-order | Cycle detection, topological sort, puzzles |

### Real-World Uses

- **Cycle detection** in directed graphs (e.g., detecting circular dependencies).
- **Topological sort** for build systems and task scheduling.
- **Maze solving** — go deep until you hit a dead end, then backtrack.
- **Tree traversals** — pre-order, in-order, post-order are all DFS variants.
- **Connected components** — find all nodes reachable from a source.

### Your Task

Implement \`dfs(graph, start)\` that returns an array of nodes visited in DFS order using recursion.`,

	starterCode: `function dfs(graph, start) {
	const visited = new Set();
	const result = [];

	function visit(node) {
		// Mark as visited, add to result
		// Recursively visit unvisited neighbors
	}

	visit(start);
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

console.log(dfs(graph, "A").join(", "));
`,

	solution: `function dfs(graph, start) {
	const visited = new Set();
	const result = [];

	function visit(node) {
		visited.add(node);
		result.push(node);
		for (const neighbor of (graph[node] || [])) {
			if (!visited.has(neighbor)) visit(neighbor);
		}
	}

	visit(start);
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

console.log(dfs(graph, "A").join(", "));
`,

	tests: [
		{
			name: "DFS from A visits all nodes depth-first",
			expected: "A, B, D, E, C, F\n",
		},
		{
			name: "DFS on a linear graph",
			code: `{{FUNC}}
const g = { 1: [2], 2: [3], 3: [4], 4: [] };
console.log(dfs(g, 1).join(", "));`,
			expected: "1, 2, 3, 4\n",
		},
		{
			name: "DFS single node",
			code: `{{FUNC}}
console.log(dfs({ "X": [] }, "X").join(", "));`,
			expected: "X\n",
		},
		{
			name: "DFS from leaf node",
			code: `{{FUNC}}
const g = { "A": ["B", "C"], "B": ["A"], "C": ["A"] };
console.log(dfs(g, "B").join(", "));`,
			expected: "B, A, C\n",
		},
	],
};
