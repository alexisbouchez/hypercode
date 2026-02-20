import type { Lesson } from "../../types";

export const dijkstra: Lesson = {
	id: "dijkstra",
	title: "Dijkstra's Algorithm",
	chapterId: "graphs",
	content: `## Dijkstra's Algorithm

Dijkstra's algorithm finds the **shortest path** from a source node to all other nodes in a weighted graph (with non-negative edge weights).

### Weighted Graph

\`\`\`js
const graph = {
  "A": [["B", 4], ["C", 2]],
  "B": [["D", 3], ["C", 1]],
  "C": [["B", 1], ["D", 5]],
  "D": [],
};
// A→C (cost 2) → B (cost 1) → D (cost 3): total A→D = 6
\`\`\`

### Algorithm

1. Initialize distances: \`dist[start] = 0\`, all others = \`Infinity\`.
2. Maintain a set of unvisited nodes.
3. Repeatedly pick the unvisited node with the smallest tentative distance.
4. For each neighbor, if \`dist[node] + weight < dist[neighbor]\`, update \`dist[neighbor]\`.
5. Mark the node as visited.
6. Stop when all nodes are visited or the smallest tentative distance is \`Infinity\`.

\`\`\`js
function dijkstra(graph, start) {
  const dist = {};
  const visited = new Set();
  for (const node in graph) dist[node] = Infinity;
  dist[start] = 0;

  while (true) {
    // Pick unvisited node with smallest distance
    let node = null;
    for (const n in dist) {
      if (!visited.has(n) && (node === null || dist[n] < dist[node])) node = n;
    }
    if (node === null || dist[node] === Infinity) break;
    visited.add(node);

    for (const [neighbor, weight] of (graph[node] || [])) {
      if (dist[node] + weight < dist[neighbor]) {
        dist[neighbor] = dist[node] + weight;
      }
    }
  }
  return dist;
}
\`\`\`

### Complexity

This naive implementation is **O(V²)**. With a priority queue (min-heap), it becomes **O((V + E) log V)**, which is how real implementations work (e.g., Dijkstra in Google Maps).

### Your Task

Implement \`dijkstra(graph, start)\` that returns an object mapping each node to its shortest distance from \`start\`.`,

	starterCode: `function dijkstra(graph, start) {
	// Initialize all distances to Infinity, start to 0
	// Repeatedly pick the closest unvisited node
	// Relax edges: update distances if a shorter path is found
}

const graph = {
	"A": [["B", 4], ["C", 2]],
	"B": [["D", 3]],
	"C": [["B", 1], ["D", 5]],
	"D": [],
};

const dist = dijkstra(graph, "A");
console.log("A:", dist["A"]);
console.log("B:", dist["B"]);
console.log("C:", dist["C"]);
console.log("D:", dist["D"]);
`,

	solution: `function dijkstra(graph, start) {
	const dist = {};
	const visited = new Set();
	for (const node in graph) dist[node] = Infinity;
	dist[start] = 0;

	while (true) {
		let node = null;
		for (const n in dist) {
			if (!visited.has(n) && (node === null || dist[n] < dist[node])) node = n;
		}
		if (node === null || dist[node] === Infinity) break;
		visited.add(node);

		for (const [neighbor, weight] of (graph[node] || [])) {
			if (dist[node] + weight < dist[neighbor]) {
				dist[neighbor] = dist[node] + weight;
			}
		}
	}
	return dist;
}

const graph = {
	"A": [["B", 4], ["C", 2]],
	"B": [["D", 3]],
	"C": [["B", 1], ["D", 5]],
	"D": [],
};

const dist = dijkstra(graph, "A");
console.log("A:", dist["A"]);
console.log("B:", dist["B"]);
console.log("C:", dist["C"]);
console.log("D:", dist["D"]);
`,

	tests: [
		{
			name: "shortest distances from A",
			expected: "A: 0\nB: 3\nC: 2\nD: 6\n",
		},
		{
			name: "single node graph",
			code: `{{FUNC}}
const g = { "X": [] };
const d = dijkstra(g, "X");
console.log(d["X"]);`,
			expected: "0\n",
		},
		{
			name: "direct edge is shorter than indirect",
			code: `{{FUNC}}
const g = { "A": [["B", 1], ["C", 10]], "B": [["C", 1]], "C": [] };
const d = dijkstra(g, "A");
console.log(d["C"]);`,
			expected: "2\n",
		},
	],
};
