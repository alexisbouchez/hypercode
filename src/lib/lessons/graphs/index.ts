import type { Chapter } from "../types";
import { adjacencyList } from "./data/01-adjacency-list";
import { bfs } from "./data/02-bfs";
import { dfs } from "./data/03-dfs";
import { hasPath } from "./data/04-has-path";
import { connectedComponents } from "./data/05-connected-components";
import { cycleDetection } from "./data/06-cycle-detection";
import { topologicalSort } from "./data/07-topological-sort";
import { dijkstra } from "./data/08-dijkstra";
import { shortestPath } from "./data/09-shortest-path";
import { directedCycle } from "./data/10-directed-cycle";
import { bipartite } from "./data/11-bipartite";
import { unionFind } from "./data/12-union-find";
import { kruskal } from "./data/13-kruskal";
import { bellmanFord } from "./data/14-bellman-ford";
import { pagerank } from "./data/15-pagerank";

export const graphsChapters: Chapter[] = [
	{ id: "graph-basics", title: "Graph Basics" },
	{ id: "graph-properties", title: "Graph Properties" },
	{ id: "shortest-paths", title: "Shortest Paths" },
	{ id: "advanced-algorithms", title: "Advanced Algorithms" },
];

export const graphsLessons = [
	adjacencyList,
	bfs,
	dfs,
	hasPath,
	connectedComponents,
	cycleDetection,
	topologicalSort,
	dijkstra,
	shortestPath,
	directedCycle,
	bipartite,
	unionFind,
	kruskal,
	bellmanFord,
	pagerank,
];
