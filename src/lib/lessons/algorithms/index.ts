import type { Chapter, Lesson } from "../types";
import { bubbleSort } from "./data/01-bubble-sort";
import { selectionSort } from "./data/02-selection-sort";
import { insertionSort } from "./data/03-insertion-sort";
import { mergeSort } from "./data/04-merge-sort";
import { quickSort } from "./data/05-quick-sort";
import { linearSearch } from "./data/06-linear-search";
import { binarySearch } from "./data/07-binary-search";
import { stack } from "./data/08-stack";
import { queue } from "./data/09-queue";
import { linkedList } from "./data/10-linked-list";
import { bfs } from "./data/11-bfs";
import { dfs } from "./data/12-dfs";
import { dijkstra } from "./data/13-dijkstra";
import { fibonacciMemoization } from "./data/14-fibonacci-memoization";
import { lcs } from "./data/15-lcs";

export const algorithmsChapters: Chapter[] = [
	{ id: "sorting", title: "Sorting" },
	{ id: "searching", title: "Searching" },
	{ id: "data-structures", title: "Data Structures" },
	{ id: "graphs", title: "Graphs" },
	{ id: "dynamic-programming", title: "Dynamic Programming" },
];

export const algorithmsLessons: Lesson[] = [
	bubbleSort,
	selectionSort,
	insertionSort,
	mergeSort,
	quickSort,
	linearSearch,
	binarySearch,
	stack,
	queue,
	linkedList,
	bfs,
	dfs,
	dijkstra,
	fibonacciMemoization,
	lcs,
];
