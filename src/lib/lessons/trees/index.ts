import type { Chapter, Lesson } from "../types";
import { treeNode } from "./data/01-tree-node";
import { inorder } from "./data/02-inorder";
import { preorder } from "./data/03-preorder";
import { postorder } from "./data/04-postorder";
import { treeSum } from "./data/05-tree-sum";
import { countNodes } from "./data/06-count-nodes";
import { height } from "./data/07-height";
import { countLeaves } from "./data/08-count-leaves";
import { bstInsert } from "./data/09-bst-insert";
import { bstSearch } from "./data/10-bst-search";
import { bstMinMax } from "./data/11-bst-min-max";
import { isBst } from "./data/12-is-bst";

export const treesChapters: Chapter[] = [
	{ id: "binary-trees", title: "Binary Trees" },
	{ id: "tree-properties", title: "Tree Properties" },
	{ id: "bst", title: "Binary Search Tree" },
	{ id: "bst-operations", title: "BST Operations" },
];

export const treesLessons: Lesson[] = [
	treeNode,
	inorder,
	preorder,
	postorder,
	treeSum,
	countNodes,
	height,
	countLeaves,
	bstInsert,
	bstSearch,
	bstMinMax,
	isBst,
];
