import type { Chapter, Lesson } from "../types";
import { listNode } from "./data/01-list-node";
import { printList } from "./data/02-print-list";
import { listLength } from "./data/03-list-length";
import { pushFront } from "./data/04-push-front";
import { pushBack } from "./data/05-push-back";
import { popFront } from "./data/06-pop-front";
import { search } from "./data/07-search";
import { nthNode } from "./data/08-nth-node";
import { deleteVal } from "./data/09-delete-val";
import { nthFromEnd } from "./data/10-nth-from-end";
import { reverse } from "./data/11-reverse";
import { mergeSorted } from "./data/12-merge-sorted";

export const linkedListsChapters: Chapter[] = [
	{ id: "the-node", title: "The Node" },
	{ id: "insertions", title: "Insertions & Deletions" },
	{ id: "deletions", title: "Search & Access" },
	{ id: "classic", title: "Classic Problems" },
];

export const linkedListsLessons: Lesson[] = [
	listNode,
	printList,
	listLength,
	pushFront,
	pushBack,
	popFront,
	search,
	nthNode,
	deleteVal,
	nthFromEnd,
	reverse,
	mergeSorted,
];
