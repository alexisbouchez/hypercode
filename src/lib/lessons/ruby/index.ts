import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { variables } from "./data/02-variables";
import { strings } from "./data/03-strings";
import { numbers } from "./data/04-numbers";
import { comparisons } from "./data/05-comparisons";
import { arrays } from "./data/06-arrays";
import { hashes } from "./data/07-hashes";
import { ranges } from "./data/08-ranges";
import { conditionals } from "./data/09-conditionals";
import { loops } from "./data/10-loops";
import { iterators } from "./data/11-iterators";
import { methods } from "./data/12-methods";
import { blocks } from "./data/13-blocks";
import { classes } from "./data/14-classes";
import { modules } from "./data/15-modules";

export const rubyChapters: Chapter[] = [
	{ id: "basics", title: "Basics" },
	{ id: "numbers", title: "Numbers" },
	{ id: "collections", title: "Collections" },
	{ id: "control_flow", title: "Control Flow" },
	{ id: "methods_oop", title: "Methods & OOP" },
];

export const rubyLessons: Lesson[] = [
	helloWorld,
	variables,
	strings,
	numbers,
	comparisons,
	arrays,
	hashes,
	ranges,
	conditionals,
	loops,
	iterators,
	methods,
	blocks,
	classes,
	modules,
];
