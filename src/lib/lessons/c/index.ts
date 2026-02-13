import type { Chapter, Lesson } from "../types";
import { helloC } from "./data/01-hello-c";
import { variablesAndTypes } from "./data/02-variables-and-types";
import { arithmetic } from "./data/03-arithmetic";
import { conditionals } from "./data/04-conditionals";
import { loops } from "./data/05-loops";
import { functions } from "./data/06-functions";
import { recursion } from "./data/07-recursion";
import { arrays } from "./data/08-arrays";
import { strings } from "./data/09-strings";
import { multidimensionalArrays } from "./data/10-multidimensional-arrays";
import { pointers } from "./data/11-pointers";
import { pointersAndArrays } from "./data/12-pointers-and-arrays";
import { structs } from "./data/13-structs";
import { structsAndPointers } from "./data/14-structs-and-pointers";

export const cChapters: Chapter[] = [
	{ id: "basics", title: "C Basics" },
	{ id: "control-flow", title: "Control Flow" },
	{ id: "functions", title: "Functions" },
	{ id: "arrays-and-strings", title: "Arrays and Strings" },
	{ id: "pointers", title: "Pointers" },
	{ id: "structs", title: "Structs" },
];

export const cLessons: Lesson[] = [
	helloC,
	variablesAndTypes,
	arithmetic,
	conditionals,
	loops,
	functions,
	recursion,
	arrays,
	strings,
	multidimensionalArrays,
	pointers,
	pointersAndArrays,
	structs,
	structsAndPointers,
];
