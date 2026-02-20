import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { variables } from "./data/02-variables";
import { strings } from "./data/03-strings";
import { conditionals } from "./data/04-conditionals";
import { forLoops } from "./data/05-for-loops";
import { whileLoops } from "./data/06-while-loops";
import { functions } from "./data/07-functions";
import { arrowFunctions } from "./data/08-arrow-functions";
import { closures } from "./data/09-closures";
import { arrays } from "./data/10-arrays";
import { map } from "./data/11-map";
import { filter } from "./data/12-filter";
import { reduce } from "./data/13-reduce";
import { objects } from "./data/14-objects";
import { destructuring } from "./data/15-destructuring";

export const jsChapters: Chapter[] = [
	{ id: "basics", title: "Basics" },
	{ id: "control-flow", title: "Control Flow" },
	{ id: "functions", title: "Functions" },
	{ id: "arrays", title: "Arrays" },
	{ id: "objects", title: "Objects" },
];

export const jsLessons: Lesson[] = [
	helloWorld,
	variables,
	strings,
	conditionals,
	forLoops,
	whileLoops,
	functions,
	arrowFunctions,
	closures,
	arrays,
	map,
	filter,
	reduce,
	objects,
	destructuring,
];
