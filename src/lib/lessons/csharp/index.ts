import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { variables } from "./data/02-variables";
import { strings } from "./data/03-strings";
import { conditionals } from "./data/04-conditionals";
import { switchExpr } from "./data/05-switch";
import { loops } from "./data/06-loops";
import { arrays } from "./data/07-arrays";
import { lists } from "./data/08-lists";
import { dictionaries } from "./data/09-dictionaries";
import { methods } from "./data/10-methods";
import { classes } from "./data/11-classes";
import { properties } from "./data/12-properties";
import { inheritance } from "./data/13-inheritance";
import { interfaces } from "./data/14-interfaces";
import { linq } from "./data/15-linq";

export const csharpChapters: Chapter[] = [
	{ id: "basics", title: "Basics" },
	{ id: "control_flow", title: "Control Flow" },
	{ id: "collections", title: "Collections" },
	{ id: "oop", title: "OOP" },
	{ id: "advanced", title: "Advanced" },
];

export const csharpLessons: Lesson[] = [
	helloWorld,
	variables,
	strings,
	conditionals,
	switchExpr,
	loops,
	arrays,
	lists,
	dictionaries,
	methods,
	classes,
	properties,
	inheritance,
	interfaces,
	linq,
];
