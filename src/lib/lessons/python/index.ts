import type { Chapter } from "../types";
import { helloPython } from "./data/01-hello-python";
import { strings } from "./data/02-strings";
import { numbers } from "./data/03-numbers";
import { lists } from "./data/04-lists";
import { dictionaries } from "./data/05-dictionaries";
import { controlFlow } from "./data/06-control-flow";
import { loops } from "./data/07-loops";
import { functions } from "./data/08-functions";
import { comprehensions } from "./data/09-comprehensions";
import { classes } from "./data/10-classes";
import { inheritance } from "./data/11-inheritance";
import { errorHandling } from "./data/12-error-handling";
import { generators } from "./data/13-generators";
import { standardLibrary } from "./data/14-standard-library";
import { recursion } from "./data/15-recursion";

export const pythonChapters: Chapter[] = [
	{ id: "foundations", title: "Foundations" },
	{ id: "control-and-iteration", title: "Control & Iteration" },
	{ id: "object-oriented", title: "Object-Oriented Programming" },
	{ id: "advanced", title: "Advanced Python" },
];

export const pythonLessons = [
	helloPython,
	strings,
	numbers,
	lists,
	dictionaries,
	controlFlow,
	loops,
	functions,
	comprehensions,
	classes,
	inheritance,
	errorHandling,
	generators,
	standardLibrary,
	recursion,
];
