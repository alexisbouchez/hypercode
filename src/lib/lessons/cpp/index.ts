import type { Chapter, Lesson } from "../types";
import { helloCpp } from "./data/01-hello-cpp";
import { variables } from "./data/02-variables";
import { strings } from "./data/03-strings";
import { conditionals } from "./data/04-conditionals";
import { loops } from "./data/05-loops";
import { overloading } from "./data/06-overloading";
import { defaultArguments } from "./data/07-default-arguments";
import { references } from "./data/08-references";
import { classes } from "./data/09-classes";
import { constructors } from "./data/10-constructors";
import { encapsulation } from "./data/11-encapsulation";
import { destructors } from "./data/18-destructors";
import { copyMove } from "./data/19-copy-move";
import { inheritance } from "./data/12-inheritance";
import { virtualFunctions } from "./data/13-virtual-functions";
import { templates } from "./data/14-templates";
import { vectors } from "./data/15-vectors";
import { smartPointers } from "./data/16-smart-pointers";
import { stlAlgorithms } from "./data/17-stl-algorithms";

export const cppChapters: Chapter[] = [
	{ id: "basics", title: "C++ Basics" },
	{ id: "control-flow", title: "Control Flow" },
	{ id: "functions", title: "Functions" },
	{ id: "classes", title: "Classes" },
	{ id: "inheritance", title: "Inheritance" },
	{ id: "templates", title: "Templates & STL" },
	{ id: "modern-cpp", title: "Modern C++" },
];

export const cppLessons: Lesson[] = [
	helloCpp,
	variables,
	strings,
	conditionals,
	loops,
	overloading,
	defaultArguments,
	references,
	classes,
	constructors,
	encapsulation,
	destructors,
	copyMove,
	inheritance,
	virtualFunctions,
	templates,
	vectors,
	smartPointers,
	stlAlgorithms,
];
