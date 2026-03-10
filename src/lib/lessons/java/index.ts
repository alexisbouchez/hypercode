import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { variables } from "./data/02-variables";
import { strings } from "./data/03-strings";
import { arrays } from "./data/04-arrays";
import { conditionals } from "./data/05-conditionals";
import { loops } from "./data/06-loops";
import { methods } from "./data/07-methods";
import { recursion } from "./data/08-recursion";
import { classes } from "./data/09-classes";
import { inheritance } from "./data/10-inheritance";
import { interfaces } from "./data/11-interfaces";
import { abstractClasses } from "./data/12-abstract";
import { generics } from "./data/13-generics";
import { arrayList } from "./data/14-arraylist";
import { hashMap } from "./data/15-hashmap";
import { exceptionHandling } from "./data/16-exception-handling";
import { enums } from "./data/17-enums";
import { staticAndFinal } from "./data/18-static-and-final";
import { lambdaExpressions } from "./data/19-lambda-expressions";
import { accessModifiers } from "./data/20-access-modifiers";

export const javaChapters: Chapter[] = [
	{ id: "basics", title: "Basics" },
	{ id: "control-flow", title: "Control Flow" },
	{ id: "methods", title: "Methods" },
	{ id: "oop", title: "Object-Oriented Programming" },
	{ id: "advanced", title: "Collections & Generics" },
	{ id: "error-handling", title: "Error Handling" },
	{ id: "functional", title: "Functional Programming" },
];

export const javaLessons: Lesson[] = [
	helloWorld,
	variables,
	strings,
	arrays,
	conditionals,
	loops,
	methods,
	recursion,
	classes,
	inheritance,
	interfaces,
	abstractClasses,
	generics,
	arrayList,
	hashMap,
	exceptionHandling,
	enums,
	staticAndFinal,
	lambdaExpressions,
	accessModifiers,
];
