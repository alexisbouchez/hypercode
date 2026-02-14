import type { Chapter, Lesson } from "../types";
import { helloC } from "./data/01-hello-c";
import { variablesAndTypes } from "./data/02-variables-and-types";
import { arithmetic } from "./data/03-arithmetic";
import { typeCasting } from "./data/18-type-casting";
import { conditionals } from "./data/04-conditionals";
import { loops } from "./data/05-loops";
import { switchStatement } from "./data/15-switch";
import { enums } from "./data/16-enums";
import { bitwiseOperators } from "./data/17-bitwise-operators";
import { functions } from "./data/06-functions";
import { recursion } from "./data/07-recursion";
import { staticVariables } from "./data/25-static";
import { arrays } from "./data/08-arrays";
import { strings } from "./data/09-strings";
import { multidimensionalArrays } from "./data/10-multidimensional-arrays";
import { pointers } from "./data/11-pointers";
import { pointersAndArrays } from "./data/12-pointers-and-arrays";
import { structs } from "./data/13-structs";
import { structsAndPointers } from "./data/14-structs-and-pointers";
import { dynamicMemory } from "./data/19-dynamic-memory";
import { callocAndRealloc } from "./data/20-calloc-and-realloc";
import { linkedLists } from "./data/24-linked-lists";
import { functionPointers } from "./data/21-function-pointers";
import { preprocessorMacros } from "./data/22-preprocessor-macros";
import { sorting } from "./data/23-variadic-functions";

export const cChapters: Chapter[] = [
	{ id: "basics", title: "C Basics" },
	{ id: "control-flow", title: "Control Flow" },
	{ id: "enums-and-bitwise", title: "Enums and Bitwise" },
	{ id: "functions", title: "Functions" },
	{ id: "arrays-and-strings", title: "Arrays and Strings" },
	{ id: "pointers", title: "Pointers" },
	{ id: "structs", title: "Structs" },
	{ id: "dynamic-memory", title: "Dynamic Memory" },
	{ id: "advanced", title: "Advanced Concepts" },
];

export const cLessons: Lesson[] = [
	helloC,
	variablesAndTypes,
	arithmetic,
	typeCasting,
	conditionals,
	loops,
	switchStatement,
	enums,
	bitwiseOperators,
	functions,
	recursion,
	staticVariables,
	arrays,
	strings,
	multidimensionalArrays,
	pointers,
	pointersAndArrays,
	structs,
	structsAndPointers,
	dynamicMemory,
	callocAndRealloc,
	linkedLists,
	functionPointers,
	preprocessorMacros,
	sorting,
];
