import type { Chapter, Lesson } from "../types";
import { helloLua } from "./data/01-hello-lua";
import { variablesAndTypes } from "./data/02-variables-and-types";
import { arithmetic } from "./data/03-arithmetic";
import { strings } from "./data/04-strings";
import { ifElseifElse } from "./data/05-if-elseif-else";
import { whileAndRepeat } from "./data/06-while-and-repeat";
import { forLoops } from "./data/07-for-loops";
import { functions } from "./data/08-functions";
import { closures } from "./data/09-closures";
import { variadicFunctions } from "./data/10-variadic-functions";
import { tablesAsArrays } from "./data/11-tables-as-arrays";
import { tablesAsDictionaries } from "./data/12-tables-as-dictionaries";
import { metatables } from "./data/13-metatables";
import { modules } from "./data/14-modules";
import { coroutines } from "./data/15-coroutines";

export const luaChapters: Chapter[] = [
  { id: "basics", title: "Lua Basics" },
  { id: "control-flow", title: "Control Flow" },
  { id: "functions", title: "Functions" },
  { id: "tables", title: "Tables" },
  { id: "advanced", title: "Advanced Lua" },
];

export const luaLessons: Lesson[] = [
  helloLua,
  variablesAndTypes,
  arithmetic,
  strings,
  ifElseifElse,
  whileAndRepeat,
  forLoops,
  functions,
  closures,
  variadicFunctions,
  tablesAsArrays,
  tablesAsDictionaries,
  metatables,
  modules,
  coroutines,
];
