import type { Chapter, Lesson } from "../types";
import { helloPhp } from "./data/01-hello-php";
import { variables } from "./data/02-variables";
import { arithmetic } from "./data/03-arithmetic";
import { strings } from "./data/04-strings";
import { ifElse } from "./data/05-if-else";
import { whileLoops } from "./data/06-while-loops";
import { forLoops } from "./data/07-for-loops";
import { arrays } from "./data/08-arrays";
import { associativeArrays } from "./data/09-associative-arrays";
import { arrayFunctions } from "./data/10-array-functions";
import { functions } from "./data/11-functions";
import { stringFunctions } from "./data/12-string-functions";
import { scopeClosures } from "./data/13-scope-closures";
import { classes } from "./data/14-classes";
import { puttingItTogether } from "./data/15-putting-it-together";

export const phpChapters: Chapter[] = [
  { id: "basics", title: "PHP Basics" },
  { id: "control-flow", title: "Control Flow" },
  { id: "data-structures", title: "Data Structures" },
  { id: "functions", title: "Functions" },
  { id: "oop", title: "Object-Oriented PHP" },
];

export const phpLessons: Lesson[] = [
  helloPhp,
  variables,
  arithmetic,
  strings,
  ifElse,
  whileLoops,
  forLoops,
  arrays,
  associativeArrays,
  arrayFunctions,
  functions,
  stringFunctions,
  scopeClosures,
  classes,
  puttingItTogether,
];
