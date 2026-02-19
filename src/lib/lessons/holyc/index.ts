import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { printAndOutput } from "./data/02-print-and-output";
import { comments } from "./data/03-comments";
import { integerTypes } from "./data/04-integer-types";
import { floatingPoint } from "./data/05-floating-point";
import { booleansAndAuto } from "./data/06-booleans-and-auto";
import { ifElse } from "./data/07-if-else";
import { switchStatements } from "./data/08-switch-statements";
import { loops } from "./data/09-loops";
import { functions } from "./data/10-functions";
import { defaultArguments } from "./data/11-default-arguments";
import { multipleReturn } from "./data/12-multiple-return";
import { classes } from "./data/13-classes";
import { inheritance } from "./data/14-inheritance";
import { arraysAndPointers } from "./data/15-arrays-and-pointers";
import { compileTimeExpressions } from "./data/16-compile-time-expressions";

export const holycChapters: Chapter[] = [
  { id: "the-temple", title: "The Temple" },
  { id: "types-and-variables", title: "Types & Variables" },
  { id: "control-flow", title: "Control Flow" },
  { id: "functions", title: "Functions" },
  { id: "classes-and-structures", title: "Classes & Structures" },
  { id: "advanced-holyc", title: "Advanced HolyC" },
];

export const holycLessons: Lesson[] = [
  helloWorld,
  printAndOutput,
  comments,
  integerTypes,
  floatingPoint,
  booleansAndAuto,
  ifElse,
  switchStatements,
  loops,
  functions,
  defaultArguments,
  multipleReturn,
  classes,
  inheritance,
  arraysAndPointers,
  compileTimeExpressions,
];
