import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { variables } from "./data/02-variables";
import { stringInterpolation } from "./data/03-string-interpolation";
import { conditionals } from "./data/04-conditionals";
import { patternMatching } from "./data/05-pattern-matching";
import { functions } from "./data/06-functions";
import { recursion } from "./data/07-recursion";
import { lists } from "./data/08-lists";
import { mapFilter } from "./data/09-map-filter";
import { forLoops } from "./data/10-for-loops";
import { higherOrder } from "./data/11-higher-order";
import { caseClasses } from "./data/12-case-classes";
import { options } from "./data/13-options";
import { traits } from "./data/14-traits";
import { objects } from "./data/15-objects";

export const chapters: Chapter[] = [
  { id: "basics", title: "Basics" },
  { id: "functions", title: "Functions" },
  { id: "collections", title: "Collections" },
  { id: "oop", title: "OOP" },
];

export const lessons: Lesson[] = [
  helloWorld,
  variables,
  stringInterpolation,
  conditionals,
  patternMatching,
  functions,
  recursion,
  lists,
  mapFilter,
  forLoops,
  higherOrder,
  caseClasses,
  options,
  traits,
  objects,
];
