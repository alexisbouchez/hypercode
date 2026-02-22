import type { Chapter, Lesson } from "../types";

import { helloWorld } from "./data/01-hello-world";
import { values } from "./data/02-values";
import { stringInterpolation } from "./data/03-string-interpolation";
import { conditionals } from "./data/04-conditionals";
import { patternMatching } from "./data/05-pattern-matching";
import { functions } from "./data/06-functions";
import { recursion } from "./data/07-recursion";
import { higherOrder } from "./data/08-higher-order";
import { pipe } from "./data/09-pipe";
import { lists } from "./data/10-lists";
import { mapFilter } from "./data/11-map-filter";
import { fold } from "./data/12-fold";
import { tuples } from "./data/13-tuples";
import { records } from "./data/14-records";
import { discriminatedUnions } from "./data/15-discriminated-unions";

export const fsharpChapters: Chapter[] = [
  { id: "basics", title: "Basics" },
  { id: "functions", title: "Functions" },
  { id: "collections", title: "Collections" },
  { id: "types", title: "Types" },
];

export const fsharpLessons: Lesson[] = [
  helloWorld,
  values,
  stringInterpolation,
  conditionals,
  patternMatching,
  functions,
  recursion,
  higherOrder,
  pipe,
  lists,
  mapFilter,
  fold,
  tuples,
  records,
  discriminatedUnions,
];
