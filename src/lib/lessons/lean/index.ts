import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { numbers } from "./data/02-numbers";
import { strings } from "./data/03-strings";
import { booleans } from "./data/04-booleans";
import { variables } from "./data/05-variables";
import { functions } from "./data/06-functions";
import { multipleParams } from "./data/07-multiple-params";
import { ifThenElse } from "./data/08-if-then-else";
import { recursion } from "./data/09-recursion";
import { lists } from "./data/10-lists";
import { patternMatching } from "./data/11-pattern-matching";
import { listRecursion } from "./data/12-list-recursion";
import { lambdas } from "./data/13-lambdas";
import { mapFilter } from "./data/14-map-filter";
import { fold } from "./data/15-fold";

export const leanChapters: Chapter[] = [
  { id: "foundations", title: "Foundations" },
  { id: "functions", title: "Functions" },
  { id: "lists", title: "Lists & Pattern Matching" },
  { id: "functional", title: "Functional Programming" },
];

export const leanLessons: Lesson[] = [
  helloWorld,
  numbers,
  strings,
  booleans,
  variables,
  functions,
  multipleParams,
  ifThenElse,
  recursion,
  lists,
  patternMatching,
  listRecursion,
  lambdas,
  mapFilter,
  fold,
];
