import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { variables } from "./data/02-variables";
import { arithmetic } from "./data/03-arithmetic";
import { strings } from "./data/04-strings";
import { definingFunctions } from "./data/05-defining-functions";
import { ifExpressions } from "./data/06-if-expressions";
import { guards } from "./data/07-guards";
import { patternMatching } from "./data/08-pattern-matching";
import { lists } from "./data/09-lists";
import { listOperations } from "./data/10-list-operations";
import { listComprehensions } from "./data/11-list-comprehensions";
import { folds } from "./data/12-folds";
import { lambdas } from "./data/13-lambdas";
import { whereLet } from "./data/14-where-let";
import { maybe_ } from "./data/15-maybe";

export const haskellChapters: Chapter[] = [
  { id: "basics", title: "Basics" },
  { id: "functions", title: "Functions" },
  { id: "lists", title: "Lists" },
  { id: "functional", title: "Functional Patterns" },
];

export const haskellLessons: Lesson[] = [
  helloWorld,
  variables,
  arithmetic,
  strings,
  definingFunctions,
  ifExpressions,
  guards,
  patternMatching,
  lists,
  listOperations,
  listComprehensions,
  folds,
  lambdas,
  whereLet,
  maybe_,
];
