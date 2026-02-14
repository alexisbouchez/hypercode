import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { variablesAndTypes } from "./data/02-variables-and-types";
import { functions } from "./data/03-functions";
import { strings } from "./data/04-strings";
import { numbers } from "./data/05-numbers";
import { caseExpressions } from "./data/06-case-expressions";
import { patternMatching } from "./data/07-pattern-matching";
import { lists } from "./data/08-lists";
import { tuples } from "./data/09-tuples";
import { customTypes } from "./data/10-custom-types";
import { records } from "./data/11-records";
import { genericTypes } from "./data/12-generic-types";
import { resultType } from "./data/13-result-type";
import { useExpressions } from "./data/14-use-expressions";
import { pipeOperator } from "./data/15-pipe-operator";
import { higherOrderFunctions } from "./data/16-higher-order-functions";

export const gleamChapters: Chapter[] = [
  { id: "foundations", title: "Foundations" },
  { id: "data-types", title: "Data Types" },
  { id: "control-flow", title: "Control Flow" },
  { id: "collections", title: "Collections" },
  { id: "custom-types", title: "Custom Types" },
  { id: "error-handling", title: "Error Handling" },
  { id: "functional-patterns", title: "Functional Patterns" },
];

export const gleamLessons: Lesson[] = [
  helloWorld,
  variablesAndTypes,
  functions,
  strings,
  numbers,
  caseExpressions,
  patternMatching,
  lists,
  tuples,
  customTypes,
  records,
  genericTypes,
  resultType,
  useExpressions,
  pipeOperator,
  higherOrderFunctions,
];
