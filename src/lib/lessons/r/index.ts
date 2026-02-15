import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { variablesAndTypes } from "./data/02-variables-and-types";
import { arithmetic } from "./data/03-arithmetic";
import { creatingVectors } from "./data/04-creating-vectors";
import { vectorOperations } from "./data/05-vector-operations";
import { indexingAndFiltering } from "./data/06-indexing-and-filtering";
import { conditionals } from "./data/07-conditionals";
import { loops } from "./data/08-loops";
import { definingFunctions } from "./data/09-defining-functions";
import { higherOrderFunctions } from "./data/10-higher-order-functions";
import { lists } from "./data/11-lists";
import { matrices } from "./data/12-matrices";
import { dataFrames } from "./data/13-data-frames";
import { applyFunctions } from "./data/14-apply-functions";
import { dataFrameOperations } from "./data/15-data-frame-operations";
import { stringOperations } from "./data/16-string-operations";

export const rChapters: Chapter[] = [
  { id: "foundations", title: "Foundations" },
  { id: "vectors", title: "Vectors" },
  { id: "control-flow", title: "Control Flow" },
  { id: "functions", title: "Functions" },
  { id: "data-structures", title: "Data Structures" },
  { id: "data-manipulation", title: "Data Manipulation" },
  { id: "strings", title: "Strings" },
];

export const rLessons: Lesson[] = [
  helloWorld,
  variablesAndTypes,
  arithmetic,
  creatingVectors,
  vectorOperations,
  indexingAndFiltering,
  conditionals,
  loops,
  definingFunctions,
  higherOrderFunctions,
  lists,
  matrices,
  dataFrames,
  applyFunctions,
  dataFrameOperations,
  stringOperations,
];
