import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { variablesAndTypes } from "./data/02-variables-and-types";
import { conditionals } from "./data/03-conditionals";
import { loops } from "./data/04-loops";
import { functions } from "./data/05-functions";
import { errorHandling } from "./data/06-error-handling";
import { arraysAndSlices } from "./data/07-arrays-and-slices";
import { structs } from "./data/08-structs";
import { pointers } from "./data/09-pointers";
import { allocators } from "./data/10-allocators";
import { comptime } from "./data/11-comptime";
import { optionalsAndUnions } from "./data/12-optionals-and-unions";
import { comptimeGenerics } from "./data/13-comptime-generics";
import { recursiveDataStructures } from "./data/14-recursive-data-structures";
import { safetyBuildModes } from "./data/15-safety-build-modes";

export const zigChapters: Chapter[] = [
  { id: "foundations", title: "Foundations" },
  { id: "control-flow", title: "Control Flow" },
  { id: "functions-chapter", title: "Functions" },
  { id: "data-structures", title: "Data Structures" },
  { id: "advanced", title: "Advanced" },
  { id: "memory", title: "Memory" },
];

export const zigLessons: Lesson[] = [
  helloWorld,
  variablesAndTypes,
  conditionals,
  loops,
  functions,
  errorHandling,
  arraysAndSlices,
  structs,
  recursiveDataStructures,
  pointers,
  comptime,
  comptimeGenerics,
  optionalsAndUnions,
  safetyBuildModes,
  allocators,
];
