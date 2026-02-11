import type { Chapter, Lesson } from "./types";
import { helloWorld } from "./data/01-hello-world";
import { variables } from "./data/02-variables";
import { conditionals } from "./data/03-conditionals";
import { loops } from "./data/04-loops";
import { functions } from "./data/05-functions";
import { slices } from "./data/06-slices";
import { maps } from "./data/07-maps";
import { pointers } from "./data/08-pointers";
import { structs } from "./data/09-structs";
import { interfaces } from "./data/10-interfaces";
import { errors } from "./data/11-errors";
import { genericFunctions } from "./data/12-generic-functions";
import { genericTypes } from "./data/13-generic-types";
import { concurrency } from "./data/14-concurrency";

export const chapters: Chapter[] = [
  { id: "foundations", title: "Foundations" },
  { id: "control-flow", title: "Control Flow" },
  { id: "functions", title: "Functions" },
  { id: "data-structures", title: "Data Structures" },
  { id: "structs-interfaces", title: "Structs and Interfaces" },
  { id: "error-handling", title: "Error Handling" },
  { id: "generics", title: "Generics" },
  { id: "concurrency", title: "Concurrency" },
];

export const lessons: Lesson[] = [
  helloWorld,
  variables,
  conditionals,
  loops,
  functions,
  slices,
  maps,
  pointers,
  structs,
  interfaces,
  errors,
  genericFunctions,
  genericTypes,
  concurrency,
];

export function getLessonById(id: string): Lesson | undefined {
  return lessons.find((l) => l.id === id);
}

export function getLessonIndex(id: string): number {
  return lessons.findIndex((l) => l.id === id);
}

export function getChapterLessons(chapterId: string): Lesson[] {
  return lessons.filter((l) => l.chapterId === chapterId);
}
