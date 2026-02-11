import type { Chapter, Lesson } from "./types";
import { helloWorld } from "./data/01-hello-world";
import { variables } from "./data/02-variables";
import { conditionals } from "./data/03-conditionals";
import { loops } from "./data/04-loops";
import { functions } from "./data/05-functions";
import { defer_ } from "./data/06-defer";
import { slices } from "./data/07-slices";
import { maps } from "./data/08-maps";
import { strings_ } from "./data/09-strings";
import { pointers } from "./data/10-pointers";
import { structs } from "./data/11-structs";
import { interfaces } from "./data/12-interfaces";
import { errors } from "./data/13-errors";
import { genericFunctions } from "./data/14-generic-functions";
import { genericTypes } from "./data/15-generic-types";
import { concurrency } from "./data/16-goroutines-channels";
import { select_ } from "./data/17-select";

export const chapters: Chapter[] = [
  { id: "foundations", title: "Foundations" },
  { id: "control-flow", title: "Control Flow" },
  { id: "functions", title: "Functions" },
  { id: "data-structures", title: "Data Structures" },
  { id: "custom-types", title: "Custom Types" },
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
  defer_,
  slices,
  maps,
  strings_,
  pointers,
  structs,
  interfaces,
  errors,
  genericFunctions,
  genericTypes,
  concurrency,
  select_,
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
