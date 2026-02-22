import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { variables } from "./data/02-variables";
import { strings } from "./data/03-strings";
import { numbers } from "./data/04-numbers";
import { conditionals } from "./data/05-conditionals";
import { when_ } from "./data/06-when";
import { loops } from "./data/07-loops";
import { functions } from "./data/08-functions";
import { defaultParams } from "./data/09-default-params";
import { lambdas } from "./data/10-lambdas";
import { lists } from "./data/11-lists";
import { listOps } from "./data/12-list-ops";
import { maps } from "./data/13-maps";
import { dataClasses } from "./data/14-data-classes";
import { higherOrder } from "./data/15-higher-order";

export const kotlinChapters: Chapter[] = [
  { id: "basics", title: "Basics" },
  { id: "control-flow", title: "Control Flow" },
  { id: "functions", title: "Functions" },
  { id: "collections", title: "Collections" },
  { id: "oop", title: "OOP & Data" },
];

export const kotlinLessons: Lesson[] = [
  helloWorld,
  variables,
  strings,
  numbers,
  conditionals,
  when_,
  loops,
  functions,
  defaultParams,
  lambdas,
  lists,
  listOps,
  maps,
  dataClasses,
  higherOrder,
];
