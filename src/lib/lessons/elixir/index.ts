import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { variables } from "./data/02-variables";
import { strings } from "./data/03-strings";
import { numbers } from "./data/04-numbers";
import { patternMatching } from "./data/05-pattern-matching";
import { case_ } from "./data/06-case";
import { conditionals } from "./data/07-conditionals";
import { anonymous } from "./data/08-anonymous";
import { modules } from "./data/09-modules";
import { pipe } from "./data/10-pipe";
import { lists } from "./data/11-lists";
import { enum_ } from "./data/12-enum";
import { tuplesMaps } from "./data/13-tuples-maps";
import { recursion } from "./data/14-recursion";
import { multihead } from "./data/15-multihead";

export const elixirChapters: Chapter[] = [
  { id: "foundations", title: "Foundations" },
  { id: "control-flow", title: "Control Flow" },
  { id: "functions", title: "Functions" },
  { id: "collections", title: "Collections" },
  { id: "functional", title: "Functional Patterns" },
];

export const elixirLessons: Lesson[] = [
  helloWorld,
  variables,
  strings,
  numbers,
  patternMatching,
  case_,
  conditionals,
  anonymous,
  modules,
  pipe,
  lists,
  enum_,
  tuplesMaps,
  recursion,
  multihead,
];
