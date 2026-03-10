import type { Chapter, Lesson } from "../types";
import { helloOCaml } from "./data/01-hello-ocaml";
import { letBindings } from "./data/02-let-bindings";
import { basicTypes } from "./data/03-basic-types";
import { arithmetic } from "./data/04-arithmetic";
import { functions } from "./data/05-functions";
import { recursion } from "./data/06-recursion";
import { higherOrder } from "./data/07-higher-order";
import { lists } from "./data/08-lists";
import { tuples } from "./data/09-tuples";
import { patternMatching } from "./data/10-pattern-matching";
import { variants } from "./data/11-variants";
import { optionType } from "./data/12-option-type";
import { records } from "./data/13-records";
import { modules } from "./data/14-modules";
import { functors } from "./data/15-functors";

export const ocamlChapters: Chapter[] = [
  { id: "basics", title: "Basics" },
  { id: "functions", title: "Functions" },
  { id: "data", title: "Data Structures" },
  { id: "types", title: "Types" },
  { id: "advanced", title: "Advanced" },
];

export const ocamlLessons: Lesson[] = [
  helloOCaml,
  letBindings,
  basicTypes,
  arithmetic,
  functions,
  recursion,
  higherOrder,
  lists,
  tuples,
  patternMatching,
  variants,
  optionType,
  records,
  modules,
  functors,
];
