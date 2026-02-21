import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { variables } from "./data/02-variables";
import { dataTypes } from "./data/03-data-types";
import { functions } from "./data/04-functions";
import { controlFlow } from "./data/05-control-flow";
import { ownership } from "./data/06-ownership";
import { references } from "./data/07-references";
import { stringTypes } from "./data/08-string-types";
import { slices } from "./data/09-slices";
import { structs } from "./data/10-structs";
import { enums } from "./data/11-enums";
import { option } from "./data/12-option";
import { result } from "./data/13-result";
import { patternMatching } from "./data/14-pattern-matching";
import { traits } from "./data/15-traits";
import { generics } from "./data/16-generics";
import { closures } from "./data/17-closures";
import { iterators } from "./data/18-iterators";
import { vectors } from "./data/19-vectors";
import { hashmaps } from "./data/20-hashmaps";

export const rustChapters: Chapter[] = [
  { id: "basics", title: "The Basics" },
  { id: "ownership", title: "Ownership & Memory" },
  { id: "structs-enums", title: "Structs, Enums & Patterns" },
  { id: "traits-generics", title: "Traits & Generics" },
  { id: "collections", title: "Collections" },
];

export const rustLessons: Lesson[] = [
  helloWorld,
  variables,
  dataTypes,
  functions,
  controlFlow,
  ownership,
  references,
  stringTypes,
  slices,
  structs,
  enums,
  option,
  result,
  patternMatching,
  traits,
  generics,
  closures,
  iterators,
  vectors,
  hashmaps,
];
