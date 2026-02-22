import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { variables } from "./data/02-variables";
import { stringInterpolation } from "./data/03-string-interpolation";
import { conditionals } from "./data/04-conditionals";
import { switchLesson } from "./data/05-switch";
import { loops } from "./data/06-loops";
import { arrays } from "./data/07-arrays";
import { dictionaries } from "./data/08-dictionaries";
import { optionals } from "./data/09-optionals";
import { closures } from "./data/10-closures";
import { higherOrder } from "./data/11-higher-order";
import { structs } from "./data/12-structs";
import { enums } from "./data/13-enums";
import { classes } from "./data/14-classes";
import { protocols } from "./data/15-protocols";

export const swiftChapters: Chapter[] = [
  { id: "basics", title: "Basics" },
  { id: "collections", title: "Collections" },
  { id: "functional", title: "Functional" },
  { id: "types", title: "Types" },
];

export const swiftLessons: Lesson[] = [
  helloWorld,
  variables,
  stringInterpolation,
  conditionals,
  switchLesson,
  loops,
  arrays,
  dictionaries,
  optionals,
  closures,
  higherOrder,
  structs,
  enums,
  classes,
  protocols,
];
