import type { Chapter, Lesson } from "../types";
import { helloPerl } from "./data/01-hello-perl";
import { scalars } from "./data/02-scalars";
import { arithmetic } from "./data/03-arithmetic";
import { strings } from "./data/04-strings";
import { ifUnless } from "./data/05-if-unless";
import { whileUntil } from "./data/06-while-until";
import { forForeach } from "./data/07-for-foreach";
import { arrays } from "./data/08-arrays";
import { hashes } from "./data/09-hashes";
import { references } from "./data/10-references";
import { subroutines } from "./data/11-subroutines";
import { stringOperations } from "./data/12-string-operations";
import { regularExpressions } from "./data/13-regular-expressions";
import { fileSimulation } from "./data/14-file-simulation";
import { puttingItTogether } from "./data/15-putting-it-together";

export const perlChapters: Chapter[] = [
  { id: "basics", title: "Perl Basics" },
  { id: "control-flow", title: "Control Flow" },
  { id: "data", title: "Data Structures" },
  { id: "functions", title: "Functions & I/O" },
  { id: "advanced", title: "Advanced Perl" },
];

export const perlLessons: Lesson[] = [
  helloPerl,
  scalars,
  arithmetic,
  strings,
  ifUnless,
  whileUntil,
  forForeach,
  arrays,
  hashes,
  references,
  subroutines,
  stringOperations,
  regularExpressions,
  fileSimulation,
  puttingItTogether,
];
