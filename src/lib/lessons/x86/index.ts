import type { Chapter, Lesson } from "../types";
import { helloX86 } from "./data/01-hello-x86";
import { registers } from "./data/02-registers";
import { movInstruction } from "./data/03-mov-instruction";
import { dataSection } from "./data/04-data-section";
import { addSub } from "./data/05-add-sub";
import { mulDiv } from "./data/06-mul-div";
import { bitwiseOps } from "./data/07-bitwise";
import { cmpFlags } from "./data/08-cmp-flags";
import { conditionalJumps } from "./data/09-conditional-jumps";
import { loops } from "./data/10-loops";
import { stackPushPop } from "./data/11-stack-push-pop";
import { callRet } from "./data/12-call-ret";
import { functionArgs } from "./data/13-function-args";
import { stringOps } from "./data/14-string-ops";
import { puttingItTogether } from "./data/15-putting-it-together";

export const x86Chapters: Chapter[] = [
  { id: "basics", title: "Basics" },
  { id: "arithmetic", title: "Arithmetic" },
  { id: "control-flow", title: "Control Flow" },
  { id: "functions", title: "Functions" },
  { id: "advanced", title: "Advanced" },
];

export const x86Lessons: Lesson[] = [
  helloX86,
  registers,
  movInstruction,
  dataSection,
  addSub,
  mulDiv,
  bitwiseOps,
  cmpFlags,
  conditionalJumps,
  loops,
  stackPushPop,
  callRet,
  functionArgs,
  stringOps,
  puttingItTogether,
];
