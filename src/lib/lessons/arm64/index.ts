import type { Chapter, Lesson } from "../types";
import { helloArm64 } from "./data/01-hello-arm64";
import { registersAndMov } from "./data/02-registers-and-mov";
import { arithmetic } from "./data/03-arithmetic";
import { memoryLdrStr } from "./data/04-memory-ldr-str";
import { addressingModes } from "./data/05-addressing-modes";
import { theStack } from "./data/06-the-stack";
import { flagsAndCmp } from "./data/07-flags-and-cmp";
import { conditionalBranches } from "./data/08-conditional-branches";
import { loops } from "./data/09-loops";
import { functionsAndBl } from "./data/10-functions-and-bl";
import { callingConvention } from "./data/11-calling-convention";
import { recursiveFunctions } from "./data/12-recursive-functions";
import { bitwiseOperations } from "./data/13-bitwise-operations";
import { bitManipulation } from "./data/14-bit-manipulation";
import { stringOperations } from "./data/15-string-operations";
import { sorting } from "./data/16-sorting";

export const arm64Chapters: Chapter[] = [
  { id: "foundations", title: "Foundations" },
  { id: "memory", title: "Memory" },
  { id: "control-flow", title: "Control Flow" },
  { id: "functions", title: "Functions" },
  { id: "bitwise", title: "Bitwise Operations" },
  { id: "putting-it-together", title: "Putting It Together" },
];

export const arm64Lessons: Lesson[] = [
  helloArm64,
  registersAndMov,
  arithmetic,
  memoryLdrStr,
  addressingModes,
  theStack,
  flagsAndCmp,
  conditionalBranches,
  loops,
  functionsAndBl,
  callingConvention,
  recursiveFunctions,
  bitwiseOperations,
  bitManipulation,
  stringOperations,
  sorting,
];
