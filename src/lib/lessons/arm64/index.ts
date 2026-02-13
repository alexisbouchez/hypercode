import type { Chapter, Lesson } from "../types";
import { helloArm64 } from "./data/01-hello-arm64";
import { registersAndMov } from "./data/02-registers-and-mov";
import { arithmetic } from "./data/03-arithmetic";
import { signedArithmetic } from "./data/04-signed-arithmetic";
import { memoryLdrStr } from "./data/05-memory-ldr-str";
import { dataSizes } from "./data/06-data-sizes";
import { addressingModes } from "./data/07-addressing-modes";
import { theStack } from "./data/08-the-stack";
import { flagsAndCmp } from "./data/09-flags-and-cmp";
import { conditionalBranches } from "./data/10-conditional-branches";
import { conditionalSelection } from "./data/11-conditional-selection";
import { loops } from "./data/12-loops";
import { functionsAndBl } from "./data/13-functions-and-bl";
import { callingConvention } from "./data/14-calling-convention";
import { functionPointers } from "./data/15-function-pointers";
import { recursiveFunctions } from "./data/16-recursive-functions";
import { bitwiseOperations } from "./data/17-bitwise-operations";
import { bitManipulation } from "./data/18-bit-manipulation";
import { xorCipher } from "./data/23-xor-cipher";
import { stringOperations } from "./data/19-string-operations";
import { multiplyAccumulate } from "./data/20-multiply-accumulate";
import { sorting } from "./data/21-sorting";
import { binarySearch } from "./data/22-binary-search";
import { fibonacci } from "./data/24-fibonacci";
import { gcdAlgorithm } from "./data/25-gcd";
import { stringReversal } from "./data/26-string-reversal";
import { stringComparison } from "./data/27-string-comparison";
import { numberPrinting } from "./data/28-number-printing";

export const arm64Chapters: Chapter[] = [
  { id: "foundations", title: "Foundations" },
  { id: "memory", title: "Memory" },
  { id: "control-flow", title: "Control Flow" },
  { id: "functions", title: "Functions" },
  { id: "bitwise", title: "Bitwise Operations" },
  { id: "putting-it-together", title: "Putting It Together" },
  { id: "challenges", title: "Challenges" },
];

export const arm64Lessons: Lesson[] = [
  helloArm64,
  registersAndMov,
  arithmetic,
  signedArithmetic,
  memoryLdrStr,
  dataSizes,
  addressingModes,
  theStack,
  flagsAndCmp,
  conditionalBranches,
  conditionalSelection,
  loops,
  functionsAndBl,
  callingConvention,
  functionPointers,
  recursiveFunctions,
  bitwiseOperations,
  bitManipulation,
  xorCipher,
  stringOperations,
  multiplyAccumulate,
  sorting,
  binarySearch,
  fibonacci,
  gcdAlgorithm,
  stringReversal,
  stringComparison,
  numberPrinting,
];
