import type { Chapter } from "../types";
import { andGate } from "./data/01-and-gate";
import { orNotGates } from "./data/02-or-not-gates";
import { nandNorGates } from "./data/03-nand-nor-gates";
import { xorXnorGates } from "./data/04-xor-xnor-gates";
import { universalGates } from "./data/05-universal-gates";
import { deMorgansLaw } from "./data/06-de-morgans-law";
import { truthTableGenerator } from "./data/07-truth-table-generator";
import { sumOfProducts } from "./data/08-sum-of-products";
import { halfAdder } from "./data/09-half-adder";
import { fullAdder } from "./data/10-full-adder";
import { rippleCarryAdder } from "./data/11-ripple-carry-adder";
import { multiplexer } from "./data/12-multiplexer";
import { decoder } from "./data/13-decoder";
import { srLatch } from "./data/14-sr-latch";
import { binaryCounter } from "./data/15-binary-counter";

export const digitalLogicChapters: Chapter[] = [
	{ id: "logic-gates", title: "Logic Gates" },
	{ id: "boolean-algebra", title: "Boolean Algebra" },
	{ id: "combinational-circuits", title: "Combinational Circuits" },
	{ id: "sequential-circuits", title: "Sequential Circuits" },
];

export const digitalLogicLessons = [
	andGate,
	orNotGates,
	nandNorGates,
	xorXnorGates,
	universalGates,
	deMorgansLaw,
	truthTableGenerator,
	sumOfProducts,
	halfAdder,
	fullAdder,
	rippleCarryAdder,
	multiplexer,
	decoder,
	srLatch,
	binaryCounter,
];
