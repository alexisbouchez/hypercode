import type { Chapter, Lesson } from "../types";
import { propositionalLogic } from "./data/01-propositional-logic";
import { predicateLogic } from "./data/02-predicate-logic";
import { proofTechniques } from "./data/03-proof-techniques";
import { setTheory } from "./data/04-set-theory";
import { relations } from "./data/05-relations";
import { functions } from "./data/06-functions";
import { basicCounting } from "./data/07-basic-counting";
import { inclusionExclusion } from "./data/08-inclusion-exclusion";
import { advancedCombinatorics } from "./data/09-advanced-combinatorics";
import { induction } from "./data/10-induction";
import { recurrences } from "./data/11-recurrences";
import { generatingFunctions } from "./data/12-generating-functions";
import { graphFundamentals } from "./data/13-graph-fundamentals";
import { eulerHamilton } from "./data/14-euler-hamilton";
import { graphColoring } from "./data/15-graph-coloring";

export const discreteMathChapters: Chapter[] = [
	{ id: "logic-proofs", title: "Logic & Proofs" },
	{ id: "sets-relations-functions", title: "Sets, Relations & Functions" },
	{ id: "counting-combinatorics", title: "Counting & Combinatorics" },
	{ id: "induction-recurrences", title: "Induction & Recurrences" },
	{ id: "graph-theory", title: "Graph Theory" },
];

export const discreteMathLessons: Lesson[] = [
	propositionalLogic,
	predicateLogic,
	proofTechniques,
	setTheory,
	relations,
	functions,
	basicCounting,
	inclusionExclusion,
	advancedCombinatorics,
	induction,
	recurrences,
	generatingFunctions,
	graphFundamentals,
	eulerHamilton,
	graphColoring,
];
