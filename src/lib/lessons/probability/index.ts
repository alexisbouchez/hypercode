import type { Chapter, Lesson } from "../types";
import { sampleSpace } from "./data/01-sample-space";
import { probabilityAxioms } from "./data/02-probability-axioms";
import { complementRule } from "./data/03-complement-rule";
import { conditionalProbability } from "./data/04-conditional-probability";
import { independence } from "./data/05-independence";
import { bayesTheorem } from "./data/06-bayes-theorem";
import { discreteRv } from "./data/07-discrete-rv";
import { continuousRv } from "./data/08-continuous-rv";
import { expectationVariance } from "./data/09-expectation-variance";
import { bernoulliBinomial } from "./data/10-bernoulli-binomial";
import { poissonExponential } from "./data/11-poisson-exponential";
import { normalDistribution } from "./data/12-normal-distribution";
import { gammaBeta } from "./data/13-gamma-beta";
import { lawOfLargeNumbers } from "./data/14-law-of-large-numbers";
import { markovChains } from "./data/15-markov-chains";

export const probabilityChapters: Chapter[] = [
	{ id: "foundations", title: "Foundations" },
	{ id: "conditional", title: "Conditional Probability & Independence" },
	{ id: "random-variables", title: "Random Variables & Expectation" },
	{ id: "distributions", title: "Common Distributions" },
	{ id: "limit-theorems", title: "Limit Theorems & Markov Chains" },
];

export const probabilityLessons: Lesson[] = [
	sampleSpace,
	probabilityAxioms,
	complementRule,
	conditionalProbability,
	independence,
	bayesTheorem,
	discreteRv,
	continuousRv,
	expectationVariance,
	bernoulliBinomial,
	poissonExponential,
	normalDistribution,
	gammaBeta,
	lawOfLargeNumbers,
	markovChains,
];
