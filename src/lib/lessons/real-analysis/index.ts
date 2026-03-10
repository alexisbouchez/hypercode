import type { Chapter, Lesson } from "../types";
import { sequences } from "./data/01-sequences";
import { epsilonNLimits } from "./data/02-epsilon-n-limits";
import { monotoneConvergence } from "./data/03-monotone-convergence";
import { cauchySequences } from "./data/04-cauchy-sequences";
import { infiniteSeries } from "./data/05-infinite-series";
import { convergenceTests } from "./data/06-convergence-tests";
import { powerSeries } from "./data/07-power-series";
import { taylorSeries } from "./data/08-taylor-series";
import { epsilonDeltaContinuity } from "./data/09-epsilon-delta-continuity";
import { intermediateValueTheorem } from "./data/10-intermediate-value-theorem";
import { differentiability } from "./data/11-differentiability";
import { meanValueTheorem } from "./data/12-mean-value-theorem";
import { riemannSums } from "./data/13-riemann-sums";
import { fundamentalTheorem } from "./data/14-fundamental-theorem";
import { improperIntegrals } from "./data/15-improper-integrals";

export const realAnalysisChapters: Chapter[] = [
	{ id: "sequences", title: "Sequences & Limits" },
	{ id: "series", title: "Series" },
	{ id: "continuity", title: "Continuity & Differentiability" },
	{ id: "integration", title: "Integration" },
];

export const realAnalysisLessons: Lesson[] = [
	sequences,
	epsilonNLimits,
	monotoneConvergence,
	cauchySequences,
	infiniteSeries,
	convergenceTests,
	powerSeries,
	taylorSeries,
	epsilonDeltaContinuity,
	intermediateValueTheorem,
	differentiability,
	meanValueTheorem,
	riemannSums,
	fundamentalTheorem,
	improperIntegrals,
];
