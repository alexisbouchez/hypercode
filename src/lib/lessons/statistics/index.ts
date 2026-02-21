import type { Chapter, Lesson } from "../types";
import { meanMedianMode } from "./data/01-mean-median-mode";
import { varianceStd } from "./data/02-variance-std";
import { percentiles } from "./data/03-percentiles";
import { zScores } from "./data/04-z-scores";
import { normalDistribution } from "./data/05-normal-distribution";
import { binomialDistribution } from "./data/06-binomial";
import { sampling } from "./data/07-sampling";
import { standardError } from "./data/08-standard-error";
import { ttestOneSample } from "./data/09-ttest-one-sample";
import { confidenceInterval } from "./data/10-confidence-interval";
import { ttestTwoSample } from "./data/11-ttest-two-sample";
import { chiSquare } from "./data/12-chi-square";
import { correlation } from "./data/13-correlation";
import { linearRegression } from "./data/14-linear-regression";
import { bootstrap } from "./data/15-bootstrap";
import { skewness } from "./data/16-skewness";
import { poissonDistribution } from "./data/17-poisson";
import { anova } from "./data/18-anova";

export const statisticsChapters: Chapter[] = [
	{ id: "descriptive", title: "Descriptive Statistics" },
	{ id: "distributions", title: "Distributions" },
	{ id: "inference", title: "Inference" },
	{ id: "regression", title: "Regression" },
];

export const statisticsLessons: Lesson[] = [
	meanMedianMode,
	varianceStd,
	percentiles,
	zScores,
	normalDistribution,
	binomialDistribution,
	sampling,
	standardError,
	ttestOneSample,
	confidenceInterval,
	ttestTwoSample,
	chiSquare,
	correlation,
	linearRegression,
	bootstrap,
	skewness,
	poissonDistribution,
	anova,
];
