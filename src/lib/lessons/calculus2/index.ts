import type { Chapter } from "../types";
import { arcLength } from "./data/01-arc-length";
import { surfaceArea } from "./data/02-surface-area";
import { work } from "./data/03-work";
import { improperIntegrals } from "./data/04-improper-integrals";
import { partialSums } from "./data/05-partial-sums";
import { geometricSeries } from "./data/06-geometric-series";
import { pSeries } from "./data/07-p-series";
import { alternatingSeries } from "./data/08-alternating-series";
import { ratioTest } from "./data/09-ratio-test";
import { taylorPolynomial } from "./data/10-taylor-polynomial";
import { maclaurinExp } from "./data/11-maclaurin-exp";
import { taylorError } from "./data/12-taylor-error";
import { parametricArcLength } from "./data/13-parametric-arc-length";
import { polarArea } from "./data/14-polar-area";
import { curvature } from "./data/15-curvature";

export const calculus2Chapters: Chapter[] = [
	{ id: "integration-applications", title: "Integration Applications" },
	{ id: "sequences-and-series", title: "Sequences & Series" },
	{ id: "taylor-series", title: "Taylor Series" },
	{ id: "parametric-and-polar", title: "Parametric & Polar" },
];

export const calculus2Lessons = [
	arcLength,
	surfaceArea,
	work,
	improperIntegrals,
	partialSums,
	geometricSeries,
	pSeries,
	alternatingSeries,
	ratioTest,
	taylorPolynomial,
	maclaurinExp,
	taylorError,
	parametricArcLength,
	polarArea,
	curvature,
];
