import type { Chapter } from "../types";
import { numericalLimit } from "./data/01-limit";
import { derivative } from "./data/02-derivative";
import { secondDerivative } from "./data/03-second-derivative";
import { tangentLine } from "./data/04-tangent-line";
import { newtonsMethod } from "./data/05-newtons-method";
import { criticalPoints } from "./data/06-critical-points";
import { meanValueTheorem } from "./data/07-mean-value-theorem";
import { leftRiemann } from "./data/08-left-riemann";
import { rightRiemann } from "./data/09-right-riemann";
import { midpointRule } from "./data/10-midpoint-rule";
import { trapezoidRule } from "./data/11-trapezoid";
import { simpsonsRule } from "./data/12-simpsons-rule";
import { averageValue } from "./data/13-average-value";
import { areaBetween } from "./data/14-area-between";
import { volumeRevolution } from "./data/15-volume-revolution";

export const calculusChapters: Chapter[] = [
	{ id: "limits-and-derivatives", title: "Limits & Derivatives" },
	{ id: "derivative-applications", title: "Derivative Applications" },
	{ id: "integration", title: "Integration" },
	{ id: "integral-applications", title: "Integral Applications" },
];

export const calculusLessons = [
	numericalLimit,
	derivative,
	secondDerivative,
	tangentLine,
	newtonsMethod,
	criticalPoints,
	meanValueTheorem,
	leftRiemann,
	rightRiemann,
	midpointRule,
	trapezoidRule,
	simpsonsRule,
	averageValue,
	areaBetween,
	volumeRevolution,
];
