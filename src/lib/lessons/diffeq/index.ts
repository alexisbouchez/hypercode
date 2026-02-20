import type { Chapter } from "../types";
import { eulerStep } from "./data/01-euler-step";
import { eulerIntegration } from "./data/02-euler-integration";
import { rk4Step } from "./data/03-rk4-step";
import { rk4Integration } from "./data/04-rk4-integration";
import { exponentialDecay } from "./data/05-exponential-decay";
import { logisticGrowth } from "./data/06-logistic-growth";
import { newtonsCooling } from "./data/07-newtons-cooling";
import { eulerSystem } from "./data/08-euler-system";
import { harmonicMotion } from "./data/09-harmonic-motion";
import { dampedOscillator } from "./data/10-damped-oscillator";
import { lotkaVolterra } from "./data/11-lotka-volterra";
import { sirModel } from "./data/12-sir-model";
import { equilibria } from "./data/13-equilibria";
import { stability } from "./data/14-stability";
import { vanDerPol } from "./data/15-van-der-pol";

export const diffeqChapters: Chapter[] = [
	{ id: "numerical-methods", title: "Numerical Methods" },
	{ id: "first-order-models", title: "First-Order Models" },
	{ id: "systems-and-oscillations", title: "Systems & Oscillations" },
	{ id: "applications", title: "Applications" },
];

export const diffeqLessons = [
	eulerStep,
	eulerIntegration,
	rk4Step,
	rk4Integration,
	exponentialDecay,
	logisticGrowth,
	newtonsCooling,
	eulerSystem,
	harmonicMotion,
	dampedOscillator,
	lotkaVolterra,
	sirModel,
	equilibria,
	stability,
	vanDerPol,
];
