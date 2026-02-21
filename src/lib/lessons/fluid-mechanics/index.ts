import type { Chapter, Lesson } from "../types";
import { density } from "./data/01-density";
import { pressure } from "./data/02-pressure";
import { hydrostaticPressure } from "./data/03-hydrostatic-pressure";
import { buoyancy } from "./data/04-buoyancy";
import { manometer } from "./data/05-manometer";
import { continuityEquation } from "./data/06-continuity";
import { bernoulliEquation } from "./data/07-bernoulli";
import { reynoldsNumber } from "./data/08-reynolds-number";
import { dragForce } from "./data/09-drag-force";
import { viscosity } from "./data/10-viscosity";
import { poiseuilleFlow } from "./data/11-poiseuille";
import { pipeLosses } from "./data/12-pipe-losses";
import { stokesLaw } from "./data/13-stokes-law";
import { surfaceTension } from "./data/14-surface-tension";
import { flowMeasurement } from "./data/15-flow-measurement";
import { machNumber } from "./data/16-mach-number";

export const fluidMechanicsChapters: Chapter[] = [
	{ id: "fundamentals", title: "Fundamentals" },
	{ id: "statics", title: "Fluid Statics" },
	{ id: "dynamics", title: "Flow Dynamics" },
	{ id: "viscous", title: "Viscous Flow" },
];

export const fluidMechanicsLessons: Lesson[] = [
	density,
	pressure,
	hydrostaticPressure,
	buoyancy,
	manometer,
	continuityEquation,
	bernoulliEquation,
	reynoldsNumber,
	dragForce,
	viscosity,
	poiseuilleFlow,
	pipeLosses,
	stokesLaw,
	surfaceTension,
	flowMeasurement,
	machNumber,
];
