import type { Chapter, Lesson } from "../types";
import { fourierSeries } from "./data/01-fourier-series";
import { dft } from "./data/02-dft";
import { legendre } from "./data/03-legendre";
import { gammaFunction } from "./data/04-gamma-function";
import { bessel } from "./data/05-bessel";
import { hermite } from "./data/06-hermite";
import { heatEquation } from "./data/07-heat-equation";
import { waveEquation } from "./data/08-wave-equation";
import { eulerLagrange } from "./data/09-euler-lagrange";
import { rungeKutta } from "./data/10-runge-kutta";
import { laplaceTransform } from "./data/11-laplace-transform";
import { greensFunctions } from "./data/12-greens-functions";
import { perturbationTheory } from "./data/13-perturbation-theory";
import { tensorOperations } from "./data/14-tensor-operations";
import { monteCarlo } from "./data/15-monte-carlo";

export const mathematicalPhysicsChapters: Chapter[] = [
	{ id: "transforms", title: "Transforms" },
	{ id: "special-functions", title: "Special Functions" },
	{ id: "pdes", title: "Partial Differential Equations" },
	{ id: "variational", title: "Variational Methods" },
	{ id: "numerical", title: "Numerical Methods" },
	{ id: "quantum-methods", title: "Quantum Methods" },
	{ id: "geometry", title: "Differential Geometry" },
];

export const mathematicalPhysicsLessons: Lesson[] = [
	fourierSeries,
	dft,
	legendre,
	gammaFunction,
	bessel,
	hermite,
	heatEquation,
	waveEquation,
	eulerLagrange,
	rungeKutta,
	laplaceTransform,
	greensFunctions,
	perturbationTheory,
	tensorOperations,
	monteCarlo,
];
