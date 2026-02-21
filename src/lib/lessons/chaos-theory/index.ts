import type { Chapter, Lesson } from "../types";
import { tentMap } from "./data/01-tent-map";
import { lyapunovSpectrum } from "./data/02-lyapunov-spectrum";
import { poincareMap } from "./data/03-poincare-map";
import { rosslerAttractor } from "./data/04-rossler-attractor";
import { duffingOscillator } from "./data/05-duffing-oscillator";
import { henonMap } from "./data/06-henon-map";
import { vanDerPol } from "./data/07-van-der-pol";
import { ikedaMap } from "./data/08-ikeda-map";
import { boxCounting } from "./data/09-box-counting";
import { correlationDimension } from "./data/10-correlation-dimension";
import { mandelbrot } from "./data/11-mandelbrot";
import { juliaSets } from "./data/12-julia-sets";
import { feigenbaum } from "./data/13-feigenbaum";
import { logisticBifurcation } from "./data/14-logistic-bifurcation";
import { chaosSynchronization } from "./data/15-chaos-synchronization";

export const chaosTheoryChapters: Chapter[] = [
	{ id: "lyapunov", title: "Lyapunov & Divergence" },
	{ id: "classic-attractors", title: "Classic Chaotic Attractors" },
	{ id: "fractal-geometry", title: "Fractal Geometry" },
	{ id: "routes-to-chaos", title: "Routes to Chaos" },
];

export const chaosTheoryLessons: Lesson[] = [
	tentMap,
	lyapunovSpectrum,
	poincareMap,
	rosslerAttractor,
	duffingOscillator,
	henonMap,
	vanDerPol,
	ikedaMap,
	boxCounting,
	correlationDimension,
	mandelbrot,
	juliaSets,
	feigenbaum,
	logisticBifurcation,
	chaosSynchronization,
];
