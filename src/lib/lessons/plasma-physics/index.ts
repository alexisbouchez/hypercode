import type { Chapter, Lesson } from "../types";
import { debyeLength } from "./data/01-debye-length";
import { plasmaFrequency } from "./data/02-plasma-frequency";
import { cyclotronMotion } from "./data/03-cyclotron-motion";
import { thermalVelocity } from "./data/04-thermal-velocity";
import { plasmaBeta } from "./data/05-plasma-beta";
import { exbDrift } from "./data/06-exb-drift";
import { magneticMirror } from "./data/07-magnetic-mirror";
import { alfvenSpeed } from "./data/08-alfven-speed";
import { bremsstrahlung } from "./data/09-bremsstrahlung";
import { coulombLogarithm } from "./data/10-coulomb-logarithm";
import { sahaEquation } from "./data/11-saha-equation";
import { langmuirProbe } from "./data/12-langmuir-probe";
import { fusionPower } from "./data/13-fusion-power";
import { mhdEquilibrium } from "./data/14-mhd-equilibrium";
import { resistivity } from "./data/15-resistivity";

export const plasmaPhysicsChapters: Chapter[] = [
	{ id: "plasma-fundamentals", title: "Plasma Fundamentals" },
	{ id: "single-particle", title: "Single Particle Motion" },
	{ id: "mhd", title: "MHD & Waves" },
	{ id: "diagnostics", title: "Diagnostics & Applications" },
	{ id: "fusion", title: "Fusion Energy" },
];

export const plasmaPhysicsLessons: Lesson[] = [
	debyeLength,
	plasmaFrequency,
	cyclotronMotion,
	thermalVelocity,
	plasmaBeta,
	exbDrift,
	magneticMirror,
	alfvenSpeed,
	bremsstrahlung,
	coulombLogarithm,
	sahaEquation,
	langmuirProbe,
	fusionPower,
	mhdEquilibrium,
	resistivity,
];
