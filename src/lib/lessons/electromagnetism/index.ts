import type { Chapter, Lesson } from "../types";
import { coulombsLawLesson } from "./data/01-coulombs-law";
import { electricFieldLesson } from "./data/02-electric-field";
import { electricPotentialLesson } from "./data/03-electric-potential";
import { ohmsLawLesson } from "./data/04-ohms-law";
import { seriesResistanceLesson } from "./data/05-series-resistance";
import { parallelResistanceLesson } from "./data/06-parallel-resistance";
import { electricPowerLesson } from "./data/07-electric-power";
import { wireMagneticFieldLesson } from "./data/08-wire-magnetic-field";
import { lorentzForceLesson } from "./data/09-lorentz-force";
import { magneticFluxLesson } from "./data/10-magnetic-flux";
import { solenoidInductanceLesson } from "./data/11-solenoid-inductance";
import { faradaysLawLesson } from "./data/12-faradays-law";
import { rcTimeConstantLesson } from "./data/13-rc-time-constant";
import { lcResonanceLesson } from "./data/14-lc-resonance";
import { skinDepthLesson } from "./data/15-skin-depth";

export const electromagnetismChapters: Chapter[] = [
	{ id: "electric-fields", title: "Electric Fields & Forces" },
	{ id: "dc-circuits", title: "DC Circuits" },
	{ id: "magnetism", title: "Magnetism" },
	{ id: "em-induction", title: "Electromagnetic Induction" },
];

export const electromagnetismLessons: Lesson[] = [
	coulombsLawLesson,
	electricFieldLesson,
	electricPotentialLesson,
	ohmsLawLesson,
	seriesResistanceLesson,
	parallelResistanceLesson,
	electricPowerLesson,
	wireMagneticFieldLesson,
	lorentzForceLesson,
	magneticFluxLesson,
	solenoidInductanceLesson,
	faradaysLawLesson,
	rcTimeConstantLesson,
	lcResonanceLesson,
	skinDepthLesson,
];
