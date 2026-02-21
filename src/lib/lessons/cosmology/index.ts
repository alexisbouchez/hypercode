import type { Chapter, Lesson } from "../types";
import { hubbleLaw } from "./data/01-hubble-law";
import { friedmannEquation } from "./data/02-friedmann";
import { redshiftScaleFactor } from "./data/03-redshift";
import { comovingDistance } from "./data/04-comoving-distance";
import { distanceMeasures } from "./data/05-distance-measures";
import { cmbTemperature } from "./data/06-cmb-temperature";
import { criticalDensity } from "./data/07-critical-density";
import { matterRadiationEquality } from "./data/08-matter-radiation-equality";
import { ageOfUniverse } from "./data/09-age-universe";
import { recombination } from "./data/10-recombination";
import { jeansInstability } from "./data/11-jeans-instability";
import { growthFactor } from "./data/12-growth-factor";
import { darkEnergy } from "./data/13-dark-energy";
import { planckUnits } from "./data/14-planck-units";
import { neutrinoTemperature } from "./data/15-neutrino-temperature";

export const cosmologyChapters: Chapter[] = [
	{ id: "expansion", title: "Cosmic Expansion" },
	{ id: "thermal-history", title: "Thermal History" },
	{ id: "structure", title: "Structure Formation" },
	{ id: "dark-sector", title: "Dark Sector" },
];

export const cosmologyLessons: Lesson[] = [
	hubbleLaw,
	friedmannEquation,
	redshiftScaleFactor,
	comovingDistance,
	distanceMeasures,
	cmbTemperature,
	criticalDensity,
	matterRadiationEquality,
	ageOfUniverse,
	recombination,
	jeansInstability,
	growthFactor,
	darkEnergy,
	planckUnits,
	neutrinoTemperature,
];
