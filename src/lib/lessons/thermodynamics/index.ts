import type { Chapter, Lesson } from "../types";
import { temperatureScales } from "./data/01-temperature-scales";
import { firstLaw } from "./data/02-first-law";
import { heatCapacity } from "./data/03-heat-capacity";
import { idealGas } from "./data/04-ideal-gas";
import { thermalExpansion } from "./data/05-thermal-expansion";
import { isothermalProcess } from "./data/06-isothermal";
import { adiabaticProcess } from "./data/07-adiabatic";
import { entropyChange } from "./data/08-entropy";
import { gibbsEnergy } from "./data/09-gibbs-energy";
import { entropyMixing } from "./data/10-entropy-mixing";
import { carnotEfficiency } from "./data/11-carnot-efficiency";
import { carnotCycle } from "./data/12-carnot-cycle";
import { heatPumps } from "./data/13-heat-pumps";
import { boltzmannDistribution } from "./data/14-boltzmann";
import { maxwellBoltzmann } from "./data/15-maxwell-boltzmann";

export const thermodynamicsChapters: Chapter[] = [
	{ id: "laws", title: "Laws of Thermodynamics" },
	{ id: "processes", title: "Thermodynamic Processes" },
	{ id: "cycles", title: "Heat Engines & Cycles" },
	{ id: "statistical", title: "Statistical Thermodynamics" },
];

export const thermodynamicsLessons: Lesson[] = [
	temperatureScales,
	firstLaw,
	heatCapacity,
	idealGas,
	thermalExpansion,
	isothermalProcess,
	adiabaticProcess,
	entropyChange,
	gibbsEnergy,
	entropyMixing,
	carnotEfficiency,
	carnotCycle,
	heatPumps,
	boltzmannDistribution,
	maxwellBoltzmann,
];
