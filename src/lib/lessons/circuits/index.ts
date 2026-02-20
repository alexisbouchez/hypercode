import type { Chapter } from "../types";
import { ohmsLaw } from "./data/01-ohms-law";
import { seriesResistance } from "./data/02-series-resistance";
import { parallelResistance } from "./data/03-parallel-resistance";
import { voltageDivider } from "./data/04-voltage-divider";
import { currentDivider } from "./data/05-current-divider";
import { powerDissipated } from "./data/06-power";
import { nodeVoltage } from "./data/07-node-voltage";
import { wheatstoneBridge } from "./data/08-wheatstone-bridge";
import { rcCharge } from "./data/09-rc-charge";
import { rlStep } from "./data/10-rl-step";
import { rlcTransient } from "./data/11-rlc-transient";
import { capacitorEnergy } from "./data/12-capacitor-energy";
import { rcLowpass } from "./data/13-rc-lowpass";
import { rcHighpass } from "./data/14-rc-highpass";
import { lcResonance } from "./data/15-lc-resonance";

export const circuitsChapters: Chapter[] = [
	{ id: "dc-fundamentals", title: "DC Fundamentals" },
	{ id: "circuit-analysis", title: "Circuit Analysis" },
	{ id: "transient-response", title: "Transient Response" },
	{ id: "ac-and-filters", title: "AC & Filters" },
];

export const circuitsLessons = [
	ohmsLaw,
	seriesResistance,
	parallelResistance,
	voltageDivider,
	currentDivider,
	powerDissipated,
	nodeVoltage,
	wheatstoneBridge,
	rcCharge,
	rlStep,
	rlcTransient,
	capacitorEnergy,
	rcLowpass,
	rcHighpass,
	lcResonance,
];
