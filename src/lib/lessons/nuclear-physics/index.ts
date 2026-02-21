import type { Chapter, Lesson } from "../types";
import { nuclearRadius } from "./data/01-nuclear-radius";
import { massDefect } from "./data/02-mass-defect";
import { semiEmpiricalMass } from "./data/03-semi-empirical-mass";
import { decayConstant } from "./data/04-decay-constant";
import { radioactiveDecay } from "./data/05-radioactive-decay";
import { radiometricDating } from "./data/06-radiometric-dating";
import { activity } from "./data/07-activity";
import { qValue } from "./data/08-q-value";
import { coulombBarrier } from "./data/09-coulomb-barrier";
import { crossSection } from "./data/10-cross-section";
import { fissionEnergy } from "./data/11-fission-energy";
import { fusionEnergy } from "./data/12-fusion-energy";
import { radioactiveChains } from "./data/13-radioactive-chains";
import { radiationDose } from "./data/14-radiation-dose";
import { neutronModeration } from "./data/15-neutron-moderation";

export const nuclearPhysicsChapters: Chapter[] = [
	{ id: "nuclear-structure", title: "Nuclear Structure" },
	{ id: "radioactive-decay", title: "Radioactive Decay" },
	{ id: "nuclear-reactions", title: "Nuclear Reactions" },
	{ id: "nuclear-energy", title: "Nuclear Energy" },
];

export const nuclearPhysicsLessons: Lesson[] = [
	nuclearRadius,
	massDefect,
	semiEmpiricalMass,
	decayConstant,
	radioactiveDecay,
	radiometricDating,
	activity,
	qValue,
	coulombBarrier,
	crossSection,
	fissionEnergy,
	fusionEnergy,
	radioactiveChains,
	radiationDose,
	neutronModeration,
];
