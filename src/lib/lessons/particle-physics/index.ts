import type { Chapter, Lesson } from "../types";
import { naturalUnits } from "./data/01-natural-units";
import { invariantMass } from "./data/02-invariant-mass";
import { cmEnergy } from "./data/03-cm-energy";
import { decayWidth } from "./data/04-decay-width";
import { twoBodyDecay } from "./data/05-two-body-decay";
import { labFrameBoost } from "./data/06-lab-frame-boost";
import { branchingRatio } from "./data/07-branching-ratio";
import { fineStructure } from "./data/08-fine-structure";
import { rutherfordScattering } from "./data/09-rutherford-scattering";
import { breitWigner } from "./data/10-breit-wigner";
import { weinbergAngle } from "./data/11-weinberg-angle";
import { runningCoupling } from "./data/12-running-coupling";
import { higgsMechanism } from "./data/13-higgs-mechanism";
import { crossSectionUnits } from "./data/14-cross-section-units";
import { particleLifetimes } from "./data/15-particle-lifetimes";

export const particlePhysicsChapters: Chapter[] = [
	{ id: "kinematics", title: "Relativistic Kinematics" },
	{ id: "decays", title: "Particle Decays" },
	{ id: "scattering", title: "Scattering" },
	{ id: "standard-model", title: "Standard Model" },
];

export const particlePhysicsLessons: Lesson[] = [
	naturalUnits,
	invariantMass,
	cmEnergy,
	decayWidth,
	twoBodyDecay,
	labFrameBoost,
	branchingRatio,
	fineStructure,
	rutherfordScattering,
	breitWigner,
	weinbergAngle,
	runningCoupling,
	higgsMechanism,
	crossSectionUnits,
	particleLifetimes,
];
