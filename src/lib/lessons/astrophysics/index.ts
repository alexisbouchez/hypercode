import type { Chapter, Lesson } from "../types";
import { stellarLuminosity } from "./data/01-stellar-luminosity";
import { blackbodyPeak } from "./data/02-blackbody-peak";
import { stellarMagnitude } from "./data/03-stellar-magnitude";
import { eddingtonLuminosity } from "./data/04-eddington-luminosity";
import { nuclearTimescale } from "./data/05-nuclear-timescale";
import { mainSequence } from "./data/06-main-sequence";
import { hydrostaticEquilibrium } from "./data/07-hydrostatic-equilibrium";
import { chandrasekharMass } from "./data/08-chandrasekhar-mass";
import { orbitalMechanics } from "./data/09-orbital-mechanics";
import { escapeVelocity } from "./data/10-escape-velocity";
import { accretion } from "./data/11-accretion";
import { gravitationalLensing } from "./data/12-gravitational-lensing";
import { galacticRotation } from "./data/13-galactic-rotation";
import { virialTheorem } from "./data/14-virial-theorem";
import { stellarAges } from "./data/15-stellar-ages";

export const astrophysicsChapters: Chapter[] = [
	{ id: "stellar-physics", title: "Stellar Physics" },
	{ id: "stellar-evolution", title: "Stellar Evolution" },
	{ id: "compact-objects", title: "Compact Objects" },
	{ id: "galactic", title: "Galactic Astrophysics" },
];

export const astrophysicsLessons: Lesson[] = [
	stellarLuminosity,
	blackbodyPeak,
	stellarMagnitude,
	eddingtonLuminosity,
	nuclearTimescale,
	mainSequence,
	hydrostaticEquilibrium,
	chandrasekharMass,
	orbitalMechanics,
	escapeVelocity,
	accretion,
	gravitationalLensing,
	galacticRotation,
	virialTheorem,
	stellarAges,
];
