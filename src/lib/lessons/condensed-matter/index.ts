import type { Chapter, Lesson } from "../types";
import { braggDiffraction } from "./data/01-bragg-diffraction";
import { fermiEnergy } from "./data/02-fermi-energy";
import { fermiDirac } from "./data/03-fermi-dirac";
import { debyeModel } from "./data/04-debye-model";
import { einsteinModel } from "./data/05-einstein-model";
import { phononDispersion } from "./data/06-phonon-dispersion";
import { hallEffect } from "./data/07-hall-effect";
import { semiconductorCarriers } from "./data/08-semiconductor-carriers";
import { superconductivity } from "./data/09-superconductivity";
import { curieWeiss } from "./data/10-curie-weiss";
import { bandGap } from "./data/11-band-gap";
import { thermalConductivity } from "./data/12-thermal-conductivity";
import { electronHeatCapacity } from "./data/13-electron-heat-capacity";
import { tightBinding } from "./data/14-tight-binding";
import { meissnerEffect } from "./data/15-meissner-effect";

export const condensedMatterChapters: Chapter[] = [
	{ id: "crystal-structure", title: "Crystal Structure & Electrons" },
	{ id: "phonons", title: "Phonons & Thermal Properties" },
	{ id: "semiconductors", title: "Semiconductors" },
	{ id: "quantum-phenomena", title: "Quantum Phenomena" },
];

export const condensedMatterLessons: Lesson[] = [
	braggDiffraction,
	fermiEnergy,
	fermiDirac,
	debyeModel,
	einsteinModel,
	phononDispersion,
	hallEffect,
	semiconductorCarriers,
	superconductivity,
	curieWeiss,
	bandGap,
	thermalConductivity,
	electronHeatCapacity,
	tightBinding,
	meissnerEffect,
];
