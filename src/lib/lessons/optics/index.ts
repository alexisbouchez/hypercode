import type { Chapter, Lesson } from "../types";
import { snellsLaw } from "./data/01-snells-law";
import { reflection } from "./data/02-reflection";
import { thinLens } from "./data/03-thin-lens";
import { lensmaker } from "./data/04-lensmaker";
import { opticalInstruments } from "./data/05-optical-instruments";
import { waveSpeed } from "./data/06-wave-speed";
import { interference } from "./data/07-interference";
import { thinFilm } from "./data/08-thin-film";
import { diffraction } from "./data/09-diffraction";
import { polarization } from "./data/10-polarization";
import { photonEnergy } from "./data/11-photon-energy";
import { laser } from "./data/12-laser";
import { fiberOptics } from "./data/13-fiber-optics";
import { dispersion } from "./data/14-dispersion";
import { opticalPath } from "./data/15-optical-path";

export const opticsChapters: Chapter[] = [
	{ id: "geometric", title: "Geometric Optics" },
	{ id: "wave", title: "Wave Optics" },
	{ id: "modern", title: "Modern Optics" },
];

export const opticsLessons: Lesson[] = [
	snellsLaw,
	reflection,
	thinLens,
	lensmaker,
	opticalInstruments,
	waveSpeed,
	interference,
	thinFilm,
	diffraction,
	polarization,
	photonEnergy,
	laser,
	fiberOptics,
	dispersion,
	opticalPath,
];
