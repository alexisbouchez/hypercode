import type { Chapter, Lesson } from "../types";
import { diffusion } from "./data/01-diffusion";
import { nernstEquation } from "./data/02-nernst-equation";
import { michaelisMenten } from "./data/03-michaelis-menten";
import { fret } from "./data/04-fret";
import { wlc } from "./data/05-wlc";
import { hillEquation } from "./data/06-hill-equation";
import { osmoticPressure } from "./data/07-osmotic-pressure";
import { beerLambert } from "./data/08-beer-lambert";
import { sedimentation } from "./data/09-sedimentation";
import { actionPotential } from "./data/10-action-potential";
import { radiationBiology } from "./data/11-radiation-biology";
import { patchClamp } from "./data/12-patch-clamp";
import { membraneElasticity } from "./data/13-membrane-elasticity";
import { proteinFolding } from "./data/14-protein-folding";
import { forceSpectroscopy } from "./data/15-force-spectroscopy";

export const biophysicsChapters: Chapter[] = [
	{ id: "molecular", title: "Molecular Biophysics" },
	{ id: "cell-kinetics", title: "Cell & Enzyme Kinetics" },
	{ id: "biophysics-methods", title: "Biophysical Methods" },
	{ id: "neural", title: "Neural Biophysics" },
];

export const biophysicsLessons: Lesson[] = [
	diffusion,
	nernstEquation,
	michaelisMenten,
	fret,
	wlc,
	hillEquation,
	osmoticPressure,
	beerLambert,
	sedimentation,
	actionPotential,
	radiationBiology,
	patchClamp,
	membraneElasticity,
	proteinFolding,
	forceSpectroscopy,
];
