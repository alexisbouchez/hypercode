import type { Chapter, Lesson } from "../types";
import { schwarzschildRadius } from "./data/01-schwarzschild-radius";
import { gravitationalTimeDilation } from "./data/02-gravitational-time-dilation";
import { gravitationalRedshift } from "./data/03-gravitational-redshift";
import { escapeVelocity } from "./data/04-escape-velocity";
import { photonSphere } from "./data/05-photon-sphere";
import { hawkingTemperature } from "./data/06-hawking-temperature";
import { blackHoleEntropy } from "./data/07-black-hole-entropy";
import { gravitationalLensing } from "./data/08-gravitational-lensing";
import { shapiroDelay } from "./data/09-shapiro-delay";
import { perihelionPrecession } from "./data/10-perihelion-precession";
import { chirpMass } from "./data/11-chirp-mass";
import { gwFrequency } from "./data/12-gw-frequency";
import { gwStrain } from "./data/13-gw-strain";
import { gwPower } from "./data/14-gw-power";
import { inspiralTime } from "./data/15-inspiral-time";

export const generalRelativityChapters: Chapter[] = [
	{ id: "schwarzschild", title: "Schwarzschild Geometry" },
	{ id: "black-holes", title: "Black Holes" },
	{ id: "gr-predictions", title: "GR Predictions" },
	{ id: "gravitational-waves", title: "Gravitational Waves" },
];

export const generalRelativityLessons: Lesson[] = [
	schwarzschildRadius,
	gravitationalTimeDilation,
	gravitationalRedshift,
	escapeVelocity,
	photonSphere,
	hawkingTemperature,
	blackHoleEntropy,
	gravitationalLensing,
	shapiroDelay,
	perihelionPrecession,
	chirpMass,
	gwFrequency,
	gwStrain,
	gwPower,
	inspiralTime,
];
