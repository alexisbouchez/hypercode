import type { Chapter, Lesson } from "../types";
import { wavePeriodLesson } from "./data/01-wave-period";
import { waveSpeedLesson } from "./data/02-wave-speed";
import { soundWavelengthLesson } from "./data/03-sound-wavelength";
import { speedOfSoundLesson } from "./data/04-speed-of-sound";
import { soundIntensityLesson } from "./data/05-sound-intensity";
import { decibelLevelLesson } from "./data/06-decibel-level";
import { beatFrequencyLesson } from "./data/07-beat-frequency";
import { dopplerEffectLesson } from "./data/08-doppler-effect";
import { stringHarmonicLesson } from "./data/09-string-harmonic";
import { openPipeLesson } from "./data/10-open-pipe";
import { closedPipeLesson } from "./data/11-closed-pipe";
import { superpositionLesson } from "./data/12-superposition";
import { reverberationLesson } from "./data/13-reverberation";
import { soundPowerLevelLesson } from "./data/14-sound-power-level";
import { reflectionCoefficientLesson } from "./data/15-reflection-coefficient";

export const wavesChapters: Chapter[] = [
	{ id: "wave-fundamentals", title: "Wave Fundamentals" },
	{ id: "intensity-and-perception", title: "Intensity & Perception" },
	{ id: "standing-waves", title: "Standing Waves & Resonance" },
	{ id: "room-acoustics", title: "Room Acoustics" },
];

export const wavesLessons: Lesson[] = [
	wavePeriodLesson,
	waveSpeedLesson,
	soundWavelengthLesson,
	speedOfSoundLesson,
	soundIntensityLesson,
	decibelLevelLesson,
	beatFrequencyLesson,
	dopplerEffectLesson,
	stringHarmonicLesson,
	openPipeLesson,
	closedPipeLesson,
	superpositionLesson,
	reverberationLesson,
	soundPowerLevelLesson,
	reflectionCoefficientLesson,
];
