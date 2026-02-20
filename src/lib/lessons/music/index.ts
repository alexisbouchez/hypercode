import type { Chapter, Lesson } from "../types";
import { noteToFreq } from "./data/01-note-to-freq";
import { freqToNote } from "./data/02-freq-to-note";
import { noteName } from "./data/03-note-name";
import { scaleLesson } from "./data/04-scale";
import { intervalRatioLesson } from "./data/05-interval-ratio";
import { chordNotesLesson } from "./data/06-chord-notes";
import { transposeLesson } from "./data/07-transpose";
import { beatDurationLesson } from "./data/08-beat-duration";
import { noteDurationLesson } from "./data/09-note-duration";
import { arpNoteLesson } from "./data/10-arp-note";
import { onsetTimeLesson } from "./data/11-onset-time";
import { dBToGainLesson } from "./data/12-db-to-gain";
import { gainToDbLesson } from "./data/13-gain-to-db";
import { lfoLesson } from "./data/14-lfo";
import { patternOnsetsLesson } from "./data/15-pattern-onsets";

export const musicChapters: Chapter[] = [
	{ id: "notes-and-frequencies", title: "Notes & Frequencies" },
	{ id: "chords-and-harmony", title: "Chords & Harmony" },
	{ id: "synthesis-and-rhythm", title: "Synthesis & Rhythm" },
	{ id: "effects-and-timbre", title: "Effects & Timbre" },
];

export const musicLessons: Lesson[] = [
	noteToFreq,
	freqToNote,
	noteName,
	scaleLesson,
	intervalRatioLesson,
	chordNotesLesson,
	transposeLesson,
	beatDurationLesson,
	noteDurationLesson,
	arpNoteLesson,
	onsetTimeLesson,
	dBToGainLesson,
	gainToDbLesson,
	lfoLesson,
	patternOnsetsLesson,
];
