import type { Chapter, Lesson } from "../types";
import { dnaSequences } from "./data/01-dna-sequences";
import { complement } from "./data/02-complement";
import { gcContent } from "./data/03-gc-content";
import { codons } from "./data/04-codons";
import { orfs } from "./data/05-orfs";
import { transcription } from "./data/06-transcription";
import { translation } from "./data/07-translation";
import { spliceSites } from "./data/08-splice-sites";
import { motifFinding } from "./data/09-motif-finding";
import { cpgWindows } from "./data/10-cpg-windows";
import { variants } from "./data/11-variants";
import { oneHot } from "./data/12-one-hot";
import { kmers } from "./data/13-kmers";
import { regulatoryScoring } from "./data/14-regulatory-scoring";
import { variantEffect } from "./data/15-variant-effect";

export const genomicsChapters: Chapter[] = [
	{ id: "dna-basics", title: "The DNA Alphabet" },
	{ id: "gene-anatomy", title: "Reading the Genome" },
	{ id: "regulation", title: "Gene Regulation" },
	{ id: "ml-genomics", title: "Genomics Meets AI" },
];

export const genomicsLessons: Lesson[] = [
	dnaSequences,
	complement,
	gcContent,
	codons,
	orfs,
	transcription,
	translation,
	spliceSites,
	motifFinding,
	cpgWindows,
	variants,
	oneHot,
	kmers,
	regulatoryScoring,
	variantEffect,
];
