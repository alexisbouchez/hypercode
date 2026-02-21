import type { Chapter, Lesson } from "../types";
import { sTGates } from "./data/01-s-t-gates";
import { rotationGates } from "./data/02-rotation-gates";
import { blochSphere } from "./data/03-bloch-sphere";
import { toffoliGate } from "./data/04-toffoli";
import { swapGate } from "./data/05-swap";
import { controlledPhase } from "./data/06-controlled-phase";
import { quantumFourierTransform } from "./data/07-qft";
import { inverseQFT } from "./data/08-inverse-qft";
import { phaseEstimation } from "./data/09-phase-estimation";
import { teleportation } from "./data/10-teleportation";
import { superdenseCoding } from "./data/11-superdense-coding";
import { bellInequality } from "./data/12-bell-inequality";
import { pauliExpectation } from "./data/13-pauli-expectation";
import { vqe } from "./data/14-vqe";
import { qaoa } from "./data/15-qaoa";

export const advancedQuantumChapters: Chapter[] = [
	{ id: "advanced-gates", title: "Advanced Quantum Gates" },
	{ id: "qft", title: "Quantum Fourier Transform" },
	{ id: "protocols", title: "Quantum Protocols" },
	{ id: "variational", title: "Variational Quantum Computing" },
];

export const advancedQuantumLessons: Lesson[] = [
	sTGates,
	rotationGates,
	blochSphere,
	toffoliGate,
	swapGate,
	controlledPhase,
	quantumFourierTransform,
	inverseQFT,
	phaseEstimation,
	teleportation,
	superdenseCoding,
	bellInequality,
	pauliExpectation,
	vqe,
	qaoa,
];
