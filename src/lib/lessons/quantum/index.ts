import type { Chapter, Lesson } from "../types";
import { qubits } from "./data/01-qubits";
import { normalization } from "./data/02-normalization";
import { measurement } from "./data/03-measurement";
import { hadamardGate } from "./data/04-hadamard";
import { xGate } from "./data/05-x-gate";
import { zGate } from "./data/06-z-gate";
import { twoQubitStates } from "./data/07-two-qubit-states";
import { cnot } from "./data/08-cnot";
import { bellStates } from "./data/09-bell-states";
import { entanglement } from "./data/10-entanglement";
import { quantumCircuits } from "./data/11-quantum-circuits";
import { deutsch } from "./data/12-deutsch";
import { grover } from "./data/13-grover";
import { errorCorrection } from "./data/14-error-correction";
import { qkd } from "./data/15-qkd";

export const quantumChapters: Chapter[] = [
	{ id: "qubits", title: "Quantum Bits" },
	{ id: "gates", title: "Single-Qubit Gates" },
	{ id: "multi-qubit", title: "Multi-Qubit Systems" },
	{ id: "algorithms", title: "Quantum Algorithms" },
];

export const quantumLessons: Lesson[] = [
	qubits,
	normalization,
	measurement,
	hadamardGate,
	xGate,
	zGate,
	twoQubitStates,
	cnot,
	bellStates,
	entanglement,
	quantumCircuits,
	deutsch,
	grover,
	errorCorrection,
	qkd,
];
