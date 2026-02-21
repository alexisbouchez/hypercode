import type { Chapter } from "../types";
import { traceResistance } from "./data/01-trace-resistance";
import { traceCurrentCapacity } from "./data/02-trace-current";
import { propagationDelay } from "./data/03-propagation-delay";
import { microstripImpedance } from "./data/04-microstrip-impedance";
import { striplineImpedance } from "./data/05-stripline-impedance";
import { differentialPair } from "./data/06-differential-pair";
import { viaResistance } from "./data/07-via-resistance";
import { viaInductance } from "./data/08-via-inductance";
import { selfResonantFrequency } from "./data/09-self-resonant-frequency";
import { decouplingImpedance } from "./data/10-decoupling-impedance";
import { targetImpedance } from "./data/11-target-impedance";
import { parallelCapacitors } from "./data/12-parallel-capacitors";
import { riseTimeBandwidth } from "./data/13-rise-time-bandwidth";
import { skinEffect } from "./data/14-skin-effect";
import { designRules } from "./data/15-design-rules";

export const pcbDesignChapters: Chapter[] = [
	{ id: "trace-design", title: "Trace Design" },
	{ id: "impedance-control", title: "Impedance Control" },
	{ id: "via-design", title: "Via Design" },
	{ id: "power-integrity", title: "Power Integrity" },
	{ id: "signal-integrity", title: "Signal Integrity" },
];

export const pcbDesignLessons = [
	traceResistance,
	traceCurrentCapacity,
	propagationDelay,
	microstripImpedance,
	striplineImpedance,
	differentialPair,
	viaResistance,
	viaInductance,
	selfResonantFrequency,
	decouplingImpedance,
	targetImpedance,
	parallelCapacitors,
	riseTimeBandwidth,
	skinEffect,
	designRules,
];
