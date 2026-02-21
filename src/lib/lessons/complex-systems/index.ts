import type { Chapter, Lesson } from "../types";
import { logisticMap } from "./data/01-logistic-map";
import { lorenzAttractor } from "./data/02-lorenz-attractor";
import { fixedPoints } from "./data/03-fixed-points";
import { networkDegree } from "./data/04-network-degree";
import { smallWorld } from "./data/05-small-world";
import { scaleFree } from "./data/06-scale-free";
import { isingModel } from "./data/07-ising-model";
import { cellularAutomata } from "./data/08-cellular-automata";
import { randomWalk } from "./data/09-random-walk";
import { shannonEntropy } from "./data/10-shannon-entropy";
import { fractals } from "./data/11-fractals";
import { percolation } from "./data/12-percolation";
import { soc } from "./data/13-soc";
import { bifurcation } from "./data/14-bifurcation";
import { agentBased } from "./data/15-agent-based";

export const complexSystemsChapters: Chapter[] = [
	{ id: "dynamical-systems", title: "Dynamical Systems" },
	{ id: "networks", title: "Network Science" },
	{ id: "statistical-physics", title: "Statistical Physics" },
	{ id: "emergence", title: "Emergence & Self-Organization" },
	{ id: "information", title: "Information Theory" },
];

export const complexSystemsLessons: Lesson[] = [
	logisticMap,
	lorenzAttractor,
	fixedPoints,
	networkDegree,
	smallWorld,
	scaleFree,
	isingModel,
	cellularAutomata,
	randomWalk,
	shannonEntropy,
	fractals,
	percolation,
	soc,
	bifurcation,
	agentBased,
];
