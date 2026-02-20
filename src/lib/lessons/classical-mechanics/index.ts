import type { Chapter, Lesson } from "../types";
import { velocityLesson } from "./data/01-velocity";
import { freeFallLesson } from "./data/02-free-fall";
import { projectileRangeLesson } from "./data/03-projectile-range";
import { accelerationLesson } from "./data/04-acceleration";
import { frictionLesson } from "./data/05-friction";
import { centripetalLesson } from "./data/06-centripetal";
import { kineticEnergyLesson } from "./data/07-kinetic-energy";
import { potentialEnergyLesson } from "./data/08-potential-energy";
import { elasticCollisionLesson } from "./data/09-elastic-collision";
import { powerLesson } from "./data/10-power";
import { shmDisplacementLesson } from "./data/11-shm-displacement";
import { springPeriodLesson } from "./data/12-spring-period";
import { pendulumPeriodLesson } from "./data/13-pendulum-period";
import { torqueLesson } from "./data/14-torque";
import { gravitationalForceLesson } from "./data/15-gravitational-force";

export const classicalMechanicsChapters: Chapter[] = [
	{ id: "kinematics", title: "Kinematics" },
	{ id: "forces", title: "Forces & Newton's Laws" },
	{ id: "energy-and-momentum", title: "Energy & Momentum" },
	{ id: "oscillations-and-gravitation", title: "Oscillations & Gravitation" },
];

export const classicalMechanicsLessons: Lesson[] = [
	velocityLesson,
	freeFallLesson,
	projectileRangeLesson,
	accelerationLesson,
	frictionLesson,
	centripetalLesson,
	kineticEnergyLesson,
	potentialEnergyLesson,
	elasticCollisionLesson,
	powerLesson,
	shmDisplacementLesson,
	springPeriodLesson,
	pendulumPeriodLesson,
	torqueLesson,
	gravitationalForceLesson,
];
