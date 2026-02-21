import type { Chapter, Lesson } from "../types";
import { lorentzFactor } from "./data/01-lorentz-factor";
import { timeDilation } from "./data/02-time-dilation";
import { lengthContraction } from "./data/03-length-contraction";
import { velocityAddition } from "./data/04-velocity-addition";
import { relativisticDoppler } from "./data/05-relativistic-doppler";
import { lorentzTransform } from "./data/06-lorentz-transform";
import { spacetimeInterval } from "./data/07-spacetime-interval";
import { properTime } from "./data/08-proper-time";
import { rapidity } from "./data/09-rapidity";
import { relativisticMomentum } from "./data/10-relativistic-momentum";
import { relativisticEnergy } from "./data/11-relativistic-energy";
import { energyMomentum } from "./data/12-energy-momentum";
import { fourVectors } from "./data/13-four-vectors";
import { relativisticCollisions } from "./data/14-relativistic-collisions";
import { massEnergy } from "./data/15-mass-energy";

export const specialRelativityChapters: Chapter[] = [
	{ id: "kinematics", title: "Relativistic Kinematics" },
	{ id: "spacetime", title: "Spacetime Geometry" },
	{ id: "dynamics", title: "Relativistic Dynamics" },
];

export const specialRelativityLessons: Lesson[] = [
	lorentzFactor,
	timeDilation,
	lengthContraction,
	velocityAddition,
	relativisticDoppler,
	lorentzTransform,
	spacetimeInterval,
	properTime,
	rapidity,
	relativisticMomentum,
	relativisticEnergy,
	energyMomentum,
	fourVectors,
	relativisticCollisions,
	massEnergy,
];
