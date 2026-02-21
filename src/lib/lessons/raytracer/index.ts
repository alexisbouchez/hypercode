import type { Chapter, Lesson } from "../types";
import { tuples } from "./data/01-tuples";
import { addSubtract } from "./data/02-add-subtract";
import { negateScale } from "./data/03-negate-scale";
import { magnitude } from "./data/04-magnitude";
import { normalize } from "./data/05-normalize";
import { dotCross } from "./data/06-dot-cross";
import { colors } from "./data/07-colors";
import { matrices } from "./data/08-matrices";
import { transpose } from "./data/09-transpose";
import { translation } from "./data/10-translation";
import { scaling } from "./data/11-scaling";
import { rotation } from "./data/12-rotation";
import { rays } from "./data/13-rays";
import { sphereIntersect } from "./data/14-sphere-intersect";
import { phong } from "./data/15-phong";

export const raytracerChapters: Chapter[] = [
	{ id: "vectors", title: "Vectors & Points" },
	{ id: "colors", title: "Colors" },
	{ id: "matrices", title: "Matrices" },
	{ id: "transforms", title: "Transformations" },
	{ id: "rays", title: "Rays & Spheres" },
	{ id: "lighting", title: "Lighting" },
];

export const raytracerLessons: Lesson[] = [
	tuples,
	addSubtract,
	negateScale,
	magnitude,
	normalize,
	dotCross,
	colors,
	matrices,
	transpose,
	translation,
	scaling,
	rotation,
	rays,
	sphereIntersect,
	phong,
];
