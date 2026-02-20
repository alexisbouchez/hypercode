import type { Chapter } from "../types";
import { dotProduct3 } from "./data/01-dot-product";
import { crossProduct3 } from "./data/02-cross-product";
import { vectorLength3 } from "./data/03-vector-length";
import { scalarProjection } from "./data/04-scalar-projection";
import { partialX } from "./data/05-partial-x";
import { partialY } from "./data/06-partial-y";
import { gradientMagnitude } from "./data/07-gradient-magnitude";
import { directionalDerivative } from "./data/08-directional-derivative";
import { tangentPlane } from "./data/09-tangent-plane";
import { laplacian2d } from "./data/10-laplacian";
import { discriminant2d } from "./data/11-discriminant";
import { doubleIntegral } from "./data/12-double-integral";
import { polarDoubleIntegral } from "./data/13-polar-double-integral";
import { tripleIntegral } from "./data/14-triple-integral";
import { divergence2d } from "./data/15-divergence";

export const calculus3Chapters: Chapter[] = [
	{ id: "vectors-in-3d", title: "Vectors in 3D" },
	{ id: "partial-derivatives", title: "Partial Derivatives" },
	{ id: "optimization", title: "Optimization" },
	{ id: "multiple-integrals", title: "Multiple Integrals" },
];

export const calculus3Lessons = [
	dotProduct3,
	crossProduct3,
	vectorLength3,
	scalarProjection,
	partialX,
	partialY,
	gradientMagnitude,
	directionalDerivative,
	tangentPlane,
	laplacian2d,
	discriminant2d,
	doubleIntegral,
	polarDoubleIntegral,
	tripleIntegral,
	divergence2d,
];
