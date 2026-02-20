import type { Chapter, Lesson } from "../types";
import { vectors } from "./data/01-vectors";
import { vectorOps } from "./data/02-vector-ops";
import { dotProduct } from "./data/03-dot-product";
import { vectorNorm } from "./data/04-vector-norm";
import { matrices } from "./data/05-matrices";
import { matrixOps } from "./data/06-matrix-ops";
import { matrixMultiply } from "./data/07-matrix-multiply";
import { determinant } from "./data/08-determinant";
import { inverse } from "./data/09-inverse";
import { solve } from "./data/10-solve";
import { eigenvalues } from "./data/11-eigenvalues";
import { leastSquares } from "./data/12-least-squares";
import { polynomialRoots } from "./data/13-polynomial-roots";
import { newtonRaphson } from "./data/14-newton-raphson";
import { powerIteration } from "./data/15-power-iteration";

export const linearAlgebraChapters: Chapter[] = [
	{ id: "vectors", title: "Vectors" },
	{ id: "matrices", title: "Matrices" },
	{ id: "systems", title: "Linear Systems" },
	{ id: "numerical", title: "Numerical Methods" },
];

export const linearAlgebraLessons: Lesson[] = [
	vectors,
	vectorOps,
	dotProduct,
	vectorNorm,
	matrices,
	matrixOps,
	matrixMultiply,
	determinant,
	inverse,
	solve,
	eigenvalues,
	leastSquares,
	polynomialRoots,
	newtonRaphson,
	powerIteration,
];
