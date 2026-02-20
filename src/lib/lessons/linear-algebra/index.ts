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
import { sympyIntro } from "./data/13-sympy-intro";
import { sympySolve } from "./data/14-sympy-solve";
import { sympyMatrices } from "./data/15-sympy-matrices";

export const linearAlgebraChapters: Chapter[] = [
	{ id: "vectors", title: "Vectors" },
	{ id: "matrices", title: "Matrices" },
	{ id: "systems", title: "Linear Systems" },
	{ id: "symbolic", title: "Symbolic Math" },
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
	sympyIntro,
	sympySolve,
	sympyMatrices,
];
