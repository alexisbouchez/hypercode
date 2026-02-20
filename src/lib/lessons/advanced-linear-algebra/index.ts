import type { Chapter, Lesson } from "../types";
import { vectorProjectionLesson } from "./data/01-vector-projection";
import { gramSchmidtLesson } from "./data/02-gram-schmidt";
import { qrDecompositionLesson } from "./data/03-qr-decomposition";
import { leastSquaresQrLesson } from "./data/04-least-squares-qr";
import { luDecompositionLesson } from "./data/05-lu-decomposition";
import { triangularSolversLesson } from "./data/06-triangular-solvers";
import { choleskyLesson } from "./data/07-cholesky";
import { powerIterationLesson } from "./data/08-power-iteration";
import { deflationLesson } from "./data/09-deflation";
import { matrixNormsLesson } from "./data/10-matrix-norms";
import { conditionNumberLesson } from "./data/11-condition-number";
import { pseudoinverseLesson } from "./data/12-pseudoinverse";
import { pcaLesson } from "./data/13-pca";
import { matrixExponentialLesson } from "./data/14-matrix-exponential";
import { conjugateGradientLesson } from "./data/15-conjugate-gradient";

export const advancedLinearAlgebraChapters: Chapter[] = [
	{ id: "orthogonality", title: "Orthogonality & Projections" },
	{ id: "decompositions", title: "Matrix Decompositions" },
	{ id: "matrix-analysis", title: "Matrix Analysis" },
	{ id: "applications", title: "Advanced Applications" },
];

export const advancedLinearAlgebraLessons: Lesson[] = [
	vectorProjectionLesson,
	gramSchmidtLesson,
	qrDecompositionLesson,
	leastSquaresQrLesson,
	luDecompositionLesson,
	triangularSolversLesson,
	choleskyLesson,
	powerIterationLesson,
	deflationLesson,
	matrixNormsLesson,
	conditionNumberLesson,
	pseudoinverseLesson,
	pcaLesson,
	matrixExponentialLesson,
	conjugateGradientLesson,
];
