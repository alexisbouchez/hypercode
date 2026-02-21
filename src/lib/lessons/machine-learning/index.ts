import type { Chapter, Lesson } from "../types";
import { linearRegression } from "./data/01-linear-regression";
import { gradientDescent } from "./data/02-gradient-descent";
import { multivariateRegression } from "./data/03-multivariate-regression";
import { logisticRegression } from "./data/04-logistic-regression";
import { decisionBoundary } from "./data/05-decision-boundary";
import { distanceMetrics } from "./data/06-distance-metrics";
import { knn } from "./data/07-knn";
import { kmeansStep } from "./data/08-kmeans-step";
import { pca2d } from "./data/09-pca-2d";
import { dbscanCore } from "./data/10-dbscan-core";
import { perceptron } from "./data/11-perceptron";
import { activationFunctions } from "./data/12-activation-functions";
import { forwardPass } from "./data/13-forward-pass";
import { trainTestSplit } from "./data/14-train-test-split";
import { regularization } from "./data/15-regularization";

export const machineLearningChapters: Chapter[] = [
	{ id: "supervised", title: "Supervised Learning" },
	{ id: "unsupervised", title: "Unsupervised Learning" },
	{ id: "neural-networks", title: "Neural Networks" },
	{ id: "evaluation", title: "Model Evaluation" },
];

export const machineLearningLessons: Lesson[] = [
	linearRegression,
	gradientDescent,
	multivariateRegression,
	logisticRegression,
	decisionBoundary,
	distanceMetrics,
	knn,
	kmeansStep,
	pca2d,
	dbscanCore,
	perceptron,
	activationFunctions,
	forwardPass,
	trainTestSplit,
	regularization,
];
