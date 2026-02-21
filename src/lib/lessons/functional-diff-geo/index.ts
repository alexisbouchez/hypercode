import type { Chapter, Lesson } from "../types";
import { compose } from "./data/01-compose";
import { derivative } from "./data/02-derivative";
import { partialDerivative } from "./data/03-partial";
import { coordinates } from "./data/04-coordinates";
import { jacobian } from "./data/05-jacobian";
import { tangentVector } from "./data/06-tangent-vector";
import { vectorField } from "./data/07-vector-field";
import { oneForm } from "./data/08-one-form";
import { exteriorDerivative } from "./data/09-exterior-derivative";
import { lieBracket } from "./data/10-lie-bracket";
import { metricTensor } from "./data/11-metric-tensor";
import { christoffel } from "./data/12-christoffel";
import { parallelTransport } from "./data/13-parallel-transport";
import { riemannCurvature } from "./data/14-riemann-curvature";
import { geodesics } from "./data/15-geodesics";

export const functionalDiffGeoChapters: Chapter[] = [
	{ id: "functional-foundations", title: "Functional Foundations" },
	{ id: "manifolds", title: "Manifolds" },
	{ id: "vector-fields", title: "Vector & Form Fields" },
	{ id: "connections-curvature", title: "Connections & Curvature" },
];

export const functionalDiffGeoLessons: Lesson[] = [
	compose,
	derivative,
	partialDerivative,
	coordinates,
	jacobian,
	tangentVector,
	vectorField,
	oneForm,
	exteriorDerivative,
	lieBracket,
	metricTensor,
	christoffel,
	parallelTransport,
	riemannCurvature,
	geodesics,
];
