import type { Chapter, Lesson } from "../types";
import { neuron } from "./data/01-neuron";
import { activations } from "./data/02-activations";
import { lossFunctions } from "./data/03-loss-functions";
import { numericalGradient } from "./data/04-numerical-gradient";
import { activationDerivatives } from "./data/05-activation-derivatives";
import { layerBackward } from "./data/06-layer-backward";
import { twoLayerBackprop } from "./data/07-two-layer-backprop";
import { denseLayer } from "./data/08-dense-layer";
import { networkForward } from "./data/09-network-forward";
import { gradientStep } from "./data/10-gradient-step";
import { trainingLoop } from "./data/11-training-loop";
import { weightInit } from "./data/12-weight-init";
import { l2Regularization } from "./data/13-l2-regularization";
import { adam } from "./data/14-adam";
import { xorNetwork } from "./data/15-xor-network";

export const neuralNetworksChapters: Chapter[] = [
	{ id: "foundations", title: "Foundations" },
	{ id: "gradients", title: "Gradients" },
	{ id: "backpropagation", title: "Backpropagation" },
	{ id: "training", title: "Training" },
	{ id: "advanced", title: "Advanced Techniques" },
];

export const neuralNetworksLessons: Lesson[] = [
	neuron,
	activations,
	lossFunctions,
	numericalGradient,
	activationDerivatives,
	layerBackward,
	twoLayerBackprop,
	denseLayer,
	networkForward,
	gradientStep,
	trainingLoop,
	weightInit,
	l2Regularization,
	adam,
	xorNetwork,
];
