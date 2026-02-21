import type { Chapter, Lesson } from "../types";
import { valueClass } from "./data/01-value-class";
import { operations } from "./data/02-operations";
import { shortcuts } from "./data/03-shortcuts";
import { topology } from "./data/04-topology";
import { backward } from "./data/05-backward";
import { tokenizer } from "./data/06-tokenizer";
import { trainingPairs } from "./data/07-training-pairs";
import { linear } from "./data/08-linear";
import { softmax } from "./data/09-softmax";
import { rmsnorm } from "./data/10-rmsnorm";
import { crossEntropy } from "./data/11-cross-entropy";
import { attention } from "./data/12-attention";
import { multiHead } from "./data/13-multi-head";
import { trainingLoop } from "./data/14-training-loop";
import { adam } from "./data/15-adam";

export const microgptChapters: Chapter[] = [
	{ id: "autograd", title: "Autograd Engine" },
	{ id: "tokens", title: "Data & Tokens" },
	{ id: "primitives", title: "Neural Net Primitives" },
	{ id: "transformer", title: "The Transformer" },
	{ id: "training", title: "Training" },
];

export const microgptLessons: Lesson[] = [
	valueClass,
	operations,
	shortcuts,
	topology,
	backward,
	tokenizer,
	trainingPairs,
	linear,
	softmax,
	rmsnorm,
	crossEntropy,
	attention,
	multiHead,
	trainingLoop,
	adam,
];
