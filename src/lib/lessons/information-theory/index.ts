import type { Chapter, Lesson } from "../types";
import { selfInformation } from "./data/01-self-information";
import { entropy } from "./data/02-entropy";
import { jointEntropy } from "./data/03-joint-entropy";
import { conditionalEntropy } from "./data/04-conditional-entropy";
import { mutualInformation } from "./data/05-mutual-information";
import { klDivergence } from "./data/06-kl-divergence";
import { crossEntropy } from "./data/07-cross-entropy";
import { jsDivergence } from "./data/08-js-divergence";
import { totalVariation } from "./data/09-total-variation";
import { huffmanLengths } from "./data/10-huffman-lengths";
import { entropyCoding } from "./data/11-entropy-coding";
import { lempelZiv } from "./data/12-lempel-ziv";
import { channelCapacity } from "./data/13-channel-capacity";
import { channelMatrix } from "./data/14-channel-matrix";
import { rateDistortion } from "./data/15-rate-distortion";

export const informationTheoryChapters: Chapter[] = [
	{ id: "entropy", title: "Entropy and Information" },
	{ id: "divergence", title: "Divergence and Distance" },
	{ id: "coding", title: "Source Coding" },
	{ id: "channel", title: "Channel Capacity" },
];

export const informationTheoryLessons: Lesson[] = [
	selfInformation,
	entropy,
	jointEntropy,
	conditionalEntropy,
	mutualInformation,
	klDivergence,
	crossEntropy,
	jsDivergence,
	totalVariation,
	huffmanLengths,
	entropyCoding,
	lempelZiv,
	channelCapacity,
	channelMatrix,
	rateDistortion,
];
