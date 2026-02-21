import type { Chapter, Lesson } from "../types";
import { sinusoids } from "./data/01-sinusoids";
import { sampling } from "./data/02-sampling";
import { quantization } from "./data/03-quantization";
import { signalPower } from "./data/04-signal-power";
import { convolution } from "./data/05-convolution";
import { dft } from "./data/06-dft";
import { idft } from "./data/07-idft";
import { powerSpectrum } from "./data/08-power-spectrum";
import { windowing } from "./data/09-windowing";
import { stftFrame } from "./data/10-stft-frame";
import { movingAverage } from "./data/11-moving-average";
import { firFilter } from "./data/12-fir-filter";
import { iirFirstOrder } from "./data/13-iir-first-order";
import { correlation } from "./data/14-correlation";
import { spectralAnalysis } from "./data/15-spectral-analysis";

export const signalProcessingChapters: Chapter[] = [
	{ id: "fundamentals", title: "Signal Fundamentals" },
	{ id: "fourier", title: "Fourier Analysis" },
	{ id: "filters", title: "Digital Filters" },
	{ id: "applications", title: "Applications" },
];

export const signalProcessingLessons: Lesson[] = [
	sinusoids,
	sampling,
	quantization,
	signalPower,
	convolution,
	dft,
	idft,
	powerSpectrum,
	windowing,
	stftFrame,
	movingAverage,
	firFilter,
	iirFirstOrder,
	correlation,
	spectralAnalysis,
];
