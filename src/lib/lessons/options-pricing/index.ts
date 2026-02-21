import type { Chapter, Lesson } from "../types";
import { callPutPayoffs } from "./data/01-call-put-payoffs";
import { putCallParity } from "./data/02-put-call-parity";
import { intrinsicTimeValue } from "./data/03-intrinsic-time-value";
import { blackScholes } from "./data/04-black-scholes";
import { impliedVolatility } from "./data/05-implied-volatility";
import { bsDelta } from "./data/06-bs-delta";
import { bsGamma } from "./data/07-bs-gamma";
import { bsTheta } from "./data/08-bs-theta";
import { vegaRho } from "./data/09-vega-rho";
import { binomial1step } from "./data/10-binomial-1step";
import { binomialNstep } from "./data/11-binomial-nstep";
import { americanOptions } from "./data/12-american-options";
import { monteCarloPricing } from "./data/13-monte-carlo-pricing";
import { asianOptions } from "./data/14-asian-options";
import { barrierOptions } from "./data/15-barrier-options";

export const optionsPricingChapters: Chapter[] = [
  { id: "basics", title: "Basics" },
  { id: "black-scholes", title: "Black-Scholes" },
  { id: "greeks", title: "Greeks" },
  { id: "numerical-methods", title: "Numerical Methods" },
];

export const optionsPricingLessons: Lesson[] = [
  callPutPayoffs,
  putCallParity,
  intrinsicTimeValue,
  blackScholes,
  impliedVolatility,
  bsDelta,
  bsGamma,
  bsTheta,
  vegaRho,
  binomial1step,
  binomialNstep,
  americanOptions,
  monteCarloPricing,
  asianOptions,
  barrierOptions,
];
