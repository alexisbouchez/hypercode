import type { Chapter, Lesson } from "../types";
import { presentFutureValue } from "./data/01-present-future-value";
import { annuities } from "./data/02-annuities";
import { npv } from "./data/03-npv";
import { irr } from "./data/04-irr";
import { bondPricing } from "./data/05-bond-pricing";
import { bondDuration } from "./data/06-bond-duration";
import { bondConvexity } from "./data/07-bond-convexity";
import { ytm } from "./data/08-ytm";
import { spotRates } from "./data/09-spot-rates";
import { forwardRates } from "./data/10-forward-rates";
import { bootstrapping } from "./data/11-bootstrapping";
import { continuousCompounding } from "./data/12-continuous-compounding";
import { discountFactors } from "./data/13-discount-factors";
import { vasicekModel } from "./data/14-vasicek-model";
import { parSwapRates } from "./data/15-par-swap-rates";

export const financialMathChapters: Chapter[] = [
  { id: "tvm", title: "Time Value of Money" },
  { id: "bond-math", title: "Bond Mathematics" },
  { id: "interest-rate-models", title: "Interest Rate Models" },
  { id: "yield-curves", title: "Yield Curves" },
];

export const financialMathLessons: Lesson[] = [
  presentFutureValue,
  annuities,
  npv,
  irr,
  bondPricing,
  bondDuration,
  bondConvexity,
  ytm,
  spotRates,
  forwardRates,
  bootstrapping,
  continuousCompounding,
  discountFactors,
  vasicekModel,
  parSwapRates,
];
