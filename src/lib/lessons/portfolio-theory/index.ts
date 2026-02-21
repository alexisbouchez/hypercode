import type { Chapter, Lesson } from "../types";
import { portfolioExpectedReturn } from "./data/01-portfolio-expected-return";
import { portfolioVariance2Asset } from "./data/02-portfolio-variance-2asset";
import { covarianceMatrix } from "./data/03-covariance-matrix";
import { minimumVariance } from "./data/04-minimum-variance";
import { efficientFrontier } from "./data/05-efficient-frontier";
import { tangencyPortfolio } from "./data/06-tangency-portfolio";
import { capitalMarketLine } from "./data/07-capital-market-line";
import { capmExpectedReturn } from "./data/08-capm-expected-return";
import { securityMarketLine } from "./data/09-security-market-line";
import { jensensAlpha } from "./data/10-jensens-alpha";
import { riskDecomposition } from "./data/11-risk-decomposition";
import { singleFactor } from "./data/12-single-factor";
import { correlationDiversification } from "./data/13-correlation-diversification";
import { blackLitterman } from "./data/14-black-litterman";
import { riskParity } from "./data/15-risk-parity";

export const portfolioTheoryChapters: Chapter[] = [
  { id: "return-risk", title: "Return & Risk" },
  { id: "optimization", title: "Optimization" },
  { id: "capital-market-theory", title: "Capital Market Theory" },
  { id: "factor-models", title: "Factor Models" },
];

export const portfolioTheoryLessons: Lesson[] = [
  portfolioExpectedReturn,
  portfolioVariance2Asset,
  covarianceMatrix,
  minimumVariance,
  efficientFrontier,
  tangencyPortfolio,
  capitalMarketLine,
  capmExpectedReturn,
  securityMarketLine,
  jensensAlpha,
  riskDecomposition,
  singleFactor,
  correlationDiversification,
  blackLitterman,
  riskParity,
];
