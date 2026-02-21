import type { Chapter, Lesson } from "../types";
import { logArithmeticReturns } from "./data/01-log-arithmetic-returns";
import { moments } from "./data/02-moments";
import { rollingStats } from "./data/03-rolling-stats";
import { covarianceCorrelation } from "./data/04-covariance-correlation";
import { normalDistribution } from "./data/05-normal-distribution";
import { historicalVolatility } from "./data/06-historical-volatility";
import { drawdown } from "./data/07-drawdown";
import { sharpeSortino } from "./data/08-sharpe-sortino";
import { betaAlpha } from "./data/09-beta-alpha";
import { tTest } from "./data/10-t-test";
import { ols } from "./data/11-ols";
import { rSquared } from "./data/12-r-squared";
import { jarqueBera } from "./data/13-jarque-bera";
import { monteCarlo } from "./data/14-monte-carlo";
import { bootstrapping } from "./data/15-bootstrapping";

export const quantStatsChapters: Chapter[] = [
  { id: "return-analysis", title: "Return Analysis" },
  { id: "risk-metrics", title: "Risk Metrics" },
  { id: "statistical-testing", title: "Statistical Testing" },
  { id: "regression", title: "Regression" },
];

export const quantStatsLessons: Lesson[] = [
  logArithmeticReturns,
  moments,
  rollingStats,
  covarianceCorrelation,
  normalDistribution,
  historicalVolatility,
  drawdown,
  sharpeSortino,
  betaAlpha,
  tTest,
  ols,
  rSquared,
  jarqueBera,
  monteCarlo,
  bootstrapping,
];
