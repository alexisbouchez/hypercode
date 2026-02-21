import type { Chapter } from "../types";
import { historicalVar } from "./data/01-historical-var";
import { parametricVar } from "./data/02-parametric-var";
import { mcVar } from "./data/03-mc-var";
import { cvar } from "./data/04-cvar";
import { marginalVar } from "./data/05-marginal-var";
import { componentVar } from "./data/06-component-var";
import { stressTesting } from "./data/07-stress-testing";
import { scenarioPnl } from "./data/08-scenario-pnl";
import { drawdownAnalysis } from "./data/09-drawdown-analysis";
import { recoveryTime } from "./data/10-recovery-time";
import { kellyCriterion } from "./data/11-kelly-criterion";
import { fixedFractional } from "./data/12-fixed-fractional";
import { volatilityTargeting } from "./data/13-volatility-targeting";
import { deltaHedging } from "./data/14-delta-hedging";
import { riskAdjustedMetrics } from "./data/15-risk-adjusted-metrics";

export const riskManagementChapters: Chapter[] = [
  {
    id: "market-risk",
    title: "Market Risk",
  },
  {
    id: "statistical-risk",
    title: "Statistical Risk",
  },
  {
    id: "tail-risk",
    title: "Tail Risk",
  },
  {
    id: "position-management",
    title: "Position Management",
  },
];

export const riskManagementLessons = [
  historicalVar,
  parametricVar,
  mcVar,
  cvar,
  marginalVar,
  componentVar,
  stressTesting,
  scenarioPnl,
  drawdownAnalysis,
  recoveryTime,
  kellyCriterion,
  fixedFractional,
  volatilityTargeting,
  deltaHedging,
  riskAdjustedMetrics,
];
