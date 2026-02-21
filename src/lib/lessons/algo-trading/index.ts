import type { Chapter, Lesson } from "../types";
import { movingAverages } from "./data/01-moving-averages";
import { bollingerBands } from "./data/02-bollinger-bands";
import { rsiLesson } from "./data/03-rsi";
import { macdLesson } from "./data/04-macd";
import { smaCrossover } from "./data/05-sma-crossover";
import { momentumLesson } from "./data/06-momentum";
import { trendFollowingReturns } from "./data/07-trend-following-returns";
import { zscoreReversion } from "./data/08-zscore-reversion";
import { pairsTrading } from "./data/09-pairs-trading";
import { rebalancing } from "./data/10-rebalancing";
import { backtestEngine } from "./data/11-backtest-engine";
import { slippageCosts } from "./data/12-slippage-costs";
import { backtestSharpe } from "./data/13-backtest-sharpe";
import { walkForward } from "./data/14-walk-forward";
import { kellyCriterion } from "./data/15-kelly-criterion";

export const algoTradingChapters: Chapter[] = [
  { id: "signals", title: "Signals & Indicators" },
  { id: "trend-following", title: "Trend Following" },
  { id: "mean-reversion", title: "Mean Reversion" },
  { id: "backtesting", title: "Backtesting" },
];

export const algoTradingLessons: Lesson[] = [
  movingAverages,
  bollingerBands,
  rsiLesson,
  macdLesson,
  smaCrossover,
  momentumLesson,
  trendFollowingReturns,
  zscoreReversion,
  pairsTrading,
  rebalancing,
  backtestEngine,
  slippageCosts,
  backtestSharpe,
  walkForward,
  kellyCriterion,
];
