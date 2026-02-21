import type { Chapter } from "../types";
import { acfLesson } from "./data/01-acf";
import { pacfLesson } from "./data/02-pacf";
import { stationarityAdfLesson } from "./data/03-stationarity-adf";
import { differencingLesson } from "./data/04-differencing";
import { maModelLesson } from "./data/05-ma-model";
import { arModelLesson } from "./data/06-ar-model";
import { armaModelLesson } from "./data/07-arma-model";
import { arimaForecastLesson } from "./data/08-arima-forecast";
import { expSmoothingLesson } from "./data/09-exp-smoothing";
import { holtWintersLesson } from "./data/10-holt-winters";
import { volatilityClusteringLesson } from "./data/11-volatility-clustering";
import { archModelLesson } from "./data/12-arch-model";
import { garchModelLesson } from "./data/13-garch-model";
import { realizedVolatilityLesson } from "./data/14-realized-volatility";
import { kalmanFilterLesson } from "./data/15-kalman-filter";

export const timeSeriesChapters: Chapter[] = [
  {
    id: "autocorrelation",
    title: "Autocorrelation",
  },
  {
    id: "stationarity",
    title: "Stationarity",
  },
  {
    id: "arma-models",
    title: "ARMA Models",
  },
  {
    id: "volatility-models",
    title: "Volatility Models",
  },
];

export const timeSeriesLessons = [
  acfLesson,
  pacfLesson,
  stationarityAdfLesson,
  differencingLesson,
  maModelLesson,
  arModelLesson,
  armaModelLesson,
  arimaForecastLesson,
  expSmoothingLesson,
  holtWintersLesson,
  volatilityClusteringLesson,
  archModelLesson,
  garchModelLesson,
  realizedVolatilityLesson,
  kalmanFilterLesson,
];
