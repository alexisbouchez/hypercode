import type { Chapter, Lesson } from "../types";
import { firstStyles } from "./data/01-first-styles";
import { selectors } from "./data/02-selectors";
import { colors } from "./data/03-colors";
import { typography } from "./data/04-typography";
import { boxModel } from "./data/05-box-model";
import { marginsPadding } from "./data/06-margins-padding";
import { borders } from "./data/07-borders";
import { display } from "./data/08-display";
import { positioning } from "./data/09-positioning";
import { flexbox } from "./data/10-flexbox";
import { grid } from "./data/11-grid";
import { backgrounds } from "./data/12-backgrounds";
import { transitions } from "./data/13-transitions";
import { animations } from "./data/14-animations";
import { responsive } from "./data/15-responsive";

export const cssChapters: Chapter[] = [
  { id: "selectors", title: "Selectors" },
  { id: "box-model", title: "Box Model" },
  { id: "layout", title: "Layout" },
  { id: "styling", title: "Styling" },
];

export const cssLessons: Lesson[] = [
  firstStyles,
  selectors,
  colors,
  typography,
  boxModel,
  marginsPadding,
  borders,
  display,
  positioning,
  flexbox,
  grid,
  backgrounds,
  transitions,
  animations,
  responsive,
];
