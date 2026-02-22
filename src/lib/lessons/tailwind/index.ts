import type { Chapter, Lesson } from "../types";
import { firstComponent } from "./data/01-first-component";
import { typographyUtilities } from "./data/02-typography";
import { colorUtilities } from "./data/03-colors";
import { spacing } from "./data/04-spacing";
import { flexboxTailwind } from "./data/05-flexbox";
import { gridTailwind } from "./data/06-grid";
import { sizing } from "./data/07-sizing";
import { bordersShadows } from "./data/08-borders-shadows";
import { cards } from "./data/09-cards";
import { buttons } from "./data/10-buttons";
import { navbar } from "./data/11-navbar";
import { formsTailwind } from "./data/12-forms";
import { responsiveBasics } from "./data/13-responsive";
import { responsiveLayouts } from "./data/14-responsive-layouts";
import { hoverDark } from "./data/15-hover-dark";

export const tailwindChapters: Chapter[] = [
  { id: "utilities", title: "Utilities" },
  { id: "layout", title: "Layout" },
  { id: "components", title: "Components" },
  { id: "responsive", title: "Responsive" },
];

export const tailwindLessons: Lesson[] = [
  firstComponent,
  typographyUtilities,
  colorUtilities,
  spacing,
  flexboxTailwind,
  gridTailwind,
  sizing,
  bordersShadows,
  cards,
  buttons,
  navbar,
  formsTailwind,
  responsiveBasics,
  responsiveLayouts,
  hoverDark,
];
