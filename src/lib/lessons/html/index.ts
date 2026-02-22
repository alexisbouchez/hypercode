import type { Chapter, Lesson } from "../types";
import { helloWorld } from "./data/01-hello-world";
import { headingsParagraphs } from "./data/02-headings-paragraphs";
import { textFormatting } from "./data/03-text-formatting";
import { lists } from "./data/04-lists";
import { links } from "./data/05-links";
import { images } from "./data/06-images";
import { tables } from "./data/07-tables";
import { divsSpans } from "./data/08-divs-spans";
import { forms } from "./data/09-forms";
import { inputTypes } from "./data/10-input-types";
import { textareaSelect } from "./data/11-textarea-select";
import { semanticStructure } from "./data/12-semantic-structure";
import { navHeader } from "./data/13-nav-header";
import { articleSection } from "./data/14-article-section";
import { attributesMeta } from "./data/15-attributes-meta";

export const htmlChapters: Chapter[] = [
  { id: "structure", title: "Structure" },
  { id: "content", title: "Content" },
  { id: "forms", title: "Forms" },
  { id: "semantic", title: "Semantic HTML" },
];

export const htmlLessons: Lesson[] = [
  helloWorld,
  headingsParagraphs,
  textFormatting,
  lists,
  links,
  images,
  tables,
  divsSpans,
  forms,
  inputTypes,
  textareaSelect,
  semanticStructure,
  navHeader,
  articleSection,
  attributesMeta,
];
