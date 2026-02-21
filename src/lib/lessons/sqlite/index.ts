import type { Chapter, Lesson } from "../types";
import { intro } from "./data/01-intro";
import { filtering } from "./data/02-filtering";
import { sortingLimiting } from "./data/03-sorting-limiting";
import { dataTypes } from "./data/04-data-types";
import { creatingTables } from "./data/05-creating-tables";
import { constraints } from "./data/06-constraints";
import { alteringSchema } from "./data/07-altering-schema";
import { insertingData } from "./data/08-inserting-data";
import { updatingData } from "./data/09-updating-data";
import { deletingData } from "./data/10-deleting-data";
import { joins } from "./data/11-joins";
import { aggregations } from "./data/12-aggregations";
import { ctes } from "./data/13-ctes";
import { windowFunctions } from "./data/14-window-functions";
import { jsonFunctions } from "./data/15-json-functions";

export const sqliteChapters: Chapter[] = [
  { id: "getting-started", title: "Getting Started" },
  { id: "schema", title: "Schema" },
  { id: "crud", title: "CRUD Operations" },
  { id: "querying", title: "Querying" },
  { id: "sqlite-features", title: "SQLite Features" },
];

export const sqliteLessons: Lesson[] = [
  intro,
  filtering,
  sortingLimiting,
  dataTypes,
  creatingTables,
  constraints,
  alteringSchema,
  insertingData,
  updatingData,
  deletingData,
  joins,
  aggregations,
  ctes,
  windowFunctions,
  jsonFunctions,
];
