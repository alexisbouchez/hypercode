import type { Chapter, Lesson } from "../types";
import { introToDatabases } from "./data/01-intro-to-databases";
import { yourFirstSelect } from "./data/02-your-first-select";
import { filteringWithWhere } from "./data/03-filtering-with-where";
import { sortingAndLimiting } from "./data/04-sorting-and-limiting";
import { dataTypes } from "./data/05-data-types";
import { creatingTables } from "./data/06-creating-tables";
import { constraintsAndKeys } from "./data/07-constraints-and-keys";
import { modifyingAndDropping } from "./data/08-modifying-and-dropping";
import { insertingData } from "./data/09-inserting-data";
import { updatingAndDeleting } from "./data/10-updating-and-deleting";
import { upsertAndBulk } from "./data/11-upsert-and-bulk";
import { innerJoins } from "./data/12-inner-joins";
import { outerJoins } from "./data/13-outer-joins";
import { selfAndCrossJoins } from "./data/14-self-and-cross-joins";
import { aggregations } from "./data/15-aggregations";
import { havingAndAdvancedAgg } from "./data/16-having-and-advanced-agg";
import { subqueries } from "./data/17-subqueries";
import { ctes } from "./data/18-ctes";
import { windowFunctions } from "./data/19-window-functions";
import { jsonAndArrays } from "./data/20-json-and-arrays";
import { indexesAndOptimization } from "./data/21-indexes-and-optimization";
import { databaseDesign } from "./data/22-database-design";

export const sqlChapters: Chapter[] = [
  { id: "sql-basics", title: "SQL Basics" },
  { id: "tables", title: "Tables" },
  { id: "crud", title: "CRUD Operations" },
  { id: "joins", title: "Joins" },
  { id: "aggregations", title: "Aggregations" },
  { id: "subqueries-ctes", title: "Subqueries & CTEs" },
  { id: "advanced-sql", title: "Advanced SQL" },
  { id: "performance", title: "Performance" },
];

export const sqlLessons: Lesson[] = [
  introToDatabases,
  yourFirstSelect,
  filteringWithWhere,
  sortingAndLimiting,
  dataTypes,
  creatingTables,
  constraintsAndKeys,
  modifyingAndDropping,
  insertingData,
  updatingAndDeleting,
  upsertAndBulk,
  innerJoins,
  outerJoins,
  selfAndCrossJoins,
  aggregations,
  havingAndAdvancedAgg,
  subqueries,
  ctes,
  windowFunctions,
  jsonAndArrays,
  indexesAndOptimization,
  databaseDesign,
];
