import type { Chapter, Lesson } from "../types";
import { introToMysql } from "./data/01-intro-to-mysql";
import { selectBasics } from "./data/02-select-basics";
import { filteringWithWhere } from "./data/03-filtering-with-where";
import { sortingAndLimiting } from "./data/04-sorting-and-limiting";
import { nullHandling } from "./data/05-null-handling";
import { stringFunctions } from "./data/06-string-functions";
import { creatingTables } from "./data/07-creating-tables";
import { insertingData } from "./data/08-inserting-data";
import { updatingData } from "./data/09-updating-data";
import { deletingData } from "./data/10-deleting-data";
import { innerJoins } from "./data/11-inner-joins";
import { outerJoins } from "./data/12-outer-joins";
import { aggregateFunctions } from "./data/13-aggregate-functions";
import { having } from "./data/14-having";
import { subqueriesAndCtes } from "./data/15-subqueries-and-ctes";

export const mysqlChapters: Chapter[] = [
  { id: "getting-started", title: "Getting Started" },
  { id: "querying", title: "Querying Data" },
  { id: "schema", title: "Table Design" },
  { id: "joins", title: "Joins" },
  { id: "aggregations", title: "Aggregations" },
  { id: "advanced", title: "Advanced Queries" },
];

export const mysqlLessons: Lesson[] = [
  introToMysql,
  selectBasics,
  filteringWithWhere,
  sortingAndLimiting,
  nullHandling,
  stringFunctions,
  creatingTables,
  insertingData,
  updatingData,
  deletingData,
  innerJoins,
  outerJoins,
  aggregateFunctions,
  having,
  subqueriesAndCtes,
];
