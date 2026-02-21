import type { Chapter, Lesson } from "../types";
import { intro } from "./data/01-intro";
import { stringOperations } from "./data/02-string-operations";
import { expiry } from "./data/03-expiry";
import { counters } from "./data/04-counters";
import { lists } from "./data/05-lists";
import { sets } from "./data/06-sets";
import { hashes } from "./data/07-hashes";
import { sortedSets } from "./data/08-sorted-sets";
import { keyManagement } from "./data/09-key-management";
import { scanning } from "./data/10-scanning";
import { transactions } from "./data/11-transactions";
import { caching } from "./data/12-caching";
import { leaderboards } from "./data/13-leaderboards";
import { hyperloglog } from "./data/14-hyperloglog";
import { dataModeling } from "./data/15-data-modeling";

export const redisChapters: Chapter[] = [
  { id: "strings", title: "Strings" },
  { id: "collections", title: "Collections" },
  { id: "key-ops", title: "Key Operations" },
  { id: "patterns", title: "Patterns" },
  { id: "advanced", title: "Advanced" },
];

export const redisLessons: Lesson[] = [
  intro,
  stringOperations,
  expiry,
  counters,
  lists,
  sets,
  hashes,
  sortedSets,
  keyManagement,
  scanning,
  transactions,
  caching,
  leaderboards,
  hyperloglog,
  dataModeling,
];
