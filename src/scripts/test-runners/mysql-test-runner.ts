import { Database } from "bun:sqlite";
import {
  EXERCISE_SCHEMA,
  EXERCISE_DATA,
  parseValidation,
  evaluateValidation,
} from "@/lib/mysql-shared";
import { mysqlLessons } from "@/lib/lessons/mysql/index";
import type { LessonTestResult } from "./types";

/**
 * Execute a possibly multi-statement SQL string.
 * Returns the rows+columns from the last SELECT-like statement.
 * Non-SELECT statements are executed with db.exec().
 */
function execSql(
  db: Database,
  sql: string,
): { columns: string[]; rows: Record<string, unknown>[] } {
  const stmts = sql.split(";").map((s) => s.trim()).filter(Boolean);
  let lastResult: { columns: string[]; rows: Record<string, unknown>[] } = { columns: [], rows: [] };

  for (const stmt of stmts) {
    const upper = stmt.trimStart().toUpperCase();
    if (upper.startsWith("SELECT") || upper.startsWith("WITH") || upper.startsWith("VALUES")) {
      const rows = db.query(stmt).all() as Record<string, unknown>[];
      const columns = rows.length > 0
        ? Object.keys(rows[0])
        : (() => {
            try { return db.prepare(stmt).columnNames; } catch { return []; }
          })();
      lastResult = { columns, rows };
    } else {
      db.exec(stmt);
    }
  }

  return lastResult;
}

export function runMysqlTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of mysqlLessons) {
    for (const test of lesson.tests) {
      try {
        const db = new Database(":memory:");

        try {
          db.exec(EXERCISE_SCHEMA);
          db.exec(EXERCISE_DATA);

          const spec = parseValidation(test.expected);

          if (test.code) {
            const parts = test.code.split("---VALIDATE---");
            const setup = parts[0].replace("{{USER_SQL}}", lesson.solution.trim());
            const validation = parts[1]?.trim() ?? "";

            execSql(db, setup);

            if (validation) {
              const { columns, rows } = execSql(db, validation);
              const ev = evaluateValidation(spec, columns, rows);
              results.push({
                course: "mysql",
                lessonId: lesson.id,
                lessonTitle: lesson.title,
                testName: test.name,
                ...ev,
              });
            } else {
              results.push({
                course: "mysql",
                lessonId: lesson.id,
                lessonTitle: lesson.title,
                testName: test.name,
                passed: true,
                actual: "executed successfully",
                expected: "executed successfully",
              });
            }
          } else {
            const { columns, rows } = execSql(db, lesson.solution.trim());
            const ev = evaluateValidation(spec, columns, rows);
            results.push({
              course: "mysql",
              lessonId: lesson.id,
              lessonTitle: lesson.title,
              testName: test.name,
              ...ev,
            });
          }
        } finally {
          db.close();
        }
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        results.push({
          course: "mysql",
          lessonId: lesson.id,
          lessonTitle: lesson.title,
          testName: test.name,
          passed: false,
          actual: message,
          expected: test.expected,
        });
      }
    }
  }

  return results;
}

// Run directly if invoked as main
const isMain = import.meta.path === Bun.main;
if (isMain) {
  const results = runMysqlTests();
  const passed = results.filter((r) => r.passed);
  const failed = results.filter((r) => !r.passed);

  for (const r of results) {
    const icon = r.passed ? "✓" : "✗";
    console.log(`  ${icon} [${r.course}] ${r.lessonTitle} - ${r.testName}`);
  }

  if (failed.length > 0) {
    console.log("\n--- Failures ---\n");
    for (const r of failed) {
      console.log(`  [${r.course}] ${r.lessonTitle} - ${r.testName}`);
      console.log(`    expected: ${JSON.stringify(r.expected)}`);
      console.log(`    actual:   ${JSON.stringify(r.actual)}`);
      console.log();
    }
  }

  console.log(`\n${passed.length}/${results.length} tests passed`);
  if (failed.length > 0) process.exit(1);
}
