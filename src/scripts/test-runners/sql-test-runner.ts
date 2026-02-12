import { PGlite } from "@electric-sql/pglite";
import {
  EXERCISE_SCHEMA,
  EXERCISE_DATA,
  parseValidation,
  evaluateValidation,
} from "@/lib/sql-shared";
import { sqlLessons } from "@/lib/lessons/sql/index";
import type { LessonTestResult } from "./types";

export async function runSqlTests(): Promise<LessonTestResult[]> {
  const results: LessonTestResult[] = [];

  for (const lesson of sqlLessons) {
    for (const test of lesson.tests) {
      const db = new PGlite();
      await db.waitReady;

      try {
        await db.exec(EXERCISE_SCHEMA);
        await db.exec(EXERCISE_DATA);

        const spec = parseValidation(test.expected);

        if (test.code) {
          const parts = test.code.split("---VALIDATE---");
          const setup = parts[0].replace("{{USER_SQL}}", lesson.solution.trim());
          const validation = parts[1]?.trim() ?? "";

          await db.exec(setup);

          if (validation) {
            const result = await db.query<Record<string, unknown>>(validation);
            const columns = result.fields?.map((f) => f.name) ?? [];
            const rows = result.rows ?? [];
            const ev = evaluateValidation(spec, columns, rows);
            results.push({
              course: "sql",
              lessonId: lesson.id,
              lessonTitle: lesson.title,
              testName: test.name,
              ...ev,
            });
          } else {
            results.push({
              course: "sql",
              lessonId: lesson.id,
              lessonTitle: lesson.title,
              testName: test.name,
              passed: true,
              actual: "executed successfully",
              expected: "executed successfully",
            });
          }
        } else {
          const result = await db.query<Record<string, unknown>>(lesson.solution.trim());
          const columns = result.fields?.map((f) => f.name) ?? [];
          const rows = result.rows ?? [];
          const ev = evaluateValidation(spec, columns, rows);
          results.push({
            course: "sql",
            lessonId: lesson.id,
            lessonTitle: lesson.title,
            testName: test.name,
            ...ev,
          });
        }
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        results.push({
          course: "sql",
          lessonId: lesson.id,
          lessonTitle: lesson.title,
          testName: test.name,
          passed: false,
          actual: message,
          expected: test.expected,
        });
      } finally {
        await db.close();
      }
    }
  }

  return results;
}
