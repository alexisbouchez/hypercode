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
  const db = new PGlite();
  await db.waitReady;

  for (const lesson of sqlLessons) {
    for (const test of lesson.tests) {
      try {
        // Reset to clean state: drop all tables and recreate schema
        await db.exec(`
          DO $$ DECLARE r RECORD;
          BEGIN
            FOR r IN (SELECT tablename FROM pg_tables WHERE schemaname = 'public') LOOP
              EXECUTE 'DROP TABLE IF EXISTS ' || quote_ident(r.tablename) || ' CASCADE';
            END LOOP;
          END $$;
        `);
        await db.exec("DROP SEQUENCE IF EXISTS products_id_seq, users_id_seq, customers_id_seq, orders_id_seq CASCADE;");
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
      }
    }
  }

  await db.close();
  return results;
}
