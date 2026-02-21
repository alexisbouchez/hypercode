import type { RunResult, Test, TestResult } from "@/lib/lessons/types";
import {
  EXERCISE_SCHEMA,
  EXERCISE_DATA,
  parseValidation,
  evaluateValidation,
} from "@/lib/sql-shared";

let pgliteReady = false;
let pgliteLoadPromise: Promise<void> | null = null;

export function isSqlReady(): boolean {
  return pgliteReady;
}

export function initSqlRunner(): Promise<void> {
  if (pgliteReady) return Promise.resolve();
  if (pgliteLoadPromise) return pgliteLoadPromise;

  pgliteLoadPromise = (async () => {
    try {
      const { PGlite } = await import("@electric-sql/pglite");
      const db = new PGlite();
      await db.waitReady;
      await db.close();
      pgliteReady = true;
    } catch {
      pgliteReady = false;
    }
  })();

  return pgliteLoadPromise;
}

function formatResultTable(
  columns: string[],
  rows: Record<string, unknown>[],
): string {
  if (columns.length === 0 || rows.length === 0) {
    if (columns.length === 0) return "(0 rows)\n";
    return (
      columns.join(" | ") + "\n" + columns.map(() => "---").join("-|-") + "\n(0 rows)\n"
    );
  }

  const widths = columns.map((col) => {
    const values = rows.map((row) => String(row[col] ?? "NULL"));
    return Math.max(col.length, ...values.map((v) => v.length));
  });

  const header = columns.map((col, i) => col.padEnd(widths[i])).join(" | ");
  const separator = widths.map((w) => "-".repeat(w)).join("-+-");
  const body = rows
    .map((row) =>
      columns
        .map((col, i) => String(row[col] ?? "NULL").padEnd(widths[i]))
        .join(" | "),
    )
    .join("\n");

  return `${header}\n${separator}\n${body}\n(${rows.length} ${rows.length === 1 ? "row" : "rows"})\n`;
}

export async function runSql(code: string): Promise<RunResult> {
  if (!pgliteReady) {
    return {
      stdout: "",
      stderr: "",
      error: "SQL runtime is not loaded.",
    };
  }

  try {
    const { PGlite } = await import("@electric-sql/pglite");
    const db = new PGlite();
    await db.waitReady;

    try {
      await db.exec(EXERCISE_SCHEMA);
      await db.exec(EXERCISE_DATA);

      const statements = code.trim();
      const result = await db.query<Record<string, unknown>>(statements);
      const columns = result.fields?.map((f) => f.name) ?? [];
      const rows = result.rows ?? [];

      return {
        stdout: formatResultTable(columns, rows),
        stderr: "",
        error: "",
      };
    } finally {
      await db.close();
    }
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return {
      stdout: "",
      stderr: "",
      error: message,
    };
  }
}

export async function runTests(
  code: string,
  tests: Test[],
): Promise<TestResult[]> {
  const results: TestResult[] = [];

  for (const test of tests) {
    try {
      const { PGlite } = await import("@electric-sql/pglite");
      const db = new PGlite();
      await db.waitReady;

      try {
        await db.exec(EXERCISE_SCHEMA);
        await db.exec(EXERCISE_DATA);

        const spec = parseValidation(test.expected);

        if (test.code) {
          // DDL exercise: test.code contains setup + validation separated by ---VALIDATE---
          const parts = test.code.split("---VALIDATE---");
          const setup = parts[0].replace("{{USER_SQL}}", code.trim());
          const validation = parts[1]?.trim() ?? "";

          await db.exec(setup);

          if (validation) {
            const result = await db.query<Record<string, unknown>>(validation);
            const columns = result.fields?.map((f) => f.name) ?? [];
            const rows = result.rows ?? [];
            const ev = evaluateValidation(spec, columns, rows);
            results.push({ name: test.name, ...ev });
          } else {
            results.push({
              name: test.name,
              passed: true,
              actual: "executed successfully",
              expected: "executed successfully",
            });
          }
        } else {
          // Standard query exercise
          const result = await db.query<Record<string, unknown>>(code.trim());
          const columns = result.fields?.map((f) => f.name) ?? [];
          const rows = result.rows ?? [];
          const ev = evaluateValidation(spec, columns, rows);
          results.push({ name: test.name, ...ev });
        }
      } finally {
        await db.close();
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      results.push({
        name: test.name,
        passed: false,
        actual: message,
        expected: test.expected,
      });
    }
  }

  return results;
}
