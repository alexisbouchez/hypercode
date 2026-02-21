import type { RunResult, Test, TestResult } from "@/lib/lessons/types";
import {
  EXERCISE_SCHEMA,
  EXERCISE_DATA,
  parseValidation,
  evaluateValidation,
} from "@/lib/sqlite-shared";

let sqliteReady = false;
let sqliteLoadPromise: Promise<void> | null = null;
// eslint-disable-next-line @typescript-eslint/no-explicit-any
let SQL: any = null;

export function isSqliteReady(): boolean {
  return sqliteReady;
}

export function initSqliteRunner(): Promise<void> {
  if (sqliteReady) return Promise.resolve();
  if (sqliteLoadPromise) return sqliteLoadPromise;

  sqliteLoadPromise = (async () => {
    try {
      const initSqlJs = (await import("sql.js")).default;
      SQL = await initSqlJs({ locateFile: () => "/sql-wasm.wasm" });
      sqliteReady = true;
    } catch {
      sqliteReady = false;
    }
  })();

  return sqliteLoadPromise;
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

function execAndGetResults(
  db: { exec: (sql: string) => { columns: string[]; values: unknown[][] }[] },
  sql: string,
): { columns: string[]; rows: Record<string, unknown>[] } {
  const results = db.exec(sql);
  if (!results || results.length === 0) {
    return { columns: [], rows: [] };
  }
  const { columns, values } = results[results.length - 1];
  const rows = values.map((row) => {
    const obj: Record<string, unknown> = {};
    columns.forEach((col, i) => {
      obj[col] = row[i];
    });
    return obj;
  });
  return { columns, rows };
}

export async function runSqlite(code: string): Promise<RunResult> {
  if (!sqliteReady || !SQL) {
    return {
      stdout: "",
      stderr: "",
      error: "SQLite runtime is not loaded.",
    };
  }

  try {
    const db = new SQL.Database();
    try {
      db.run(EXERCISE_SCHEMA);
      db.run(EXERCISE_DATA);

      const { columns, rows } = execAndGetResults(db, code.trim());

      return {
        stdout: formatResultTable(columns, rows),
        stderr: "",
        error: "",
      };
    } finally {
      db.close();
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
      const db = new SQL.Database();
      try {
        db.run(EXERCISE_SCHEMA);
        db.run(EXERCISE_DATA);

        const spec = parseValidation(test.expected);

        if (test.code) {
          // DDL exercise: test.code contains setup + validation separated by ---VALIDATE---
          const parts = test.code.split("---VALIDATE---");
          const setup = parts[0].replace("{{USER_SQL}}", code.trim());
          const validation = parts[1]?.trim() ?? "";

          db.run(setup);

          if (validation) {
            const { columns, rows } = execAndGetResults(db, validation);
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
          const { columns, rows } = execAndGetResults(db, code.trim());
          const ev = evaluateValidation(spec, columns, rows);
          results.push({ name: test.name, ...ev });
        }
      } finally {
        db.close();
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
