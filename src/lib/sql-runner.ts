import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let pgliteReady = false;
let pgliteLoading = false;
let pgliteLoadPromise: Promise<void> | null = null;

const EXERCISE_SCHEMA = `
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  price DECIMAL(10,2) NOT NULL,
  category TEXT NOT NULL
);

CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE
);

CREATE TABLE customers (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE
);

CREATE TABLE orders (
  id SERIAL PRIMARY KEY,
  customer_id INTEGER REFERENCES customers(id),
  product_id INTEGER REFERENCES products(id),
  quantity INTEGER NOT NULL DEFAULT 1,
  total DECIMAL(10,2) NOT NULL
);
`;

const EXERCISE_DATA = `
INSERT INTO products (name, price, category) VALUES
  ('Laptop', 999.99, 'Electronics'),
  ('Headphones', 79.99, 'Electronics'),
  ('Coffee Maker', 49.99, 'Kitchen'),
  ('Notebook', 4.99, 'Office'),
  ('Pen Set', 12.99, 'Office'),
  ('Desk Lamp', 34.99, 'Office'),
  ('Water Bottle', 19.99, 'Kitchen'),
  ('Backpack', 59.99, 'Accessories');

INSERT INTO users (name, email) VALUES
  ('Alice', 'alice@example.com'),
  ('Bob', 'bob@example.com'),
  ('Charlie', 'charlie@example.com');

INSERT INTO customers (name, email) VALUES
  ('Alice', 'alice@example.com'),
  ('Bob', 'bob@example.com'),
  ('Charlie', 'charlie@example.com');

INSERT INTO orders (customer_id, product_id, quantity, total) VALUES
  (1, 1, 1, 999.99),
  (1, 2, 2, 159.98),
  (2, 3, 1, 49.99);
`;

export function isSqlReady(): boolean {
  return pgliteReady;
}

export function initSqlRunner(): Promise<void> {
  if (pgliteReady) return Promise.resolve();
  if (pgliteLoadPromise) return pgliteLoadPromise;

  pgliteLoadPromise = (async () => {
    pgliteLoading = true;
    try {
      const { PGlite } = await import("@electric-sql/pglite");
      const db = new PGlite();
      await db.waitReady;
      await db.close();
      pgliteReady = true;
    } catch {
      pgliteReady = false;
    } finally {
      pgliteLoading = false;
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

interface ValidationSpec {
  type: "rowCount" | "contains" | "exact" | "custom";
  value?: string | number;
  columns?: string[];
}

function parseValidation(expected: string): ValidationSpec {
  try {
    return JSON.parse(expected) as ValidationSpec;
  } catch {
    return { type: "custom" };
  }
}

function evaluateValidation(
  spec: ValidationSpec,
  columns: string[],
  rows: Record<string, unknown>[],
): { passed: boolean; actual: string; expected: string } {
  switch (spec.type) {
    case "rowCount": {
      const expectedCount = Number(spec.value);
      return {
        passed: rows.length === expectedCount,
        actual: `${rows.length} rows`,
        expected: `${expectedCount} rows`,
      };
    }
    case "contains": {
      if (spec.columns) {
        const lower = columns.map((c) => c.toLowerCase());
        const missing = spec.columns.filter(
          (c) => !lower.includes(c.toLowerCase()),
        );
        return {
          passed: missing.length === 0,
          actual: `columns: [${columns.join(", ")}]`,
          expected: `columns include: [${spec.columns.join(", ")}]`,
        };
      }
      const val = String(spec.value ?? "");
      const found = rows.some((row) =>
        Object.values(row).some((v) =>
          String(v).toLowerCase().includes(val.toLowerCase()),
        ),
      );
      return {
        passed: found,
        actual: found ? `found "${val}"` : `"${val}" not found`,
        expected: `contains "${val}"`,
      };
    }
    case "exact": {
      const val = String(spec.value ?? "");
      const firstVal =
        rows.length > 0
          ? String(Object.values(rows[0])[0] ?? "")
          : "";
      return {
        passed: firstVal === val,
        actual: firstVal,
        expected: val,
      };
    }
    case "custom":
    default: {
      return {
        passed: rows.length > 0,
        actual: rows.length > 0 ? `${rows.length} rows returned` : "0 rows",
        expected: "at least 1 row",
      };
    }
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
