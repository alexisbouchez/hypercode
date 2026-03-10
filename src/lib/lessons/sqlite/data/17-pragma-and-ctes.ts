import type { Lesson } from "../../types";

export const pragmaAndCtes: Lesson = {
  id: "pragma-and-ctes",
  title: "PRAGMA & Recursive CTEs",
  chapterId: "sqlite-features",
  content: `## PRAGMA Statements

SQLite provides **PRAGMA** commands to query and configure database internals. They are unique to SQLite — you won't find them in PostgreSQL or MySQL.

### PRAGMA table_info

Inspect the columns of a table:

\`\`\`sql
PRAGMA table_info(products);
\`\`\`

Returns one row per column with fields: \`cid\` (column index), \`name\`, \`type\`, \`notnull\`, \`dflt_value\`, and \`pk\` (1 if primary key).

### PRAGMA foreign_keys

SQLite does **not** enforce foreign keys by default. You must enable them per connection:

\`\`\`sql
PRAGMA foreign_keys;       -- check current setting (0 = off)
PRAGMA foreign_keys = ON;  -- enable enforcement
\`\`\`

### PRAGMA journal_mode

Controls the transaction journaling strategy:

\`\`\`sql
PRAGMA journal_mode;        -- check current mode
PRAGMA journal_mode = WAL;  -- Write-Ahead Logging
\`\`\`

WAL mode allows readers and a single writer to operate concurrently, which improves performance in multi-threaded applications.

---

## Recursive CTEs

You saw basic CTEs in an earlier lesson. **Recursive CTEs** use \`WITH RECURSIVE\` to generate sequences or traverse hierarchical data.

### Generating a Sequence

\`\`\`sql
WITH RECURSIVE seq(n) AS (
  SELECT 1            -- anchor (base case)
  UNION ALL
  SELECT n + 1 FROM seq WHERE n < 5  -- recursive step
)
SELECT n FROM seq;
-- Returns: 1, 2, 3, 4, 5
\`\`\`

The anchor member runs once to produce the initial row(s). The recursive member repeatedly references the CTE itself until the \`WHERE\` condition stops it.

### Fibonacci Numbers

\`\`\`sql
WITH RECURSIVE fib(a, b) AS (
  SELECT 0, 1
  UNION ALL
  SELECT b, a + b FROM fib WHERE b < 100
)
SELECT a AS fibonacci FROM fib;
\`\`\`

### Hierarchical Data (Tree Traversal)

Imagine an \`employees\` table with a \`manager_id\` column. A recursive CTE can walk the org chart:

\`\`\`sql
WITH RECURSIVE chain(id, name, level) AS (
  SELECT id, name, 0 FROM employees WHERE manager_id IS NULL
  UNION ALL
  SELECT e.id, e.name, c.level + 1
  FROM employees e
  JOIN chain c ON e.manager_id = c.id
)
SELECT * FROM chain;
\`\`\`

### Your Task

Write a recursive CTE named \`nums\` that generates all integers from 1 to 10. Select the column \`n\` from the CTE. The result should have 10 rows containing the values 1 through 10.`,

  starterCode: `-- Generate numbers 1 to 10 using WITH RECURSIVE
WITH RECURSIVE nums(n) AS (
  SELECT 1
  UNION ALL
  SELECT n + 1 FROM nums WHERE ___
)
SELECT n FROM nums;`,

  solution: `WITH RECURSIVE nums(n) AS (
  SELECT 1
  UNION ALL
  SELECT n + 1 FROM nums WHERE n < 10
)
SELECT n FROM nums;`,

  tests: [
    {
      name: "returns 10 rows",
      expected: '{"type":"rowCount","value":10}',
    },
    {
      name: "contains column n",
      expected: '{"type":"contains","columns":["n"]}',
    },
    {
      name: "first value is 1",
      expected: '{"type":"exact","value":"1"}',
    },
  ],
};
