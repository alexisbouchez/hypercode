import type { Lesson } from "../../types";

export const dataTypes: Lesson = {
  id: "data-types",
  title: "Data Types and NULL",
  chapterId: "getting-started",
  content: `## SQLite's Type System

SQLite has a unique approach to types called **type affinity**. Unlike most databases that enforce strict column types, SQLite is flexible: a column declared as \`INTEGER\` can store text, and a column declared as \`TEXT\` can store a number. The declared type is just a hint that influences how SQLite coerces values.

### Storage Classes

SQLite has five storage classes — the actual types values are stored as:

| Storage Class | Description |
|--------------|-------------|
| \`NULL\` | A missing value |
| \`INTEGER\` | Signed integer (1, 2, 3, 4, 6, or 8 bytes) |
| \`REAL\` | 8-byte IEEE 754 floating-point number |
| \`TEXT\` | UTF-8 or UTF-16 string |
| \`BLOB\` | Binary data, stored exactly as input |

### Type Affinity

When you declare a column with a type name, SQLite maps it to one of five affinities: TEXT, NUMERIC, INTEGER, REAL, or BLOB. This determines how SQLite tries to coerce inserted values:

\`\`\`sql
-- All of these declare INTEGER affinity
CREATE TABLE t (a INT, b INTEGER, c TINYINT, d BIGINT);

-- TEXT affinity
CREATE TABLE t (a TEXT, b VARCHAR(100), c CHAR(10));

-- REAL affinity
CREATE TABLE t (a REAL, b FLOAT, c DOUBLE);

-- NUMERIC affinity (can store either integer or real)
CREATE TABLE t (a NUMERIC, b DECIMAL(10,2));
\`\`\`

### TYPEOF()

The \`TYPEOF()\` function returns the storage class of any value:

\`\`\`sql
SELECT
  TYPEOF(1),          -- 'integer'
  TYPEOF(1.5),        -- 'real'
  TYPEOF('hello'),    -- 'text'
  TYPEOF(NULL),       -- 'null'
  TYPEOF(x'ff00');    -- 'blob'
\`\`\`

You can use \`TYPEOF()\` on column values to inspect what SQLite actually stored:

\`\`\`sql
SELECT name, price, TYPEOF(price) FROM products;
\`\`\`

### NULL in SQLite

\`NULL\` represents a missing or unknown value. It propagates through expressions:

\`\`\`sql
SELECT NULL + 1;     -- NULL
SELECT NULL = NULL;  -- NULL (not TRUE!)
SELECT NULL OR TRUE; -- TRUE
SELECT NULL AND FALSE; -- FALSE
\`\`\`

Key rules:
- \`NULL = NULL\` is \`NULL\`, not \`TRUE\`. Use \`IS NULL\` to check for null.
- Any arithmetic with \`NULL\` returns \`NULL\`.
- Aggregate functions (\`COUNT\`, \`SUM\`, etc.) ignore \`NULL\` values, except \`COUNT(*)\`.

### COALESCE and IFNULL

Replace \`NULL\` with a default value:

\`\`\`sql
-- COALESCE returns the first non-NULL argument
SELECT COALESCE(NULL, NULL, 42); -- 42

-- IFNULL is a SQLite shorthand for COALESCE with two arguments
SELECT IFNULL(NULL, 'default'); -- 'default'

-- Practical use: treat NULL prices as 0
SELECT name, IFNULL(price, 0) AS price FROM products;
\`\`\`

### Your Task

Use \`TYPEOF()\` to select the \`name\`, \`price\`, and the type of the price column as \`price_type\` from all products.`,

  starterCode: `-- Select name, price, and typeof(price) as price_type
SELECT name, price,`,

  solution: `SELECT name, price, TYPEOF(price) AS price_type FROM products;`,

  tests: [
    {
      name: "returns name, price, and price_type columns",
      expected: '{"type":"contains","columns":["name","price","price_type"]}',
    },
    {
      name: "returns all 8 products",
      expected: '{"type":"rowCount","value":8}',
    },
  ],
};
