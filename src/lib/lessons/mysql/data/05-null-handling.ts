import type { Lesson } from "../../types";

export const nullHandling: Lesson = {
  id: "null-handling",
  title: "NULL Values",
  chapterId: "querying",
  content: `## NULL in MySQL

\`NULL\` represents the absence of a value — it means "unknown" or "not applicable". It is not the same as zero, an empty string, or \`false\`.

In our database, the \`customers.city\` column can be \`NULL\` for customers who didn't provide their city when signing up.

### NULL Is Not Equal to Anything

This is a common pitfall:

\`\`\`sql
-- WRONG: This never matches NULL
SELECT * FROM customers WHERE city = NULL;

-- CORRECT: Use IS NULL
SELECT * FROM customers WHERE city IS NULL;
\`\`\`

NULL is not equal even to itself. \`NULL = NULL\` evaluates to \`NULL\` (not \`TRUE\`).

### IS NULL and IS NOT NULL

\`\`\`sql
-- Customers with no city
SELECT name FROM customers WHERE city IS NULL;

-- Customers who have provided a city
SELECT name FROM customers WHERE city IS NOT NULL;
\`\`\`

### NULL in Comparisons

Any comparison with NULL returns NULL (which is treated as false in WHERE):

\`\`\`sql
SELECT NULL = NULL;   -- NULL (not TRUE)
SELECT NULL > 5;      -- NULL (not FALSE)
SELECT NULL + 10;     -- NULL
\`\`\`

### COALESCE — Default Values for NULL

\`COALESCE(expr1, expr2, ...)\` returns the first non-NULL argument:

\`\`\`sql
SELECT name, COALESCE(city, 'Unknown') AS city
FROM customers;
\`\`\`

When \`city\` is NULL, \`COALESCE\` returns \`'Unknown'\`. When \`city\` has a value, it returns the value unchanged.

### IFNULL — MySQL Shorthand

MySQL's \`IFNULL(expr, default)\` is equivalent to \`COALESCE\` with two arguments:

\`\`\`sql
SELECT name, IFNULL(city, 'Unknown') AS city
FROM customers;
\`\`\`

### NULLIF — Conditional NULL

\`NULLIF(expr1, expr2)\` returns NULL if both expressions are equal, otherwise returns \`expr1\`:

\`\`\`sql
-- Treat 'N/A' as NULL for reporting
SELECT name, NULLIF(city, 'N/A') AS city FROM customers;
\`\`\`

### NULL in Aggregations

Aggregate functions like \`COUNT\`, \`SUM\`, \`AVG\` ignore NULL values:

\`\`\`sql
-- Counts only non-NULL city values
SELECT COUNT(city) FROM customers;

-- Counts all rows including NULL city
SELECT COUNT(*) FROM customers;
\`\`\`

This distinction matters: \`COUNT(city)\` ≠ \`COUNT(*)\` when some cities are NULL.

### Your Task

Select the \`name\` and \`city\` from the \`customers\` table. For customers with a \`NULL\` city, display \`'Unknown'\` instead. Use \`COALESCE\` and alias the result as \`city\`.`,

  starterCode: `-- Replace NULL city with 'Unknown'
SELECT name, COALESCE(city, 'Unknown') AS city
FROM`,

  solution: `SELECT name, COALESCE(city, 'Unknown') AS city
FROM customers;`,

  tests: [
    {
      name: "returns all 5 customers",
      expected: '{"type":"rowCount","value":5}',
    },
    {
      name: "returns name and city columns",
      expected: '{"type":"contains","columns":["name","city"]}',
    },
    {
      name: "NULL city is replaced with Unknown",
      expected: '{"type":"contains","value":"Unknown"}',
    },
    {
      name: "known cities are preserved",
      expected: '{"type":"contains","value":"New York"}',
    },
  ],
};
