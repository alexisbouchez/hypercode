import type { Lesson } from "../../types";

export const havingAndAdvancedAgg: Lesson = {
  id: "having-and-advanced-agg",
  title: "HAVING & Advanced Aggregations",
  chapterId: "aggregations",
  content: `## Filtering Groups with HAVING

The \`WHERE\` clause filters individual rows **before** grouping. The \`HAVING\` clause filters groups **after** aggregation. This distinction is critical.

\`\`\`sql
-- WHERE filters rows before GROUP BY
SELECT category, COUNT(*) AS product_count
FROM products
WHERE price > 10
GROUP BY category;

-- HAVING filters groups after GROUP BY
SELECT category, COUNT(*) AS product_count
FROM products
GROUP BY category
HAVING COUNT(*) > 2;
\`\`\`

The first query excludes cheap products, then counts per category. The second query counts all products per category, then keeps only categories with more than two products.

> **Tip:** A common mistake is using \`WHERE\` with aggregate functions. \`WHERE COUNT(*) > 2\` is invalid because \`WHERE\` runs before aggregation. Always use \`HAVING\` when your condition involves an aggregate function.

### Combining WHERE and HAVING

You can use both in the same query. \`WHERE\` narrows the input rows, then \`GROUP BY\` groups them, then \`HAVING\` filters the resulting groups:

\`\`\`sql
SELECT category, AVG(price) AS avg_price
FROM products
WHERE price > 5
GROUP BY category
HAVING AVG(price) > 30;
\`\`\`

The execution order is: \`FROM\` -> \`WHERE\` -> \`GROUP BY\` -> \`HAVING\` -> \`SELECT\` -> \`ORDER BY\`.

### Conditional Aggregation with CASE

You can use \`CASE\` expressions inside aggregate functions to count or sum only specific subsets of rows:

\`\`\`sql
SELECT
  COUNT(*) AS total_products,
  COUNT(CASE WHEN price > 50 THEN 1 END) AS expensive_count,
  COUNT(CASE WHEN price <= 50 THEN 1 END) AS affordable_count
FROM products;
\`\`\`

This produces a single row with three counts, each based on different conditions. The \`CASE\` returns \`1\` when the condition is met and \`NULL\` otherwise. Since \`COUNT\` ignores \`NULL\` values, only matching rows are counted.

You can apply this pattern with \`SUM\` and \`AVG\` as well:

\`\`\`sql
SELECT
  category,
  SUM(CASE WHEN price > 50 THEN price ELSE 0 END) AS expensive_total,
  AVG(CASE WHEN price > 50 THEN price END) AS expensive_avg
FROM products
GROUP BY category;
\`\`\`

### The FILTER Clause (PostgreSQL)

PostgreSQL provides the \`FILTER\` clause as a cleaner alternative to conditional aggregation:

\`\`\`sql
SELECT
  COUNT(*) AS total_products,
  COUNT(*) FILTER (WHERE price > 50) AS expensive_count,
  COUNT(*) FILTER (WHERE price <= 50) AS affordable_count
FROM products;
\`\`\`

\`FILTER\` is more readable and often preferred in PostgreSQL-specific code. It works with any aggregate function:

\`\`\`sql
SELECT
  category,
  AVG(price) AS overall_avg,
  AVG(price) FILTER (WHERE price > 20) AS avg_above_20
FROM products
GROUP BY category;
\`\`\`

### ROLLUP and CUBE

\`ROLLUP\` and \`CUBE\` generate additional summary rows for your grouped data.

**ROLLUP** creates subtotals that roll up from the most detailed level to a grand total:

\`\`\`sql
SELECT category, COUNT(*) AS product_count, SUM(price) AS total_price
FROM products
GROUP BY ROLLUP(category);
\`\`\`

This produces one row per category plus a final row where \`category\` is \`NULL\`, representing the grand total across all categories.

**CUBE** creates subtotals for every possible combination of the grouped columns:

\`\`\`sql
SELECT category, COUNT(*) AS product_count
FROM products
GROUP BY CUBE(category);
\`\`\`

### The GROUPING() Function

When using \`ROLLUP\` or \`CUBE\`, summary rows contain \`NULL\` in the grouped columns. The \`GROUPING()\` function lets you distinguish between a real \`NULL\` value in the data and a \`NULL\` that represents a subtotal:

\`\`\`sql
SELECT
  category,
  COUNT(*) AS product_count,
  GROUPING(category) AS is_total
FROM products
GROUP BY ROLLUP(category);
\`\`\`

\`GROUPING(category)\` returns \`1\` for the summary row and \`0\` for regular grouped rows.

### Your Task

Find categories that have more than 2 products.`,

  starterCode: `-- Find categories with more than 2 products
SELECT category, COUNT(*) AS product_count
FROM products
`,

  solution: `SELECT category, COUNT(*) AS product_count
FROM products
GROUP BY category
HAVING COUNT(*) > 2;`,

  tests: [
    {
      name: "returns categories with more than 2 products",
      expected: '{"type":"custom"}',
    },
  ],
};
