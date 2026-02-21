import type { Lesson } from "../../types";

export const aggregations: Lesson = {
  id: "aggregations",
  title: "Aggregations",
  chapterId: "querying",
  content: `## Aggregate Functions

Aggregate functions compute a single result from a set of rows. They are most useful with \`GROUP BY\`, but can also be used without it to aggregate the entire table.

### COUNT

Count rows:

\`\`\`sql
SELECT COUNT(*) FROM products;        -- total rows
SELECT COUNT(price) FROM products;    -- rows where price IS NOT NULL
SELECT COUNT(DISTINCT category) FROM products; -- distinct categories
\`\`\`

### SUM, AVG, MIN, MAX

\`\`\`sql
SELECT SUM(price) FROM products;       -- total of all prices
SELECT AVG(price) FROM products;       -- average price
SELECT MIN(price) FROM products;       -- cheapest
SELECT MAX(price) FROM products;       -- most expensive
\`\`\`

All aggregate functions ignore \`NULL\` values (except \`COUNT(*)\`).

### GROUP BY

\`GROUP BY\` divides rows into groups and applies the aggregate function to each group:

\`\`\`sql
SELECT category, COUNT(*) AS product_count
FROM products
GROUP BY category;
\`\`\`

This returns one row per category, with the count of products in each.

\`\`\`sql
SELECT category, AVG(price) AS avg_price, MAX(price) AS max_price
FROM products
GROUP BY category;
\`\`\`

### HAVING

\`WHERE\` filters rows before grouping. \`HAVING\` filters groups after aggregation:

\`\`\`sql
-- Only categories with more than 2 products
SELECT category, COUNT(*) AS count
FROM products
GROUP BY category
HAVING COUNT(*) > 2;
\`\`\`

You cannot use aggregate functions in \`WHERE\` — use \`HAVING\` instead.

### Combining with JOIN

\`\`\`sql
SELECT c.name, COUNT(o.id) AS order_count, SUM(o.total) AS total_spent
FROM customers c
LEFT JOIN orders o ON o.customer_id = c.id
GROUP BY c.id, c.name;
\`\`\`

### Your Task

Count the number of products in each category. Return \`category\` and \`product_count\` (the count), ordered by \`product_count\` descending.`,

  starterCode: `-- Count products per category
SELECT category, COUNT(*) AS product_count
FROM products
GROUP BY
ORDER BY product_count DESC;`,

  solution: `SELECT category, COUNT(*) AS product_count
FROM products
GROUP BY category
ORDER BY product_count DESC;`,

  tests: [
    {
      name: "returns category and product_count columns",
      expected: '{"type":"contains","columns":["category","product_count"]}',
    },
    {
      name: "returns 4 category groups",
      expected: '{"type":"rowCount","value":4}',
    },
  ],
};
