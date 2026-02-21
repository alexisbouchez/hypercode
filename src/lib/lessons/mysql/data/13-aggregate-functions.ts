import type { Lesson } from "../../types";

export const aggregateFunctions: Lesson = {
  id: "aggregate-functions",
  title: "Aggregate Functions",
  chapterId: "aggregations",
  content: `## Aggregate Functions

Aggregate functions compute a single result from a set of rows. They collapse multiple rows into one summary value.

### The Core Aggregates

| Function | Description |
|----------|-------------|
| \`COUNT(*)\` | Number of rows |
| \`COUNT(col)\` | Number of non-NULL values |
| \`SUM(col)\` | Total of all values |
| \`AVG(col)\` | Average of all values |
| \`MIN(col)\` | Smallest value |
| \`MAX(col)\` | Largest value |

\`\`\`sql
SELECT
  COUNT(*) AS total_products,
  MIN(price) AS cheapest,
  MAX(price) AS most_expensive,
  ROUND(AVG(price), 2) AS avg_price,
  SUM(stock) AS total_stock
FROM products;
\`\`\`

### GROUP BY — Aggregating by Category

Without \`GROUP BY\`, aggregates collapse all rows into one. With \`GROUP BY\`, rows are grouped first, then aggregated per group:

\`\`\`sql
SELECT category, COUNT(*) AS count, ROUND(AVG(price), 2) AS avg_price
FROM products
GROUP BY category;
\`\`\`

This produces one row per distinct category value.

### Rules for GROUP BY

Any column in \`SELECT\` must either:
1. Appear in the \`GROUP BY\` clause, or
2. Be wrapped in an aggregate function

\`\`\`sql
-- WRONG: name is not in GROUP BY and not aggregated
SELECT category, name, COUNT(*) FROM products GROUP BY category;

-- CORRECT: name is aggregated
SELECT category, MAX(name) AS sample_name, COUNT(*) FROM products GROUP BY category;
\`\`\`

### Counting with Conditions

\`COUNT\` only counts non-NULL values. Use \`SUM\` with a conditional expression to count matching rows:

\`\`\`sql
-- Count how many products are low stock (< 100)
SELECT
  category,
  COUNT(*) AS total,
  SUM(CASE WHEN stock < 100 THEN 1 ELSE 0 END) AS low_stock
FROM products
GROUP BY category;
\`\`\`

### DISTINCT in Aggregates

\`COUNT(DISTINCT col)\` counts unique non-NULL values:

\`\`\`sql
-- How many unique cities do our customers live in?
SELECT COUNT(DISTINCT city) AS unique_cities FROM customers;
\`\`\`

### GROUP_CONCAT

MySQL's \`GROUP_CONCAT\` concatenates values from a group into a single string:

\`\`\`sql
SELECT
  category,
  GROUP_CONCAT(name ORDER BY name SEPARATOR ', ') AS products
FROM products
GROUP BY category;
-- Electronics: Headphones, Laptop, Smartphone
\`\`\`

### Your Task

For each product \`category\`, compute:
- \`product_count\` — number of products
- \`avg_price\` — average price rounded to 2 decimal places
- \`max_price\` — the highest price

Group by \`category\`.`,

  starterCode: `-- Aggregate products by category
SELECT
  category,
  COUNT(*) AS product_count,
  ROUND(AVG(price), 2) AS avg_price,
  MAX(price) AS max_price
FROM products
GROUP BY`,

  solution: `SELECT
  category,
  COUNT(*) AS product_count,
  ROUND(AVG(price), 2) AS avg_price,
  MAX(price) AS max_price
FROM products
GROUP BY category;`,

  tests: [
    {
      name: "returns 4 category rows",
      expected: '{"type":"rowCount","value":4}',
    },
    {
      name: "returns category, product_count, avg_price, and max_price columns",
      expected: '{"type":"contains","columns":["category","product_count","avg_price","max_price"]}',
    },
    {
      name: "includes Electronics category",
      expected: '{"type":"contains","value":"Electronics"}',
    },
    {
      name: "includes max price of 999.99 (Laptop)",
      expected: '{"type":"contains","value":"999.99"}',
    },
  ],
};
