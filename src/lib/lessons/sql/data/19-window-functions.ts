import type { Lesson } from "../../types";

export const windowFunctions: Lesson = {
  id: "window-functions",
  title: "Window Functions",
  chapterId: "advanced-sql",
  content: `## What are Window Functions?

Window functions perform calculations across a set of rows that are related to the current row. Unlike \`GROUP BY\`, which collapses rows into groups, window functions **keep every row** in the result while adding computed values.

This makes them ideal for running totals, rankings, moving averages, and comparisons within groups.

### Window Function Syntax

Every window function uses the \`OVER()\` clause:

\`\`\`sql
SELECT
  name,
  price,
  SUM(price) OVER() AS total_price
FROM products;
\`\`\`

This adds a \`total_price\` column to every row containing the sum of all prices. Each row still appears individually, but now carries the aggregate alongside it.

The general syntax is:

\`\`\`sql
function_name(...) OVER (
  [PARTITION BY column1, column2, ...]
  [ORDER BY column3, column4, ...]
)
\`\`\`

### PARTITION BY

\`PARTITION BY\` divides the rows into groups (partitions) and applies the window function within each group independently:

\`\`\`sql
SELECT
  name,
  category,
  price,
  SUM(price) OVER(PARTITION BY category) AS category_total
FROM products;
\`\`\`

Each row shows the total price for its own category. Electronics products see the Electronics total, Kitchen products see the Kitchen total, and so on. All original rows are preserved.

### ORDER BY in Windows

Adding \`ORDER BY\` inside \`OVER()\` creates a running (cumulative) calculation:

\`\`\`sql
SELECT
  name,
  price,
  SUM(price) OVER(ORDER BY price) AS running_total
FROM products;
\`\`\`

Rows are processed in order of ascending price, and \`SUM\` accumulates as it goes. The first row shows just its own price, the second row shows the sum of the first two, and so on.

You can combine \`PARTITION BY\` and \`ORDER BY\` for running totals within each group:

\`\`\`sql
SELECT
  name,
  category,
  price,
  SUM(price) OVER(PARTITION BY category ORDER BY price) AS running_category_total
FROM products;
\`\`\`

### Running Totals with SUM OVER

Running totals are one of the most practical applications of window functions:

\`\`\`sql
SELECT
  o.id,
  c.name AS customer_name,
  o.total,
  SUM(o.total) OVER(ORDER BY o.id) AS running_total
FROM orders o
JOIN customers c ON o.customer_id = c.id;
\`\`\`

This shows each order alongside a cumulative total of all orders up to that point.

### ROW_NUMBER

\`ROW_NUMBER()\` assigns a unique sequential number to each row within its partition:

\`\`\`sql
SELECT
  name,
  price,
  ROW_NUMBER() OVER(ORDER BY price DESC) AS row_num
FROM products;
\`\`\`

The most expensive product gets \`1\`, the next gets \`2\`, and so on. If two products have the same price, the assignment is arbitrary for tied rows.

### RANK and DENSE_RANK

\`RANK()\` and \`DENSE_RANK()\` handle ties differently than \`ROW_NUMBER()\`:

| Function | Behavior with ties |
|----------|--------------------|
| \`ROW_NUMBER()\` | No ties; each row gets a unique number |
| \`RANK()\` | Tied rows get the same rank; next rank is skipped |
| \`DENSE_RANK()\` | Tied rows get the same rank; next rank is **not** skipped |

Example with tied values:

\`\`\`sql
SELECT
  name,
  price,
  ROW_NUMBER() OVER(ORDER BY price DESC) AS row_num,
  RANK() OVER(ORDER BY price DESC) AS rank,
  DENSE_RANK() OVER(ORDER BY price DESC) AS dense_rank
FROM products;
\`\`\`

If two products both cost $79.99:
- \`ROW_NUMBER\` gives them 2 and 3 (arbitrary)
- \`RANK\` gives them both 2, then the next is 4 (skips 3)
- \`DENSE_RANK\` gives them both 2, then the next is 3 (no gap)

### Ranking Within Groups

Combine \`PARTITION BY\` with ranking functions to rank items within each category:

\`\`\`sql
SELECT
  name,
  category,
  price,
  DENSE_RANK() OVER(PARTITION BY category ORDER BY price DESC) AS rank_in_category
FROM products;
\`\`\`

This ranks products by price within each category independently. The most expensive product in each category gets rank 1.

> **Tip:** Window functions are evaluated after \`WHERE\`, \`GROUP BY\`, and \`HAVING\`. If you need to filter based on a window function result (e.g., only the top 3 per category), wrap the query in a subquery or CTE and filter in the outer query.

### Your Task

Rank all products by price (highest first) using \`DENSE_RANK\`. Return the columns \`name\`, \`price\`, and \`price_rank\`.`,

  starterCode: `-- Rank all products by price (highest first)
SELECT
  name,
  price,
`,

  solution: `SELECT
  name,
  price,
  DENSE_RANK() OVER(ORDER BY price DESC) AS price_rank
FROM products;`,

  tests: [
    {
      name: "returns name, price, and price_rank columns",
      expected: '{"type":"contains","columns":["name","price","price_rank"]}',
    },
    {
      name: "returns all 8 products",
      expected: '{"type":"rowCount","value":8}',
    },
  ],
};
