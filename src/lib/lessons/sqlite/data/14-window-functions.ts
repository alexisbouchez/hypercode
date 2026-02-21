import type { Lesson } from "../../types";

export const windowFunctions: Lesson = {
  id: "window-functions",
  title: "Window Functions",
  chapterId: "sqlite-features",
  content: `## Window Functions

Window functions compute values across a set of rows related to the current row — without collapsing them like \`GROUP BY\` does. Every row in the result is preserved; window functions just add computed columns alongside.

SQLite has supported window functions since version 3.25.0 (2018).

### Syntax

\`\`\`sql
function_name(...) OVER (
  [PARTITION BY column1, ...]
  [ORDER BY column2, ...]
)
\`\`\`

### ROW_NUMBER

Assigns a unique sequential integer to each row within the window:

\`\`\`sql
SELECT
  name,
  category,
  price,
  ROW_NUMBER() OVER (ORDER BY price DESC) AS row_num
FROM products;
\`\`\`

### RANK and DENSE_RANK

Handle ties differently:

\`\`\`sql
SELECT
  name,
  price,
  RANK() OVER (ORDER BY price DESC) AS price_rank,
  DENSE_RANK() OVER (ORDER BY price DESC) AS dense_rank
FROM products;
\`\`\`

- \`RANK()\`: ties get the same rank; next rank is skipped (1, 2, 2, 4, ...)
- \`DENSE_RANK()\`: ties get the same rank; no gaps (1, 2, 2, 3, ...)

### PARTITION BY — Rank Within Groups

\`PARTITION BY\` applies the window function independently within each group:

\`\`\`sql
SELECT
  name,
  category,
  price,
  RANK() OVER (PARTITION BY category ORDER BY price DESC) AS rank_in_category
FROM products;
\`\`\`

Each category is ranked independently. The most expensive product in each category gets rank 1.

### SUM OVER — Running Totals

\`\`\`sql
SELECT
  name,
  price,
  SUM(price) OVER (ORDER BY price) AS running_total
FROM products;
\`\`\`

\`ORDER BY\` inside \`OVER\` creates a cumulative sum as rows are processed in order.

### LAG and LEAD

Access values from adjacent rows:

\`\`\`sql
SELECT
  name,
  price,
  LAG(price) OVER (ORDER BY price) AS prev_price,
  LEAD(price) OVER (ORDER BY price) AS next_price
FROM products;
\`\`\`

- \`LAG(col)\`: value from the previous row (\`NULL\` for the first row)
- \`LEAD(col)\`: value from the next row (\`NULL\` for the last row)

### Your Task

Rank all products by price within their category (highest first) using \`DENSE_RANK\`. Return \`name\`, \`category\`, \`price\`, and \`rank_in_category\`.`,

  starterCode: `-- Rank products by price within each category
SELECT
  name,
  category,
  price,
  DENSE_RANK() OVER (
    PARTITION BY
    ORDER BY price DESC
  ) AS rank_in_category
FROM products;`,

  solution: `SELECT
  name,
  category,
  price,
  DENSE_RANK() OVER (PARTITION BY category ORDER BY price DESC) AS rank_in_category
FROM products;`,

  tests: [
    {
      name: "returns name, category, price, rank_in_category columns",
      expected: '{"type":"contains","columns":["name","category","price","rank_in_category"]}',
    },
    {
      name: "returns all 8 products",
      expected: '{"type":"rowCount","value":8}',
    },
  ],
};
