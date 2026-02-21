import type { Lesson } from "../../types";

export const ctes: Lesson = {
  id: "ctes",
  title: "Common Table Expressions",
  chapterId: "querying",
  content: `## WITH Clause (CTEs)

A **Common Table Expression** (CTE) is a named temporary result set defined within a query using the \`WITH\` clause. CTEs make complex queries more readable by breaking them into named, reusable parts.

### Basic CTE

\`\`\`sql
WITH expensive_products AS (
  SELECT * FROM products WHERE price > 50
)
SELECT * FROM expensive_products;
\`\`\`

The CTE \`expensive_products\` acts like a temporary view that exists only for the duration of the query.

### Why Use CTEs?

Without a CTE, complex queries become deeply nested and hard to read:

\`\`\`sql
-- Without CTE (hard to read)
SELECT name, price FROM (
  SELECT * FROM products WHERE price > 50
) WHERE category = 'Electronics';

-- With CTE (clear and structured)
WITH expensive AS (
  SELECT * FROM products WHERE price > 50
)
SELECT name, price FROM expensive WHERE category = 'Electronics';
\`\`\`

### Multiple CTEs

Define multiple CTEs by separating them with commas. Later CTEs can reference earlier ones:

\`\`\`sql
WITH
  electronics AS (
    SELECT * FROM products WHERE category = 'Electronics'
  ),
  cheap_electronics AS (
    SELECT * FROM electronics WHERE price < 100
  )
SELECT name, price FROM cheap_electronics;
\`\`\`

### CTEs with Aggregations

\`\`\`sql
WITH
  category_stats AS (
    SELECT
      category,
      COUNT(*) AS count,
      AVG(price) AS avg_price
    FROM products
    GROUP BY category
  )
SELECT *
FROM category_stats
WHERE avg_price > 50
ORDER BY avg_price DESC;
\`\`\`

### CTEs with Joins

\`\`\`sql
WITH
  order_totals AS (
    SELECT customer_id, SUM(total) AS total_spent
    FROM orders
    GROUP BY customer_id
  )
SELECT c.name, ot.total_spent
FROM customers c
JOIN order_totals ot ON c.id = ot.customer_id
ORDER BY ot.total_spent DESC;
\`\`\`

### Recursive CTEs

SQLite supports recursive CTEs for hierarchical data:

\`\`\`sql
WITH RECURSIVE counter(n) AS (
  SELECT 1
  UNION ALL
  SELECT n + 1 FROM counter WHERE n < 5
)
SELECT n FROM counter;
-- Returns: 1, 2, 3, 4, 5
\`\`\`

### Your Task

Use a CTE named \`office_products\` to select all products in the \`'Office'\` category. Then select \`name\` and \`price\` from the CTE, ordered by price ascending.`,

  starterCode: `-- Define a CTE for Office products
WITH office_products AS (
  SELECT * FROM products WHERE
)
SELECT name, price FROM office_products ORDER BY price;`,

  solution: `WITH office_products AS (
  SELECT * FROM products WHERE category = 'Office'
)
SELECT name, price FROM office_products ORDER BY price;`,

  tests: [
    {
      name: "returns name and price columns",
      expected: '{"type":"contains","columns":["name","price"]}',
    },
    {
      name: "returns 3 Office products",
      expected: '{"type":"rowCount","value":3}',
    },
  ],
};
