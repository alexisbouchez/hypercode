import type { Lesson } from "../../types";

export const subqueriesAndCtes: Lesson = {
  id: "subqueries-and-ctes",
  title: "Subqueries and CTEs",
  chapterId: "advanced",
  content: `## Subqueries

A subquery is a SELECT statement nested inside another SQL statement.

### Subquery in WHERE

\`\`\`sql
-- Find products more expensive than the average price
SELECT name, price
FROM products
WHERE price > (SELECT AVG(price) FROM products);
\`\`\`

The inner query runs first and returns a single value (the average price), which the outer query then uses.

### IN with Subquery

\`\`\`sql
-- Find customers who have placed at least one order
SELECT name FROM customers
WHERE id IN (SELECT DISTINCT customer_id FROM orders);

-- Find products that have never been ordered
SELECT name FROM products
WHERE id NOT IN (SELECT DISTINCT product_id FROM order_items);
\`\`\`

### EXISTS

\`EXISTS\` returns \`TRUE\` if the subquery returns any rows:

\`\`\`sql
-- Customers with at least one completed order
SELECT c.name
FROM customers c
WHERE EXISTS (
  SELECT 1 FROM orders o
  WHERE o.customer_id = c.id AND o.status = 'completed'
);
\`\`\`

\`EXISTS\` is often faster than \`IN\` for large datasets because it stops scanning as soon as one match is found.

### Subquery in FROM (Derived Table)

\`\`\`sql
-- Average of per-category averages
SELECT AVG(cat_avg) AS overall_avg
FROM (
  SELECT category, AVG(price) AS cat_avg
  FROM products
  GROUP BY category
) AS category_stats;
\`\`\`

Subqueries in \`FROM\` must be aliased (\`AS category_stats\`).

### Common Table Expressions (CTEs)

A CTE defines a named, reusable subquery at the top of a statement using the \`WITH\` clause:

\`\`\`sql
WITH category_stats AS (
  SELECT category, COUNT(*) AS cnt, AVG(price) AS avg_price
  FROM products
  GROUP BY category
)
SELECT * FROM category_stats WHERE cnt >= 2;
\`\`\`

### Multiple CTEs

Chain multiple CTEs separated by commas:

\`\`\`sql
WITH
  order_totals AS (
    SELECT customer_id, SUM(total) AS total_spent
    FROM orders
    GROUP BY customer_id
  ),
  high_spenders AS (
    SELECT customer_id FROM order_totals WHERE total_spent > 500
  )
SELECT c.name, ot.total_spent
FROM customers c
JOIN order_totals ot ON c.id = ot.customer_id
JOIN high_spenders hs ON c.id = hs.customer_id;
\`\`\`

### When to Use CTEs vs Subqueries

| Use | Prefer |
|-----|--------|
| One-time use, simple | Subquery |
| Reused multiple times in same query | CTE |
| Complex, multi-step logic | CTE (more readable) |
| Recursive queries | CTE (required) |

### Recursive CTEs

MySQL 8.0+ supports recursive CTEs for hierarchical data:

\`\`\`sql
WITH RECURSIVE numbers AS (
  SELECT 1 AS n
  UNION ALL
  SELECT n + 1 FROM numbers WHERE n < 10
)
SELECT * FROM numbers;
\`\`\`

### Your Task

Use a CTE named \`customer_totals\` to compute each customer's total spending (\`total_spent\`) from the \`orders\` table. Then join with \`customers\` to return customers whose total spending is **over $100**, showing their \`name\` and \`total_spent\`, ordered by \`total_spent\` descending.`,

  starterCode: `-- Use a CTE to find high-spending customers
WITH customer_totals AS (
  SELECT customer_id, SUM(total) AS total_spent
  FROM orders
  GROUP BY customer_id
  HAVING SUM(total) > 100
)
SELECT c.name, ct.total_spent
FROM customers c
INNER JOIN customer_totals ct ON c.id = ct.customer_id
ORDER BY ct.total_spent DESC;`,

  solution: `WITH customer_totals AS (
  SELECT customer_id, SUM(total) AS total_spent
  FROM orders
  GROUP BY customer_id
  HAVING SUM(total) > 100
)
SELECT c.name, ct.total_spent
FROM customers c
INNER JOIN customer_totals ct ON c.id = ct.customer_id
ORDER BY ct.total_spent DESC;`,

  tests: [
    {
      name: "returns 3 high-spending customers",
      expected: '{"type":"rowCount","value":3}',
    },
    {
      name: "returns name and total_spent columns",
      expected: '{"type":"contains","columns":["name","total_spent"]}',
    },
    {
      name: "Alice Johnson is included (top spender)",
      expected: '{"type":"contains","value":"Alice Johnson"}',
    },
    {
      name: "Alice Johnson is first (highest total spend)",
      expected: '{"type":"exact","value":"Alice Johnson"}',
    },
  ],
};
