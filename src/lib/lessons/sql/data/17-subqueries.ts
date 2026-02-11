import type { Lesson } from "../../types";

export const subqueries: Lesson = {
  id: "subqueries",
  title: "Subqueries",
  chapterId: "subqueries-ctes",
  content: `## What is a Subquery?

A subquery is a \`SELECT\` statement nested inside another SQL statement. The inner query runs first, and its result is used by the outer query. Subqueries can appear in the \`WHERE\` clause, the \`FROM\` clause, or even the \`SELECT\` list.

### Subquery in WHERE

The most common use of a subquery is inside a \`WHERE\` clause. For example, to find all products priced above the average:

\`\`\`sql
SELECT name, price
FROM products
WHERE price > (SELECT AVG(price) FROM products);
\`\`\`

The inner query \`(SELECT AVG(price) FROM products)\` returns a single value. The outer query then uses that value to filter rows. This is called a **scalar subquery** because it returns exactly one row and one column.

### IN with a Subquery

When a subquery returns multiple rows, use \`IN\` to check membership:

\`\`\`sql
SELECT name, email
FROM customers
WHERE id IN (SELECT customer_id FROM orders);
\`\`\`

This finds all customers who have placed at least one order. The subquery returns a list of \`customer_id\` values, and the outer query filters customers whose \`id\` appears in that list.

You can also negate it with \`NOT IN\`:

\`\`\`sql
SELECT name, email
FROM customers
WHERE id NOT IN (SELECT customer_id FROM orders);
\`\`\`

> **Warning:** Be careful with \`NOT IN\` when the subquery might return \`NULL\` values. If any value in the list is \`NULL\`, the entire \`NOT IN\` condition evaluates to unknown, and no rows are returned. Use \`NOT EXISTS\` instead in those cases.

### Subquery in FROM (Derived Table)

A subquery in the \`FROM\` clause creates a temporary result set called a **derived table**. You must give it an alias:

\`\`\`sql
SELECT sub.category, sub.avg_price
FROM (
  SELECT category, AVG(price) AS avg_price
  FROM products
  GROUP BY category
) AS sub
WHERE sub.avg_price > 30;
\`\`\`

The inner query computes the average price per category. The outer query filters those results. This is useful when you need to filter on an aggregated value without using \`HAVING\`, or when you want to join against an aggregated result.

### Scalar Subqueries in SELECT

A scalar subquery in the \`SELECT\` list adds a computed column to each row:

\`\`\`sql
SELECT
  name,
  price,
  (SELECT AVG(price) FROM products) AS avg_price,
  price - (SELECT AVG(price) FROM products) AS diff_from_avg
FROM products;
\`\`\`

Each row now includes the overall average price and how far each product's price deviates from it.

> **Tip:** Scalar subqueries in \`SELECT\` run once if they are uncorrelated (no reference to the outer query). The database engine is usually smart enough to cache the result.

### Correlated Subqueries

A correlated subquery references a column from the outer query. It runs once for each row in the outer query:

\`\`\`sql
SELECT p.name, p.price, p.category
FROM products p
WHERE p.price > (
  SELECT AVG(p2.price)
  FROM products p2
  WHERE p2.category = p.category
);
\`\`\`

This finds products that are more expensive than the average price **within their own category**. The subquery depends on \`p.category\` from the outer query, so it must be re-evaluated for each row.

Correlated subqueries are powerful but can be slow on large tables because of the repeated execution.

### EXISTS and NOT EXISTS

\`EXISTS\` tests whether a subquery returns any rows at all. It returns \`TRUE\` as soon as the subquery finds at least one row:

\`\`\`sql
SELECT name, email
FROM customers c
WHERE EXISTS (
  SELECT 1
  FROM orders o
  WHERE o.customer_id = c.id
);
\`\`\`

This returns customers who have at least one order. \`EXISTS\` is often more efficient than \`IN\` because it short-circuits: it stops scanning as soon as it finds a match.

\`NOT EXISTS\` returns the opposite:

\`\`\`sql
SELECT name, email
FROM customers c
WHERE NOT EXISTS (
  SELECT 1
  FROM orders o
  WHERE o.customer_id = c.id
);
\`\`\`

This finds customers with no orders.

> **Tip:** Prefer \`EXISTS\` / \`NOT EXISTS\` over \`IN\` / \`NOT IN\` when dealing with nullable columns or large result sets. \`EXISTS\` handles \`NULL\` values correctly and often performs better.

### Your Task

Find all products that cost more than the average price.`,

  starterCode: `-- Find products priced above the average
SELECT name, price
FROM products
WHERE`,

  solution: `SELECT name, price
FROM products
WHERE price > (SELECT AVG(price) FROM products);`,

  tests: [
    {
      name: "returns products above average price",
      expected: '{"type":"custom"}',
    },
  ],
};
