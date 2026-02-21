import type { Lesson } from "../../types";

export const outerJoins: Lesson = {
  id: "outer-joins",
  title: "LEFT JOIN and RIGHT JOIN",
  chapterId: "joins",
  content: `## Outer Joins

\`INNER JOIN\` only returns rows that match in both tables. **Outer joins** include unmatched rows from one or both sides.

### LEFT JOIN

\`LEFT JOIN\` returns all rows from the **left** table, plus matched rows from the right table. When there is no match on the right side, the right columns are \`NULL\`.

\`\`\`sql
-- All customers, even those with no orders
SELECT c.name, o.id AS order_id, o.total
FROM customers c
LEFT JOIN orders o ON o.customer_id = c.id;
\`\`\`

In our database, Eve Martinez (id=5) has no orders. With \`LEFT JOIN\`, she appears in the result with \`NULL\` for the order columns.

### RIGHT JOIN

\`RIGHT JOIN\` is the mirror of \`LEFT JOIN\` — all rows from the **right** table are included:

\`\`\`sql
SELECT c.name, o.id AS order_id
FROM customers c
RIGHT JOIN orders o ON o.customer_id = c.id;
\`\`\`

\`RIGHT JOIN\` is less common because you can always rewrite it as a \`LEFT JOIN\` by swapping the tables.

### Finding Unmatched Rows

A classic use of \`LEFT JOIN\` is finding rows with no match in the other table:

\`\`\`sql
-- Customers who have never placed an order
SELECT c.name
FROM customers c
LEFT JOIN orders o ON o.customer_id = c.id
WHERE o.id IS NULL;
\`\`\`

When \`o.id\` is \`NULL\`, it means no matching order exists for that customer.

### LEFT JOIN with Aggregation

Combine \`LEFT JOIN\` with \`GROUP BY\` and \`COUNT\` to count matches per row:

\`\`\`sql
SELECT
  c.name AS customer_name,
  COUNT(o.id) AS order_count,
  COALESCE(SUM(o.total), 0) AS total_spent
FROM customers c
LEFT JOIN orders o ON o.customer_id = c.id
GROUP BY c.id, c.name;
\`\`\`

Note: \`COUNT(o.id)\` counts non-NULL order ids — it returns 0 for customers with no orders. \`COUNT(*)\` would return 1 for unmatched customers (it counts the NULL row).

### FULL OUTER JOIN

MySQL does **not** support \`FULL OUTER JOIN\` natively. Simulate it with a \`UNION\`:

\`\`\`sql
SELECT c.name, o.id
FROM customers c LEFT JOIN orders o ON o.customer_id = c.id
UNION
SELECT c.name, o.id
FROM customers c RIGHT JOIN orders o ON o.customer_id = c.id;
\`\`\`

### Your Task

For each customer, count how many orders they have placed. Use a \`LEFT JOIN\` so that customers with zero orders are also included. Return \`customer_name\` and \`order_count\`.`,

  starterCode: `-- Count orders per customer, including those with zero orders
SELECT c.name AS customer_name, COUNT(o.id) AS order_count
FROM customers c
LEFT JOIN orders o ON o.customer_id = c.id
GROUP BY`,

  solution: `SELECT c.name AS customer_name, COUNT(o.id) AS order_count
FROM customers c
LEFT JOIN orders o ON o.customer_id = c.id
GROUP BY c.id, c.name;`,

  tests: [
    {
      name: "returns all 5 customers",
      expected: '{"type":"rowCount","value":5}',
    },
    {
      name: "returns customer_name and order_count columns",
      expected: '{"type":"contains","columns":["customer_name","order_count"]}',
    },
    {
      name: "Eve Martinez is included",
      expected: '{"type":"contains","value":"Eve Martinez"}',
    },
    {
      name: "includes 0 order count for customer with no orders",
      expected: '{"type":"contains","value":"0"}',
    },
  ],
};
