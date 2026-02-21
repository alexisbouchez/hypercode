import type { Lesson } from "../../types";

export const innerJoins: Lesson = {
  id: "inner-joins",
  title: "INNER JOIN",
  chapterId: "joins",
  content: `## Joining Tables

Data spread across multiple tables is combined using \`JOIN\`. The most common is \`INNER JOIN\`.

### Why Joins Exist

In our store database, orders don't store the customer's name — they store a \`customer_id\`. To see the order along with the customer's name, you need to join the two tables.

### INNER JOIN Syntax

\`\`\`sql
SELECT o.id, c.name, o.total
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;
\`\`\`

Breaking this down:
- \`orders o\` — the \`orders\` table, aliased as \`o\`
- \`INNER JOIN customers c\` — join with \`customers\` table, aliased as \`c\`
- \`ON o.customer_id = c.id\` — the join condition (how rows match)

An \`INNER JOIN\` returns only rows where the join condition is satisfied in **both** tables. Unmatched rows are excluded from both sides.

### Table Aliases

Table aliases shorten queries when referencing columns from multiple tables:

\`\`\`sql
-- Without aliases (verbose)
SELECT orders.id, customers.name, orders.total
FROM orders INNER JOIN customers ON orders.customer_id = customers.id;

-- With aliases (clean)
SELECT o.id, c.name, o.total
FROM orders o INNER JOIN customers c ON o.customer_id = c.id;
\`\`\`

### Joining Three Tables

Chain multiple JOINs to combine three or more tables:

\`\`\`sql
SELECT
  c.name AS customer,
  p.name AS product,
  oi.quantity,
  oi.unit_price
FROM order_items oi
INNER JOIN orders o    ON oi.order_id = o.id
INNER JOIN customers c ON o.customer_id = c.id
INNER JOIN products p  ON oi.product_id = p.id;
\`\`\`

### Filtering Joined Results

Combine \`JOIN\` with \`WHERE\` to filter:

\`\`\`sql
SELECT c.name, o.total, o.status
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id
WHERE o.status = 'completed';
\`\`\`

### Column Name Conflicts

When two tables share a column name, prefix with the table alias:

\`\`\`sql
-- Both orders and customers have an 'id' column
SELECT o.id AS order_id, c.id AS customer_id, c.name
FROM orders o INNER JOIN customers c ON o.customer_id = c.id;
\`\`\`

### JOIN vs INNER JOIN

\`JOIN\` and \`INNER JOIN\` are synonymous in MySQL. \`INNER JOIN\` is more explicit and preferred for clarity.

### Your Task

Join \`orders\` with \`customers\` to show each order's details alongside the customer's name. Return \`customer_name\` (the customer's name), \`total\`, and \`status\`.`,

  starterCode: `-- Join orders with customers
SELECT c.name AS customer_name, o.total, o.status
FROM orders o
INNER JOIN customers c ON`,

  solution: `SELECT c.name AS customer_name, o.total, o.status
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;`,

  tests: [
    {
      name: "returns 6 order rows",
      expected: '{"type":"rowCount","value":6}',
    },
    {
      name: "returns customer_name, total, and status columns",
      expected: '{"type":"contains","columns":["customer_name","total","status"]}',
    },
    {
      name: "Alice Johnson's orders are included",
      expected: '{"type":"contains","value":"Alice Johnson"}',
    },
    {
      name: "includes completed status",
      expected: '{"type":"contains","value":"completed"}',
    },
  ],
};
