import type { Lesson } from "../../types";

export const joins: Lesson = {
  id: "joins",
  title: "Joins",
  chapterId: "querying",
  content: `## Combining Tables with Joins

Tables in a relational database are connected through foreign keys. A \`JOIN\` lets you combine rows from two or more tables based on a related column.

### INNER JOIN

An \`INNER JOIN\` returns only rows that have matching values in both tables:

\`\`\`sql
SELECT o.id, c.name, o.total
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;
\`\`\`

- \`orders o\` gives the \`orders\` table an alias \`o\`
- \`customers c\` gives the \`customers\` table an alias \`c\`
- \`ON o.customer_id = c.id\` is the join condition

Rows from \`orders\` that have no matching \`customers\` row are excluded. Rows from \`customers\` with no orders are also excluded.

### Joining Three Tables

\`\`\`sql
SELECT
  o.id AS order_id,
  c.name AS customer_name,
  p.name AS product_name,
  o.quantity,
  o.total
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id
INNER JOIN products p ON o.product_id = p.id;
\`\`\`

### LEFT JOIN (LEFT OUTER JOIN)

A \`LEFT JOIN\` returns all rows from the left table, and matching rows from the right table. When there is no match, the right-side columns are \`NULL\`:

\`\`\`sql
-- All customers, even those with no orders
SELECT c.name, o.total
FROM customers c
LEFT JOIN orders o ON o.customer_id = c.id;
\`\`\`

Charlie has no orders, so \`o.total\` will be \`NULL\` for Charlie's row.

### Filtering Unmatched Rows

Find all customers who have never placed an order:

\`\`\`sql
SELECT c.name
FROM customers c
LEFT JOIN orders o ON o.customer_id = c.id
WHERE o.id IS NULL;
\`\`\`

When \`o.id IS NULL\`, it means no matching order exists.

### RIGHT JOIN and FULL JOIN

SQLite does **not** support \`RIGHT JOIN\` or \`FULL OUTER JOIN\`. You can achieve the same results by swapping the table order (for \`RIGHT JOIN\`) or using \`UNION\` (for \`FULL OUTER JOIN\`).

### Your Task

Join the \`orders\` table with the \`customers\` table and the \`products\` table. Return columns: \`customer_name\` (customer's name), \`product_name\` (product's name), and \`total\` (order total).`,

  starterCode: `-- Join orders with customers and products
SELECT
  c.name AS customer_name,
  p.name AS product_name,
  o.total
FROM orders o
INNER JOIN customers c ON
INNER JOIN products p ON`,

  solution: `SELECT
  c.name AS customer_name,
  p.name AS product_name,
  o.total
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id
INNER JOIN products p ON o.product_id = p.id;`,

  tests: [
    {
      name: "returns customer_name, product_name, and total columns",
      expected: '{"type":"contains","columns":["customer_name","product_name","total"]}',
    },
    {
      name: "returns 3 order rows",
      expected: '{"type":"rowCount","value":3}',
    },
  ],
};
