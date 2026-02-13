import type { Lesson } from "../../types";

export const outerJoins: Lesson = {
  id: "outer-joins",
  title: "Outer Joins",
  chapterId: "joins",
  content: `## Keeping Unmatched Rows

An \`INNER JOIN\` discards rows that have no match. But sometimes you need to see **all** rows from one or both tables, even when there is no corresponding row in the other table. That is what outer joins do.

### LEFT JOIN

A \`LEFT JOIN\` (or \`LEFT OUTER JOIN\`) returns all rows from the left table, plus matching rows from the right table. Where there is no match, the right-side columns are filled with \`NULL\`:

\`\`\`sql
SELECT c.name, o.total
FROM customers c
LEFT JOIN orders o ON c.id = o.customer_id;
\`\`\`

Result with the sandbox data:

| name    | total  |
|---------|--------|
| Alice   | 999.99 |
| Alice   | 159.98 |
| Bob     | 49.99  |
| Charlie | NULL   |

Charlie has no orders, but still appears in the result. The \`total\` column is \`NULL\` because there is no matching row in the \`orders\` table.

> First Contact protocol: a \`LEFT JOIN\` includes everyone from your ship, even if they have no match on the alien vessel. No crew member gets left behind.

> **Tip:** The "left" table is the one that appears first --- before the \`LEFT JOIN\` keywords. The "right" table is the one that appears after. The naming refers to their position in the query, not any inherent property of the tables.

### RIGHT JOIN

A \`RIGHT JOIN\` is the mirror image of a \`LEFT JOIN\`. It returns all rows from the right table, plus matching rows from the left:

\`\`\`sql
SELECT c.name, o.total
FROM customers c
RIGHT JOIN orders o ON c.id = o.customer_id;
\`\`\`

In practice, \`RIGHT JOIN\` is rarely used. You can always rewrite it as a \`LEFT JOIN\` by swapping the table order, which most developers find more readable.

### FULL OUTER JOIN

A \`FULL OUTER JOIN\` combines both sides: it returns all rows from both tables, using \`NULL\` where there is no match on either side:

\`\`\`sql
SELECT c.name, o.id AS order_id, o.total
FROM customers c
FULL OUTER JOIN orders o ON c.id = o.customer_id;
\`\`\`

This is useful when you need a complete picture of both tables. Rows that exist only in the left table get \`NULL\` for the right columns, and vice versa.

### Finding Missing Matches with IS NULL

A common pattern is using an outer join with a \`WHERE ... IS NULL\` filter to find rows that have **no match** in the other table:

\`\`\`sql
-- Find customers who have never placed an order
SELECT c.name
FROM customers c
LEFT JOIN orders o ON c.id = o.customer_id
WHERE o.id IS NULL;
\`\`\`

This works because unmatched rows have \`NULL\` in all columns from the right table. Filtering on \`IS NULL\` for a column that would normally never be null (like a primary key) isolates exactly the unmatched rows.

Other practical examples:

\`\`\`sql
-- Find products that have never been ordered
SELECT p.name
FROM products p
LEFT JOIN orders o ON p.id = o.product_id
WHERE o.id IS NULL;

-- Find orphaned orders (customer was deleted)
SELECT o.id, o.total
FROM orders o
LEFT JOIN customers c ON o.customer_id = c.id
WHERE c.id IS NULL;
\`\`\`

### LEFT JOIN vs LEFT OUTER JOIN

Like \`INNER\`, the \`OUTER\` keyword is optional:

\`\`\`sql
-- These are identical
SELECT * FROM customers c LEFT OUTER JOIN orders o ON c.id = o.customer_id;
SELECT * FROM customers c LEFT JOIN orders o ON c.id = o.customer_id;
\`\`\`

### Your Task

List all customers and their order totals. Include customers who have never placed an order. Your result should have 4 rows (Alice appears twice because she has two orders, and Charlie shows a \`NULL\` total).`,

  starterCode: `-- List all customers with their order totals (include those with no orders)
SELECT`,

  solution: `SELECT c.name, o.total
FROM customers c
LEFT JOIN orders o ON c.id = o.customer_id;`,

  tests: [
    {
      name: "returns name and total columns",
      expected: '{"type":"contains","columns":["name","total"]}',
    },
    {
      name: "returns 4 rows (all customers including Charlie with NULL)",
      expected: '{"type":"rowCount","value":4}',
    },
  ],
};
