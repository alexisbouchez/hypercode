import type { Lesson } from "../../types";

export const innerJoins: Lesson = {
  id: "inner-joins",
  title: "Inner Joins",
  chapterId: "joins",
  content: `## Combining Data from Multiple Tables

Real databases spread data across multiple related tables. A **join** combines rows from two or more tables based on a related column between them.

### The INNER JOIN

An \`INNER JOIN\` returns only the rows where there is a match in **both** tables. If a row in one table has no corresponding row in the other, it is excluded from the result.

\`\`\`sql
SELECT orders.id, customers.name, orders.total
FROM orders
INNER JOIN customers ON orders.customer_id = customers.id;
\`\`\`

This query pairs each order with its customer. Only orders that have a valid \`customer_id\` matching a \`customers.id\` appear in the result. Customers with no orders and orders with no matching customer are both excluded.

### How It Works

Think of an inner join as a two-step process:

1. **Cartesian product** --- every row in the first table is paired with every row in the second table
2. **Filter** --- only pairs where the \`ON\` condition is true survive

The database engine optimizes this internally (it does not actually build the full cartesian product), but the logical model helps you reason about the results.

### Table Aliases

When queries involve multiple tables, column names can become ambiguous. Table aliases shorten the syntax and improve readability:

\`\`\`sql
SELECT o.id, c.name, o.total
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;
\`\`\`

Here, \`o\` is an alias for \`orders\` and \`c\` is an alias for \`customers\`. Once you define an alias, you must use it throughout the query; the original table name is no longer valid in that context.

> **Tip:** Single-letter aliases like \`o\` and \`c\` work well for simple queries. For complex queries with many tables, use more descriptive aliases like \`ord\` and \`cust\` to keep things readable.

### Joining More Than Two Tables

You can chain multiple joins to combine three or more tables:

\`\`\`sql
SELECT o.id, c.name, p.name AS product_name, o.quantity, o.total
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id
INNER JOIN products p ON o.product_id = p.id;
\`\`\`

This produces a result where each row shows the order, the customer who placed it, and the product that was ordered. All three tables must have matching rows for a row to appear.

### Join Conditions

The \`ON\` clause typically compares a foreign key to a primary key, but it can use any boolean expression:

\`\`\`sql
-- Join on multiple columns
SELECT *
FROM table_a a
INNER JOIN table_b b ON a.col1 = b.col1 AND a.col2 = b.col2;

-- Join with an inequality
SELECT p1.name, p2.name
FROM products p1
INNER JOIN products p2 ON p1.category = p2.category AND p1.id < p2.id;
\`\`\`

### INNER JOIN vs JOIN

The \`INNER\` keyword is optional. Writing \`JOIN\` alone is equivalent to \`INNER JOIN\`:

\`\`\`sql
-- These are identical
SELECT * FROM orders INNER JOIN customers ON orders.customer_id = customers.id;
SELECT * FROM orders JOIN customers ON orders.customer_id = customers.id;
\`\`\`

Most developers include \`INNER\` for clarity, especially in queries that also use outer joins.

### Your Task

Select all orders along with the customer name. Join the \`orders\` table with the \`customers\` table using \`customer_id\`. Your result should include at least the \`name\` and \`total\` columns.`,

  starterCode: `-- Join orders with customers to show customer names and order totals
SELECT`,

  solution: `SELECT c.name, o.total
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;`,

  tests: [
    {
      name: "returns name and total columns",
      expected: '{"type":"contains","columns":["name","total"]}',
    },
    {
      name: "returns all 3 orders",
      expected: '{"type":"rowCount","value":3}',
    },
  ],
};
