import type { Lesson } from "../../types";

export const deletingData: Lesson = {
  id: "deleting-data",
  title: "Deleting Data",
  chapterId: "crud",
  content: `## DELETE FROM

The \`DELETE FROM\` statement removes rows from a table:

\`\`\`sql
DELETE FROM products WHERE id = 4;
DELETE FROM orders WHERE total < 10;
\`\`\`

**Always include a \`WHERE\` clause** unless you want to delete every row. Without it, \`DELETE FROM\` empties the entire table.

### Deleting All Rows

To clear a table while keeping its structure:

\`\`\`sql
DELETE FROM logs;           -- removes all rows, slow on large tables
-- vs --
DELETE FROM logs WHERE 1=1; -- equivalent but explicit
\`\`\`

For large tables, \`DELETE FROM\` without \`WHERE\` is slow because SQLite still processes row by row to support rollbacks. Use \`DROP TABLE\` and recreate if you want speed.

### DELETE with Subquery

Use a subquery in \`WHERE\` to delete rows based on data from another table:

\`\`\`sql
-- Delete all orders placed by 'Charlie'
DELETE FROM orders
WHERE customer_id = (SELECT id FROM customers WHERE name = 'Charlie');
\`\`\`

### DELETE with IN

Delete rows matching a set of values from a subquery:

\`\`\`sql
-- Delete all orders for Electronics products
DELETE FROM orders
WHERE product_id IN (SELECT id FROM products WHERE category = 'Electronics');
\`\`\`

### Cascade Behavior

By default (without \`PRAGMA foreign_keys = ON\`), SQLite does not enforce foreign keys. With foreign keys enabled, deleting a referenced row raises an error unless you:
- Define \`ON DELETE CASCADE\` on the foreign key
- Define \`ON DELETE SET NULL\`

\`\`\`sql
CREATE TABLE orders (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  customer_id INTEGER REFERENCES customers(id) ON DELETE CASCADE
);
\`\`\`

### TRUNCATE vs DELETE

SQLite does not have \`TRUNCATE\`. Use \`DELETE FROM table_name\` to clear all rows, or \`DROP TABLE\` + recreate to reset including auto-increment counters.

### Your Task

Delete all products in the \`'Office'\` category. Then select all remaining products to verify.`,

  starterCode: `-- Delete all Office products
DELETE FROM products WHERE category = '';

-- Verify
SELECT * FROM products;`,

  solution: `DELETE FROM products WHERE category = 'Office';

SELECT * FROM products;`,

  tests: [
    {
      name: "5 products remain after deleting Office category",
      expected: '{"type":"rowCount","value":5}',
    },
  ],
};
