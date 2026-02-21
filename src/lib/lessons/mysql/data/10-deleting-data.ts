import type { Lesson } from "../../types";

export const deletingData: Lesson = {
  id: "deleting-data",
  title: "Deleting Data",
  chapterId: "schema",
  content: `## DELETE

The \`DELETE\` statement removes rows from a table.

### Basic Syntax

\`\`\`sql
DELETE FROM products WHERE id = 10;
\`\`\`

Always include a \`WHERE\` clause. \`DELETE FROM products;\` without a \`WHERE\` deletes **all rows** from the table (the table structure remains).

### Deleting Multiple Rows

Any condition works in the \`WHERE\` clause:

\`\`\`sql
-- Delete all cancelled orders
DELETE FROM orders WHERE status = 'cancelled';

-- Delete cheap products
DELETE FROM products WHERE price < 5;

-- Delete products that have never been ordered
DELETE FROM products
WHERE id NOT IN (SELECT DISTINCT product_id FROM order_items);
\`\`\`

### Referential Integrity

When tables have foreign key relationships, you must delete child rows before parent rows. In our database, \`order_items\` references \`orders\`, so:

\`\`\`sql
-- CORRECT order
DELETE FROM order_items WHERE order_id = 5;
DELETE FROM orders WHERE id = 5;

-- WRONG — will fail with foreign key violation
DELETE FROM orders WHERE id = 5;
-- ERROR: Cannot delete a parent row (a foreign key constraint fails)
\`\`\`

MySQL's \`ON DELETE CASCADE\` can automate this if defined on the foreign key:

\`\`\`sql
CREATE TABLE order_items (
  ...
  order_id INT,
  FOREIGN KEY (order_id) REFERENCES orders(id) ON DELETE CASCADE
);
-- Now deleting from orders auto-deletes matching order_items
\`\`\`

### TRUNCATE — Fast Delete All

\`TRUNCATE TABLE\` deletes all rows faster than \`DELETE\` without a \`WHERE\`:

\`\`\`sql
TRUNCATE TABLE order_items;
\`\`\`

Key differences from \`DELETE\`:
- \`TRUNCATE\` resets \`AUTO_INCREMENT\` counter to 1
- \`TRUNCATE\` cannot be rolled back in MySQL (it's DDL, not DML)
- \`TRUNCATE\` does not fire \`DELETE\` triggers
- \`DELETE\` is logged row-by-row; \`TRUNCATE\` deallocates pages

### Soft Deletes

A common pattern is to never actually delete data, instead marking it as deleted:

\`\`\`sql
ALTER TABLE products ADD COLUMN deleted_at DATETIME DEFAULT NULL;

-- "Delete" a product
UPDATE products SET deleted_at = NOW() WHERE id = 10;

-- Query only active products
SELECT * FROM products WHERE deleted_at IS NULL;
\`\`\`

This preserves history and allows recovery, at the cost of filtering overhead.

### Your Task

The order with \`id = 5\` has status \`'cancelled'\`. Delete all its items from \`order_items\` first, then delete the order itself from \`orders\`.`,

  starterCode: `-- Delete cancelled order (id=5) and its items
DELETE FROM order_items WHERE order_id = 5;
DELETE FROM orders WHERE id = 5;`,

  solution: `DELETE FROM order_items WHERE order_id = 5;
DELETE FROM orders WHERE id = 5;`,

  tests: [
    {
      name: "order_items has 7 rows after deletion",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT COUNT(*) FROM order_items`,
      expected: '{"type":"exact","value":"7"}',
    },
    {
      name: "orders has 5 rows after deletion",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT COUNT(*) FROM orders`,
      expected: '{"type":"exact","value":"5"}',
    },
    {
      name: "no cancelled orders remain",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT COUNT(*) FROM orders WHERE status='cancelled'`,
      expected: '{"type":"exact","value":"0"}',
    },
    {
      name: "order 5 items are all gone",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT COUNT(*) FROM order_items WHERE order_id=5`,
      expected: '{"type":"exact","value":"0"}',
    },
  ],
};
