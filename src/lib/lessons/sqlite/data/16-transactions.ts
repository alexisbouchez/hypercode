import type { Lesson } from "../../types";

export const transactions: Lesson = {
  id: "transactions",
  title: "Transactions & Savepoints",
  chapterId: "sqlite-features",
  content: `## Transactions

A **transaction** groups multiple SQL statements into a single atomic unit. Either all statements succeed, or none of them take effect.

### BEGIN / COMMIT

Wrap statements in \`BEGIN\` and \`COMMIT\` to execute them as a transaction:

\`\`\`sql
BEGIN;
INSERT INTO products (name, price, category) VALUES ('Monitor', 299.99, 'Electronics');
UPDATE products SET price = price * 1.1 WHERE category = 'Electronics';
COMMIT;
\`\`\`

If every statement succeeds, \`COMMIT\` makes all changes permanent.

### ROLLBACK

If something goes wrong, \`ROLLBACK\` undoes all changes since the last \`BEGIN\`:

\`\`\`sql
BEGIN;
DELETE FROM products WHERE category = 'Office';
-- Oops, that was a mistake!
ROLLBACK;
-- The Office products are still there
\`\`\`

### Savepoints

A **savepoint** is a named marker within a transaction. You can roll back to a savepoint without aborting the entire transaction:

\`\`\`sql
BEGIN;
INSERT INTO products (name, price, category) VALUES ('Tablet', 399.99, 'Electronics');

SAVEPOINT before_delete;
DELETE FROM products WHERE name = 'Pen Set';

-- Undo only the delete, keep the insert
ROLLBACK TO before_delete;

COMMIT;
\`\`\`

\`RELEASE\` removes a savepoint (confirming changes up to that point within the transaction):

\`\`\`sql
BEGIN;
SAVEPOINT sp1;
INSERT INTO products (name, price, category) VALUES ('Mouse', 29.99, 'Electronics');
RELEASE sp1;  -- savepoint confirmed, changes kept
COMMIT;
\`\`\`

### PRAGMA journal_mode

SQLite uses a journal to implement transactions. You can check or change the journal mode with:

\`\`\`sql
PRAGMA journal_mode;        -- returns current mode (default: 'delete')
PRAGMA journal_mode = WAL;  -- switch to Write-Ahead Logging
\`\`\`

WAL mode allows concurrent readers and a single writer, which often improves performance for multi-threaded applications.

### Your Task

Write a transaction that:
1. Inserts a new product \`'Smartwatch'\` with price \`199.99\` and category \`'Electronics'\`
2. Creates a savepoint called \`before_update\`
3. Updates **all** Electronics products to have their price increased by 10%
4. Rolls back to \`before_update\` (undoing the price update)
5. Commits the transaction

Then select the \`name\` and \`price\` from products where the name is \`'Smartwatch'\`.`,

  starterCode: `BEGIN;
INSERT INTO products (name, price, category) VALUES ('Smartwatch', 199.99, 'Electronics');

SAVEPOINT before_update;
UPDATE products SET price = price * 1.1 WHERE category = 'Electronics';

-- Undo the price update

-- Finish the transaction

-- Verify the Smartwatch was inserted with its original price
SELECT name, price FROM products WHERE name = 'Smartwatch';`,

  solution: `BEGIN;
INSERT INTO products (name, price, category) VALUES ('Smartwatch', 199.99, 'Electronics');

SAVEPOINT before_update;
UPDATE products SET price = price * 1.1 WHERE category = 'Electronics';

ROLLBACK TO before_update;

COMMIT;

SELECT name, price FROM products WHERE name = 'Smartwatch';`,

  tests: [
    {
      name: "Smartwatch exists with correct price",
      expected: '{"type":"exact","value":"199.99"}',
      code: `{{USER_SQL}}
---VALIDATE---
SELECT price FROM products WHERE name = 'Smartwatch';`,
    },
    {
      name: "returns name and price columns",
      expected: '{"type":"contains","columns":["name","price"]}',
    },
    {
      name: "Smartwatch is the only new product (1 row returned)",
      expected: '{"type":"rowCount","value":1}',
    },
  ],
};
