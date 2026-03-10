import type { Lesson } from "../../types";

export const transactions: Lesson = {
  id: "transactions",
  title: "Transactions",
  chapterId: "advanced-sql",
  content: `## Transactions

Think of a transaction like the Enterprise's **transporter beam** — either the entire away team materializes on the surface, or nobody does. You'd never want half the crew stuck in the pattern buffer! A **transaction** is a sequence of SQL statements that execute as a single unit of work. Either every statement succeeds, or none of them take effect. This all-or-nothing guarantee is fundamental to data integrity.

### ACID Properties

Transactions provide four guarantees known as **ACID**:

- **Atomicity** — All statements succeed or all are rolled back. No partial updates.
- **Consistency** — The database moves from one valid state to another. Constraints are never violated.
- **Isolation** — Concurrent transactions do not interfere with each other.
- **Durability** — Once committed, the data survives crashes and power failures.

### BEGIN, COMMIT, ROLLBACK

By default, every SQL statement runs in its own implicit transaction. To group multiple statements into a single transaction, wrap them with \`BEGIN\` and \`COMMIT\`:

\`\`\`sql
BEGIN;

UPDATE products SET price = price - 10 WHERE name = 'Laptop';
UPDATE products SET price = price + 10 WHERE name = 'Notebook';

COMMIT;
\`\`\`

If something goes wrong and you want to discard all changes made since \`BEGIN\`, use \`ROLLBACK\`:

\`\`\`sql
BEGIN;

UPDATE products SET price = -50 WHERE name = 'Laptop';
-- Oops, negative price! Undo everything.
ROLLBACK;
\`\`\`

After a \`ROLLBACK\`, the database is exactly as it was before \`BEGIN\`.

### SAVEPOINT

Sometimes you want to undo part of a transaction without discarding the whole thing. A \`SAVEPOINT\` marks a point you can roll back to:

\`\`\`sql
BEGIN;

INSERT INTO products (name, price, category)
VALUES ('Monitor', 299.99, 'Electronics');

SAVEPOINT before_discount;

UPDATE products SET price = 0 WHERE name = 'Monitor';
-- That was too aggressive. Roll back to the savepoint.
ROLLBACK TO SAVEPOINT before_discount;

-- The INSERT still stands; only the UPDATE was undone.
COMMIT;
\`\`\`

After \`ROLLBACK TO SAVEPOINT\`, you remain inside the transaction and can continue issuing statements.

### Transaction Isolation Levels

PostgreSQL supports four isolation levels that control what a transaction can see from other concurrent transactions:

| Level | Dirty Reads | Non-repeatable Reads | Phantom Reads |
|-------|------------|---------------------|---------------|
| READ UNCOMMITTED | Prevented* | Possible | Possible |
| READ COMMITTED (default) | Prevented | Possible | Possible |
| REPEATABLE READ | Prevented | Prevented | Prevented* |
| SERIALIZABLE | Prevented | Prevented | Prevented |

*PostgreSQL actually prevents dirty reads at all levels and prevents phantom reads at REPEATABLE READ.

You set the isolation level at the start of a transaction:

\`\`\`sql
BEGIN TRANSACTION ISOLATION LEVEL SERIALIZABLE;
-- statements here see a frozen snapshot of the database
COMMIT;
\`\`\`

### When to Use Transactions

Just as Starfleet operations require coordinated precision, transactions are essential when multiple steps must succeed or fail together:

- **Resource transfers**: Move dilithium reserves between starbases atomically.
- **Multi-table inserts**: Register a new crew member and their quarters assignment together.
- **Bulk updates with validation**: Update many records, check the result, roll back if the readings look wrong.

### Your Task

Write a transaction that transfers \`$100\` from the \`'Laptop'\` product price to the \`'Notebook'\` product price. Specifically:

1. Begin a transaction
2. Decrease the \`'Laptop'\` price by \`100\`
3. Increase the \`'Notebook'\` price by \`100\`
4. Commit the transaction

The original prices are: Laptop = 999.99, Notebook = 4.99.
After your transaction: Laptop = 899.99, Notebook = 104.99.`,

  starterCode: `-- Transfer $100 from Laptop price to Notebook price
BEGIN;

-- Decrease Laptop price by 100


-- Increase Notebook price by 100


COMMIT;`,

  solution: `BEGIN;

UPDATE products SET price = price - 100 WHERE name = 'Laptop';
UPDATE products SET price = price + 100 WHERE name = 'Notebook';

COMMIT;`,

  tests: [
    {
      name: "Laptop price decreased to 899.99",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT price FROM products WHERE name = 'Laptop';`,
      expected: '{"type":"exact","value":"899.99"}',
    },
    {
      name: "Notebook price increased to 104.99",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT price FROM products WHERE name = 'Notebook';`,
      expected: '{"type":"exact","value":"104.99"}',
    },
    {
      name: "Total price across both products is unchanged",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT SUM(price) AS total FROM products WHERE name IN ('Laptop', 'Notebook');`,
      expected: '{"type":"exact","value":"1004.98"}',
    },
  ],
};
