import type { Lesson } from "../../types";

export const updatingData: Lesson = {
  id: "updating-data",
  title: "Updating Data",
  chapterId: "crud",
  content: `## UPDATE

The \`UPDATE\` statement modifies existing rows in a table:

\`\`\`sql
UPDATE products SET price = 89.99 WHERE name = 'Headphones';
\`\`\`

**Always include a \`WHERE\` clause** unless you intentionally want to update every row. Without \`WHERE\`, \`UPDATE\` modifies all rows in the table.

### Updating Multiple Columns

Separate multiple column assignments with commas:

\`\`\`sql
UPDATE products
SET price = 89.99, category = 'Audio'
WHERE name = 'Headphones';
\`\`\`

### Using Expressions

You can use expressions in \`SET\`:

\`\`\`sql
-- Apply a 10% discount to all Electronics
UPDATE products
SET price = price * 0.9
WHERE category = 'Electronics';

-- Increase all Office product prices by $5
UPDATE products
SET price = price + 5
WHERE category = 'Office';
\`\`\`

### Updating by Primary Key

The most precise and safe way to update a single row:

\`\`\`sql
UPDATE users SET email = 'newalice@example.com' WHERE id = 1;
\`\`\`

### UPDATE with Subquery

You can use a subquery in the \`WHERE\` clause:

\`\`\`sql
-- Update orders for a specific customer by name
UPDATE orders
SET quantity = 3
WHERE customer_id = (SELECT id FROM customers WHERE name = 'Alice');
\`\`\`

### Checking How Many Rows Were Updated

SQLite does not return affected row counts directly in SQL. In most drivers, you can call \`changes()\`:

\`\`\`sql
UPDATE products SET price = 99.99 WHERE category = 'Office';
SELECT changes(); -- number of rows affected by the last statement
\`\`\`

### Your Task

Update the \`price\` of the \`'Coffee Maker'\` product to \`54.99\`. Then select all Kitchen products to verify.`,

  starterCode: `-- Update the Coffee Maker price
UPDATE products SET price =
WHERE name = 'Coffee Maker';

-- Verify
SELECT * FROM products WHERE category = 'Kitchen';`,

  solution: `UPDATE products SET price = 54.99
WHERE name = 'Coffee Maker';

SELECT * FROM products WHERE category = 'Kitchen';`,

  tests: [
    {
      name: "returns Kitchen products",
      expected: '{"type":"rowCount","value":2}',
    },
    {
      name: "Coffee Maker price updated to 54.99",
      expected: '{"type":"contains","value":"54.99"}',
    },
  ],
};
