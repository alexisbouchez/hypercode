import type { Lesson } from "../../types";

export const updatingAndDeleting: Lesson = {
  id: "updating-and-deleting",
  title: "Updating & Deleting Records",
  chapterId: "crud",
  content: `## Modifying Existing Data

Once data is in a table, you need ways to change and remove it. SQL provides \`UPDATE\` for modifying rows and \`DELETE\` for removing them.

### The UPDATE Statement

\`UPDATE\` changes the values of existing rows. The basic structure is:

\`\`\`sql
UPDATE products
SET price = 5.99
WHERE name = 'Notebook';
\`\`\`

This sets the \`price\` column to \`5.99\` for every row where \`name\` equals \`'Notebook'\`.

> **Warning:** An \`UPDATE\` without a \`WHERE\` clause modifies **every row** in the table. This is almost never what you want. Always double-check that your \`WHERE\` clause is present and correct before running an \`UPDATE\` in production.

### Updating Multiple Columns

You can set several columns in a single statement by separating them with commas:

\`\`\`sql
UPDATE products
SET price = 89.99, category = 'Premium Electronics'
WHERE name = 'Headphones';
\`\`\`

### Using Expressions in SET

The \`SET\` clause accepts expressions, not just literal values. You can reference the current value of the column:

\`\`\`sql
-- Increase all prices by 10%
UPDATE products
SET price = price * 1.10
WHERE category = 'Electronics';

-- Append text to a column
UPDATE products
SET name = name || ' (Discontinued)'
WHERE id = 4;
\`\`\`

### UPDATE with RETURNING

Like \`INSERT\`, PostgreSQL's \`UPDATE\` supports a \`RETURNING\` clause to output the modified rows:

\`\`\`sql
UPDATE products
SET price = 5.99
WHERE name = 'Notebook'
RETURNING id, name, price;
\`\`\`

This is useful for confirming what was actually changed, and for passing updated values to application code without a separate query.

### The DELETE Statement

\`DELETE\` removes rows from a table:

\`\`\`sql
DELETE FROM products
WHERE name = 'Backpack';
\`\`\`

> **Warning:** A \`DELETE\` without a \`WHERE\` clause removes **every row** in the table. The table structure remains, but all data is gone. Always include a \`WHERE\` clause unless you intentionally want to empty the table.

### DELETE with RETURNING

You can return the deleted rows for confirmation or logging:

\`\`\`sql
DELETE FROM products
WHERE category = 'Accessories'
RETURNING *;
\`\`\`

### TRUNCATE vs DELETE

Both can remove all rows from a table, but they work differently:

| Feature | \`DELETE FROM table\` | \`TRUNCATE table\` |
|---------|---------------------|-------------------|
| **Speed** | Slower (row-by-row) | Much faster |
| **WHERE clause** | Supported | Not supported |
| **Triggers** | Fires row-level triggers | Does not fire row-level triggers |
| **Transaction** | Fully transactional | Transactional in PostgreSQL |
| **RETURNING** | Supported | Not supported |

Use \`TRUNCATE\` when you need to clear an entire table quickly. Use \`DELETE\` when you need to remove specific rows or need trigger support.

\`\`\`sql
-- Remove all rows, fast
TRUNCATE products;

-- Remove all rows, with trigger support
DELETE FROM products;
\`\`\`

### Your Task

Update the price of the product named \`'Notebook'\` to \`5.99\`.`,

  starterCode: `-- Update the Notebook's price to 5.99
UPDATE products`,

  solution: `UPDATE products
SET price = 5.99
WHERE name = 'Notebook';`,

  tests: [
    {
      name: "updates Notebook price to 5.99",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT price FROM products WHERE name = 'Notebook';`,
      expected: '{"type":"exact","value":"5.99"}',
    },
  ],
};
