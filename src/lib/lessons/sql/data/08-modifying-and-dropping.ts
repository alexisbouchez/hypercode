import type { Lesson } from "../../types";

export const modifyingAndDropping: Lesson = {
  id: "modifying-and-dropping",
  title: "Modifying & Dropping Tables",
  chapterId: "tables",
  content: `## ALTER TABLE

Once a table exists, you will inevitably need to change its structure. The \`ALTER TABLE\` statement modifies an existing table without losing its data.

### Adding a Column

\`\`\`sql
ALTER TABLE products ADD COLUMN description TEXT;
\`\`\`

This adds a new \`description\` column to the \`products\` table. Existing rows will have \`NULL\` for the new column (unless you specify a \`DEFAULT\`):

\`\`\`sql
ALTER TABLE products ADD COLUMN in_stock BOOLEAN DEFAULT TRUE;
\`\`\`

### Dropping a Column

\`\`\`sql
ALTER TABLE products DROP COLUMN description;
\`\`\`

> **Warning:** Dropping a column permanently deletes all data in that column. There is no undo. Always back up your data before dropping columns in production.

### Renaming a Column

\`\`\`sql
ALTER TABLE products RENAME COLUMN name TO product_name;
\`\`\`

This changes the column name without affecting the data it contains.

### Changing a Column's Data Type

\`\`\`sql
ALTER TABLE products ALTER COLUMN price TYPE NUMERIC(12, 2);
\`\`\`

PostgreSQL will attempt to cast existing values to the new type. If the cast is not possible (e.g., converting text \`'hello'\` to \`INT\`), the operation fails. You can provide an explicit conversion:

\`\`\`sql
ALTER TABLE products ALTER COLUMN price TYPE INT USING price::INT;
\`\`\`

### Adding Constraints

\`\`\`sql
-- Add a NOT NULL constraint
ALTER TABLE products ALTER COLUMN name SET NOT NULL;

-- Add a UNIQUE constraint
ALTER TABLE products ADD CONSTRAINT unique_product_name UNIQUE (name);

-- Add a CHECK constraint
ALTER TABLE products ADD CONSTRAINT positive_price CHECK (price >= 0);
\`\`\`

### Dropping Constraints

\`\`\`sql
ALTER TABLE products DROP CONSTRAINT unique_product_name;
ALTER TABLE products ALTER COLUMN name DROP NOT NULL;
\`\`\`

### Setting and Removing Defaults

\`\`\`sql
-- Set a default value
ALTER TABLE products ALTER COLUMN category SET DEFAULT 'Uncategorized';

-- Remove a default value
ALTER TABLE products ALTER COLUMN category DROP DEFAULT;
\`\`\`

### Renaming a Table

\`\`\`sql
ALTER TABLE products RENAME TO inventory;
\`\`\`

## DROP TABLE

> "Computer, initiate auto-destruct sequence." \`DROP TABLE\` is the self-destruct of SQL --- once it is done, there is no coming back.

\`DROP TABLE\` permanently removes a table and all of its data:

\`\`\`sql
DROP TABLE reviews;
\`\`\`

### IF EXISTS

If the table does not exist, \`DROP TABLE\` raises an error. Use \`IF EXISTS\` to avoid this:

\`\`\`sql
DROP TABLE IF EXISTS reviews;
\`\`\`

### CASCADE

If other tables have foreign keys referencing the table you are dropping, the operation will fail. Use \`CASCADE\` to also drop all dependent objects:

\`\`\`sql
DROP TABLE IF EXISTS products CASCADE;
\`\`\`

> **Warning:** \`CASCADE\` can have far-reaching effects. It drops all foreign key constraints, views, and other objects that depend on the table. Use it with extreme caution in production environments.

## TRUNCATE

\`TRUNCATE\` removes all rows from a table but keeps the table structure intact. It is much faster than \`DELETE\` without a \`WHERE\` clause because it does not scan individual rows:

\`\`\`sql
TRUNCATE TABLE orders;
\`\`\`

Like \`DROP TABLE\`, you can use \`CASCADE\` with \`TRUNCATE\` to also truncate tables with foreign key references:

\`\`\`sql
TRUNCATE TABLE customers CASCADE;
\`\`\`

> **Tip:** \`TRUNCATE\` resets the identity/serial counter by default. If you want to keep the counter, add \`CONTINUE IDENTITY\`:
> \`\`\`sql
> TRUNCATE TABLE orders CONTINUE IDENTITY;
> \`\`\`

### Multiple ALTER Operations

You can combine multiple alterations in a single statement:

\`\`\`sql
ALTER TABLE products
  ADD COLUMN sku TEXT,
  ADD COLUMN weight DECIMAL(8, 2),
  DROP COLUMN IF EXISTS old_column;
\`\`\`

### Your Task

Add a \`description\` column of type \`TEXT\` to the \`products\` table.`,

  starterCode: `-- Add a description column to the products table
ALTER TABLE products`,

  solution: `ALTER TABLE products ADD COLUMN description TEXT;`,

  tests: [
    {
      name: "adds description column to products",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT column_name FROM information_schema.columns WHERE table_name = 'products' AND column_name = 'description';`,
      expected: '{"type":"rowCount","value":1}',
    },
  ],
};
