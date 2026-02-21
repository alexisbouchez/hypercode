import type { Lesson } from "../../types";

export const insertingData: Lesson = {
  id: "inserting-data",
  title: "Inserting Data",
  chapterId: "crud",
  content: `## INSERT INTO

The \`INSERT INTO\` statement adds new rows to a table:

\`\`\`sql
INSERT INTO products (name, price, category)
VALUES ('Mouse', 29.99, 'Electronics');
\`\`\`

You list the target columns in parentheses, then provide matching values. Columns you omit will use their default values (or \`NULL\` if no default is set).

### Inserting Multiple Rows

Insert multiple rows in a single statement — more efficient than one \`INSERT\` per row:

\`\`\`sql
INSERT INTO products (name, price, category) VALUES
  ('Monitor', 299.99, 'Electronics'),
  ('Keyboard', 49.99, 'Electronics'),
  ('Mouse Pad', 9.99, 'Accessories');
\`\`\`

### INSERT OR REPLACE

If the new row would violate a \`UNIQUE\` or \`PRIMARY KEY\` constraint, \`INSERT OR REPLACE\` deletes the conflicting row and inserts the new one:

\`\`\`sql
-- If a product with id=1 already exists, delete it and insert this row
INSERT OR REPLACE INTO products (id, name, price, category)
VALUES (1, 'Laptop Pro', 1299.99, 'Electronics');
\`\`\`

This is equivalent to the SQL standard \`INSERT ... ON CONFLICT DO UPDATE\` (which SQLite also supports as \`UPSERT\`).

### INSERT OR IGNORE

If the new row would violate a constraint, silently skip the insert:

\`\`\`sql
-- If a user with this email already exists, do nothing
INSERT OR IGNORE INTO users (name, email)
VALUES ('Alice', 'alice@example.com');
\`\`\`

This is useful for idempotent data loading — run it multiple times without errors.

### LAST_INSERT_ROWID()

After an insert, get the ID of the last inserted row:

\`\`\`sql
INSERT INTO products (name, price, category) VALUES ('Stylus', 19.99, 'Accessories');
SELECT LAST_INSERT_ROWID();
\`\`\`

Each database connection has its own \`last_insert_rowid\` — concurrent connections do not interfere.

### INSERT ... SELECT

Insert rows from a query:

\`\`\`sql
-- Copy all Electronics to an archive table
INSERT INTO product_archive (name, price, category)
SELECT name, price, category FROM products WHERE category = 'Electronics';
\`\`\`

### Your Task

Insert a new product into the \`products\` table:
- name: \`'Tablet'\`
- price: \`349.99\`
- category: \`'Electronics'\`

Then select all products to verify the insert.`,

  starterCode: `-- Insert the new product
INSERT INTO products (name, price, category)
VALUES ();

-- Verify by selecting all products
SELECT * FROM products;`,

  solution: `INSERT INTO products (name, price, category)
VALUES ('Tablet', 349.99, 'Electronics');

SELECT * FROM products;`,

  tests: [
    {
      name: "returns 9 products after insert",
      expected: '{"type":"rowCount","value":9}',
    },
    {
      name: "Tablet is in the results",
      expected: '{"type":"contains","value":"Tablet"}',
    },
  ],
};
