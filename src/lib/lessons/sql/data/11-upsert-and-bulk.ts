import type { Lesson } from "../../types";

export const upsertAndBulk: Lesson = {
  id: "upsert-and-bulk",
  title: "Upsert & Bulk Operations",
  chapterId: "crud",
  content: `## Handling Conflicts and Bulk Data

Real-world applications frequently need to insert data that might already exist, or process large batches of records efficiently. PostgreSQL provides powerful tools for both scenarios.

### INSERT ... ON CONFLICT (Upsert)

An "upsert" is an operation that inserts a row if it does not exist, or updates it if it does. PostgreSQL implements this with the \`ON CONFLICT\` clause.

#### DO NOTHING

If a conflict occurs (e.g., a duplicate unique key), simply skip the row:

\`\`\`sql
INSERT INTO users (name, email)
VALUES ('Alice', 'alice@example.com')
ON CONFLICT (email) DO NOTHING;
\`\`\`

This is useful for idempotent imports where you want to avoid duplicates without raising an error.

#### DO UPDATE

If a conflict occurs, update the existing row instead:

\`\`\`sql
INSERT INTO products (name, price, category)
VALUES ('Laptop', 1099.99, 'Electronics')
ON CONFLICT (name) DO UPDATE
SET price = EXCLUDED.price,
    category = EXCLUDED.category;
\`\`\`

### The EXCLUDED Keyword

Inside the \`DO UPDATE\` clause, \`EXCLUDED\` refers to the row that was proposed for insertion. It lets you access the values that would have been inserted:

- \`EXCLUDED.price\` is the price from the \`VALUES\` clause (\`1099.99\`)
- \`products.price\` (or just \`price\`) is the current value in the existing row

This distinction is essential when you want to conditionally update:

\`\`\`sql
INSERT INTO products (name, price, category)
VALUES ('Laptop', 1099.99, 'Electronics')
ON CONFLICT (name) DO UPDATE
SET price = GREATEST(products.price, EXCLUDED.price);
\`\`\`

This only updates the price if the new value is higher than the existing one.

### Bulk INSERT

Inserting multiple rows in a single statement is significantly faster than individual inserts:

\`\`\`sql
INSERT INTO products (name, price, category) VALUES
  ('USB Cable', 9.99, 'Electronics'),
  ('Monitor Stand', 49.99, 'Office'),
  ('Desk Mat', 24.99, 'Office');
\`\`\`

PostgreSQL processes the entire batch in one round trip, reducing network overhead and transaction costs.

### INSERT from SELECT

You can populate a table using data from a query. This is powerful for data migration, transformation, or creating summary tables:

\`\`\`sql
INSERT INTO premium_products (name, price, category)
SELECT name, price, category
FROM products
WHERE price > 100;
\`\`\`

No \`VALUES\` keyword is needed when using \`SELECT\`. The selected columns must match the target column list in number and compatible types.

### Bulk UPDATE with CASE

When you need to update many rows with different values, a \`CASE\` expression avoids running multiple \`UPDATE\` statements:

\`\`\`sql
UPDATE products
SET price = CASE name
  WHEN 'Notebook' THEN 5.99
  WHEN 'Pen Set' THEN 14.99
  WHEN 'Desk Lamp' THEN 39.99
  ELSE price
END
WHERE name IN ('Notebook', 'Pen Set', 'Desk Lamp');
\`\`\`

The \`ELSE price\` clause ensures that rows not listed in the \`CASE\` keep their original value. The \`WHERE\` clause limits the update to only the rows that actually need to change.

> **Tip:** For very large bulk operations, consider wrapping them in an explicit transaction and temporarily disabling indexes or constraints if appropriate. This can improve performance by orders of magnitude.

### Your Task

Insert three new products in a single statement: \`'USB Cable'\` for \`9.99\`, \`'Monitor Stand'\` for \`49.99\`, and \`'Desk Mat'\` for \`24.99\`, all in the \`'Office'\` category.`,

  starterCode: `-- Insert three products in one statement
INSERT INTO products (name, price, category) VALUES`,

  solution: `INSERT INTO products (name, price, category) VALUES
  ('USB Cable', 9.99, 'Office'),
  ('Monitor Stand', 49.99, 'Office'),
  ('Desk Mat', 24.99, 'Office');`,

  tests: [
    {
      name: "inserts three new products",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT * FROM products WHERE name IN ('USB Cable', 'Monitor Stand', 'Desk Mat');`,
      expected: '{"type":"rowCount","value":3}',
    },
  ],
};
