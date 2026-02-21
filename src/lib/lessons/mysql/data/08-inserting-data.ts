import type { Lesson } from "../../types";

export const insertingData: Lesson = {
  id: "inserting-data",
  title: "Inserting Data",
  chapterId: "schema",
  content: `## INSERT INTO

The \`INSERT INTO\` statement adds new rows to a table.

### Basic Insert

\`\`\`sql
INSERT INTO products (name, price, category, stock)
VALUES ('USB Hub', 29.99, 'Electronics', 100);
\`\`\`

You list the columns, then the corresponding values in the same order. Columns with \`DEFAULT\` values or \`AUTO_INCREMENT\` can be omitted.

### Inserting Without Column Names

If you provide values for every column in order, you can omit the column list:

\`\`\`sql
INSERT INTO products VALUES (11, 'USB Hub', 29.99, 'Electronics', 100);
\`\`\`

This is fragile — if you add a column later, this query breaks. Always prefer the explicit column list.

### Multi-Row Insert

MySQL can insert multiple rows in one statement, which is much faster than separate INSERT statements:

\`\`\`sql
INSERT INTO products (name, price, category, stock)
VALUES
  ('USB Hub', 29.99, 'Electronics', 100),
  ('Mouse Pad', 9.99, 'Accessories', 250),
  ('Webcam', 89.99, 'Electronics', 45);
\`\`\`

### INSERT IGNORE

If a row violates a unique constraint, \`INSERT IGNORE\` silently skips it instead of throwing an error:

\`\`\`sql
INSERT IGNORE INTO customers (id, name, email, city)
VALUES (1, 'Alice', 'alice@example.com', 'New York');
-- If id=1 already exists, this is silently ignored
\`\`\`

### INSERT ... ON DUPLICATE KEY UPDATE

This is MySQL's upsert syntax — insert a row, or update it if a duplicate key is found:

\`\`\`sql
INSERT INTO products (id, name, price, category, stock)
VALUES (1, 'Laptop', 999.99, 'Electronics', 60)
ON DUPLICATE KEY UPDATE stock = stock + 10;
-- If id=1 exists, updates stock instead of inserting
\`\`\`

### LAST_INSERT_ID()

After inserting a row with \`AUTO_INCREMENT\`, retrieve the generated id:

\`\`\`sql
INSERT INTO customers (name, email, city)
VALUES ('Frank', 'frank@example.com', 'Seattle');

SELECT LAST_INSERT_ID();  -- Returns the auto-generated id
\`\`\`

### Inserting from SELECT

You can insert data selected from another table:

\`\`\`sql
INSERT INTO archive_customers (name, email, city)
SELECT name, email, city FROM customers WHERE city IS NULL;
\`\`\`

### Your Task

Insert a new product into the \`products\` table:
- name: \`'USB Hub'\`
- price: \`29.99\`
- category: \`'Electronics'\`
- stock: \`100\``,

  starterCode: `-- Insert a new product
INSERT INTO products (name, price, category, stock)
VALUES`,

  solution: `INSERT INTO products (name, price, category, stock)
VALUES ('USB Hub', 29.99, 'Electronics', 100);`,

  tests: [
    {
      name: "products table now has 11 rows",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT COUNT(*) FROM products`,
      expected: '{"type":"exact","value":"11"}',
    },
    {
      name: "USB Hub exists in the table",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT name FROM products WHERE name='USB Hub'`,
      expected: '{"type":"rowCount","value":1}',
    },
    {
      name: "price is correct (29.99)",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT price FROM products WHERE name='USB Hub'`,
      expected: '{"type":"exact","value":"29.99"}',
    },
    {
      name: "category is Electronics",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT category FROM products WHERE name='USB Hub'`,
      expected: '{"type":"exact","value":"Electronics"}',
    },
  ],
};
