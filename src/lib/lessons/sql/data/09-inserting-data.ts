import type { Lesson } from "../../types";

export const insertingData: Lesson = {
  id: "inserting-data",
  title: "Inserting Data",
  chapterId: "crud",
  content: `## The INSERT Statement

The \`INSERT INTO\` statement adds new rows to a table. It is one of the four fundamental data manipulation operations in SQL (alongside \`SELECT\`, \`UPDATE\`, and \`DELETE\`).

### Basic Syntax

To insert a single row, specify the table name, the columns you want to fill, and the values:

\`\`\`sql
INSERT INTO products (name, price, category)
VALUES ('Wireless Mouse', 29.99, 'Electronics');
\`\`\`

The number of values must match the number of columns listed, and the types must be compatible. PostgreSQL will raise an error if they do not match.

> **Tip:** Always list column names explicitly. Writing \`INSERT INTO products VALUES (...)\` without column names works, but it is fragile. If someone adds or reorders columns in the table, your insert will break or, worse, silently put data in the wrong columns.

### Inserting Multiple Rows

You can insert several rows in a single statement by separating each set of values with a comma:

\`\`\`sql
INSERT INTO products (name, price, category) VALUES
  ('USB Cable', 9.99, 'Electronics'),
  ('Monitor Stand', 49.99, 'Office'),
  ('Desk Mat', 24.99, 'Office');
\`\`\`

This is significantly more efficient than running three separate \`INSERT\` statements. The database processes the entire batch in a single transaction by default.

### DEFAULT Values

When a column has a default value defined (such as \`SERIAL\` for auto-incrementing IDs or \`DEFAULT\` clauses), you can either omit the column from the column list or use the \`DEFAULT\` keyword:

\`\`\`sql
-- Omit the id column entirely (uses SERIAL default)
INSERT INTO products (name, price, category)
VALUES ('Stylus', 14.99, 'Electronics');

-- Explicitly request the default
INSERT INTO products (id, name, price, category)
VALUES (DEFAULT, 'Stylus', 14.99, 'Electronics');
\`\`\`

Both approaches produce the same result. Omitting the column is the more common convention.

### The RETURNING Clause

PostgreSQL supports a \`RETURNING\` clause that outputs the inserted row immediately. This is extremely useful for getting auto-generated IDs without running a separate query:

\`\`\`sql
INSERT INTO products (name, price, category)
VALUES ('Webcam', 59.99, 'Electronics')
RETURNING id, name;
\`\`\`

You can return all columns with \`RETURNING *\`, or select specific ones. This eliminates the need for a follow-up \`SELECT\` query and avoids race conditions in concurrent environments.

### Data Type Considerations

PostgreSQL validates types on insert. Common pitfalls include:

- **Strings** must use single quotes: \`'Electronics'\`, not \`"Electronics"\`
- **Numbers** do not use quotes: \`29.99\`, not \`'29.99'\`
- **NULL** is inserted explicitly with the \`NULL\` keyword, or implicitly by omitting a nullable column

### Your Task

Insert a new product with name \`'Wireless Mouse'\`, price \`29.99\`, and category \`'Electronics'\`.`,

  starterCode: `-- Insert a new product into the products table
INSERT INTO products`,

  solution: `INSERT INTO products (name, price, category)
VALUES ('Wireless Mouse', 29.99, 'Electronics');`,

  tests: [
    {
      name: "inserts Wireless Mouse product",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT * FROM products WHERE name = 'Wireless Mouse';`,
      expected: '{"type":"rowCount","value":1}',
    },
  ],
};
