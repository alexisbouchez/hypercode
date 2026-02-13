import type { Lesson } from "../../types";

export const constraintsAndKeys: Lesson = {
  id: "constraints-and-keys",
  title: "Constraints & Keys",
  chapterId: "tables",
  content: `## What are Constraints?

Constraints are rules enforced by the database to ensure data integrity. They prevent invalid data from being inserted or updated. When a constraint is violated, the database rejects the operation and returns an error.

Constraints are your first line of defense against bad data. They are enforced at the database level, regardless of which application or tool is writing the data.

> Starfleet security protocols do not let unauthorized personnel through the force fields. Constraints work the same way --- they ensure no invalid data breaches your tables.

### PRIMARY KEY

A primary key uniquely identifies each row in a table. Every table should have one. A primary key implies two things: the column is \`UNIQUE\` and it is \`NOT NULL\`.

\`\`\`sql
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL
);
\`\`\`

You can also define a composite primary key that spans multiple columns:

\`\`\`sql
CREATE TABLE enrollments (
  student_id INT,
  course_id INT,
  enrolled_at TIMESTAMPTZ DEFAULT NOW(),
  PRIMARY KEY (student_id, course_id)
);
\`\`\`

### FOREIGN KEY

A foreign key creates a link between two tables. It ensures that a value in one table must exist in another table:

\`\`\`sql
CREATE TABLE orders (
  id SERIAL PRIMARY KEY,
  customer_id INT REFERENCES customers(id),
  total DECIMAL(10, 2)
);
\`\`\`

The \`REFERENCES\` keyword tells PostgreSQL that \`customer_id\` must match an existing \`id\` in the \`customers\` table. If you try to insert an order with a \`customer_id\` that does not exist, the database will reject it.

You can also specify what happens when the referenced row is deleted:

\`\`\`sql
-- Delete the order when the customer is deleted
customer_id INT REFERENCES customers(id) ON DELETE CASCADE

-- Set to NULL when the customer is deleted
customer_id INT REFERENCES customers(id) ON DELETE SET NULL

-- Prevent deleting the customer if orders exist (default)
customer_id INT REFERENCES customers(id) ON DELETE RESTRICT
\`\`\`

### UNIQUE

The \`UNIQUE\` constraint ensures no two rows have the same value in a column:

\`\`\`sql
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email TEXT UNIQUE NOT NULL
);
\`\`\`

Unlike \`PRIMARY KEY\`, you can have multiple \`UNIQUE\` columns in a table, and \`UNIQUE\` columns can contain \`NULL\` (unless you also add \`NOT NULL\`).

### NOT NULL

\`NOT NULL\` prevents a column from storing \`NULL\` values. Any insert or update that tries to set the column to \`NULL\` will fail:

\`\`\`sql
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,     -- required
  description TEXT        -- optional (NULL allowed)
);
\`\`\`

> **Tip:** Default to \`NOT NULL\` for most columns. Only allow \`NULL\` when the absence of a value has a meaningful interpretation in your domain. This prevents an entire class of bugs related to unexpected \`NULL\` values.

### CHECK

\`CHECK\` constraints validate that a value satisfies a boolean expression:

\`\`\`sql
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  price DECIMAL(10, 2) CHECK (price >= 0),
  quantity INT CHECK (quantity >= 0)
);
\`\`\`

You can also name your constraints for clearer error messages:

\`\`\`sql
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  price DECIMAL(10, 2) CONSTRAINT positive_price CHECK (price >= 0)
);
\`\`\`

### DEFAULT

\`DEFAULT\` provides a value when none is specified during insert:

\`\`\`sql
CREATE TABLE orders (
  id SERIAL PRIMARY KEY,
  status TEXT DEFAULT 'pending',
  created_at TIMESTAMPTZ DEFAULT NOW()
);
\`\`\`

### Combining Constraints

Constraints can be combined freely on a single column:

\`\`\`sql
CREATE TABLE accounts (
  id SERIAL PRIMARY KEY,
  username TEXT UNIQUE NOT NULL,
  email TEXT UNIQUE NOT NULL,
  balance DECIMAL(10, 2) NOT NULL DEFAULT 0 CHECK (balance >= 0),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
\`\`\`

> **Warning:** Be thoughtful about cascading deletes. \`ON DELETE CASCADE\` can remove large amounts of data if you are not careful. In many cases, \`ON DELETE RESTRICT\` (the default) is safer because it forces you to explicitly handle related records before deleting a parent row.

### Your Task

Create a \`reviews\` table with the following columns and constraints:

- \`id\` - auto-incrementing primary key (\`SERIAL PRIMARY KEY\`)
- \`product_id\` - integer that references \`products(id)\` (\`INT REFERENCES products(id)\`)
- \`rating\` - integer between 1 and 5 (\`INT CHECK (rating >= 1 AND rating <= 5)\`)
- \`comment\` - optional text (\`TEXT\`)`,

  starterCode: `-- Create the reviews table with constraints
CREATE TABLE reviews (

);`,

  solution: `CREATE TABLE reviews (
  id SERIAL PRIMARY KEY,
  product_id INT REFERENCES products(id),
  rating INT CHECK (rating >= 1 AND rating <= 5),
  comment TEXT
);`,

  tests: [
    {
      name: "creates reviews table with 4 columns",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT column_name FROM information_schema.columns WHERE table_name = 'reviews' ORDER BY ordinal_position;`,
      expected: '{"type":"rowCount","value":4}',
    },
  ],
};
