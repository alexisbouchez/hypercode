import type { Lesson } from "../../types";

export const creatingTables: Lesson = {
  id: "creating-tables",
  title: "Creating Tables",
  chapterId: "tables",
  content: `## CREATE TABLE

The \`CREATE TABLE\` statement defines a new table in the database. You specify the table name and a list of columns, each with a name and a data type:

\`\`\`sql
CREATE TABLE employees (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT UNIQUE NOT NULL,
  department TEXT,
  salary DECIMAL(10, 2)
);
\`\`\`

Each column definition follows the pattern: \`column_name DATA_TYPE [constraints]\`.

> You would not build a starship without blueprints. Similarly, \`CREATE TABLE\` is your schema blueprint --- you need to define the structure before you can fill it with data.

### Column Definitions

A column definition consists of:

1. **Name**: The column identifier (e.g., \`name\`, \`email\`)
2. **Data type**: What kind of data it stores (e.g., \`TEXT\`, \`INT\`, \`DECIMAL(10,2)\`)
3. **Constraints** (optional): Rules the data must follow (e.g., \`NOT NULL\`, \`UNIQUE\`)

\`\`\`sql
CREATE TABLE products (
  id SERIAL PRIMARY KEY,        -- auto-incrementing integer, primary key
  name TEXT NOT NULL,            -- required text field
  price DECIMAL(10, 2),         -- decimal with 10 digits total, 2 after the point
  category TEXT,                 -- optional text field
  created_at TIMESTAMPTZ DEFAULT NOW()  -- timestamp, defaults to current time
);
\`\`\`

### SERIAL and Primary Keys

\`SERIAL\` is a convenience type that creates an auto-incrementing integer column. It is commonly used for primary keys:

\`\`\`sql
id SERIAL PRIMARY KEY
\`\`\`

This is shorthand for creating an integer column with a sequence that automatically generates the next value when a new row is inserted.

> **Tip:** In modern PostgreSQL (version 10+), the SQL-standard \`GENERATED ALWAYS AS IDENTITY\` syntax is preferred over \`SERIAL\`:
> \`\`\`sql
> id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
> \`\`\`
> Both accomplish the same thing, but \`GENERATED ALWAYS AS IDENTITY\` follows the SQL standard and provides better safeguards against accidental manual inserts.

### IF NOT EXISTS

If you try to create a table that already exists, PostgreSQL will raise an error. Use \`IF NOT EXISTS\` to avoid this:

\`\`\`sql
CREATE TABLE IF NOT EXISTS employees (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL
);
\`\`\`

This creates the table only if it does not already exist. If the table is already there, the statement does nothing and no error is raised.

### DEFAULT Values

You can set a default value for a column. When a row is inserted without specifying that column, the default is used:

\`\`\`sql
CREATE TABLE orders (
  id SERIAL PRIMARY KEY,
  status TEXT DEFAULT 'pending',
  created_at TIMESTAMPTZ DEFAULT NOW()
);
\`\`\`

### Common Patterns

Here are some patterns you will see frequently in real-world schemas:

\`\`\`sql
-- Timestamps for auditing
CREATE TABLE posts (
  id SERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  body TEXT,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Lookup table
CREATE TABLE categories (
  id SERIAL PRIMARY KEY,
  name TEXT UNIQUE NOT NULL,
  description TEXT
);
\`\`\`

> **Warning:** Table and column names in PostgreSQL are case-insensitive by default and are folded to lowercase. If you use double quotes around a name (e.g., \`"MyTable"\`), the case is preserved, but you must always quote it in subsequent queries. Stick to lowercase names with underscores to avoid confusion.

### Your Task

Create a \`books\` table with the following columns:

- \`id\` - auto-incrementing primary key (\`SERIAL PRIMARY KEY\`)
- \`title\` - required text (\`TEXT NOT NULL\`)
- \`author\` - required text (\`TEXT NOT NULL\`)
- \`published_year\` - integer (\`INT\`)
- \`price\` - decimal with 10 digits total and 2 decimal places (\`DECIMAL(10, 2)\`)`,

  starterCode: `-- Create the books table
CREATE TABLE books (

);`,

  solution: `CREATE TABLE books (
  id SERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  author TEXT NOT NULL,
  published_year INT,
  price DECIMAL(10, 2)
);`,

  tests: [
    {
      name: "creates books table with 5 columns",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT column_name FROM information_schema.columns WHERE table_name = 'books' ORDER BY ordinal_position;`,
      expected: '{"type":"rowCount","value":5}',
    },
  ],
};
