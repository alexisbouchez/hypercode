import type { Lesson } from "../../types";

export const creatingTables: Lesson = {
  id: "creating-tables",
  title: "Creating Tables",
  chapterId: "schema",
  content: `## CREATE TABLE

The \`CREATE TABLE\` statement defines the structure of a new table. You specify the table name and a list of columns, each with a name and an optional type affinity:

\`\`\`sql
CREATE TABLE employees (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  email TEXT UNIQUE NOT NULL,
  department TEXT,
  salary REAL
);
\`\`\`

### INTEGER PRIMARY KEY

In SQLite, \`INTEGER PRIMARY KEY\` is special. A column declared as \`INTEGER PRIMARY KEY\` becomes an alias for the internal \`rowid\` — SQLite's built-in integer row identifier. This means:

- The column auto-increments by default (without needing the \`AUTOINCREMENT\` keyword)
- Inserting \`NULL\` into the column fills in the next available integer

\`\`\`sql
CREATE TABLE items (
  id INTEGER PRIMARY KEY,  -- automatically assigns 1, 2, 3, ...
  name TEXT
);
\`\`\`

### AUTOINCREMENT

Adding \`AUTOINCREMENT\` prevents SQLite from reusing IDs from deleted rows:

\`\`\`sql
CREATE TABLE items (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT
);
\`\`\`

Without \`AUTOINCREMENT\`, if you delete the row with the highest ID and insert a new row, it could get that same ID again. With \`AUTOINCREMENT\`, IDs are strictly monotonically increasing — they never reuse old values.

Use \`AUTOINCREMENT\` when row identity must be globally unique across the lifetime of the table.

### CREATE TABLE IF NOT EXISTS

Prevent an error when the table already exists:

\`\`\`sql
CREATE TABLE IF NOT EXISTS logs (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  message TEXT NOT NULL,
  created_at TEXT DEFAULT (datetime('now'))
);
\`\`\`

### DEFAULT Values

Columns can have default values used when no value is provided at insert time:

\`\`\`sql
CREATE TABLE tasks (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  title TEXT NOT NULL,
  done INTEGER DEFAULT 0,
  priority TEXT DEFAULT 'normal',
  created_at TEXT DEFAULT (datetime('now'))
);
\`\`\`

Note: SQLite stores dates as TEXT (ISO 8601), REAL (Julian day), or INTEGER (Unix time). There is no dedicated \`DATE\` or \`TIMESTAMP\` type — these names just get TEXT affinity.

### Your Task

Create a \`books\` table with the following columns:
- \`id\` — integer primary key with autoincrement
- \`title\` — required text
- \`author\` — required text
- \`published_year\` — integer
- \`price\` — real number`,

  starterCode: `-- Create the books table
CREATE TABLE books (

);`,

  solution: `CREATE TABLE books (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  title TEXT NOT NULL,
  author TEXT NOT NULL,
  published_year INTEGER,
  price REAL
);`,

  tests: [
    {
      name: "creates books table with 5 columns",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT name FROM pragma_table_info('books') ORDER BY cid;`,
      expected: '{"type":"rowCount","value":5}',
    },
    {
      name: "books table has title column",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT name FROM pragma_table_info('books') WHERE name = 'title';`,
      expected: '{"type":"rowCount","value":1}',
    },
  ],
};
