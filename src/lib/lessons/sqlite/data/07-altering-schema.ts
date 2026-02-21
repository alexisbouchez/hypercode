import type { Lesson } from "../../types";

export const alteringSchema: Lesson = {
  id: "altering-schema",
  title: "Altering Schema",
  chapterId: "schema",
  content: `## ALTER TABLE

Once a table is created, you can modify its schema with \`ALTER TABLE\`. SQLite's \`ALTER TABLE\` is more limited than PostgreSQL's — it supports only a subset of modifications.

### Adding a Column

\`\`\`sql
ALTER TABLE products ADD COLUMN description TEXT;
ALTER TABLE users ADD COLUMN created_at TEXT DEFAULT (datetime('now'));
\`\`\`

New columns are appended to the end of the table. The added column gets \`NULL\` for all existing rows unless you specify a default.

Restrictions on adding columns in SQLite:
- The column cannot have a \`PRIMARY KEY\` or \`UNIQUE\` constraint
- The column cannot have a non-constant default (well, expressions in parens are OK since SQLite 3.37.0)
- The column cannot be \`NOT NULL\` without a default value

### Renaming a Column (SQLite 3.25.0+)

\`\`\`sql
ALTER TABLE products RENAME COLUMN price TO unit_price;
\`\`\`

### Renaming a Table

\`\`\`sql
ALTER TABLE old_name RENAME TO new_name;
\`\`\`

### DROP TABLE

Remove a table entirely:

\`\`\`sql
DROP TABLE employees;
DROP TABLE IF EXISTS temp_data;  -- no error if it doesn't exist
\`\`\`

This permanently deletes the table and all its data.

### CREATE TABLE IF NOT EXISTS

A common pattern when setting up a schema that might already partially exist:

\`\`\`sql
CREATE TABLE IF NOT EXISTS logs (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  message TEXT NOT NULL,
  level TEXT DEFAULT 'info',
  created_at TEXT DEFAULT (datetime('now'))
);
\`\`\`

### Limitations: No DROP COLUMN (before SQLite 3.35.0)

Older SQLite versions do not support \`ALTER TABLE DROP COLUMN\`. The workaround is:

1. Create a new table with the desired structure
2. Copy data from the old table
3. Drop the old table
4. Rename the new table

Since SQLite 3.35.0 (2021), \`DROP COLUMN\` is supported:

\`\`\`sql
ALTER TABLE products DROP COLUMN description;
\`\`\`

### Your Task

First create a \`notes\` table with \`id\` (integer primary key autoincrement) and \`content\` (text not null). Then add a \`created_at\` column of type TEXT to it.`,

  starterCode: `-- Create notes table then add a column
CREATE TABLE notes (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  content TEXT NOT NULL
);

-- Now add the created_at column
ALTER TABLE notes`,

  solution: `CREATE TABLE notes (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  content TEXT NOT NULL
);

ALTER TABLE notes ADD COLUMN created_at TEXT;`,

  tests: [
    {
      name: "notes table has created_at column",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT name FROM pragma_table_info('notes') WHERE name = 'created_at';`,
      expected: '{"type":"rowCount","value":1}',
    },
    {
      name: "notes table has 3 columns total",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT name FROM pragma_table_info('notes') ORDER BY cid;`,
      expected: '{"type":"rowCount","value":3}',
    },
  ],
};
