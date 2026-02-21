import type { Lesson } from "../../types";

export const constraints: Lesson = {
  id: "constraints",
  title: "Constraints",
  chapterId: "schema",
  content: `## Constraints

Constraints enforce rules on column values. They are checked whenever a row is inserted or updated — if a constraint is violated, SQLite raises an error and rejects the change.

### NOT NULL

Prevents a column from storing \`NULL\`:

\`\`\`sql
CREATE TABLE users (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,   -- required
  bio TEXT              -- optional (can be NULL)
);
\`\`\`

### UNIQUE

Prevents duplicate values in a column (or combination of columns):

\`\`\`sql
CREATE TABLE users (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  email TEXT NOT NULL UNIQUE
);

-- Multi-column uniqueness
CREATE TABLE enrollments (
  student_id INTEGER,
  course_id INTEGER,
  UNIQUE(student_id, course_id)  -- a student can't enroll twice in the same course
);
\`\`\`

### CHECK

Validates column values against a boolean expression:

\`\`\`sql
CREATE TABLE products (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  price REAL CHECK(price >= 0),
  quantity INTEGER CHECK(quantity >= 0),
  status TEXT CHECK(status IN ('active', 'inactive', 'discontinued'))
);
\`\`\`

If the expression evaluates to false, the insert or update is rejected.

### DEFAULT

Provides a value when none is specified:

\`\`\`sql
CREATE TABLE tasks (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  title TEXT NOT NULL,
  priority INTEGER DEFAULT 3,
  done INTEGER DEFAULT 0,
  created_at TEXT DEFAULT (datetime('now'))
);
\`\`\`

Defaults can be literals (\`0\`, \`'pending'\`) or expressions in parentheses (\`(datetime('now'))\`).

### FOREIGN KEY

References a row in another table. SQLite supports foreign key syntax but **foreign key enforcement is off by default**. You must enable it per connection:

\`\`\`sql
PRAGMA foreign_keys = ON;
\`\`\`

\`\`\`sql
CREATE TABLE orders (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  customer_id INTEGER NOT NULL REFERENCES customers(id),
  total REAL NOT NULL CHECK(total >= 0)
);
\`\`\`

Without \`PRAGMA foreign_keys = ON\`, SQLite allows inserting an \`order\` with a \`customer_id\` that does not exist in \`customers\`.

### Constraint Names

You can name constraints for better error messages:

\`\`\`sql
CREATE TABLE products (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  price REAL,
  CONSTRAINT price_non_negative CHECK(price >= 0)
);
\`\`\`

### Your Task

Create a \`members\` table with:
- \`id\` — integer primary key autoincrement
- \`username\` — required text, unique
- \`email\` — required text, unique
- \`age\` — integer with a CHECK that it must be at least 13
- \`score\` — real, defaults to 0.0`,

  starterCode: `-- Create the members table with constraints
CREATE TABLE members (

);`,

  solution: `CREATE TABLE members (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT NOT NULL UNIQUE,
  email TEXT NOT NULL UNIQUE,
  age INTEGER CHECK(age >= 13),
  score REAL DEFAULT 0.0
);`,

  tests: [
    {
      name: "creates members table",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT name FROM pragma_table_info('members') ORDER BY cid;`,
      expected: '{"type":"rowCount","value":5}',
    },
    {
      name: "members table has username column",
      code: `{{USER_SQL}}\n---VALIDATE---\nSELECT name FROM pragma_table_info('members') WHERE name = 'username';`,
      expected: '{"type":"rowCount","value":1}',
    },
  ],
};
