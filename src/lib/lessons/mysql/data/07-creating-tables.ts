import type { Lesson } from "../../types";

export const creatingTables: Lesson = {
  id: "creating-tables",
  title: "Creating Tables",
  chapterId: "schema",
  content: `## CREATE TABLE

The \`CREATE TABLE\` statement defines a new table's structure. In MySQL, every table needs at least one column and typically a primary key.

### Basic Syntax

\`\`\`sql
CREATE TABLE employees (
  id        INT AUTO_INCREMENT PRIMARY KEY,
  name      VARCHAR(100) NOT NULL,
  email     VARCHAR(100) UNIQUE,
  salary    DECIMAL(10, 2),
  hired_at  DATE
);
\`\`\`

### MySQL Data Types

| Type | Description | Example |
|------|-------------|---------|
| \`INT\` | Whole numbers (-2B to 2B) | \`42\` |
| \`BIGINT\` | Large whole numbers | \`9876543210\` |
| \`DECIMAL(p,s)\` | Exact decimal (p digits, s after point) | \`999.99\` |
| \`FLOAT\` / \`DOUBLE\` | Approximate decimal | \`3.14159\` |
| \`VARCHAR(n)\` | Variable-length string, max n chars | \`'Alice'\` |
| \`TEXT\` | Long text, up to 65535 chars | Long descriptions |
| \`CHAR(n)\` | Fixed-length string, always n chars | Country codes |
| \`DATE\` | Date only: YYYY-MM-DD | \`'2024-01-15'\` |
| \`DATETIME\` | Date and time: YYYY-MM-DD HH:MM:SS | \`'2024-01-15 14:30:00'\` |
| \`TIMESTAMP\` | Like DATETIME, stored in UTC | Auto-updated on insert |
| \`BOOLEAN\` | 0 or 1 (MySQL stores as TINYINT(1)) | \`TRUE\`, \`FALSE\` |
| \`JSON\` | Validated JSON documents | \`'{"key":"val"}'\` |
| \`ENUM\` | One value from a defined list | \`ENUM('active','inactive')\` |

### PRIMARY KEY

Every table should have a primary key — a column (or combination) that uniquely identifies each row:

\`\`\`sql
-- Single-column primary key
id INT AUTO_INCREMENT PRIMARY KEY

-- Or declared separately (required for composite keys)
PRIMARY KEY (order_id, product_id)
\`\`\`

### AUTO_INCREMENT

\`AUTO_INCREMENT\` tells MySQL to automatically assign the next integer value when you insert a row without specifying the id:

\`\`\`sql
CREATE TABLE categories (
  id   INT AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(100) NOT NULL
);

INSERT INTO categories (name) VALUES ('Books');
-- id is automatically assigned as 1
\`\`\`

### NOT NULL and DEFAULT

\`\`\`sql
name    VARCHAR(100) NOT NULL,           -- must always have a value
status  VARCHAR(20) DEFAULT 'active',    -- uses 'active' if not specified
score   INT DEFAULT 0,
notes   TEXT                             -- nullable (default NULL)
\`\`\`

### IF NOT EXISTS

Prevent errors if the table already exists:

\`\`\`sql
CREATE TABLE IF NOT EXISTS employees (...);
\`\`\`

### Your Task

Create a table called \`employees\` with these columns:
- \`id\` — integer primary key
- \`name\` — up to 100 characters, required
- \`email\` — up to 100 characters, must be unique
- \`salary\` — decimal with 2 decimal places
- \`department\` — up to 50 characters`,

  starterCode: `-- Create the employees table
CREATE TABLE employees (
  id        INTEGER PRIMARY KEY,
  name      VARCHAR(100) NOT NULL,
  email     VARCHAR(100) UNIQUE,
  salary    DECIMAL(10, 2),
  department VARCHAR(50)
);`,

  solution: `CREATE TABLE employees (
  id        INTEGER PRIMARY KEY,
  name      VARCHAR(100) NOT NULL,
  email     VARCHAR(100) UNIQUE,
  salary    DECIMAL(10, 2),
  department VARCHAR(50)
);`,

  tests: [
    {
      name: "employees table has 5 columns",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT COUNT(*) FROM pragma_table_info('employees')`,
      expected: '{"type":"exact","value":"5"}',
    },
    {
      name: "name column exists",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT name FROM pragma_table_info('employees') WHERE name='name'`,
      expected: '{"type":"rowCount","value":1}',
    },
    {
      name: "salary column exists",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT name FROM pragma_table_info('employees') WHERE name='salary'`,
      expected: '{"type":"rowCount","value":1}',
    },
    {
      name: "email column exists",
      code: `{{USER_SQL}}
---VALIDATE---
SELECT name FROM pragma_table_info('employees') WHERE name='email'`,
      expected: '{"type":"rowCount","value":1}',
    },
  ],
};
