import type { Lesson } from "../../types";

export const dataTypes: Lesson = {
  id: "data-types",
  title: "Understanding Data Types",
  chapterId: "tables",
  content: `## Why Data Types Matter

Every column in a PostgreSQL table has a data type. The type determines what kind of values the column can store, how much storage it uses, and what operations are valid on it.

Choosing the right data type is important for correctness, performance, and storage efficiency.

### Numeric Types

PostgreSQL provides several numeric types:

| Type | Description | Range / Precision |
|------|-------------|-------------------|
| \`SMALLINT\` | 2-byte integer | -32,768 to 32,767 |
| \`INTEGER\` (or \`INT\`) | 4-byte integer | -2.1 billion to 2.1 billion |
| \`BIGINT\` | 8-byte integer | -9.2 quintillion to 9.2 quintillion |
| \`DECIMAL(p,s)\` / \`NUMERIC(p,s)\` | Exact decimal | User-specified precision |
| \`REAL\` | 4-byte floating point | ~6 decimal digits of precision |
| \`DOUBLE PRECISION\` | 8-byte floating point | ~15 decimal digits of precision |
| \`SERIAL\` | Auto-incrementing 4-byte integer | 1 to 2.1 billion |
| \`BIGSERIAL\` | Auto-incrementing 8-byte integer | 1 to 9.2 quintillion |

> **Tip:** Use \`DECIMAL\` or \`NUMERIC\` for money and financial data. Floating-point types like \`REAL\` and \`DOUBLE PRECISION\` can introduce rounding errors that are unacceptable for financial calculations.

### Text Types

| Type | Description |
|------|-------------|
| \`TEXT\` | Variable-length string with no limit |
| \`VARCHAR(n)\` | Variable-length string with a maximum of \`n\` characters |
| \`CHAR(n)\` | Fixed-length string, padded with spaces |

In PostgreSQL, \`TEXT\` and \`VARCHAR\` perform identically. There is no performance penalty for using \`TEXT\` over \`VARCHAR(n)\`. The only reason to use \`VARCHAR(n)\` is if you want the database to enforce a maximum length.

### Date and Time Types

| Type | Description | Example |
|------|-------------|---------|
| \`DATE\` | Calendar date | \`2024-01-15\` |
| \`TIME\` | Time of day (no timezone) | \`14:30:00\` |
| \`TIMESTAMP\` | Date and time (no timezone) | \`2024-01-15 14:30:00\` |
| \`TIMESTAMPTZ\` | Date and time with timezone | \`2024-01-15 14:30:00+00\` |
| \`INTERVAL\` | Time span | \`3 days 4 hours\` |

> **Tip:** Always use \`TIMESTAMPTZ\` (timestamp with time zone) instead of \`TIMESTAMP\` when storing points in time. Without timezone information, timestamps become ambiguous when your application serves users in different timezones.

### Boolean

The \`BOOLEAN\` type stores \`TRUE\`, \`FALSE\`, or \`NULL\`:

\`\`\`sql
CREATE TABLE tasks (
  id SERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  completed BOOLEAN DEFAULT FALSE
);
\`\`\`

### UUID

The \`UUID\` type stores universally unique identifiers (128-bit values):

\`\`\`sql
CREATE TABLE sessions (
  id UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  user_id INT NOT NULL,
  created_at TIMESTAMPTZ DEFAULT NOW()
);
\`\`\`

### JSON and JSONB

PostgreSQL has native support for JSON data:

| Type | Description |
|------|-------------|
| \`JSON\` | Stores JSON as text, preserves formatting |
| \`JSONB\` | Stores JSON in binary format, supports indexing |

\`\`\`sql
CREATE TABLE events (
  id SERIAL PRIMARY KEY,
  payload JSONB NOT NULL
);

INSERT INTO events (payload) VALUES ('{"type": "click", "x": 100, "y": 200}');
SELECT payload->>'type' FROM events;  -- returns 'click'
\`\`\`

> **Tip:** Prefer \`JSONB\` over \`JSON\` in almost all cases. It is faster to query, supports indexing, and removes duplicate keys.

### Arrays

PostgreSQL supports array columns:

\`\`\`sql
CREATE TABLE articles (
  id SERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  tags TEXT[] DEFAULT '{}'
);

INSERT INTO articles (title, tags) VALUES ('PostgreSQL Tips', ARRAY['sql', 'postgres', 'database']);
\`\`\`

### Your Task

Inspect the data types of the \`products\` table by querying the \`information_schema.columns\` system table.`,

  starterCode: `SELECT column_name, data_type
FROM information_schema.columns
WHERE table_name = 'products';`,

  solution: `SELECT column_name, data_type
FROM information_schema.columns
WHERE table_name = 'products';`,

  tests: [
    {
      name: "returns column information",
      expected: '{"type":"custom"}',
    },
  ],
};
