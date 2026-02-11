import type { Lesson } from "../../types";

export const jsonAndArrays: Lesson = {
  id: "json-and-arrays",
  title: "JSON & Array Operations",
  chapterId: "advanced-sql",
  content: `## JSON in PostgreSQL

PostgreSQL has first-class support for JSON data through two types:

| Type | Description |
|------|-------------|
| \`JSON\` | Stores JSON as plain text; validated on input |
| \`JSONB\` | Stores JSON in a binary format; faster to query, supports indexing |

In almost all cases, prefer \`JSONB\` over \`JSON\`. It is more efficient for reads and supports a richer set of operators.

### Creating JSON Data

You can store JSON directly in a column:

\`\`\`sql
CREATE TABLE events (
  id SERIAL PRIMARY KEY,
  data JSONB NOT NULL
);

INSERT INTO events (data) VALUES
  ('{"type": "click", "page": "/home", "duration": 3.5}'),
  ('{"type": "scroll", "page": "/about", "depth": 80}');
\`\`\`

### Accessing JSON Fields

PostgreSQL provides two key operators for extracting values from JSON:

| Operator | Returns | Example |
|----------|---------|---------|
| \`->\` | JSON value | \`data -> 'page'\` returns \`"/home"\` (with quotes) |
| \`->>\` | Text value | \`data ->> 'page'\` returns \`/home\` (without quotes) |

\`\`\`sql
SELECT data -> 'type' AS type_json, data ->> 'type' AS type_text
FROM events;
\`\`\`

Use \`->\` when you need to chain access into nested objects. Use \`->>\` when you need the final value as text:

\`\`\`sql
-- Nested access
SELECT data -> 'metadata' ->> 'source' AS source
FROM events;
\`\`\`

### JSON Containment with @>

The \`@>\` operator checks if the left JSON value contains the right JSON value:

\`\`\`sql
SELECT * FROM events
WHERE data @> '{"type": "click"}';
\`\`\`

This returns all events where the \`type\` field is \`"click"\`. Containment checks are efficient with a GIN index on the JSONB column.

### Key Existence with ?

The \`?\` operator checks whether a key exists in a JSON object:

\`\`\`sql
SELECT * FROM events
WHERE data ? 'duration';
\`\`\`

This returns only events that have a \`duration\` key in their JSON data.

### Building JSON from Rows

PostgreSQL can convert table rows into JSON:

\`\`\`sql
SELECT row_to_json(p) FROM products p;
\`\`\`

This converts each product row into a JSON object. You can also build JSON objects selectively:

\`\`\`sql
SELECT json_build_object('product', name, 'cost', price)
FROM products;
\`\`\`

## Arrays in PostgreSQL

PostgreSQL supports array columns natively. An array stores multiple values of the same type in a single column.

### Defining Array Columns

\`\`\`sql
CREATE TABLE articles (
  id SERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  tags TEXT[] NOT NULL
);

INSERT INTO articles (title, tags) VALUES
  ('PostgreSQL Tips', ARRAY['database', 'postgres', 'sql']),
  ('Web Development', '{"javascript", "html", "css"}');
\`\`\`

You can use either the \`ARRAY[...]\` constructor or the \`'{...}'\` literal syntax.

### Querying Arrays with ANY

\`ANY()\` checks if a value matches any element in an array:

\`\`\`sql
SELECT * FROM articles
WHERE 'postgres' = ANY(tags);
\`\`\`

### Array Containment with @>

The \`@>\` operator works with arrays too. It checks if the left array contains all elements of the right array:

\`\`\`sql
SELECT * FROM articles
WHERE tags @> ARRAY['sql', 'database'];
\`\`\`

This finds articles whose \`tags\` array includes both \`'sql'\` and \`'database'\`.

### Expanding Arrays with unnest

\`unnest()\` converts an array into a set of rows:

\`\`\`sql
SELECT title, unnest(tags) AS tag
FROM articles;
\`\`\`

This produces one row per tag. If an article has three tags, it appears three times in the output.

### Building Arrays with array_agg

\`array_agg()\` is an aggregate function that collects values into an array:

\`\`\`sql
SELECT category, array_agg(name ORDER BY name) AS product_names
FROM products
GROUP BY category;
\`\`\`

This groups products by category and collects the product names into an array for each category.

## Full-Text Search (Brief Overview)

PostgreSQL has built-in full-text search capabilities using two specialized types:

| Type | Purpose |
|------|---------|
| \`tsvector\` | A sorted list of normalized words (lexemes) from a document |
| \`tsquery\` | A search pattern with boolean operators |

\`\`\`sql
SELECT *
FROM articles
WHERE to_tsvector('english', title) @@ to_tsquery('english', 'postgresql');
\`\`\`

The \`@@\` operator matches a \`tsvector\` against a \`tsquery\`. For production use, store the \`tsvector\` in a column and create a GIN index on it for fast full-text search.

> **Tip:** JSON, arrays, and full-text search become much more powerful when combined with the right indexes. A GIN index on a JSONB column enables fast containment checks (\`@>\`) and key existence queries (\`?\`).

### Your Task

Select the name from each product as JSON using \`row_to_json\`.`,

  starterCode: `-- Select each product row as JSON
SELECT row_to_json(p)
FROM`,

  solution: `SELECT row_to_json(p)
FROM products p;`,

  tests: [
    {
      name: "returns products as JSON",
      expected: '{"type":"custom"}',
    },
  ],
};
