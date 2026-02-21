import type { Lesson } from "../../types";

export const jsonFunctions: Lesson = {
  id: "json-functions",
  title: "JSON Functions",
  chapterId: "sqlite-features",
  content: `## JSON in SQLite

SQLite has built-in JSON functions since version 3.38.0 (2022). JSON data is stored as text, and these functions let you extract and manipulate it within SQL queries.

### json_extract()

Extract a value from a JSON string using a path expression:

\`\`\`sql
SELECT json_extract('{"name":"Alice","age":30}', '$.name');
-- Returns: Alice

SELECT json_extract('{"user":{"id":1,"role":"admin"}}', '$.user.role');
-- Returns: admin

SELECT json_extract('[10,20,30]', '$[1]');
-- Returns: 20 (zero-indexed)
\`\`\`

The \`$\` refers to the root of the document. Use \`.key\` for object keys and \`[n]\` for array indices.

### json_object()

Build a JSON object from key-value pairs:

\`\`\`sql
SELECT json_object('name', 'Alice', 'age', 30);
-- Returns: {"name":"Alice","age":30}

-- Build JSON from table data
SELECT json_object('id', id, 'name', name, 'price', price)
FROM products
LIMIT 3;
\`\`\`

### json_array()

Build a JSON array from values:

\`\`\`sql
SELECT json_array(1, 2, 3);
-- Returns: [1,2,3]

SELECT json_array('a', 'b', NULL, 42);
-- Returns: ["a","b",null,42]
\`\`\`

### json_each()

Treat a JSON array as a table of rows — a **table-valued function**:

\`\`\`sql
SELECT value FROM json_each('[10, 20, 30, 40]');
-- Returns 4 rows: 10, 20, 30, 40

SELECT key, value FROM json_each('{"x":1,"y":2,"z":3}');
-- Returns: x/1, y/2, z/3
\`\`\`

This is useful for unnesting JSON arrays stored in columns:

\`\`\`sql
-- Suppose products had a JSON tags column:
-- SELECT p.name, t.value AS tag
-- FROM products p, json_each(p.tags) t;
\`\`\`

### json_type()

Returns the JSON type of a value or path:

\`\`\`sql
SELECT json_type('{"x":1}', '$.x');   -- 'integer'
SELECT json_type('[1,2,3]', '$[0]');   -- 'integer'
SELECT json_type('{"x":null}', '$.x'); -- 'null'
SELECT json_type('{"x":[]}', '$.x');   -- 'array'
\`\`\`

### Practical Example

\`\`\`sql
-- Build a JSON summary of each product category
SELECT
  category,
  json_object(
    'category', category,
    'count', COUNT(*),
    'avg_price', ROUND(AVG(price), 2)
  ) AS summary
FROM products
GROUP BY category;
\`\`\`

### Your Task

Use \`json_object()\` to return each product as a JSON object with keys \`name\` and \`price\`. Name the result column \`product_json\`.`,

  starterCode: `-- Return each product as a JSON object
SELECT json_object(
  'name', name,
  'price',
) AS product_json
FROM products;`,

  solution: `SELECT json_object(
  'name', name,
  'price', price
) AS product_json
FROM products;`,

  tests: [
    {
      name: "returns product_json column",
      expected: '{"type":"contains","columns":["product_json"]}',
    },
    {
      name: "returns all 8 products",
      expected: '{"type":"rowCount","value":8}',
    },
  ],
};
