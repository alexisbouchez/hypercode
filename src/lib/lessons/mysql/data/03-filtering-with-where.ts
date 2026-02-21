import type { Lesson } from "../../types";

export const filteringWithWhere: Lesson = {
  id: "filtering-with-where",
  title: "Filtering with WHERE",
  chapterId: "getting-started",
  content: `## The WHERE Clause

\`WHERE\` filters rows before they are returned. Only rows where the condition is true are included in the result.

\`\`\`sql
SELECT * FROM products WHERE category = 'Electronics';
\`\`\`

### Comparison Operators

| Operator | Meaning | Example |
|----------|---------|---------|
| \`=\` | Equal | \`category = 'Electronics'\` |
| \`!=\` or \`<>\` | Not equal | \`category != 'Office'\` |
| \`>\` | Greater than | \`price > 100\` |
| \`>=\` | Greater than or equal | \`price >= 99.99\` |
| \`<\` | Less than | \`price < 50\` |
| \`<=\` | Less than or equal | \`stock <= 100\` |

### Logical Operators: AND, OR, NOT

Combine conditions with logical operators:

\`\`\`sql
-- Both conditions must be true
SELECT * FROM products
WHERE category = 'Electronics' AND price < 100;

-- At least one condition must be true
SELECT * FROM products
WHERE category = 'Kitchen' OR price < 10;

-- Negate a condition
SELECT * FROM products
WHERE NOT category = 'Office';
\`\`\`

Use parentheses to control precedence. \`AND\` binds more tightly than \`OR\`:

\`\`\`sql
-- This: (category = 'Electronics' AND price < 100) OR category = 'Kitchen'
SELECT * FROM products
WHERE category = 'Electronics' AND price < 100
   OR category = 'Kitchen';

-- vs this: category = 'Electronics' AND (price < 100 OR category = 'Kitchen')
SELECT * FROM products
WHERE category = 'Electronics'
  AND (price < 100 OR category = 'Kitchen');
\`\`\`

### BETWEEN — Range Conditions

\`BETWEEN\` is a shortcut for range checks (inclusive on both ends):

\`\`\`sql
SELECT * FROM products WHERE price BETWEEN 20 AND 60;
-- Equivalent to: WHERE price >= 20 AND price <= 60
\`\`\`

### IN — Matching a List

\`IN\` checks if a value is in a list:

\`\`\`sql
SELECT * FROM products
WHERE category IN ('Electronics', 'Kitchen');

-- Equivalent to:
-- WHERE category = 'Electronics' OR category = 'Kitchen'
\`\`\`

### LIKE — Pattern Matching

\`LIKE\` matches string patterns. Use \`%\` for any sequence of characters and \`_\` for exactly one character:

\`\`\`sql
SELECT * FROM products WHERE name LIKE 'Head%';   -- starts with Head
SELECT * FROM products WHERE name LIKE '%er';      -- ends with er
SELECT * FROM products WHERE name LIKE '%Book%';  -- contains Book
\`\`\`

### Your Task

Find all products that are in the \`'Electronics'\` category **or** have a price below \`20\`. Return the \`name\` and \`price\` columns.`,

  starterCode: `-- Find Electronics products or products under $20
SELECT name, price
FROM products
WHERE`,

  solution: `SELECT name, price
FROM products
WHERE category = 'Electronics' OR price < 20;`,

  tests: [
    {
      name: "returns 6 products",
      expected: '{"type":"rowCount","value":6}',
    },
    {
      name: "returns name and price columns",
      expected: '{"type":"contains","columns":["name","price"]}',
    },
    {
      name: "includes Laptop (Electronics)",
      expected: '{"type":"contains","value":"Laptop"}',
    },
    {
      name: "includes Notebook (price < $20)",
      expected: '{"type":"contains","value":"Notebook"}',
    },
  ],
};
